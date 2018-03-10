
let arg_print_mute = ref 0
let arg_print_reg = ref true
let arg_print_profiling = ref false
let arg_print_stmt = ref true
let arg_print_assign = ref false
let arg_print_func = ref true
let arg_print_stp = ref false
let arg_print_guard = ref false
let arg_print_debug = ref false
let arg_print_error = ref true
let arg_print_branch = ref true
let arg_print_report = ref true
let arg_print_mustprint = ref true


(* type annotations are needed because a formatter is stored in a ref cell below; it would be nice to get rid of that
   ref cell eventually. *)
type color = [ `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan | `White ]
type term = [ `Color of color | `Reset ]


let is_console () =
    (* detecting terminal capabilities is serious black magic; this will at least catch Emacs' shell mode *)
    Unix.isatty Unix.stdin && Unix.isatty Unix.stdout && (try Unix.getenv "TERM" <> "dumb" with Not_found -> false)


let set_term ff (term : term) =
    (* see http://invisible-island.net/xterm/ctlseqs/ctlseqs.html for a list of xterm control sequences *)
    if (try ignore (Unix.getenv "CLICOLOR_FORCE"); true with Not_found -> false) || is_console () then
        let term_string = match term with
            | `Color `Black -> "[0;30m"
            | `Color `Red -> "[0;31m"
            | `Color `Green -> "[0;32m"
            | `Color `Yellow -> "[0;33m"
            | `Color `Blue -> "[0;34m"
            | `Color `Magenta -> "[0;35m"
            | `Color `Cyan -> "[0;36m"
            | `Color `White -> "[0;37m"
            | `Reset -> "[0m"
        in
        Format.pp_print_as ff 0 "\027";
        Format.pp_print_as ff 0 term_string
    else
        ()


let get_console_size =
    (* probe the console only once, since using xterm escape sequences to do so isn't very robust, and is noticeably slow *)
    let default_size = (24, 80) in
    let size =
        if is_console () then
            (* if on a terminal, use xterm escape sequence to query for column size *)
            let attr = Unix.tcgetattr Unix.stdin in
            try
                (* turn off line buffering, echoing, set the minimum characters for read to return and the
                   read timeout on stdin and flush the input *)
                Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
                    { attr with Unix.c_icanon = false; Unix.c_echo = false; Unix.c_vmin = 0; Unix.c_vtime = 1 };
                Unix.tcflush Unix.stdin Unix.TCIFLUSH;
                (* send the "query screen size" escape sequence *)
                ignore (Unix.write Unix.stdout "\027[18t" 0 5);
                (* read the result *)
                let s = String.create 16 in
                ignore (Unix.read Unix.stdin s 0 16);
                (* restore the state of stdin *)
                Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
                (* parse the result *)
                Scanf.sscanf s "\027[8;%d;%dt" begin fun h w ->
                    (h, w)
                end
            with e ->
                (* whatever exceptions arise, restore the terminal and flush it *)
                Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
                Unix.tcflush Unix.stdin Unix.TCIFLUSH;
                match e with
                    | Unix.Unix_error _ | Scanf.Scan_failure _ ->
                        (* if the exception was from here, return the an old result which may still be right *)
                        default_size
                    | e ->
                        raise e
        else
            default_size
    in
    fun () -> size

let get_console_width = ref (fun () -> snd (get_console_size ()))


class virtual ['self] t =
    object (self : 'self)
        val virtual formatter : Format.formatter

        method printf : 'a . ?term:'term list -> ('a, Format.formatter, unit) format -> 'a = fun ?(term=[]) ->
            try
                List.iter (set_term formatter) term;
                Format.kfprintf (fun ff -> set_term ff `Reset) formatter
            with e ->
                set_term formatter `Reset;
                raise e

        method kprintf : 'a 'b . ('self -> 'a) -> ?term:'term list -> ('b, Format.formatter, unit, 'a) format4 -> 'b = fun k ?(term=[]) ->
            try
                List.iter (set_term formatter) term;
                Format.kfprintf (fun ff -> set_term ff `Reset; k self) formatter
            with e ->
                set_term formatter `Reset;
                raise e

        method flush =
            Format.pp_print_flush formatter ()
    end

class ['self] plain =
	object (self : 'self)
		inherit ['self] t
		val formatter =
			(* flush after every write *)
			Format.make_formatter (fun str pos len -> output stdout str pos len; flush stdout) (fun () -> ())
	end

class ['self] labeled label =
	object (self : 'self)
		inherit ['self] t
		val formatter =
			(* flush after every line, prefixing each line with a label *)
			let width = (!get_console_width) () in
			let buffer = Buffer.create width in
			let rec labeled_output str pos len =
				let newline_index = 1 + try String.index_from str pos '\n' - pos with Not_found -> len in
				if newline_index <= len then begin
					(* print the label *)
					Pervasives.output_string stdout label;
					(* print the buffer *)
					Buffer.output_buffer stdout buffer;
					Buffer.clear buffer;
					(* print the new string up to the end of the line *)
					Pervasives.output stdout str pos newline_index;
					Pervasives.flush stdout;
					(* recurse on the remainder *)
					labeled_output str (newline_index + pos) (len - newline_index)
				end else begin
					Buffer.add_substring buffer str pos len;
				end
			in
			let formatter = Format.make_formatter labeled_output (fun () -> ()) in
			(* adjust the margin to account for the length of the label *)
			Format.pp_set_margin formatter (width - String.length label);
			formatter
	end

let formatter = ref (new plain)

let set_formatter ff =
	!formatter#flush;
	formatter := ff

let () = at_exit (fun () -> !formatter#flush)

type msg_type =
    | MSG_REG
    | MSG_PROFILING
    | MSG_STMT
    | MSG_ASSIGN
    | MSG_FUNC
    | MSG_STP
    | MSG_GUARD
    | MSG_DEBUG
    | MSG_ERROR
    | MSG_BRANCH
    | MSG_REPORT
    | MSG_MUSTPRINT

let current_msg_type = ref MSG_REG
let set_mode msg_type = current_msg_type := msg_type
let get_mode () = !current_msg_type

let need_print msg_type =
    if !arg_print_mute > 0 then false else
    match msg_type with
        | MSG_REG          -> !arg_print_reg
        | MSG_PROFILING    -> !arg_print_profiling
        | MSG_STMT         -> !arg_print_stmt
        | MSG_ASSIGN       -> !arg_print_assign
        | MSG_FUNC         -> !arg_print_func
        | MSG_STP          -> !arg_print_stp
        | MSG_GUARD        -> !arg_print_guard
        | MSG_ERROR        -> !arg_print_error
        | MSG_DEBUG        -> !arg_print_debug
        | MSG_BRANCH       -> !arg_print_branch
        | MSG_REPORT       -> !arg_print_report
        | MSG_MUSTPRINT   -> !arg_print_mustprint


let printf ?term format =
	if (need_print (!current_msg_type)) then
		!formatter#printf ?term format
	else
		Format.ifprintf Format.std_formatter format

let kprintf k ?term format =
	if (need_print (!current_msg_type)) then
		!formatter#kprintf k ?term format
 	else
		Format.ikfprintf (fun _ -> k !formatter) Format.std_formatter format

let must_printf ?(term=[]) format =
    let old_mode = get_mode () in
    set_mode MSG_MUSTPRINT;
    kprintf (fun _ -> set_mode old_mode) ~term:(`Color `Red::term) format

let debug_printf ?(term=[]) format =
    let old_mode = get_mode () in
    set_mode MSG_DEBUG;
    kprintf (fun _ -> set_mode old_mode) ~term:(`Color `Cyan::term) format


let mprint_formatter =
  Format.make_formatter
    (fun  str pos len -> output stdout str pos len; flush stdout)
    (fun () -> ())
let mprintf format = Format.fprintf mprint_formatter format

let options = [
	(* TODO: for each msg type, a --print and --noprint option*)
	(* STP *)
	("--printSTP",
		Arg.Set arg_print_stp,
		" Print STP programs");

	(* Assignment in the form lval = rval *)
	("--printAssign",
		Arg.Set arg_print_assign,
		" Print assignments (from rval to lval)");

	("--printFunctionCall",
		Arg.Set arg_print_func,
		" Print function calls");

	(* Print the guard of an if statement *)
	("--printIf",
		Arg.Set arg_print_guard,
		" Print the guard of an if statement");

	(* Sparse printing *)
	("--printLittle",
		Arg.Unit (fun () ->
			arg_print_reg := false;
			arg_print_stmt := false;
			arg_print_func := false;
			arg_print_assign := false;
		),
		" Suppress most output");

	(* Even sparser printing *)
	("--printErrorsOnly",
		Arg.Unit (fun () ->
            arg_print_reg := false;
            arg_print_profiling := false;
            arg_print_stmt := false;
            arg_print_assign := false;
            arg_print_func := false;
            arg_print_stp := false;
            arg_print_guard := false;
            arg_print_debug := false;
            arg_print_branch := false;
            arg_print_report := false;
            arg_print_error := true;
            arg_print_mustprint := true;
		),
		" Suppress all output except errors and mustprints");

	("--printNothing",
		Arg.Unit (fun () -> arg_print_mute := 1),
		" Suppress (pretty much) all output. This trumps all other --print* options");

	("--console-width",
		Arg.Int (fun width -> get_console_width := fun () -> width),
		" Set the console with (if not set, the width is auto-detected)");
]
