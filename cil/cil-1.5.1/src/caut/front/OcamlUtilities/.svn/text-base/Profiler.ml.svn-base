
module Label = String
module Context = struct type t = Label.t list let compare = Pervasives.compare end

module LabelMap = Map.Make (Label)
module ContextMap = Map.Make (struct type t = string list let compare = Pervasives.compare end)

exception ProfilerCallException of Context.t * exn 
exception Empty

class virtual ['self] abstract =
    object (self : 'self)
        val virtual indent : int
        val virtual record_width : int
        method virtual checkpoint : 'checkpoint
        method virtual record_order : ([ `Some of 'record | `None ] as 'record_opt) -> 'record_opt -> int
        method virtual header_printer : Format.formatter -> unit

        val context = []
        val checkpoints = []
        val records = ContextMap.empty

        method records = records

        method push label =
            {<
                context = label::context;
                checkpoints = (self#checkpoint)::checkpoints
            >}

        method pop label =
            match context, checkpoints with
                | label'::context', checkpoint::checkpoints' ->
                    if label <> label' then
                        FormatPlus.invalid_arg "Cannot pop profiling context %s: top-most context is %s" label label';
                    let record = checkpoint#measure in
                    let record =
                        try
                            record#add (ContextMap.find context records)
                        with Not_found ->
                            record
                    in
                    {<
                        context = context';
                        checkpoints = checkpoints';
                        records = ContextMap.add context record records;
                    >}
                | _, _ ->
                    raise Empty

        method save =
            List.rev context

        method restore target =
            let rec restore pop push = match pop, push with
                | p::pop, q::push when p = q ->
                    restore pop push
                | pop, push ->
                    let self = List.fold_left (fun self label -> self#pop label) self (List.rev pop) in
                    let self = List.fold_left (fun self label -> self#push label) self push in
                    self
            in
            restore (List.rev context) target


        method call : 'a . string -> ?catch:(exn -> 'self -> 'self * 'a) -> ('self -> 'self * 'a) -> ('self * 'a) = fun label ?catch:catch_opt f ->
            let self = self#push label in
            try
                let self, x = f self in
                (self#pop label, x)
            with
                | ProfilerCallException (context, exn) ->
                    raise (ProfilerCallException (label::context, exn))
                | exn ->
                    match catch_opt with
                        | Some catch ->
                            catch exn (self#pop label)
                        | None ->
                            raise (ProfilerCallException (label::[], exn))

        method finish =
            match context with
                | label::_ -> (self#pop label)#finish
                | [] -> self

        method reset =
            {< context = []; checkpoints = []; records = ContextMap.empty >}

        method add (other : 'self) =
            let records = ContextMap.fold begin fun context record records ->
                let record =
                    try
                        record#add (ContextMap.find context records)
                    with Not_found ->
                        record
                in
                ContextMap.add context record records
            end other#records records in
            {< records = records >}

        method is_empty =
            ContextMap.is_empty records

        method private table_printer width ff data_printer =
            let width = min (width + indent) (Format.pp_get_margin ff () - record_width - 10) in

            Format.pp_open_tbox ff ();

            Format.fprintf ff "%*s" width "";
            Format.pp_set_tab ff ();
            self#header_printer ff;

            data_printer
                (fun indent label -> Format.fprintf ff "%*s%s" indent "" label)
                (fun indent label -> Format.fprintf ff "%*s%s%s" indent "" label (String.make (max 0 (width - String.length label - indent)) '.'));

            Format.pp_close_tbox ff ()

        method flat_printer ff =
            let flat, width = ContextMap.fold begin fun context record (flat, width) ->
                let label = List.hd context in
                let flat =
                    try
                        let total = LabelMap.find label flat in
                        let total = total#add record in
                        LabelMap.add label total flat
                    with Not_found ->
                        LabelMap.add label record flat
                in
                (flat, max width (String.length label))
            end records (LabelMap.empty, 0) in

            let flat = LabelMap.fold (fun label record flat -> (label, record)::flat) flat [] in
            let sorted = List.fast_sort (fun (_, x) (_, y) -> self#record_order (`Some x) (`Some y)) flat in

            self#table_printer width ff begin fun undotted dotted ->
                ignore begin List.fold_left begin fun n (label, total) ->
                    if n > 0 then Format.pp_force_newline ff ();
                    if n mod 5 = 4 then
                        dotted 0 label
                    else
                        undotted 0 label;
                    Format.pp_print_tab ff ();
                    total#printer ff;
                    n + 1
                end 0 sorted end
            end

        method private tree_printer width ff tree =
            self#table_printer width ff begin fun undotted dotted ->
                let rec printer n level children =
                    let children = LabelMap.fold (fun label (`Tree (children', total_opt)) children -> ((label, children'), total_opt)::children) children [] in
                    let sorted = List.fast_sort (fun (_, x) (_, y) -> self#record_order x y) children in

                    List.fold_left begin fun n ((label, children), total_opt) ->
                        if n > 0 then Format.pp_force_newline ff ();
                        undotted (level * indent) label;
                        Format.pp_print_tab ff ();
                        begin match total_opt with
                            | `Some total -> total#printer ff
                            | `None -> ()
                        end;
                        let n = printer (n + 1) (level + 1) children in
                        n + 1
                    end n sorted
                in
                ignore (printer 0 0 tree)
            end

        method bottom_up_printer ff =
            let organize context record (children, width) =
                let rec organize context record children =
                    match context with
                        | label::rest ->
                            let children', total =
                                try
                                    let `Tree (children', `Some record') = LabelMap.find label children in
                                    (children', record#add record')
                                with Not_found ->
                                    (LabelMap.empty, record)
                            in
                            let children', width = organize rest record children' in
                            (LabelMap.add label (`Tree (children', `Some total)) children, max (String.length label) (width + indent))
                        | [] ->
                            (children, 0)
                in
                let tree, width' = organize context record children in
                (tree, max width width')
            in
            let tree, width = ContextMap.fold organize records (LabelMap.empty, 0) in
            self#tree_printer width ff (tree : [ `Tree of 'x * [ `Some of _ ] ] LabelMap.t as 'x :> [ `Tree of 'y * [ `None | `Some of _ ] ] LabelMap.t as 'y)

        method top_down_printer ff =
            let organize context record (children, width) =
                let rec organize context record children =
                    match context with
                        | label::rest ->
                            let `Tree (children', record_opt') =
                                try
                                    LabelMap.find label children
                                with Not_found ->
                                    `Tree (LabelMap.empty, `None)
                            in
                            if rest = [] then
                                (LabelMap.add label (`Tree (children', `Some record)) children, String.length label)
                            else
                                let children', width = organize rest record children' in
                                (LabelMap.add label (`Tree (children', record_opt')) children, max (String.length label) (width + indent))
                        | [] ->
                            failwith "Impossible!"
                in
                let tree, width' = organize (List.rev context) record children in
                (tree, max width width')
            in
            let tree, width = ContextMap.fold organize records (LabelMap.empty, 0) in
            self#tree_printer width ff tree

        method printer ff =
            if self#is_empty then
                Format.fprintf ff "Empty profile@\n"
            else begin
                Format.fprintf ff "Flat profile:@\n  @[%t@]@\n@\n" self#flat_printer;
                Format.fprintf ff "Bottom-up profile:@\n  @[%t@]@\n@\n" self#bottom_up_printer;
                Format.fprintf ff "Top-down profile:@\n  @[%t@]@\n@\n" self#top_down_printer
            end
    end


class virtual record =
    object (self : 'self)
        val virtual count : int
        val virtual user : float
        val virtual system : float
        val virtual allocated_bytes : float

        method count = count
        method user = user
        method system = system
        method allocated_bytes = allocated_bytes

        method add (other : 'self) = {<
            count = self#count + other#count;
            user = self#user +. other#user;
            system = self#system +. other#system;
            allocated_bytes = self#allocated_bytes +. other#allocated_bytes;
        >}

        method printer ff =
            Format.fprintf ff " %8d %8.2f %5.2f"
                count user system;
            if allocated_bytes >= 1e9 then
                Format.fprintf ff " %7.2fG" (allocated_bytes /. 1e9)
            else if allocated_bytes >= 1e6 then
                Format.fprintf ff " %7.2fM" (allocated_bytes /. 1e6)
            else if allocated_bytes >= 1e3 then
                Format.fprintf ff " %7.2fk" (allocated_bytes /. 1e3)
            else
                Format.fprintf ff " %8.0f" allocated_bytes
    end


class checkpoint () =
    let times = Unix.times () in
    let allocated_bytes = Gc.allocated_bytes () in
    object
        val start_count = 0
        val start_user = times.Unix.tms_utime
        val start_system = times.Unix.tms_stime
        val start_allocated_bytes = allocated_bytes

        method measure =
            let stop_times = Unix.times () in
            let stop_allocated_bytes = Gc.allocated_bytes () in
            object
                inherit record
                val count = 1
                val user = stop_times.Unix.tms_utime -. start_user
                val system = stop_times.Unix.tms_stime -. start_system
                val allocated_bytes = stop_allocated_bytes -. start_allocated_bytes
            end
    end


class t =
    object (_ : 'self)
        inherit ['self] abstract
        val indent = 2
        val record_width = 33

        method checkpoint = new checkpoint ()
        method record_order x y = if x == y then 0 else match x, y with
            | `Some x, `Some y -> compare x#user y#user
            | `None, `Some _ -> -1
            | `Some _, `None -> 1
            | `None, `None -> 0

        method header_printer ff =
            Format.fprintf ff " %8s %8s %5s %8s@\n" "count" "user/s" "sys/s" "alloc/b"
    end

let do_profiling = ref false

let global =
    object (_ : 'self)
        val mutable actual_profiler = new t

        method call : 'a . string -> (unit -> 'a) -> 'a = fun label f ->
            if !do_profiling then
            begin
                actual_profiler <- actual_profiler#push label;
                try
                    let x = f () in
                    actual_profiler <- actual_profiler#pop label;
                    x
                with exn ->
                    actual_profiler <- actual_profiler#pop label;
                    raise exn
            end else
                f ()

        method add (other : #t) =
            if !do_profiling then actual_profiler <- actual_profiler#add other

        method save =
            actual_profiler#save

        method restore context =
            actual_profiler <- actual_profiler#restore context

        method flat_printer ff = if !do_profiling then actual_profiler#flat_printer ff
        method bottom_printer ff = if !do_profiling then actual_profiler#bottom_up_printer ff
        method top_down_printer ff = if !do_profiling then actual_profiler#top_down_printer ff
        method printer ff =
            if !do_profiling then begin
                actual_profiler#printer ff;

                let max_heap = float_of_int (Gc.quick_stat ()).Gc.top_heap_words *. float_of_int Sys.word_size in            
                Format.fprintf ff "Max heap: ";
                if max_heap >= 1e9 then
                    Format.fprintf ff " %7.2fG" (max_heap /. 1e9)
                else if max_heap >= 1e6 then
                    Format.fprintf ff " %7.2fM" (max_heap /. 1e6)
                else if max_heap >= 1e3 then
                    Format.fprintf ff " %7.2fk" (max_heap /. 1e3)
                else
                    Format.fprintf ff " %8.0f" max_heap;
                Format.fprintf ff "@\n"
            end else
                ()

    end

let options = [
	("--profile",
		Arg.Set do_profiling,
		" Gather and print timing and memory usage statistics");
	("--printProfiling",
		Arg.Set Output.arg_print_profiling,
		" Print verbose profiling statistics");
]
