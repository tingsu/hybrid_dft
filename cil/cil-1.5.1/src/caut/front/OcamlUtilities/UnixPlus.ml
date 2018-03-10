(** Useful extensions to the {!Unix} module. *)


(** {2 Forking Execution} *)

exception ForkCallException of exn
exception ForkCallTimedOut
exception ForkCallFailure of exn
exception ForkCallExited of int
exception ForkCallKilled of int
(**/**) (* should never occur *)
exception ForkCallStopped of int
(**/**)

(**/**)
let marshal_failure_re = Str.regexp "\\(input_value\\|Marshal\\)"
(**/**)

(** Call a function in a forked process and return the result. Note that {!Format.std_formatter} and
    {!Format.err_formatter} are reset in the forked process.
        @param time_limit optionally specifies the maximum time the forked process is allowed to run
                (note that this uses [Unix.setitimer Unix.ITIMER_REAL])
        @param f is the function to call

        @return the result of [f ()]

        @raise ForkCallException if [f ()] raises an exception; {e note that the raised exception cannot be
                pattern-matched due to a limitation in the Ocaml runtime (see http://caml.inria.fr/mantis/view.php?id=1624)}
        @raise ForkCallTimedOut if the forked process exceeds the time limit
        @raise ForkCallFailure if the forked process failed to launch or did not return any results
        @raise ForkCallExited if the forked process exited unexpectedly
        @raise ForkCallKilled if the forked process was killed unexpectedly
*)
let fork_call ?time_limit:time_limit_opt (f : (unit -> 'a)) : 'a =
    (* first, flush stdout/stderr to avoid printing twice *)
    flush stdout;
    flush stderr;

    (* create a pipe to proxy the result from child to parent *)
    let fdin, fdout = Unix.pipe () in

    let child = try
        Unix.fork ()
    with e ->
        raise (ForkCallFailure e)
    in
    if child = 0 then begin
        (* reset Format.std_formatter/Format.err_formatter *)
        List.iter begin fun ff ->
            let output, flush = Format.pp_get_formatter_output_functions ff () in
            Format.pp_set_formatter_output_functions ff (fun _ _ _ -> ()) (fun _ -> ());
            Format.pp_print_flush ff ();
            Format.pp_set_formatter_output_functions ff output flush;
        end [ Format.std_formatter; Format.err_formatter ];

        (* child process runs the function and proxies the result to the parent *)
        Unix.close fdin;

        (* set an alarm if given a time limit; just overwrite sigalrm since this child will exit anyway *)
        begin match time_limit_opt with
            | Some time_limit ->
                Sys.set_signal Sys.sigalrm Sys.Signal_default;
                ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; Unix.it_value = time_limit; })
            | None ->
                ()
        end;

        let result = try `Result (f ()) with e -> `Exception e in
        Marshal.to_channel (Unix.out_channel_of_descr fdout) result [ Marshal.Closures ];
        exit 0
    end else begin
        (* parent process waits for child, and captures the result *)
        Unix.close fdout;

        (* get the result *)
        let result = try
            (Marshal.from_channel (Unix.in_channel_of_descr fdin) : [> `Result of 'b | `Exception of exn ])
        with e ->
            (* kill the child *)
            Unix.kill child Sys.sigterm;
            (* catch only exception that arise from Marshal.from_channel *)
            match e with
                | End_of_file -> `Failure e
                | Failure s when Str.string_match marshal_failure_re s 0 -> `Failure e
                | e -> raise e
        in

        (* make sure to not exhaust file descriptors *)
        Unix.close fdin;

        (* get the child's exit status *)
        let _, status = Unix.waitpid [] child in
        match status, result with
            | Unix.WEXITED 0, `Result res ->
                res
            | Unix.WEXITED 0, `Exception e ->
                (* Marshal does not serialize exceptions faithfully: http://caml.inria.fr/mantis/view.php?id=1624 *)
                (* TODO: provide a helper to match exception by label, using the generic printer trick in Printexc *)
                raise (ForkCallException e)
            | Unix.WSIGNALED i, _ when i = Sys.sigalrm ->
                raise ForkCallTimedOut
            | _, `Failure e ->
                raise (ForkCallFailure e)
            | Unix.WEXITED i, _ ->
                raise (ForkCallExited i)
            | Unix.WSIGNALED i, _ ->
                raise (ForkCallKilled i)
            | Unix.WSTOPPED i, _ ->
                (* this should never occur since waitpid wasn't given the WUNTRACED flag *)
                raise (ForkCallStopped i)
    end

let mkdir_p path file_perm =
    let rec ex path =
        (Filename.basename path) :: (
            match Filename.dirname path with
            | "." -> [ "." ]
            | "/" -> [ "/" ]
            | d -> ex d
        )
    in
    let dirs = List.rev (ex path) in
    let cwd = Unix.getcwd () in
    let rec mkdir_p = function
        | [] -> ()
        | "." :: dirs -> mkdir_p dirs
        | "/" :: dirs -> Unix.chdir "/"; mkdir_p dirs
        | ".." :: dirs -> Unix.chdir ".."; mkdir_p dirs
        | d :: dirs -> (try Unix.mkdir d file_perm with Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> () | e -> raise e); Unix.chdir d; mkdir_p dirs
    in
    mkdir_p dirs;
    Unix.chdir cwd
