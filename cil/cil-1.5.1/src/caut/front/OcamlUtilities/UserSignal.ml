(** Useful signal handlers. *)

(** Raised by {!using_signals} upon Ctrl-C *)
exception UserInterrupt

(** Raised by {!using_signals} upon timeout, if given *)
exception TimedOut

let default_timeout = ref 0


(** Run a function with useful signal handlers:
        - SIGINT from Ctrl-C raises {!UserInterrupt}
        - SIGALRM from an optional timeout raises {!TimedOut}
        - SIGQUIT from Ctrl-\ prints a backtrace
        - SIGUSR1 user-specific signal handler
*)
let using_signals ?(timeout=(!default_timeout)) ?(usr1_handler=(fun _ -> ())) f =
    (* first, get the original handlers; they can't be taken below since reset requires it *)
    let old_ALRM_handler_opt =
        if timeout < 0 then
            invalid_arg "timeout must not be negative"
        else if timeout > 0 then
            Some (Sys.signal Sys.sigalrm Sys.Signal_ignore)
        else
            None
    in
    let old_INT_handler = Sys.signal Sys.sigint Sys.Signal_ignore in
    let old_QUIT_handler = Sys.signal Sys.sigquit Sys.Signal_ignore in
    let old_USR1_handler = Sys.signal Sys.sigusr1 Sys.Signal_ignore in

    (* clean up after any signal, or just before returning *)
    let reset () =
        begin match old_ALRM_handler_opt with
            | Some old_ALRM_handler ->
                ignore (Unix.alarm 0);
                Sys.set_signal Sys.sigalrm old_ALRM_handler;
            | None ->
                ()
        end;
        Sys.set_signal Sys.sigint old_INT_handler;
        Sys.set_signal Sys.sigint old_QUIT_handler;
        Sys.set_signal Sys.sigint old_USR1_handler
    in
    let handle exn =
        Sys.Signal_handle (fun _ -> reset (); raise exn)
    in

    (* raise TimedOut upon a timeout *)
    begin match old_ALRM_handler_opt with
        | Some _ ->
            Sys.set_signal Sys.sigalrm (handle TimedOut);
            ignore (Unix.alarm timeout);
        | None ->
            ()
    end;

    (* raise UserInterrupt upon Ctrl-C *)
    Sys.set_signal Sys.sigint (handle UserInterrupt);

    (* print a stack trace upon Ctrl-\ *)
    Sys.set_signal Sys.sigquit begin Sys.Signal_handle begin fun _ ->
        let child = Unix.fork () in
        if child = 0 then begin
            (* move stderr to a different file descriptor and redirect the original to /dev/null, to avoid printing
             * any unflushed output as well as the standard "Fatal error" message from the uncaught exception below *)
            let stderr = Unix.out_channel_of_descr (Unix.dup Unix.stderr) in
            let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
            Unix.dup2 null Unix.stdout;
            Unix.dup2 null Unix.stderr;
            (* start recording, raise an (uncaught) exception, and print the recorded backtrace; use a local exception
             * so that it cannot be caught anywhere else *)
            Printexc.record_backtrace true;
            let module M = struct exception StackTrace end in
            at_exit begin fun () ->
                output_string stderr "UserSignal.StackTrace\n";
                Printexc.print_backtrace stderr
            end;
            raise M.StackTrace
        end else
            ignore (Unix.waitpid [] child)
    end end;

    (* USR1 *)
    Sys.set_signal Sys.sigusr1 begin Sys.Signal_handle usr1_handler end;

    let x = f () in
    reset ();
    x


let options = [
	("--timeout",
		Arg.Set_int default_timeout,
		"<time-in-seconds> Set a timeout");
]

