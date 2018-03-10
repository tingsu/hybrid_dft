(** This module takes a context-free grammar and prints out C code that
    generates symbolic strings in that grammar. The generated strings can be
    given a maximum size by specifying the preprocessor variable
    MAX_GRAMMAR_STRING_LENGTH. If this is left undefined, no bound is placed on
    the generated strings, and all strings in the grammar will (eventually) be
    generated.

    Warning: left-recursive grammars may cause symbolic execution to fail to
    terminate.
*)

(** Reads in a grammar from the given input channel. *)
let parse_grammar_from_in_chan in_chan =
    let lexbuf = Lexing.from_channel in_chan in
    Grammar.input GrammarLexer.token lexbuf

(** Returns a set of all productions of all nonterminals *)
let all_productions grammar =
    GrammarTypes.NontermMap.fold
        (fun _ productions old_productions ->
             GrammarTypes.SetOfProductions.union productions old_productions)
        grammar
        GrammarTypes.SetOfProductions.empty

module StringSet = Set.Make(String)

(** Adds to a set of nonterminals the nonterminals used in a production *)
let add_nonterminals_from_production nonterm_set production =
    List.fold_left
        (fun new_nonterms next_production_elt ->
             match next_production_elt with
                 | GrammarTypes.Term _ -> new_nonterms
                 | GrammarTypes.Nonterm { GrammarTypes.name = name } ->
                       StringSet.add name new_nonterms)
        nonterm_set
        production

(** Returns a set of all nonterminals mentioned in any production in the
    grammar *)
let all_rhs_nonterms grammar =
    GrammarTypes.SetOfProductions.fold
        (fun production nonterm_names -> add_nonterminals_from_production nonterm_names production)
        (all_productions grammar)
        StringSet.empty

(** Prints code for generate_stringN *)
let print_generate_string n =
    Format.printf "const char *generate_string%d() {
    char *str = malloc(%d), c;
" n (n + 1);
    for i = 0 to (n - 1) do
        Format.printf "    __SYMBOLIC(&c); __ASSUME(c); str[%d] = c;\n" i
    done;
    Format.printf "    str[%d] = 0;
    USE(%d);
    return str;
}

" n n

(** Prints some helper code for grammar generation C code *)
let print_prelude () = Format.printf
"#ifdef MAX_GRAMMAR_STRING_LENGTH
int max_grammar_string_length = MAX_GRAMMAR_STRING_LENGTH;
#define USE(n) __ASSUME((max_grammar_string_length -= (n)) >= 0)
#else
#define USE (void)
#endif

#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

/** Concatenate all of the arguments after the first, and store the result in the first argument. */
void concat(char *dst, const char *string, ...) {
    va_list args;
    va_start(args, string);
    const char *next = string;
    while (next) {
        dst = stpcpy(dst, next);
        next = va_arg(args, const char *);
    }
    va_end(args);
}

"

(** Prints the code for generate_digit *)
let print_generate_digit () = Format.printf
"const char *generate_digit() {
    char *result = malloc(2);
    unsigned char byte;
    __SYMBOLIC(&byte);
    __ASSUME(byte >= '0');
    __ASSUME(byte <= '9');
    result[0] = byte;
    result[1] = 0;
    USE(1);
    return result;
}

"

(** Prints the code for generate_letter *)
let print_generate_letter () = Format.printf
"const char *generate_letter() {
    char *result = malloc(2);
    unsigned char byte;
    __SYMBOLIC(&byte);
    __ASSUME(byte >= 'A');
    __ASSUME(byte <= 'z');
    __ASSUME(OR(byte <= 'Z', byte >= 'a'));
    result[0] = byte;
    result[1] = 0;
    USE(1);
    return result;
}

"

(** Prints code for all built-in nonterminals that are used in the grammar. The
    current built-ins are "letter", "digit", and "stringN" for any positive
    N. *)
let print_builtins grammar =
    let nonterms_used = all_rhs_nonterms grammar in
    if StringSet.mem "letter" nonterms_used
    then print_generate_letter ();
    if StringSet.mem "digit" nonterms_used
    then print_generate_digit ();
    StringSet.iter
        (fun name ->
             if Str.string_match (Str.regexp "^string\\([0-9]*\\)$") name 0
             then print_generate_string (int_of_string (Str.matched_group 1 name));
        )
        nonterms_used

(** Prints declarations of the C functions for all nonterminals defined by the
    grammar *)
let print_declarations grammar =
    GrammarTypes.N.iter
        (fun { GrammarTypes.name = name } _ -> Format.printf "const char *generate_%s(void);\n" name)
        grammar;
    Format.printf "\n"

(** Like List.iter, but the function also takes the index of the element in the
    list. *)
let iteri f lst =
    ignore (List.fold_left (fun i x -> f i x; succ i) 0 lst)

(** Is this [either] a terminal? *)
let is_terminal = function GrammarTypes.Term _ -> true | GrammarTypes.Nonterm _ -> false

(** Is this production just a single terminal? *)
let is_single_terminal = function [x] -> is_terminal x | _ -> false

(** Returns the text of the terminal enclosed in quotation marks *)
let terminal_to_string { GrammarTypes.text = text } = Format.sprintf "\"%s\"" text

(** Returns the name of the nonterminal as a string *)
let nonterminal_to_string { GrammarTypes.name = name } = name

(** Prints code to expand all the given nonterminals and store the results in
    temp variables *)
let generate_nonterminals nonterminals =
    iteri
        (fun i nonterminal ->
             Format.printf "        temp%d = generate_%s();\n" i (nonterminal_to_string nonterminal))
        nonterminals

(** Returns the string 'sizeof("...")', where the ellipsis is the concatenation
    of all the terminals *)
let sizeof terminals =
    Format.sprintf "sizeof(\"%s\")"
        (String.concat ""
             (List.map (fun { GrammarTypes.text = text } -> text) terminals))

(** Prints code to allocate memory to hold the result of expanding the
    production with these terminals and nonterminals *)
let malloc_result terminals nonterminals =
    Format.printf "        result = malloc(";
    iteri (fun i _ -> Format.printf "strlen(temp%d) + " i) nonterminals;
    (* Find the total length of all terminals, plus terminating null byte. *)
    Format.printf "%s);\n" (sizeof terminals)

(** Prints code to write the production into the variable 'result' *)
let write_result production =
    Format.printf "        concat(result, ";
    ignore (List.fold_left
                (fun nonterminal_index next ->
                     (match next with
                          | GrammarTypes.Term terminal -> Format.printf "%s, " (terminal_to_string terminal)
                          | GrammarTypes.Nonterm _ -> Format.printf "temp%d, " nonterminal_index);
                     if is_terminal next then nonterminal_index else succ nonterminal_index) (* Increment the index of temporary variables only when reaching a nonterminal *)
                0
                production);
    Format.printf "0);\n"

(** Prints code to free all temporary variables *)
let free_temps nonterminals =
    iteri (fun i _ -> Format.printf "        free((void*)temp%d);\n" i) nonterminals

(** Prints code to expand all nonterminals in this production, allocate space to
    concatenate all the results and the terminals, write the result into that
    allocate memory, and free the temporary variables used *)
let print_common production =
    let terminals, nonterminals = List.partition is_terminal production in
    let terminals = List.map (function GrammarTypes.Term t -> t | _ -> assert false) terminals
    and nonterminals = List.map (function GrammarTypes.Nonterm n -> n | _ -> assert false) nonterminals in
    match terminals, nonterminals with
      | [], [nonterminal] ->
            Format.printf "        return generate_%s();\n" (nonterminal_to_string nonterminal)
      | _ ->
            if terminals <> [] then Format.printf "        USE(%s - 1);\n" (sizeof terminals);
            generate_nonterminals nonterminals;
            malloc_result terminals nonterminals;
            write_result production;
            free_temps nonterminals

(** Prints code for one production that is not the final production for a
    nonterminal. The first argument is which number production this is for the
    current nonterminal. *)
let print_production n production =
    Format.printf "    if (choice == %d) {\n" n;
    print_common production;
    Format.printf "    } else "

(** Prints code for the final production of a nonterminal *)
let print_default production =
    Format.printf "    {\n";
    print_common production;
    Format.printf "    }\n"

let always_fork = ref false
(** If true, don't use '?:' to keep terminals merged together in an
    if-then-else value. Instead, use if-then-else statements, forcing
    execution to fork. *)

(** Prints code for all productions that consist of only a terminal. This is
    treated specially because these do not require forking. In the call
    [print_terminal_productions start_n terminal terminals], [start_n] is the
    index of the first terminal production (i.e., one more than the number of
    productions which involved nonterminals), [terminal] is one terminal which
    is held aside to be the default value, and [terminals] is the list of all
    other terminals *)
let print_terminal_productions start_n terminal terminals =
    if !always_fork then (
        iteri (fun n prod -> print_production (n + start_n) prod)
            (List.map (fun t -> [GrammarTypes.Term t]) terminals);
        print_default [GrammarTypes.Term terminal]
    ) else (
        Format.printf "    {\n        result = strdup(\n";
        iteri (fun n terminal ->
                   Format.printf "            choice == %d ? %s :\n"
                       (n + start_n)
                       (terminal_to_string terminal))
            terminals;
        Format.printf "            %s);\n" (terminal_to_string terminal);
        Format.printf "        USE(strlen(result));\n    }\n";
    )

(** [count p list] returns how many elements of [list] satisfy predicate [p] *)
let count p list =
    List.fold_left
        (fun count x -> if p x then succ count else count)
        0
        list

(** Returns the maximum number of nonterminals in any single production from
    among the given productions *)
let max_num_nonterminals productions =
    GrammarTypes.SetOfProductions.fold
        (fun production max_so_far ->
             max max_so_far (count (fun x -> not (is_terminal x)) production))
        productions
        (-1)

(** Prints code for all the given productions *)
let print_cases productions =
    let productions = GrammarTypes.SetOfProductions.elements productions in
    let terminals, others = List.partition is_single_terminal productions in
    let terminals = List.map (function [ GrammarTypes.Term t ] -> t | _ -> assert false) terminals in
    match terminals with
        | terminal :: terminals ->
              iteri (fun n prod -> print_production n prod) others;
              print_terminal_productions (List.length others) terminal terminals
        | [] -> match others with
              | [] -> failwith "No productions"
              | hd :: tl ->
                    iteri (fun n prod -> print_production n prod) tl;
                    print_default hd

(** Prints the definition of the function that generates the given nonterminal,
    which can produce the given productions *)
let print_definition { GrammarTypes.name = name } productions =
    Format.printf "const char *generate_%s(void) {\n" name;
    Format.printf "    char *result;\n";
    (* Declare all needed temporary variables *)
    let num_temps = max_num_nonterminals productions in
    if num_temps > 0
    then begin
        Format.printf "    const char *temp0";
        for i = 1 to num_temps - 1 do
            Format.printf ", *temp%d" i
        done;
        Format.printf ";\n";
    end;
    (* Create a 'choice' variable unless there is only one production. *)
    if GrammarTypes.SetOfProductions.cardinal productions > 1
    then Format.printf "    int choice; __SYMBOLIC(&choice);\n";
    print_cases productions;
    Format.printf "    return result;
}

"

(** Prints the functions that generate the grammar's nonterminals *)
let print_definitions grammar = GrammarTypes.N.iter print_definition grammar

let speclist = [
    ("--always-fork",
     Arg.Set always_fork,
     " Don't use '?:' to keep terminals merged together in an if-then-else value. Instead, use if-then-else statements, forcing execution to fork.");
]

let usageMsg =
    "Usage: grammar takes its input, a file specifying a context-free grammar, on stdin, for example:\n  ./grammar < file\n"

(** Prints out C code that generates a string in the grammar *)
let print_grammar_generation_code grammar =
    print_prelude ();
    print_builtins grammar;
    print_declarations grammar;
    print_definitions grammar

let main () =
    Arg.parse
        (Arg.align speclist)
        (fun _ -> raise (Arg.Help usageMsg))
        usageMsg;
    let grammar = parse_grammar_from_in_chan stdin in
    (* TODO: Consider simplifying the grammar, for example by inlining nonterminals with only terminal productions. *)
    print_grammar_generation_code grammar
;;

main ()
