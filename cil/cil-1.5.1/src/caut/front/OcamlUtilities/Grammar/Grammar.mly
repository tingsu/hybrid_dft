%{
%}

%token <GrammarTypes.terminal> TERM
%token <GrammarTypes.nonterminal> NONTERM
%token DEF PIPE SEMICOLON EOF LBRACKET RBRACKET STAR PLUS LPAREN RPAREN

/* It's clear that input must return a grammar, but it's not obvious why
everything else does, too. The reason is that optional productions, star, and
plus create fresh nonterminals, and the mappings of these to their productions
need to passed up the chain so that they can be incorporated into the final
result. */

%start input
%type <GrammarTypes.grammar> input
%type <GrammarTypes.nonterminal * GrammarTypes.productions * GrammarTypes.grammar> rewrite_rule
%type <GrammarTypes.productions * GrammarTypes.grammar> productions
%type <GrammarTypes.production * GrammarTypes.grammar> production
%type <GrammarTypes.either * GrammarTypes.grammar> production_bit

%% /* Grammar rules and actions follow */
input:
  EOF                { GrammarTypes.empty_grammar }
| rewrite_rule input { let nonterm, prods, grammar = $1 in
                       let grammar = GrammarTypes.merge_grammars grammar $2 in
                       GrammarTypes.add_productions nonterm prods grammar }
;

rewrite_rule:
  NONTERM DEF productions SEMICOLON
    { match GrammarTypes.reserved_prefix $1.GrammarTypes.name with
        | None -> let productions, grammar = $3 in ($1, productions, grammar)
        | Some s -> invalid_arg ("Do not use a nonterminal with a name starting with " ^ s) }
| NONTERM DEF PIPE productions SEMICOLON
    { match GrammarTypes.reserved_prefix $1.GrammarTypes.name with
        | None -> let productions, grammar = $4 in ($1, productions, grammar)
        | Some s -> invalid_arg ("Do not use a nonterminal with a name starting with " ^ s) }
;

productions:
  production { let production, grammar = $1 in
               (GrammarTypes.SetOfProductions.singleton production, grammar) }
| production PIPE productions { let production, grammar1 = $1 in
                                let productions, grammar2 = $3 in
                                let grammar = GrammarTypes.merge_grammars grammar1 grammar2 in
                                (GrammarTypes.SetOfProductions.add production productions, grammar) }
;

production:
  production_bit { let bit, grammar = $1 in ([bit], grammar) }
| production_bit production
    { let bit, grammar1 = $1 in
      let production, grammar2 = $2 in
      (bit :: production, GrammarTypes.merge_grammars grammar1 grammar2) }
;

production_bit:
  TERM    { (GrammarTypes.Term $1, GrammarTypes.empty_grammar) }
| NONTERM { (GrammarTypes.Nonterm $1,  GrammarTypes.empty_grammar) }
| LBRACKET productions RBRACKET
    { let productions, grammar = $2 in
      let auxiliary_nonterm, grammar2 = GrammarTypes.get_auxiliary productions in
      let grammar = GrammarTypes.merge_grammars grammar2 grammar in
      let optional_nonterm, grammar2 = GrammarTypes.get_optional [auxiliary_nonterm] in
      (optional_nonterm, GrammarTypes.merge_grammars grammar2 grammar) }
| production STAR
    { let production, grammar = $1 in
      let star_nonterm, grammar2 = GrammarTypes.get_star production in
      (star_nonterm, GrammarTypes.merge_grammars grammar2 grammar) }
| production PLUS
    { let production, grammar = $1 in
      let plus_nonterm, grammar2 = GrammarTypes.get_plus production in
      (plus_nonterm, GrammarTypes.merge_grammars grammar2 grammar) }
| LPAREN productions RPAREN
    { let productions, grammar = $2 in
      let auxiliary_nonterm, grammar2 = GrammarTypes.get_auxiliary productions in
      (auxiliary_nonterm, GrammarTypes.merge_grammars grammar2 grammar) }
;

%%
