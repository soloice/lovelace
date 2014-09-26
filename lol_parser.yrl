Nonterminals forms form args arg matchspec expressions expression case_clauses case_clause.
Terminals case end fun of '->' atom var ignore int '(' ')' ',' ';' '+' '-' '='.
Rootsymbol forms.

Right 100 '='.
Left 100 '+' '-'.


forms -> forms form:
    '$1' ++ ['$2'].


forms -> '$empty':
    [].


form -> fun atom '(' ')' '->' expressions end:
    {'fun', '$2', [], '$6'}.


form -> fun atom '(' args ')' '->' expressions end:
    {'fun', '$2', '$4', '$7'}.


args -> args ',' arg:
   '$1' ++ ['$3'].


args -> arg:
    ['$1'].


arg -> var:
    '$1'.


matchspec -> matchspec '=' matchspec:
    '$1' ++ '$3'.


matchspec -> var:
    ['$1'].

matchspec -> ignore:
    ['$1'].


matchspec -> int:
    ['$1'].


expressions -> expression:
    ['$1'].


expressions -> expressions ',' expression:
    '$1' ++ ['$3'].


expression -> case expression of case_clauses end:
    {'case', '$2', '$4'}.


case_clauses -> case_clauses ';' case_clause:
    '$1' ++ ['$3'].


case_clauses -> case_clause:
    ['$1'].


case_clause -> matchspec '->' expressions:
    {'$1', '$3'}.


expression -> expression '+' expression:
    {call, '$2', ['$1', '$3']}.


expression -> expression '-' expression:
    {call, '$2', ['$1', '$3']}.


expression -> atom '(' ')':
    {call, '$1', []}.


expression -> atom '(' expressions ')':
    {call, '$1', '$3'}.


expression -> var:
    '$1'.


expression -> int:
    '$1'.


Erlang code.
