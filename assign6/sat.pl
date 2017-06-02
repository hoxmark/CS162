% X is a logical variable
%
% b \in Boolean ::= true | false
% e \in Exp ::= true | false | variable(X)
%             | or(e1, e2)
%             | and(e1, e2)
%             | not(e)

% boolOr: InBoolean1, InBoolean2, OutBoolean

% boolAnd: InBoolean1, InBoolean2, OutBoolean

% boolNot: InBoolean, OutBoolean

% eval: Exp, Boolean


runTest(Test) :-
    format('Running ~w: ', [Test]),
    once(call(Test)) ->
        format('pass~n');
        format('----FAIL----').

boolOrTests :-
    runTest(boolOr(true, true, true)),
    runTest(boolOr(true, false, true)),
    runTest(boolOr(false, true, true)),
    runTest(boolOr(false, false, false)).

boolAndTests :-
    runTest(boolAnd(true, true, true)),
    runTest(boolAnd(true, false, false)),
    runTest(boolAnd(false, true, false)),
    runTest(boolAnd(false, false, false)).

evalTests :-
    runTest(eval(and(true, true), true)),
    runTest(eval(or(false, false), false)),
    runTest(eval(not(true), false)),
    runTest((eval(and(true, variable(A)), true), A == true)),
    runTest((eval(and(variable(B), variable(C)), true), B == true, C == true)),
    runTest(eval(or(variable(D), not(variable(D))), true)),
    runTest(eval(and(variable(E), not(variable(E))), false)),
    runTest((eval(or(false, variable(F)), true), F == true)),
    runTest((eval(or(false, variable(G)), false), G == false)).

runTests :-
    boolOrTests,
    boolAndTests,
    evalTests.
