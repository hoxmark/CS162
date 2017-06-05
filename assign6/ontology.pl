is_a(cell, 'cellular component').
is_a(membrane, 'cellular component').
is_a(organelle, 'cellular component').
is_a(intracellular, 'cell part').
is_a(cytoplasm, 'intracellular part').
is_a('DNA helicase complex', 'catalytic complex').
is_a('DNA helicase complex', 'intracellular part').
is_a('catalytic complex', 'protein complex').
is_a('protein complex', 'macromolecular complex').
is_a('macromolecular complex', 'cellular component').
is_a('amino acid transport complex', 'plasma membrane part').
is_a('amino acid transport complex', 'protein complex').
is_a('plasma membrane part', 'membrane part').
is_a('plasma membrane', membrane).
is_a('cell periphery', 'cell part').
is_a('cytochrome complex', 'protein complex').
is_a('transmembrane transporter complex', 'protein complex').
is_a('chloroplast ATP synthase complex', 'membrane part').
is_a('chloroplast ATP synthase complex', 'protein complex').
is_a(thylakoid, 'intracellular part').
is_a('chloroplast part', 'plastid part').
is_a(chloroplast, plastid).
is_a(plastid, 'cytoplasmic part').
is_a(plastid, 'intracellular membrane-bounded organelle').
is_a('intracellular membrane-bounded organelle', 'intracellular organelle').
is_a('intracellular membrane-bounded organelle', 'membrane-bounded organelle').
is_a('intracellular organelle', 'intracellular part').
is_a('intracellular organelle', organelle).
is_a('membrane-bounded organelle', organelle).
is_a('chloroplast part', 'plastid part').
is_a('plastid part', 'intracellular organelle part').
is_a('intracellular organelle part', 'organelle part').
is_a('chloroplast thylakoid membrane', 'plastid thylakoid membrane').
is_a('chloroplast thylakoid', 'chloroplast part').
is_a('chloroplast thylakoid', 'plastid thylakoid').
is_a('plastid thylakoid', 'organelle subcompartment').
is_a('plastid thylakoid', 'plastid part').
is_a('plastid thylakoid', thylakoid).
is_a('organelle subcompartment', 'intracellular organelle part').
is_a('organelle subcompartment', 'membrane-bounded organelle').
is_a('plastid thylakoid membrane', 'thylakoid membrane').
is_a('thylakoid membrane', 'photosynthetic membrane').
is_a('photosynthetic membrane', membrane).
is_a('photosynthetic membrane', 'thylakoid part').
is_a('vesicle membrane', 'cell part').
is_a('vesicle membrane', 'organelle membrane').
is_a('membrane-bounded vesicle', vesicle).
is_a(vesicle, organelle).
is_a('organelle membrane', membrane).
is_a('organelle membrane', 'organelle part').

part_of('cell part', cell).
part_of('cell part', 'cellular component').
part_of('membrane part', 'cellular component').
part_of('membrane part', membrane).
part_of('organelle part', 'cellular component').
part_of('organelle part', organelle).
part_of('intracellular part', 'cell part').
part_of('intracellular part', intracellular).
part_of('plasma membrane part', 'cell part').
part_of('plasma membrane part', 'plasma membrane').
part_of('plasma membrane', 'cell part').
part_of('plasma membrane', 'cell periphery').
part_of('chloroplast ATP synthase complex', 'thylakoid part').
part_of('chloroplast ATP synthase complex', 'chloroplast part').
part_of('chloroplast ATP synthase complex', 'chloroplast thylakoid membrane').
part_of('thylakoid part', 'intracellular part').
part_of('thylakoid part', 'thylakoid').
part_of('chloroplast part', chloroplast).
part_of('cytoplasmic part', 'intracellular part').
part_of('cytoplasmic part', cytoplasm).
part_of('chloroplast part', chloroplast).
part_of('plastid part', plastid).
part_of('organelle part', 'cellular component').
part_of('organelle part', organelle).
part_of('chloroplast thylakoid membrane', 'chloroplast part').
part_of('chloroplast thylakoid membrane', 'chloroplast thylakoid').
part_of('plastid thylakoid membrane', 'plastid part').
part_of('plastid thylakoid membrane', 'plastid thylakoid').
part_of('photosynthetic membrane', thylakoid).
part_of('vesicle membrane', 'membrane-bounded vesicle').
part_of('organelle membrane', 'membrane-bounded organelle').

% toisaroot: StartingPoint, PathToRoot

toisaroot('cellular component', ['cellular component']).

toisaroot(X,L):-
    is_a(X, Y),
    toisaroot(Y,L1), 
    L = [X|L1]. 


% subconcept: TheSubConcept, ThingItIsASubConceptOf

subconcept(TheSubConcept, ThingItIsASubConceptOf):-
    part_of(TheSubConcept, ThingItIsASubConceptOf). 

subconcept(TheSubConcept, ThingItIsASubConceptOf):-
    is_a(TheSubConcept, ThingItIsASubConceptOf). 


subconcept(C1, C3):-
    is_a(C2, C3),
    % part_of(C2, C3),
    subconcept(C1, C2).

subconcept(C1, C3):-
    % is_a(C2, C3),
    part_of(C2, C3),
    subconcept(C1, C2).
    
% subconcept(TheSubConcept, ThingItIsASubConceptOf):-
%     part_of(Y, ThingItIsASubConceptOf), 
%     subconcept(TheSubConcept, Y).


% subconcept(C1, C3):-
%     part_of(C2, C3), 
%     subconcept(C1, C2).



% subconcept(TheSubConcept, ThingItIsASubConceptOf):-
%     part_of(TheSubConcept, X), 
%     subconcept(X, ThingItIsASubConceptOf).



runTest(Test) :-
    format('Running ~w: ', [Test]),
    once(call(Test)) ->
        format('pass~n');
        format('----FAIL----').

toisarootTests :-
    runTest(toisaroot('cellular component', ['cellular component'])),
    runTest(toisaroot('DNA helicase complex', ['DNA helicase complex',
                                               'catalytic complex',
                                               'protein complex',
                                               'macromolecular complex',
                                               'cellular component'])),
    runTest(toisaroot('organelle subcompartment', ['organelle subcompartment',
                                                   'membrane-bounded organelle',
                                                   'organelle',
                                                   'cellular component'])).

subconceptTests :-
    runTest(subconcept('intracellular part', 'cell part')),
    runTest(subconcept('intracellular part', 'intracellular')),
    runTest(subconcept('intracellular part', 'cellular component')),
    runTest(subconcept('plastid thylakoid', 'cell')),
    runTest(subconcept('plastid thylakoid', 'cell part')),
    runTest(subconcept('plastid thylakoid', 'cellular component')),
    runTest(subconcept('plastid thylakoid', 'cytoplasm')),
    runTest(subconcept('plastid thylakoid', 'cytoplasmic part')),
    runTest(subconcept('plastid thylakoid', 'intracellular')),
    runTest(subconcept('plastid thylakoid', 'intracellular membrane-bounded organelle')),
    runTest(subconcept('plastid thylakoid', 'intracellular organelle')),
    runTest(subconcept('plastid thylakoid', 'intracellular organelle part')),
    runTest(subconcept('plastid thylakoid', 'intracellular part')),
    runTest(subconcept('plastid thylakoid', 'membrane-bounded organelle')),
    runTest(subconcept('plastid thylakoid', 'organelle')),
    runTest(subconcept('plastid thylakoid', 'organelle part')),
    runTest(subconcept('plastid thylakoid', 'organelle subcompartment')),
    runTest(subconcept('plastid thylakoid', 'plastid')),
    runTest(subconcept('plastid thylakoid', 'plastid part')),
    runTest(subconcept('plastid thylakoid', 'thylakoid')).


runTests :-
    toisarootTests,
    subconceptTests.
