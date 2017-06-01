match: 
Called match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]
first_match: Called first_match on input: (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),Constructor ("egg",Const 4)]],[ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]), should have gotten: SOME([]) but your function returned otherwise. [incorrect answer]

prob13 tests failed to run (most likely caused by an incorrect function signature in the submission)

all_answers: Called all_answers on: [the,walrus,and,the,carpenter,talked,of,many,things,,of,shoes,and,ships,and,ceiling,wax,,of,Cabbages,and,Kings.], should have gotten: NONE but your function returned something else [incorrect answer]

all_answers: Called all_answers on: [this,list,has,no,capital,letters], should have gotten: NONE but your function returned something else [incorrect answer]

all_answers: Called all_answers on: [Alabama,Alaska,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Hawaii,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,NorthCarolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,southCarolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming], should have gotten: NONE but your function returned something else [incorrect answer]

all_answers: Called all_answers on: [], should have gotten: SOME([]) but your function returned something else [incorrect answer]

all_answers: Called all_answers on: [], should have gotten: SOME([]) but your function returned something else [incorrect answer]

count_some_var: Called count_some_var on input: (y,Variable "x"), should have gotten: 0 but your function returned otherwise. [incorrect answer]

count_some_var: Called count_some_var on input: (y,TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard),TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]]), should have gotten: 0 but your function returned otherwise. [incorrect answer]

count_some_var: Called count_some_var on input: (y,TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]), should have gotten: 0 but your function returned otherwise. [incorrect answer]

count_some_var: Called count_some_var on input: (wild,TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]), should have gotten: 0 but your function returned otherwise. [incorrect answer]

