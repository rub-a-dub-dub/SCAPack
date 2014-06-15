(* Mathematica Test File *)

Test[
	testNetlist20 = {
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R}},
   {"Cap", {Vx, 0}, "C1", {Cvalue -> C}},
   {"Res", {Vx, Vo}, "R2", {Gvalue -> 1/R}},
   {"Cap", {Vo, 0}, "C2", {Cvalue -> C}}
   };
testNetlist21 = {
   {"SubCkt", {Vo, Vi}, "X1", {testNetlist20}},
   {"Volt", {Vi, 0}, "V1", {}},
   {"Vprobe", {Vo, 0}, "Vp1", {}}
   };
GetTransferFunction[testNetlist21, {Vo, Vi}],
{(1 + 3*C*R*s + C^2*R^2*s^2)^(-1)}
,
TestID -> "ElementSubcircuit-1"
]