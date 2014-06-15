(* Mathematica Test File *)

Test[
testNetlist26 = {
   {"volt", {Vi, 0}, "Vin", {}},
   {"res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"nullor", {Vx, 0, Vo, 0}, "O1", {}},
   {"res", {Vo, Vx}, "R2", {Gvalue -> 1/R2}}
   };
GetTransferFunction[testNetlist26, {Vo, Vi}],
{-(R2/R1)}
,
TestID -> "ElementNullor-1"	
]

Test[
	temp = {"nullor", {v1, v2, v3, v4}, "n1", {}};
	SCAPack`Private`NullorModel["ErrorCheck"][temp, temp],
	Null,
	TestID -> "ElementNullor-2"
]