(* Mathematica Test File *)

Test[
	testNetlist27 = {
   {"gyrator", {vi, 0, vo, 0}, "G1", {}},
   {"cur", {vi, 0}, "I1", {}},
   {"res", {vo, 0}, "R1", {}}
   };
GetTransferFunction[testNetlist27, {vo, I$I1}],
{A},
{Solve::svars}
,
TestID -> "ElementGyrator-1"
]

Test[
	temp = {"gyrator", {v1, v2, v3, v4}, "gyr1", {}};
	SCAPack`Private`GyratorModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementGyrator-2"
]

Test[
	temp = {
  {"volt", {vin, 0}, "v1", {}},
  {"gyrator", {vin, 0, v1, 0}, "g1", {A -> n1}},
  {"gyrator", {v1, 0, vout, 0}, "g2", {A -> n2}},
  {"res", {vout, 0}, "r1", {Gvalue -> 1/R}}
  };
	SolveFullSystem[ temp ],
	{{I$v1 -> -((n2^2 vin)/(n1^2 R)), v1 -> (n2^2 vin)/(n1 R), 
  vout -> -((n2 vin)/n1)}}
	,
	TestID -> "ElementGyrator-3"
]