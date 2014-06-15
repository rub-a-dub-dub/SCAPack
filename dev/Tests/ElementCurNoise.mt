(* Mathematica Test File *)

Test[
	temp={
  {"curnoise", {vi, 0}, "I1", {}},
  {"res", {vi, 0}, "R1", {Gvalue -> 1/R}},
  {"cap", {vi, vout}, "C1", {Cvalue -> C}},
  {"vprobe", {vout, 0}, "Vp1", {}}
  };
GetNoisePSD[temp, {vout, 0}, "I1"],
{tf$I1 -> R^2, zn$I1 -> R}
,
TestID -> "ElementCurNoise-1"
]

Test[
	temp = { "curnoise", {v1, v2}, "i1", {} };
	SCAPack`Private`CurNoiseModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementCurNoise-2"
];