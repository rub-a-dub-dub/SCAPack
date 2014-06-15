(* Mathematica Test File *)

Test[
	temp={
  {"voltnoise", {vi, 0}, "V1", {}},
  {"res", {vi, vx}, "r1", {Gvalue -> 1/R}},
  {"cap", {vx, vo}, "c1", {Cvalue -> C}},
  {"res", {vo, 0}, "r2", {Gvalue -> 1/R}}
  };
GetNoisePSD[temp, {vo, 0}, "V1"],
{tf$V1 -> (C^2 I$V1^2 R^2 s^2)/(I$V1 + 2 C I$V1 R s)^2, 
 zn$V1 -> -((I$V1 + 2 C I$V1 R s)/(C I$V1 s))}
,
TestID -> "ElementVoltNoise-1"
]

Test[
	temp = {"voltnoise", {v1, v2}, "v1", {}};
	SCAPack`Private`VoltNoiseModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVoltNoise-2"
]