(* Mathematica Test File *)

Test[
	temp = {"xfmr", {v1,v2,v3,v4}, "xfmr1", {TurnsP->10,TurnsS->2}};
	SCAPack`Private`TransformerModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementTransformer-1"
]

Test[
	temp = {
 {"volt", {vin, 0}, "v1", {}},
 {"res", {vin, vp}, "r1", {Gvalue -> 1/R1}},
 {"xfmr", {vp, 0, vs, 0}, "xfmr1", {}},
 {"res", {vs, 0}, "r2", {Gvalue -> 1/R2}}
 };
SolveFullSystem[temp],
{{I$v1 -> -((TurnsS^2 vin)/(R2 TurnsP^2 + R1 TurnsS^2)), 
  I$s$xfmr1 -> (TurnsP TurnsS vin)/(R2 TurnsP^2 + R1 TurnsS^2), 
  I$p$xfmr1 -> (TurnsS^2 vin)/(R2 TurnsP^2 + R1 TurnsS^2), 
  vp -> (R2 TurnsP^2 vin)/(R2 TurnsP^2 + R1 TurnsS^2), 
  vs -> (R2 TurnsP TurnsS vin)/(R2 TurnsP^2 + R1 TurnsS^2)}}
,
TestID -> "ElementTransformer-2"
]

Test[
	temp = {
  {"xfmr", {vp, 0, vs, 0}, "xfmr1", {TurnsP -> 1}},
  {"res", {vs, 0}, "r2", {Gvalue -> 1/R2}},
  {"vprobe", {vp, 0}, "vp1", {}}
  };
GetInputImpedance[temp, {vp, 0}],
{R2/TurnsS^2}
,
TestID -> "ElementTransformer-3"
]