(* Mathematica Test File *)

Test[
	temp={
  {"volt", {vin, 0}, "v1", {}},
  {"res", {vin, vpi}, "r1", {Gvalue -> 1/Rsrc}},
  {"res", {vpi, 0}, "r2", {Gvalue -> 1/Rpi}},
  {"vccs", {vpi, 0, vo, 0}, "vccs1", {A -> Gm}},
  {"res", {vo, 0}, "rl", {Gvalue -> 1/Rl}}
  };
GetTransferFunction[temp, {vo, vin}]
,
{-((Gm Rl Rpi)/(Rpi + Rsrc))}
,
TestID -> "ElementVCCS-1"
]

Test[
	testNetlist22 = {
   {"volt", {Vi, 0}, "V1", {}},
   {"res", {Vi, Vpi}, "R1", {Gvalue -> 1/R}},
   {"res", {Vpi, 0}, "R2", {Gvalue -> 1/(2 R)}},
   {"vccs", {Vpi, 0, Vo, 0}, "Gm1", {A -> Gm}},
   {"res", {Vo, 0}, "Ro", {Gvalue -> 1/Rl}}
   };
GetTransferFunction[testNetlist22, {Vo, Vi}],
{(-2*Gm*Rl)/3}
,
TestID -> "ElementVCCS-2"
]

Test[
	temp = {"vccs", {vin, 0, v2, v4}, "vccs1", {}};
	SCAPack`Private`VCCSModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVCCS-3"
]

Test[
	temp = {"vccs", {vin, 0, v4, v2}, "vccs1", { A -> Gm}};
	SCAPack`Private`VCCSModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVCCS-4"
]