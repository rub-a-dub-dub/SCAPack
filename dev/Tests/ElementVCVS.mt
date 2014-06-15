(* Mathematica Test File *)

Test[
temp={
  {"volt", {vin, 0}, "v1", {}},
  {"res", {vin, vp}, "r1", {Gvalue -> 1/R1}},
  {"res", {vp, vo}, "r2", {Gvalue -> 1/R2}},
  {"vcvs", {vp, 0, vo, 0}, "vcvs1", {}}
  };
GetTransferFunction[temp, {vo, vin}]
,
{(A R2)/(R1 - A R1 + R2)}
,
TestID -> "ElementVCVS-1"	
]

Test[
	testNetlist25 = {
   {"volt", {Vi, 0}, "V1", {}},
   {"res", {Vi, Vpi}, "R1", {Gvalue -> 1/R}},
   {"res", {Vpi, 0}, "R2", {Gvalue -> 1/(2 R)}},
   {"vcvs", {Vpi, 0, Vo, 0}, "S1", {A -> Vgain}},
   {"vprobe", {Vo, 0}, "Vp1", {}}
   };
GetTransferFunction[testNetlist25, {Vo, Vi}],
{(2 Vgain)/3}
,
TestID -> "ElementVCVS-2"
]

Test[
	temp = {"vcvs", {v1, v2, v3, v4}, "vcvs1", {} };
	SCAPack`Private`VCVSModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVCVS-3"
]

Test[
	temp = {"vcvs", {v1, v2, v3, v4}, "vcvs1", { A-> Vgain} };
	SCAPack`Private`VCVSModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVCVS-4"
]