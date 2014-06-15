(* Mathematica Test File *)

Test[
	temp={
  {"volt", {vin, 0}, "v1", {}},
  {"res", {vin, vx}, "r1", {Gvalue -> 1/R}},
  {"res", {vx, 0}, "r2", {Gvalue -> 1/R}},
  {"vcvs", {vx, 0, vo, 0}, "e1", {}},
  {"vprobe", {vo, 0}, "vp1", {}}
  };
GetTransferFunction[temp, {vo, vin}],
{A/2},
TestID->"ElementVoltageProbe-1"
]

Test[
	temp = { "vprobe", {v0, v1}, "vp1", {} };
	SCAPack`Private`VoltProbeModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementVoltageProbe-2"
]