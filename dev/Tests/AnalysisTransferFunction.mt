(* Mathematica Test File *)

Test[
	
	testNetlist23 = {
   {"volt", {Vi, 0}, "V1", {}},
   {"res", {Vi, Vpi}, "R1", {Gvalue -> 1/R}},
   {"res", {Vpi, Vx}, "R2", {Gvalue -> 1/(2 R)}},
   {"iprobe", {Vx, 0}, "Ip1", {}},
   {"cccs", {Vo, 0}, "S1", {probe -> "Ip1", A -> Igain}},
   {"res", {Vo, 0}, "Rl", {Gvalue -> 1/Rl}}
   };
GetTransferFunction[testNetlist23, {Vo, Vi}]
,
{-((Igain Rl)/(3 R))}
,
TestID -> "AnalysisTransferFunction-1"
]

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
TestID -> "AnalysisTransferFunction-2"
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
TestID -> "AnalysisTransferFunction-3"
]

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
TestID->"AnalysisTransferFunction-4"
]