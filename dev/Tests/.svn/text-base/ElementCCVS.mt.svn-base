(* Mathematica Test File *)

Test[
testNetlist24 = {
   {"volt", {Vi, 0}, "V1", {}},
   {"res", {Vi, Vpi}, "R1", {Gvalue -> 1/R}},
   {"res", {Vpi, Vx}, "R2", {Gvalue -> 1/(2 R)}},
   {"iprobe", {Vx, 0}, "Ip1", {}},
   {"ccvs", {Vo, 0}, "S1", {probe -> "Ip1", A -> Zgain}},
   {"vprobe", {Vo, 0}, "Vp1", {}}
   };
GetTransferFunction[testNetlist24, {Vo, Vi}],
{Zgain/(3 R)}
,
TestID -> "ElementCCVS-1"	
]

Test[
	temp = {"ccvs", {v1, v2}, "ccvs1", { probe-> "Ip1" } };
	netlist = { temp, {"iprobe", {vp, vn}, "Ip1", {}} };
	SCAPack`Private`CCVSModel["ErrorCheck"][ temp, netlist ],
	Null,
	TestID -> "ElementCCVS-2"
]

Test[
	temp = {"ccvs", {v1, v2}, "ccvs1", { probe-> "Ip1", A -> gain } };
	netlist = { temp, {"iprobe", {vp, vn}, "Ip1", {}} };
	SCAPack`Private`CCVSModel["ErrorCheck"][ temp, netlist ],
	Null,
	TestID -> "ElementCCVS-3"
]