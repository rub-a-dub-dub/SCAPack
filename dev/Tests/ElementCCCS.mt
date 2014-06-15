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
TestID -> "ElementCCCS-1"
]

Test[
	temp = {"cccs", {Vo, 0}, "S1", {probe -> "Ip1", A -> Igain}};
	netlist= { temp, {"iprobe", {n, p}, "Ip1", {} } };
	SCAPack`Private`CCCSModel["ErrorCheck"][temp, netlist],
	Null,
	TestID -> "ElementCCCS-2"	
]

Test[
	temp = {"cccs", {Vo, 0}, "S1", {probe -> "Ip1"}};
	netlist= { temp, {"iprobe", {n, p}, "Ip1", {} } };
	SCAPack`Private`CCCSModel["ErrorCheck"][temp, netlist],
	Null,
	TestID -> "ElementCCCS-2"	
]