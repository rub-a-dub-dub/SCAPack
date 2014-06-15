(* Mathematica Test File *)

Test[
	shuntR = {
		{"res", {v1, 0}, "r1", {Rvalue -> R}},
   		{"vprobe", {v1, 0}, "vp1", {}}
	};
	txZ = ComputeABCDNetwork[shuntR, {{v1, 0}, {v1, 0}}];
	txZ // Normal,
	{{1, R}, {0, 1}},
	TestID -> "AnalysisNetworkData-1"
]
