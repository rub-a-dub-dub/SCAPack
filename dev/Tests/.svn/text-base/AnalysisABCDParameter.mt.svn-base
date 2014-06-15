(* Mathematica Test File *)

Test[
	temp = {
		{"res", {v1, 0}, "r1", {Rvalue -> rpi}},
		{"vccs", {v1, 0, v2, 0}, "vccs1", {A -> gm}},
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	ComputeABCDNetwork[temp, {{v1, 0}, {v2, 0}}] // Normal,
	{{-gm ro, -gm ro rpi}, {-gm, -gm rpi}}
	,
	TestID -> "AnalysisABCDParameter-1"
]

Test[
	shuntR = {
	   {"res", {v1, 0}, "r1", {Rvalue -> R}},
	   {"vprobe", {v1, 0}, "vp1", {}}
	};
	ComputeABCDNetwork[shuntR, {{v1, 0}, {v1, 0}}] // Normal,
	{{1, R}, {0, 1}}
	,
	TestID -> "AnalysisABCDParameter-2"
]

Test[
	seriesR = {
		{"res", {v1, v2}, "r1", {Rvalue -> R}},
		{"vprobe", {v1, 0}, "vp1", {}},
		{"vprobe", {v2, 0}, "vp2", {}}
	};
	ComputeABCDNetwork[ seriesR, {{v1,0},{v2,0}} ] // Normal,
	{{1, 0}, {1/R, 1}},
	TestID -> "AnalysisABCDParameter-3"
]