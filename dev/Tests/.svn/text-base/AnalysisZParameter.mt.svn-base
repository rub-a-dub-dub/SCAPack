(* Mathematica Test File *)

Test[
	temp = {
	{"res", {v1, 0}, "r1", {Rvalue -> R}},
	{"res", {v1, v2}, "r2", {Rvalue -> 2 R}},
	{"res", {v2, 0}, "r3", {Rvalue -> R}},
	{"res", {v2, 0}, "r4", {Rvalue -> 2 R}}
	};
	ComputeZNetwork[ temp, {{v1, 0}, {v2, 0}}]//Normal,
	{{(8 R)/11, (2 R)/11}, {(2 R)/11, (6 R)/11}}
	,
	TestID -> "AnalysisZParameter-1"
]