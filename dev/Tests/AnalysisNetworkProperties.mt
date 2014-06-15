(* Mathematica Test File *)

(* SymmetricQ tests *)
Test[
	shuntR = {
	   {"res", {v1, 0}, "r1", {Rvalue -> R}},
	   {"vprobe", {v1, 0}, "vp1", {}}
   };
   zNet = ComputeZNetwork[ shuntR, {{v1,0},{v1,0}}];
   SymmetricQ[ zNet ],
   True,
   TestID -> "AnalysisNetworkParameters-6"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeABCDNetwork[temp, {{v1, 0}, {v2, 0}}];
	SymmetricQ[ temp ],
	-(gm*ro)==-(gm*rpi),
	TestID -> "AnalysisNetworkParameters-7"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeGNetwork[temp, {{v1, 0}, {v2, 0}}];
	SymmetricQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-8"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeHNetwork[temp, {{v1, 0}, {v2, 0}}];
	SymmetricQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-9"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeYNetwork[temp, {{v1, 0}, {v2, 0}}];
	SymmetricQ[ temp ],
	rpi^(-1)==ro^(-1),
	TestID -> "AnalysisNetworkParameters-10"
]

(* LosslessQ tests *)
Test[
	shuntR = {
	   {"res", {v1, 0}, "r1", {Rvalue -> R}},
	   {"vprobe", {v1, 0}, "vp1", {}}
   };
   zNet = ComputeZNetwork[ shuntR, {{v1,0},{v1,0}}];
   LosslessQ[ zNet ],
   R==0,
   TestID -> "AnalysisNetworkParameters-1"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeABCDNetwork[temp, {{v1, 0}, {v2, 0}}];
	LosslessQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-2"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeGNetwork[temp, {{v1, 0}, {v2, 0}}];
	LosslessQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-3"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeHNetwork[temp, {{v1, 0}, {v2, 0}}];
	LosslessQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-4"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeYNetwork[temp, {{v1, 0}, {v2, 0}}];
	LosslessQ[ temp ],
	gm==0,
	TestID -> "AnalysisNetworkParameters-5"
]
(* ReciprocalQ tests *)
Test[
	shuntR = {
	   {"res", {v1, 0}, "r1", {Rvalue -> R}},
	   {"vprobe", {v1, 0}, "vp1", {}}
   };
   zNet = ComputeZNetwork[ shuntR, {{v1,0},{v1,0}}];
   ReciprocalQ[ zNet ],
   R==0,
   TestID -> "AnalysisNetworkParameters-11"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeABCDNetwork[temp, {{v1, 0}, {v2, 0}}];
	ReciprocalQ[ temp ],
	False,
	TestID -> "AnalysisNetworkParameters-12"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeGNetwork[temp, {{v1, 0}, {v2, 0}}];
	ReciprocalQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-13"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeHNetwork[temp, {{v1, 0}, {v2, 0}}];
	ReciprocalQ[ temp ],
	$Failed,
	TestID -> "AnalysisNetworkParameters-14"
]

Test[
	temp = {
		{"res", {v1, 0},"r1", {Rvalue -> rpi}}, 
		{"vccs", {v1, 0, v2, 0},"vccs1", {A -> gm}}, 
		{"res", {v2, 0}, "r2", {Rvalue -> ro}}
	};
	temp = ComputeYNetwork[temp, {{v1, 0}, {v2, 0}}];
	ReciprocalQ[ temp ],
	False,
	TestID -> "AnalysisNetworkParameters-15"
]