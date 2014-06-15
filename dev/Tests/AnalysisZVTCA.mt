(* Mathematica Test File *)

Test[
	testNetlist18 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R}},
   {"Cap", {Vx, 0}, "C1", {Cvalue -> C}},
   {"Res", {Vx, Vo}, "R2", {Gvalue -> 1/R}},
   {"Cap", {Vo, 0}, "C2", {Cvalue -> C}}
   };
GetZVTimeConstants[DisableSources[testNetlist18, {"V1" -> "Short"}]],
{C R, 2 C R}
,
TestID -> "AnalysisZVTCA-1"
]

Test[
	testNetlist19 = {
   {"MOS", {Vo, Vi, 0, 0}, "M1", {}},
   {"Volt", {Vi, 0}, "V1", {}},
   {"Vprobe", {Vo, 0}, "Vo", {}}
   };
GetZVTimeConstants[DisableSources[testNetlist19, {"V1" -> "Short"}]],
{Cgd/Gds, 0, Cds/Gds, Cdb/Gds, 0, 0},
{Power::infy},
TestID -> "AnalysisZVTCA-2"
]

Test[
	temp = {
		{"volt", {vin, 0}, "v1", {}},
		{"res", {vin, v1}, "r1", {Gvalue -> 1/R1}},
		{"cap", {v1, 0}, "c1", {Cvalue -> C1}},
		{"ind", {v1, v2}, "l1", {Lvalue -> L}},
		{"cap", {v2, 0}, "c2", {Cvalue -> C2}}
	};
	GetZVTimeConstants[ DisableSources[ temp, {"v1" -> "Short"} ] ],
	{C1*R1, C2*R1, 0},
	TestID -> "AnalysisZVTCA-3"	
]

Test[
	temp = {
		{"volt", {vin, 0}, "v1", {}},
		{"ind", {vin, v1}, "l1", {Lvalue -> L1}},
		{"res", {v1, v2}, "r1", {Gvalue -> 1/R1}},
		{"cap", {v2, 0}, "c2", {Cvalue -> C2}},
		{"res", {v2, v3}, "r2", {Gvalue -> 1/R2}},
		{"ind", {v3, 0}, "l2", {Lvalue -> L2}},
		{"cap", {v3, vout}, "c1", {Cvalue -> C1}},
		{"res", {vout, 0}, "r3", {Gvalue -> 1/R3}}
	};
	GetZVTimeConstants[ DisableSources[ temp, {"v1" -> "Short"} ] ] // Simplify,
	{(C2*R1*R2)/(R1+R2), C1*R3, L1/(R1+R2), L2/(R1+R2)}
	,
	TestID -> "AnalysisZVTCA-4"
]