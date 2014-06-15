(* Mathematica Test File *)

Test[
	testNetlist16 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"Res", {Vx, Vo}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vo, 0}, "R4", {Gvalue -> 1/R2}}
   };
GetInputImpedance[ DisableSources[ testNetlist16, {"V1" -> "Short"} ], {Vx, 0}]
,
{(R1 (-R1 - R2) R2)/(-R1^2 - 3 R1 R2 - R2^2)}
,
TestID -> "AnalysisInputImpedance-1"
]

Test[
	testNetlist16 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"Res", {Vx, Vo}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vo, 0}, "R4", {Gvalue -> 1/R2}}
   };
GetInputImpedance[DisableSources[ testNetlist16, {"V1" -> "Short"} ], {Vo, 0}]
,
{-((R1 R2 (R1 + 2 R2))/(-R1^2 - 3 R1 R2 - R2^2))}
,
TestID -> "AnalysisInputImpedance-2"
]

Test[
	(* This is a cross coupled diff pair's input *)
testNetlist17 = {
   {"MOS", {Vop, Von, 0, 0}, 
    "Q1", {Gds -> 0, Cgs -> 0, Cgd -> 0, Cds -> 0}},
   {"MOS", {Von, Vop, 0, 0}, 
    "Q2", {Gds -> 0, Cgs -> 0, Cgd -> 0, Cds -> 0}}
   };
GetInputImpedance[testNetlist17, {Vop, Von}]
,
{-(2/(Gm - Cdb s - Cgb s))}
,
TestID -> "AnalysisInputImpedance-3"
]