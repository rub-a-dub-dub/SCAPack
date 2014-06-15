(* Mathematica Test File *)

Test[
	testNetlist11 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vx, 0}, "I1", {}}
   };
	GetNoisePSD[testNetlist11, {Vy, 0}, "I1"]
	,
	{tf$I1 -> (R1^2 R2^4)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$I1 -> (R1 R2 (R1 + R2))/(R1^2 + 3 R1 R2 + R2^2)},
 TestID -> "AnalysisNoise-1"
]

Test[
	testNetlist11 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"CurNoise", {Vx, 0}, "In1", {}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vy, 0}, "In2", {}}
   };
GetNoisePSD[testNetlist11, {Vy, 0}, "In1"]
,
{tf$In1 -> (R1^2 R2^4)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In1 -> (R1 R2 (R1 + R2))/(R1^2 + 3 R1 R2 + R2^2)}
 ,
 TestID -> "AnalysisNoise-2"
]

Test[
	testNetlist11 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"CurNoise", {Vx, 0}, "In1", {}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vy, 0}, "In2", {}}
   };
GetNoisePSD[testNetlist11, {Vy, 0}, "In2"]
,
{tf$In2 -> (R1^2 R2^2 (R1 + 2 R2)^2)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In2 -> (R1 R2 (R1 + 2 R2))/(R1^2 + 3 R1 R2 + R2^2)}
 ,
 TestID -> "AnalysisNoise-3"
]

Test[
	testNetlist11 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"CurNoise", {Vx, 0}, "In1", {}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vy, 0}, "In2", {}}
   };
GetTotalNoisePSD[testNetlist11, {Vy, 0}]
,
{tf$In1 -> (R1^2 R2^4)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In1 -> (R1 R2 (R1 + R2))/(R1^2 + 3 R1 R2 + R2^2), 
 tf$In2 -> (R1^2 R2^2 (R1 + 2 R2)^2)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In2 -> (R1 R2 (R1 + 2 R2))/(R1^2 + 3 R1 R2 + R2^2)}
 ,
 TestID -> "AnalysisNoise-4"
]

Test[
	testNetlist12 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vx, Vxn}, "R2", {Gvalue -> 1/R2}},
   {"VoltNoise", {Vxn, 0}, "In1", {}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vy, 0}, "In2", {}}
   };
Simplify[GetNoisePSD[testNetlist12, {Vy, 0}, "In1"]]
,
{tf$In1 -> (R1^2 R2^2)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In1 -> -((R1^2 + 3 R1 R2 + R2^2)/(2 R1 + R2))}
,
TestID -> "AnalysisNoise-5"
]

Test[
	testNetlist14 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"Res", {Vxn, 0}, "R2", {Gvalue -> 1/R2}},
   {"VoltNoise", {Vx, Vxn}, "In1", {}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}},
   {"CurNoise", {Vy, 0}, "In2", {}}
   };
GetNoisePSD[testNetlist14, {Vy, 0}, "In1"] // Simplify
,
{tf$In1 -> (R1^2 R2^2)/(R1^2 + 3 R1 R2 + R2^2)^2, 
 zn$In1 -> -((R1^2 + 3 R1 R2 + R2^2)/(2 R1 + R2))}
,
TestID -> "AnalysisNoise-6"
]

Test[
	testNetlist13 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"Res", {Vi, Vx}, "R1", {Gvalue -> 1/R1}},
   {"ResNoisy", {Vx, 0}, "R2", {Gvalue -> 1/R2}},
   {"Res", {Vx, Vy}, "R3", {Gvalue -> 1/R1}},
   {"Res", {Vy, 0}, "R4", {Gvalue -> 1/R2}}
   };
GetNoisePSD[testNetlist13, {Vy, 0}, "R2"]
,
{tf$R2 -> (4 k R1^2 R2^3 T)/(R1^2 + 3 R1 R2 + R2^2)^2, zn$R2 -> Null}
,
TestID -> "AnalysisNoise-7"
]

Test[
	testNetlist15 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"ResNoisy", {Vi, Vo}, "R1", {Gvalue -> 1/R}},
   {"Cap", {Vo, 0}, "C1", {Cvalue -> C}}
   };
GetNoisePSD[testNetlist15, {Vo, 0}, "R1"]
,
{tf$R1 -> (4 k R T)/(1 + C R s)^2, zn$R1 -> Null}
,
TestID -> "AnalysisNoise-8"
]

Test[
testNetlist15 = {
   {"Volt", {Vi, 0}, "V1", {}},
   {"ResNoisy", {Vi, Vo}, "R1", {Gvalue -> 1/R}},
   {"Cap", {Vo, 0}, "C1", {Cvalue -> C}}
   };
temp = GetNoisePSD[testNetlist15, {Vo, 0}, "R1"];
temp = tf$R1 /. temp[[1]] /. s -> I 2 \[Pi] f;
Integrate[ComplexExpand[Abs[temp]], {f, 0, \[Infinity]}, 
 Assumptions -> {{C, R, k, T} \[Element] Reals, C > 0, R > 0, k > 0, 
   T > 0}]
,
(k T)/C
,
TestID -> "AnalysisNoise-9"
]

Test[
	temp = {
		{"mos", {vout, 0, 0, 0}, "m1", {IsNoisy -> True, Cgd -> 0, Cdb -> 0, Gds -> 0}},
		{"res", {vout, 0}, "r1", {Rvalue -> R}}
	};
	GetNoisePSD[temp, {vout, 0}, "m1"]
	,
	{tf$m1 -> R^2 (Sid$m1 + Gm Svg$m1)^2, zn$m1 -> Null}
	,
	TestID -> "AnalysisNoise-10"
]

Test[
	temp = {
		{"mos", {vout, 0, 0, 0}, "m1", {IsNoisy -> True, Cgd -> 0, Cdb -> 0, Gds -> 0}},
		{"mos", {vout, 0, 0, 0}, "m2", {IsNoisy -> True, Cgd -> 0, Cdb -> 0, Gds -> 0}},
		{"res", {vout, 0}, "r1", {Rvalue -> R}}
	};
	GetTotalNoisePSD[temp, {vout, 0}]
	,
	{tf$m1 -> R^2 (Sid$m1 + Gm Svg$m1)^2, zn$m1 -> Null, 
 tf$m2 -> R^2 (Sid$m2 + Gm Svg$m2)^2, zn$m2 -> Null}
	,
	TestID -> "AnalysisNoise-11"
]

Test[
	temp = {
		{"mosr", {vout, 0, 0, 0}, "m1", {IsNoisy -> True, Cgd -> 0, Cdb -> 0, Gds -> 0, Rg -> 0, Rd -> 0, Rs -> 0, Rb -> 0}},
		{"res", {vout, 0}, "r1", {Rvalue -> R}}
	};
	GetNoisePSD[temp, {vout, 0}, "m1"]
	,
	{tf$m1 -> R^2 (Sid$m1 + Gm Svg$m1)^2, zn$m1 -> Null}
	,
	TestID -> "AnalysisNoise-12"
]

Test[
	temp = {
		{"mosr", {vout, 0, 0, 0}, "m1", {IsNoisy -> True, Cgd -> 0, Cdb -> 0, Gds -> 0, Cgb -> 0, Cgs -> 0, Rd -> 0, Rs -> 0, Rb -> 0}},
		{"res", {vout, 0}, "r1", {Rvalue -> R}}
	};
	GetNoisePSD[temp, {vout, 0}, "m1"]
	,
	{tf$m1 -> R^2 (Sid$m1 + Gm Rg Sirg$m1 + Gm Svg$m1)^2, zn$m1 -> Null}
	,
	TestID -> "AnalysisNoise-13"
]