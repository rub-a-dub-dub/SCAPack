(* Mathematica Test File *)

Test[
	temp={
		{"volt", {vin, 0}, "v1", {}},
		{"mos", {vout, vin, 0, 0}, "q1", {}},
		{"vprobe", {vout, 0}, "vp1", {}}
	};
	temp=GetTransferFunction[ temp, {vout, vin} ];
	Simplify[temp]
	,
	{(-Gm + Cgd s)/(Gds + (Cdb + Cgd) s)}
	,
	TestID -> "ElementMOS-1"
]

Test[
	temp={
		{"volt", {vin, 0}, "v1", {}},
		{"res", {vin, vmos}, "r1", {Gvalue -> 1/Rs} },
		{"mos", {0, vmos, vout, 0}, "m1", {Cgb -> 0, Cdb -> 0, Csb -> 0}},
		{"cap", {vout, 0}, "c1", {Cvalue -> Cl}}
	};
	temp=GetTransferFunction[ temp, {vout, vin} ] // Simplify
	,
	{(Gm + Cgs s)/(Gds + Gm + Gmb + Cgs s + Cl s + Cgd Gds Rs s + 
    Cgs Gds Rs s + Cgd Gm Rs s + Cgd Gmb Rs s + Cgs Gmb Rs s + 
    Cgd Cgs Rs s^2 + Cgd Cl Rs s^2 + Cgs Cl Rs s^2)}
    ,
    TestID -> "ElementMOS-2"
]

Test[
	testNetlist2 = {
   {"MOS", {Vop, Vin, 0, 0}, "Q1", {Cds -> 0, Cgd -> 0, Cgs -> 0}},
   {"MOS", {Von, Vip, 0, 0}, "Q2", {Cds -> 0, Cgd -> 0, Cgs -> 0}},
   {"Volt", {Vip, 0}, "Vip", {}},
   {"Volt", {Vin, 0}, "Vin", {}},
   {"Vprobe", {Vop, Von}, "Vout", {}}
   };
	ans = GetTransferFunction[testNetlist2, {Vop-Von,Vip-Vin}]
,
{Gm/(Gds + Cdb s)}
,
TestID -> "ElementMOS-3"
]

Test[
	testNetlist3 = {
   {"MOS", {Vop, Vin, 0, 0}, "Q1", {Cds -> 0, Cgd -> 0, Cgs -> 0}},
   {"MOS", {Von, Vip, 0, 0}, "Q2", {Cds -> 0, Cgd -> 0, Cgs -> 0}},
   {"Volt", {Vip, Vin}, "Vip", {}},
   {"Vprobe", {Vop, Von}, "Vout", {}}
   };
   GetTransferFunction[testNetlist3, {Vop-Von,Vip-Vin}]
   ,
   {Gm/(Gds + Cdb s)}
   ,
   TestID -> "ElementMOS-4"
]

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {} };
	SCAPack`Private`MOSModelBasic["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOS-5"
]

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {Gm->2, Gds->3, Gmb->4} };
	SCAPack`Private`MOSModelBasic["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOS-6"
]

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {Gm->1, Gds->3, Gmb->2, Cgs->4, Cgd->5, Cds->6, Cdb->7, Cgb->8, Csb->9} };
	SCAPack`Private`MOSModelBasic["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOS-7"
]

Test[
	temp={
		{"volt", {vin, 0}, "v1", {}},
		{"mos", {vout, vin, 0, 0}, "q1", {IsNoisy->True}},
		{"vprobe", {vout, 0}, "vp1", {}}
	};
	temp=GetTransferFunction[ temp, {vout, vin} ];
	Simplify[temp]
	,
	{(-Gm + Cgd s)/(Gds + (Cdb + Cgd) s)}
	,
	TestID -> "ElementMOS-8"
]