(* Mathematica Test File *)

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {} };
	SCAPack`Private`MOSModelResistances["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOSR-1"
]

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {Gm->2, Gds->3, Gmb->4} };
	SCAPack`Private`MOSModelResistances["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOSR-2"
]

Test[
	temp = { "mos", {v0, v1, v2, v3}, "q1", {Gm->1, Gds->3, Gmb->2, Cgs->4, Cgd->5, Cds->6, Cdb->7, Cgb->8, Csb->9, Rg->10, Rs->11, Rb->12, Rd->13} };
	SCAPack`Private`MOSModelResistances["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementMOSR-3"
]

Test[
	temp={
		{"volt", {vin, 0}, "v1", {}},
		{"mosr", {vout, vin, 0, 0}, "q1", {Rg->0, Rb->0, Rs->0, Rd->0}},
		{"vprobe", {vout, 0}, "vp1", {}}
	};
	temp=GetTransferFunction[ temp, {vout, vin} ];
	Simplify[temp]
	,
	{(-Gm + Cgd s)/(Gds + (Cdb + Cgd) s)}
	,
	TestID -> "ElementMOSR-4"
]