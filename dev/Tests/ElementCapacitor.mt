(* Mathematica Test File *)

Test[
	temp = {"cap", {vin,vout}, "c1", {}};
	SCAPack`Private`CapModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementCapacitor-1"
]

Test[
	temp = {"cap", {vin,vout}, "c1", {Cvalue->20}};
	SCAPack`Private`CapModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementCapacitor-2"
]