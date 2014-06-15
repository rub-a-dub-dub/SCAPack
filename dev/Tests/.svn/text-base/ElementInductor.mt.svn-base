(* Mathematica Test File *)

Test[
	temp = {"ind", {vin,vout}, "l1", {}};
	SCAPack`Private`IndModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementInductor-1"
]

Test[
	temp = {"ind", {vin,vout}, "l1", {Lvalue->20}};
	SCAPack`Private`IndModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementInductor-2"
]