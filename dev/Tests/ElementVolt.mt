(* Mathematica Test File *)

Test[
	temp = {"volt", {vin, 0}, "v1", {}};
	SCAPack`Private`VoltModel["ErrorCheck"][temp, temp],
	Null,
	TestID -> "ElementVolt-1"
]