(* Mathematica Test File *)

Test[
	temp = {"cur", {v1, v2}, "i1", {}};
	SCAPack`Private`CurModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementCur-1"
]