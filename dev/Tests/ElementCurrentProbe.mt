(* Mathematica Test File *)

Test[
	temp = {"iprobe", {vin,vout}, "cp1", {}};
	SCAPack`Private`CurProbeModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementCurrentProbe-1"
]