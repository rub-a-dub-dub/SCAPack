(* Mathematica Test File *)

Test[
	temp = {"resnoisy", {vin,vout}, "r1", {}};
	SCAPack`Private`ResNoisyModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementResistorNoisy-1"
]

Test[
	temp = {"resnoisy", {vin,vout}, "r1", {Gvalue->20}};
	SCAPack`Private`ResNoisyModel["ErrorCheck"][ temp, temp ],
	Null,
	TestID -> "ElementResistorNoisy-2"
]