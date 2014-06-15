(* Mathematica Test File *)

(* positive test *)
Test[
	NetlistCircuit[{
		{"volt",{vin,0},"v1",{}},
		{"res",{vin,vo},"r1",{Gvalue->G}},
		{"res",{vo,0},"r2",{Gvalue->1/R2}}	
	}],
	{I$v1 + G (vin - vo) == 0, -G (vin - vo) + vo/R2 == 0},
	TestID->"ElementResistor-1"
]

Test[
	temp = {"res", {vin, 0}, "v1", {}};
	SCAPack`Private`ResModel["ErrorCheck"][temp, temp],
	Null,
	TestID -> "ElementResistor-2"
]

Test[
	temp = {"res", {vin, 0}, "v1", {Gvalue->10}};
	SCAPack`Private`ResModel["ErrorCheck"][temp, temp],
	Null,
	TestID -> "ElementResistor-3"
]

Test[
	NetlistCircuit[{
		{"volt",{vin,0},"v1",{}},
		{"res",{vin,vo},"r1",{Rvalue->1/G}},
		{"res",{vo,0},"r2",{Rvalue->R2}}	
	}],
	{I$v1 + G (vin - vo) == 0, -G (vin - vo) + vo/R2 == 0},
	TestID->"ElementResistor-4"
]