(* Mathematica Test File *)

Test[
	temp = {
   {"volt", {vin, 0}, "v1", {}}, {"res", {vin, vx}, 
    "r1", {Gvalue -> 1/R}}, {"res", {vx, 0}, 
    "r2", {Gvalue -> 1/R}}, {"res", {vx, vy}, 
    "r3", {Gvalue -> 1/R}}, {"res", {vy, 0}, "r4", {Gvalue -> 1/R}},
   {"nodeset", {}, "ns1", {vin -> 1}}
   };
	NetlistCircuit[temp],
	{I$v1 + (1 - vx)/R == 0, -((1 - vx)/R) + vx/R + (vx - vy)/R == 
  0, -((vx - vy)/R) + vy/R == 0, vin == 1},
  TestID -> "ControlNodeset-1"
]