(* Mathematica Test File *)

Test[
	 temp = {
   {"volt", {vin, 0}, "v1", {}},
   {"res", {vin, vx}, "r1", {Gvalue -> 1/R}},
   {"res", {vx, 0}, "r2", {Gvalue -> 1/R}},
   {"res", {vx, vy}, "r3", {Gvalue -> 1/R}},
   {"res", {vy, 0}, "r4", {Gvalue -> 1/R}},
   {"cur", {vy, 0}, "i1", {}}
   };
	Drop[ DisableSources[temp, {"v1" -> "Open", "i1" -> "Short"}], 1 ],
	{{"res", {vin, vx}, 
  "r1", {Gvalue -> 1/R}}, {"res", {vx, 0}, 
  "r2", {Gvalue -> 1/R}}, {"res", {vx, vy}, 
  "r3", {Gvalue -> 1/R}}, {"res", {vy, 0}, 
  "r4", {Gvalue -> 1/R}}, {"iprobe", {vy, 0}, "i1", {}}},
  TestID -> "LowLevelDisableSources-1"
]

Test[
	 temp = {
   {"volt", {vin, 0}, "v1", {}},
   {"res", {vin, vx}, "r1", {Gvalue -> 1/R}},
   {"res", {vx, 0}, "r2", {Gvalue -> 1/R}},
   {"res", {vx, vy}, "r3", {Gvalue -> 1/R}},
   {"res", {vy, 0}, "r4", {Gvalue -> 1/R}},
   {"cur", {vy, 0}, "i1", {}}
   };
alt = DisableSources[temp, {"v1" -> "Open", "i1" -> "Short"}];
SolveFullSystem[Append[alt, {"cur", {vin, 0}, "itest", {}}]],
{{I$i1 -> vin/(3 R), I$itest -> (2 vin)/(3 R), vx -> vin/3}},
{Solve::svars},
TestID -> "LowLevelDisableSources-2"
]