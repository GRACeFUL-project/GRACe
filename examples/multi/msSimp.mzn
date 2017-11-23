var -2 .. 2: v1;
var -2 .. 2: v0;
var -10 .. 10: v2;
constraint
  ((v0 = 0) /\ (v1 != 0)) -> (v2 =5);
constraint
  ((v1 = 0) /\ (v0 != 0)) -> (v2 =5);
constraint
  ((v0 != 0) /\ (v1 != 0)) -> (v2 =0);
constraint
  ((v1 = 2) \/ (v0 = 2));

solve maximize 0 + v2;
output ["{", "'p1' : ", show((v0)), ",\n", "'p2' : ", show((v1)),
        ",\n", "'s' : ", show((v2)), "}"];