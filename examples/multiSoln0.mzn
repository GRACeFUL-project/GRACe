var -10000000..10000000: v4;
var float: v3;
var -10000000..10000000: v2;
var -2..2: v1;
var -2..2: v0;

constraint ((v3) == (int2float(v4)));
constraint ((v2) == (v4));
constraint ((not (((not ((not (((v0) == (0)) /\ ((not ((v1) == (0))))))))) /\ ((not ((v2) == (5)))))));
constraint ((not (((not ((not (((v1) == (0)) /\ ((not ((v0) == (0))))))))) /\ ((not ((v2) == (5)))))));
constraint ((not (((not ((not (((not ((v0) == (0)))) /\ ((not ((v1) == (0))))))))) /\ ((not ((v2) == (0)))))));
constraint ((not (((not ((not (((v0) == (0)) /\ ((v1) == (0))))))) /\ ((not ((v2) == (10)))))));
constraint ((not (((not ((v0) == (2)))) /\ ((not ((v1) == (2)))))));


solve maximize (0+v3);
output ["{\"p1\" : \(v0),\n\"p2\" : \(v1),\n\"s\" : \(v2)}"];
