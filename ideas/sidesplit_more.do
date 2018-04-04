// Explore reporting direct and indirect effects in network sidesplit

use http://www.mrc-bsu.cam.ac.uk/IW_Stata/meta/thromb.dta, clear
network setup r n, studyvar(study) trtvar(treat)
network sidesplit A B, show
l _des _y_B _S_B_B in 1
nlcom (direct: _b[__000001B] + _b[__000003B]) (indirect: _b[__000001B])
mat l r(V)


network sidesplit B H, show
l _design _y_B _y_H _S_B_B _S_B_H _S_H_H if _design=="B H"
* inco model gives 1.357 (0.435) = direct effect (approx)
nlcom (direct: - _b[__000001B] + _b[__000002H] + _b[__000003B]) ///
	(indirect: - _b[__000001B] + _b[__000002H])
mat l r(V)
