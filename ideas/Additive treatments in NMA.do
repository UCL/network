// Demonstration of allowing additivity between selected treatment effects
// using smoking data and pretending D=B+C

use smoking, clear
network setup d n, studyvar(stud) trtvar(trt)
network convert standard

* NMA with D as a separate treatment
network meta c

* assume D-A = B-A + C-A
replace _trtdiff1_B = _trtdiff1_B + _trtdiff1_D
replace _trtdiff1_C = _trtdiff1_C + _trtdiff1_D
replace _trtdiff2_B = _trtdiff2_B + _trtdiff2_D
replace _trtdiff2_C = _trtdiff2_C + _trtdiff2_D
* now press F9 and remove _trtdiff*_D from the eq() option, giving:
mvmeta _y _S  , bscovariance(exch 0.5) ///
	commonparm noconstant suppress(uv mm) ///
	eq(_y_1: _trtdiff1_B _trtdiff1_C, _y_2: _trtdiff2_B _trtdiff2_C) ///
	vars(_y_1 _y_2)
