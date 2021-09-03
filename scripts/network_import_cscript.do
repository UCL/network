/* 
Initial test for new network import
IW 2sep2021
*/

pda

// Open smoking data
use smoking, clear
keep study trt d n
network setup d n, studyvar(stud) trtvar(trt) format(sta) 

// Run NMA and store results
network meta c
local b1 = _b[ _trtdiff1_B]
local s1 = _se[ _trtdiff1_B]

// Unset data 
network unset
drop _design d? n?

// make things harder
order _contrast_2 _contrast_1 _y_2 _S_1_2 _y_1 _S_1_1 _S_2_2
rename (_y*) (myeffect*)
replace _contrast_1 = subinstr( _contrast_1," - ","-",.)
replace _contrast_2 = subinstr( _contrast_2," - ","  -  ",.)
* replace _contrast_2 = "D-C" in 3 // causes error

// see if import works
network import, studyvar(study) effect(myeffect) variance(_S) contrast(_contrast) ref(B)
network meta c
assert reldif(-_b[ _trtdiff1_A], `b1') < 1E-7
assert reldif(_se[ _trtdiff1_A], `s1') < 1E-7



