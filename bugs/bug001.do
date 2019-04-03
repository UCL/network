/*
Reproduce bug 001
*/

clear
set seed 481606

drop _all
qui set obs 2000
gen study=_n
gen u = rnormal(-0.6,0.3)
gen v = rnormal(0.6,0.3)
gen n = 10
qui expand 2
sort study
by study: gen treat = _n-1
gen prob = invlogit(u+treat*v)
drop u v
gen n1 = rbinomial(n,prob)
drop prob

network setup n1 n, study(study) trt(treat)
