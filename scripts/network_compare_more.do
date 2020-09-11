* extra code to make league plot
* follow network compare, clear
keep if trt1=="A" // ref treat
sort trt2 // or other sort order
gen id=_N-_n+1
labmask id, val(trt2)
gen low=b-1.96*se
gen upp=b+1.96*se
scatter id b [w=1/se^2], ms(S) mstyle(p1) ///
	|| rspike low upp id, horiz lstyle(p1) ///
	ylab(1/14,val notick angle(0)) ///
	legend(off) ///
	xtitle(Comparison of treatment with A) ///
	ytitle(Treatment) ///
	xli(0)
