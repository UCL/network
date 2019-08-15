/*
remember: C (ICSAC) is compared with G (IFS) 
*/
local stb N:\Old Home Drives\ado\stbplus\n

foreach version in 1.2 2.0.2 {
	local vname = strtoname("`version'")
	copy "`stb'/networkplot_v`version'.ado" "`stb'/networkplot.ado", replace
	pda
	use "Continuous outcome arm format.dta", clear
	network setup mean sd n, studyvar(studyid) trtvar(trt) md ref(IFS)
	network map, list trtcodes name(codes`vname',replace) title(networkplot v`version') debug 
	network map, list name(names`vname',replace) title(networkplot v`version') debug 
}
