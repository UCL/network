/*
Check revised network map 4sep2019
Tough test: non-default reference treatment
Needs visual inspection
*/
cd "N:\Old Home Drives\ado\ian\network\scripts"

foreach ref in SK tPA {
foreach ver in 1.0 1.2 2.0.2 {
	local ver2 = strtoname("`ver'")
	pda
	copy "N:\Old Home Drives\ado\stbplus\n\networkplot_v`ver'.ado" ///
		./networkplot.ado, replace
	use thromb, clear
	encode treat, gen(trt)
	drop treat
	label def trt 1 "SK" 2 "AtPA" 3 "tPA" 6 "Ret" ///
		5 "Ten"  4 "SK+tPA"  7 "UK"  8 "ASPAC", modify
	network setup r n, studyvar(study) trtvar(trt) numcodes ref(`ref')
	network table

	which networkplot
	vercheck networkplot
	network map
	graph rename map`ver2'_`ref',replace
}
}
