/*
*! version 1.6.1 # Ian White # 4sep2019
	treatments are displayed in correct order (ref first)
	- previously treatment names were given in alphabetical order
*! version 1.3.0 # Ian White # 17aug2017 
	drops unwanted variables before reshape 
	(avoids failure e.g. if a variable starts "d")
version 1.1 # Ian White # 8jun2015 
	"binomial" changed to "count"
14mar2015
	for disconnected networks, tabulates by component
version 1.0 # Ian White # 9Sep2014 
version 0.8 # 31jul2014 
    works with new trt codes & names
    new trtcodes option
version 0.7 # 11jul2014
version 0.6 # 6jun2014
version 0.5 # 27jan2014 
    Works from pairs format
*/
prog def network_table
syntax [if] [in], [TRTCodes *]
foreach thing in `_dta[network_allthings]' {
    local `thing' : char _dta[network_`thing']
}
if "`outcome'"=="count" local rawvars `d' `n'
else if "`outcome'"=="quantitative" local rawvars `mean' `sd' `n'
if mi("`rawvars'") {
    di as error "network table: no raw data found - probably because you didn't use -network setup-"
    exit 498
}
preserve
marksample touse
qui keep if `touse'

qui network convert augmented, nowarning

* delete augmented values
local keeplist `studyvar'
foreach trt in `ref' `trtlistnoref' {
    foreach type in `rawvars' {
	    qui replace `type'`trt'=. if strpos(" "+`design'+" ", " `trt' ")==0
        local keeplist `keeplist' `type'`trt'
	}
}

* new 17aug2017: keep only variables needed
local keepvars `studyvar'
foreach rawvar of local rawvars {
	foreach trt in `ref' `trtlistnoref' {
		local keepvars `keepvars' `rawvar'`trt'
	}
}
keep `keepvars'

tempvar trt A stat
* reformat data and display
qui reshape long `rawvars', i(`studyvar') j(`trt') string
local i 0
foreach vartype in `rawvars' {
    local ++i
    rename `vartype' `A'`i'
}
qui reshape long `A', i(`studyvar' `trt') j(`stat') 
qui drop if mi(`A')
if "`outcome'"=="count" label def `stat' 1 "`d'" 2 "`n'"
else if "`outcome'"=="quantitative" label def `stat' 1 "`mean'" 2 "`sd'" 3 "`n'"
label val `stat' `stat'
label var `stat' "Statistic"

* create new treatment variable in correct order
qui gen `trt'2=.
local i 0
foreach code in `ref' `trtlistnoref' {
	local ++i
	qui replace `trt'2=`i' if `trt'=="`code'"
	if mi(`"`trtcodes'"') local trtdisplay `trtname`code''
	else local trtdisplay `code'
	label def `trt'2 `i' `"`trtdisplay'"', modify
}
label val `trt'2 `trt'2
label var `trt'2 "Treatment"

if !mi("`component'") {
	di "Displaying components separately"
	local bycpt bysort `component': 
}
`bycpt' tabdisp `studyvar' `stat' `trt'2, c(`A') `options'
end
