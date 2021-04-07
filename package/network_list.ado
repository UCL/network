/*
Utility to list non-missing treatment effects and variances by study
v0.1 IW 7apr2021
*/

prog def network_list
syntax, [*]
// Load saved network parameters
if mi("`_dta[network_allthings]'") {
	di as error "Data are not in network format"
	exit 459
}
foreach thing in `_dta[network_allthings]' {
    local `thing' : char _dta[network_`thing']
}

// Handle possibility that network parameters are undefined
cap confirm var `design'
if _rc local design
cap confirm var `studyvar'
if _rc local studyvar

// Listing, different for each format
di as txt "Data are in " as res "`format'" as txt " format"
if "`format'"=="standard" {
	l `studyvar' `design' `y'* `S'*, `options'
}

if "`format'"=="pairs" {
	l `studyvar' `design' `t1' `t2' `y' `stderr', `options'
}

if "`format'"=="augmented" {
	di as txt "Listing of non-missing treatment effects and variances by study"
	forvalues i=1/`=_N' {
		local yvars
		local Svars
		foreach trt in `trtlistnoref' {
			if !mi(`y'_`trt'[`i']) local yvars `yvars' `y'_`trt'
			foreach trt2 in `trtlistnoref' {
				cap di `S'_`trt'_`trt2'
				if _rc continue
				if !mi(`S'_`trt'_`trt2'[`i']) local Svars `Svars' `S'_`trt'_`trt2'
			}
		}
		l `studyvar' `design' `yvars' `Svars' in `i', `options'
	}
}

end
