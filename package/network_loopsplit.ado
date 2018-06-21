/* 
31may2018
	uses `y', `S' not _y, _S
	allows bscov() and other mvmeta options
Version 1, 5 Jan 2018
	Loop-split ABC by DIY method
To do: 
	tempvars
	allow standard network meta options
*/
program define network_loopsplit

// PARSING
syntax namelist(min=3 max=3), [BSCOVariance(passthru) fixed options]
local loopall `namelist'
tokenize `loopall'
local loop1 `1'
local loop2 `2'
local loop3 `3'

// LOAD SAVED NETWORK PARAMETERS
foreach thing in `_dta[network_allthings]' {
	local `thing' : char _dta[network_`thing']
}

local trtlist `ref' `trtlistnoref'

// check args are known treatments
local diff : list loopall - trtlist
if !mi("`diff'") {
	di as error "Unknown treatment(s): `diff'"
	exit 198
}

// START
preserve
cap network convert augmented
foreach dropvar of local metavars {
	cap drop `dropvar'
}
local metavars

// define contrasts
foreach trt of local trtlistnoref {
	gen _inco`trt' = 0
	local metavars `metavars' _inco`trt'
}
tempvar thisdesign
gen byte `thisdesign' = 0
forvalues i=1/3 {
	local iplus1 = 1 + mod(`i', 3)
	local iplus2 = 1 + mod(`i'+1, 3)
	
	local trt1 `loop`i''
	local trt2 `loop`iplus1''
	local trt3 `loop`iplus2''

	qui replace `thisdesign' ///
		=  strpos(" " + _design + " ", " `trt1' ") ///
		&  strpos(" " + _design + " ", " `trt2' ") ///
		& !strpos(" " + _design + " ", " `trt3' ")
	
	local loopnot : list trtlist - loopall
	if "`trt1'"=="`ref'" {
		qui replace _inco`trt2' = 1 if `thisdesign'
		foreach trt of local loopnot {
			qui replace _inco`trt' = 1/2 if `thisdesign' ///
				& strpos(" " + _design + " ", " `trt' ")
		}
	}
	else if "`trt2'"=="`ref'" {
		qui replace _inco`trt1' = -1 if `thisdesign'
		foreach trt of local loopnot {
			qui replace _inco`trt' = -1/2 if `thisdesign' ///
				& strpos(" " + _design + " ", " `trt' ")
		}
	}
	else {
		qui replace _inco`trt1' = -1/2 if `thisdesign'
		qui replace _inco`trt2' =  1/2 if `thisdesign'
	}
}
tabstat _inco*, by(_des) nototal

// run model
gen _trtdiffzero = 0
gen _trtdiffone  = 1
local metavars `metavars' _trtdiffzero _trtdiffone
local first 1
foreach trt of local trtlistnoref {
	gen _trtdiff`trt' = `first'
	rename _inco`trt' _inco`trt'_`loop1'`loop2'`loop3'
	local metavars `metavars' _trtdiff`trt' _inco`trt'_`loop1'`loop2'`loop3'
	local eq`trt' `y'_`trt': 
	foreach trt2 of local trtlistnoref {
		if `first' local eq`trt' `eq`trt'' _trtdiff`trt2'
		else if "`trt2'"=="`trt'" local eq`trt' `eq`trt'' _trtdiffone
		else if "`trt2'"!="`trt'" local eq`trt' `eq`trt'' _trtdiffzero
	}
	local eq`trt' `eq`trt'' _inco`trt'_`loop1'`loop2'`loop3'
	if `first' local eqs `eq`trt''
	else local eqs `eqs', `eq`trt''
	local first 0
}

if mi("`bscovariance'`fixed'") local bscovariance bscovariance(exch 0.5) 
global F9 mvmeta `y' `S', `bscovariance' `fixed' eq( `eqs' ) ///
	commonparm noconst network(sidesplit) suppress(uv mm)
di as input "Command is: $F9"
$F9
char _dta[network_metavars] `metavars'
restore, not

end
