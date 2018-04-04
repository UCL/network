program define _makehas
* make dummies has*
if mi("`_dta[network_allthings]'") {
	di as error "Data are not network format"
	exit 498
}

foreach thing in `_dta[network_allthings]' {
    local `thing' : char _dta[network_`thing']
}

foreach trt in `ref' `trtlistnoref' { 
	qui gen has`trt' = strpos(" "+`design'+" "," `trt' ")>0
}

di as text "Variables has* created:"
tabstat has*, by(_des)

end
