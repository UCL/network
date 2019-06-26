prog def network_bayes_output
cap use `savedir'`name'_sample, clear
if _rc {
	di as error "No model statement, and no saved results found in `savedir'`name'_sample"
	exit 498
}
di as text "Using results stored in " as result "`savedir'`name'_sample.dta"

// OUTPUT RESULTS
di as text "" _c // just gets font right for next
* identify parms of interest
if `commonhet' local outparms muC_* sigC
else local outparms muC_* sigC_*
if `setsigA' {	
	if "`model'"=="3CB" local outparms `outparms' sigA_`ref'
	else local outparms `outparms' sigA
}
if `setSigmaA' local outparms `outparms' sigA_*
local outparms : list outparms | extraparms
* check if any parm should be replaced by parm*
foreach parm of local outparms {
	cap unab junk : `parm'
	if !_rc { // var or varlist exists as named
		local wbparms0 `wbparms0' `parm'
		continue
	}
	cap unab junk : `parm'*
	if !_rc {
		local wbparms0 `wbparms0' `parm'*
		continue
	}
	di as error "Program error: parameter `parm' not found"
}
* check if any parm has zero variance (crashed wbstats)
foreach parm of local wbparms0 {
	qui summ `parm'
	if r(sd)==0 {
		di as error "Parameter `parm' does not vary - ignored"
	}
	else local wbparms `wbparms' `parm'
}
* do results
if mi("`stats'") wbstats `wbparms'
if mi("`trace'") fasttrace, `trace2'
if !mi("`ac'`ac2'") fastac `wbparms', `ac2'
if !mi("`density'`density2'") wbdensity `wbparms', `density2'

// RESTORE OR NOT
if !mi("`clear'") {
	restore, not
	di as text "Bugs output is now loaded into memory"
}

end

****************** END OF NETWORK_BAYES *********************************************

********************* PROGRAM FASTTRACE ******************************************

prog def fasttrace
// quick trace
syntax [anything] [using/] [if] [in], [title(passthru) note(passthru) byopts(string) *]
preserve
if !mi("`using'") qui use `using', clear
if !mi("`anything'") qui keep order `anything'
if !mi("`if'`in'") qui keep `if' `in'
rename (*) (value*)
rename valueorder order
qui reshape long value, i(order) j(parm) string
line value order, by(parm, yrescale note("") `byopts' `title' `note') ///
	xtitle("Samples") ytitle("") `options'
end

********************* PROGRAM FASTAC ******************************************

prog def fastac
// quick trace
syntax [anything] [using/] [if] [in], ///
	[title(passthru) note(passthru) LAGs(int 40) SEParate *]
preserve
if !mi("`using'") qui use `using', clear
if !mi("`anything'") qui keep order `anything'
else {
	unab allvars : _all
	local dropvars order
	local anything : list allvars - dropvars
}
if !mi("`if'`in'") qui keep `if' `in'
tempname post
tempfile file
postfile `post' str20 parm lag corr using `file'
tempvar lagvar
qui gen `lagvar'=.
foreach parm of varlist `anything' {
	qui gen corr`parm'=.
	forvalues lag=1/`lags' {
		qui replace `lagvar'=`parm'[_n-`lag']
		qui corr `parm' `lagvar'
		qui replace corr`parm'=r(rho) in `lag'
	}
	local corrlist `corrlist' corr`parm'
	label  var corr`parm' "`parm'"
}
qui keep in 1/`lags'
keep `corrlist'
gen lag=_n
local opts1 xtitle("Lags") ytitle("Autocorrelation") ///
		yscale(range(0 1)) ylabel(0(0.2)1)
if mi("`separate'") {
	line `corrlist' lag, `opts1' `title' `note' `options' 
}
else {
	qui reshape long corr, i(lag) j(parm) string
	line corr lag, by(parm, note("") `title' `note') ///
		`opts1' `options' 
}
end

********************* PROGRAM WRITEINITS ******************************************

/*
Example of use:
	file open inits using zinits.txt, write replace
	writeinits A, dim(24,4) na(.,1)
	writeinits B, dim(6) value(1) na(3)
	writeinits C, value(2)
	file close inits
*/
prog def writeinits
syntax name, [dim(string) value(real 0) na(string) IDentity]
local name `namelist'
if mi("`dim'") {
	if !mi("`na'") exit198 "writeinits: can't have na() without dim()"
	local dims 0
}
else {
	tokenize "`dim'", parse(",")
	if !mi("`4'") exit198 "dim() too long"
	if !inlist("`2'","",",") exit198 "writeinits: wrong dim()"
	if mi("`3'") {
		local dims 1
		local n1 `1'
		if !mi("`na'") {
			tokenize "`na'", parse(",")
			if "`2'"!="" exit198 "writeinits: wrong dim() for 1dim"
			local na1 `1'
		}
		else local na1 .
	}
	else {
		local dims 2
		local n1 `1'
		local n2 `3'
		if !mi("`na'") {
			tokenize "`na'", parse(",")
			if "`2'"!="," exit198 "writeinits: wrong dim() for 2dim"
			local na1 `1'
			local na2 `3'
		}
		else {
			local na1 .
			local na2 .
		}
	}
}
if !mi("`identity'") & (`dims'<2 | "`n1'"!="`n2'" | !mi("`na'") | `value' != 0) {
	exit198 "writeinits: error in identity"
}

if `dims'==2 {
	file write inits "`name'=structure(" _n
	file write inits _col(5) ".Data=c("
	forvalues i=1/`n1' {
		forvalues j=1/`n2' {
			if !mi("`identity'") file write inits (`=`i'==`j'')
			else if `i'==`na1' | `j'==`na2' file write inits "NA"
			else file write inits (`value')
			if `i'<`n1' | `j'<`n2' file write inits ","
			else file write inits ")," _n
		}
	}
	file write inits _col(5) ".Dim=c(`n1',`n2'))," _n
}
	
if `dims'==1 {
	file write inits "`name'=c("
	forvalues i=1/`n1' {
		if `i'==`na1' file write inits "NA"
		else file write inits (`value')
		if `i'<`n1' file write inits ","
		else file write inits ")," _n
	}
}
if `dims'==0 {
   	file write inits "`name'=" (`value') "," _n
}

end

prog def exit198
di as error `0'
exit 198
end
