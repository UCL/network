/*
*! 2sep2021
	added import from standard format
		includes check that each treatment name contains only alphanumeric characters
	returns #components
version 1.7.0 # Ian White # 7apr2021
	added check that ref is one of the treatments
	added check that contrasts are unique and complete within each study
16oct2020
	unabbreviates effect() - avoids errors later in -network map-
4dec2019
	more helpful error message if effect() is not a varname
31may2018
	bug avoider: some choices of treatment codes made designs appear non-unique once spaces are removed
		e.g. design  "B CD" = "BC D"
		this caused error in network meta i
		avoided by returning error here if space-removed designs are non-unique 
6apr2018
	new measure() option
version 1.1 # Ian White # 27may2015
11may2015
    import also from augmented format
    restructured to do this
version 1.0 # Ian White # 9Sep2014 
version 0.8 # 31jul2014
    _design is in sorted order
version 0.7 # Ian White # 11jul2014
version 0.6 # Ian White # 6jun2014
version 0.5 # Ian White # 27jan2014

NB networkplot takes treatments in alphabetical order
*/

program define network_import

// PARSE

* main parse, to identify syntax errors and store options
syntax [if] [in], STUDyvar(varname) [TReat(varlist min=2 max=2) EFFect(name) ///
	STDErr(varname) VARiance(name) ref(string) CONtrast(string) ///
	MEAsure(string) trtlist(string) GENPrefix(string) GENSuffix(string) mult(real 1000)]

* is it pairs format?
cap syntax [if] [in], STUDyvar(varname) TReat(varlist min=2 max=2) ///
	EFFect(name) STDErr(varname) [*]
if !_rc {
	local format pairs
	*confirm var `effect'
	unab effect : `effect', min(1) max(1) name(effect option)
}

else {
	* is it augmented format?
	cap syntax [if] [in], STUDyvar(varname) EFFect(name) ///
		VARiance(name) ref(string) [*]
	if !_rc local format augmented
}

else {
	* is it standard format?
	cap syntax [if] [in], STUDyvar(varname) EFFect(name) ///
		VARiance(name) contrast(name) [*]
	if !_rc local format standard
}

if mi("`format'") {
    di as error "Please specify the option set corresponding to your current data format:" 
    di _col(5) "pairs format:     treat(), effect() and stderr()"
    di _col(5) "augmented format: effect(), variance() and ref()"
    di _col(5) "standard format:  effect(), variance() and contrast()"
    exit 198
}

di as text "Importing from " as result "`format'" as text " format"

if mi("`measure'") local measure (unidentified measure)

// END OF PARSING

preserve // only in case of error - see restore, not later

if "`format'"=="pairs" {

    marksample touse

    tokenize "`treat'"
    local t1 `1'
    local t2 `2'

    * names for variables
    local y `effect'
    local names S trtdiff design contrast component // names to be read or defaulted, as in network_setup
    if mi("`genprefix'`gensuffix'") local genprefix _ // default
    foreach name in `names' {
        local `name' `genprefix'`name'`gensuffix'
    }
    
    // Convert treatment variables to string
    cap confirm numeric var `t1'
    local numeric = _rc==0
    cap confirm numeric var `t2'
    if `numeric' != (_rc==0) {
        di as error "`t1' and `t2' must be both numeric or both string"
        exit 198
    }
    if `numeric' {
        forvalues i=1/2 {
			local vallab`i' : value label `t`i''
			if !mi("`vallab`i''") { 	// has value labels
				decode `t`i'', gen(`t`i''char)
				cap assert mi(`t`i'')==mi(`t`i''char)
				if _rc {
					di as error "`t`i'' is incompletely labelled"
					exit 498
				}
			}
			else { 						// doesn't have value labels
				gen `t`i''char = string(`t`i'')
			}
		}
		if mi("`vallab1'")!=mi("`vallab2'") {
			di as error "`t1' and `t2' must be both labelled or both unlabelled"
			exit 198
		}
        order `t1'char `t2'char, after(`t1')
        drop `t1' `t2'
        rename `t1'char `t1'
        rename `t2'char `t2'
        tempvar length
        gen `length'=max(length(`t1'),length(`t2'))
        summ `length', meanonly
        qui replace `t1' = "0"*(r(max)-length(`t1')) + `t1' if length(`t1')==1
        qui replace `t2' = "0"*(r(max)-length(`t2')) + `t2' if length(`t2')==1
        di "Converted `t1' and `t2' to string"
    }

    // Identify treatments
    qui levelsof `t1', local(t1list) clean
    qui levelsof `t2', local(t2list) clean
    local trtlist : list t1list | t2list
    local trtlist : list sort trtlist
    if mi("`ref'") local ref = word(`"`trtlist'"', 1)
    di `"Reference treatment: `ref'"'
    local trtlistnoref : list trtlist - ref
    local ntrts : word count `trtlist'
    di `"All treatments: `trtlist'"'
	if !`: list ref in trtlist' {
		di as error "Error in ref(`ref'): `ref' is not one of the treatments"
		exit 498
	}

    tempvar row one nrows narms 

*    qui gen `y' = `effect' if `touse'
*    qui gen `stderr' = `stderrvar' if `touse'
    sort `studyvar'
    qui by `studyvar': gen `row'=_n if `touse'

    qui gen `one'=1 if `touse'
    qui egen `nrows' = count(`one') if `touse', by(`studyvar')
    qui gen `narms' = (1+sqrt(1+8*`nrows'))/2 if `touse'
    summ `narms', meanonly
    local maxarms = r(max)
    local maxdim = r(max)-1
    local dim 1

	* create design of study = all treatments reported in that study
    qui gen `design'=""
    tempvar hasone hastrt
    foreach trt in `trtlist' {
    	qui gen `hasone' = (`t1'=="`trt'") | (`t2'=="`trt'") if `touse'
    	qui egen `hastrt' = max(`hasone') if `touse', by(`studyvar')
    	qui replace `design' = `design' + " `trt'" if `hastrt' & `touse'
    	drop `hasone' `hastrt'
    }
	
    * sort designs
    forvalues i=1/`=_N' {
    	local thisdesign = `design'[`i']
    	local thisdesign : list sort thisdesign
    	qui replace `design' = "`thisdesign'" in `i'
    }

    qui gen `contrast' = `t2' + " - " + `t1' if `touse'
	
	* check no contrast is duplicated within a study
	cap isid `studyvar' `contrast'
	if _rc {
		di as error "Error: one or more contrasts is duplicated in the studies listed"
		duplicates list `studyvar' `contrast'
		exit 498
	}

	* check all contrasts are present: it's enough to count them
	tempvar ncontrasts designsize
	egen `ncontrasts' = count(`contrast'), by(`studyvar')
	gen `designsize' = wordcount(`design')
	cap assert `ncontrasts' == `designsize'*(`designsize'-1)/2
	if _rc {
		di as error "Error: there are missing contrasts in the studies listed"
		l `studyvar' `design' `t1' `t2' `y' `se' if `ncontrasts' != `designsize'*(`designsize'-1)/2
		exit 498
	}
	
    // store as characteristics
    local format pairs

} // end of importing pairs data

else if "`format'"=="augmented" {
    * names for variables
    local y `effect'
    local S `variance'
    local names stderr trtdiff design contrast t1 t2 component // names to be read or defaulted, as in network_setup
    if mi("`genprefix'`gensuffix'") local genprefix _ // default
    foreach name in `names' {
        local `name' `genprefix'`name'`gensuffix'
    }
    
    // identify treatments, if not specified
    if mi("`trtlist'") {
        foreach var of varlist `effect'_* {
            local name = subinstr("`var'","`effect'_","",1)
            local trtlistnoref `trtlistnoref' `name'
        }
    }
    else foreach trt of local trtlist {
        if "`trt'"=="`ref'" continue
        confirm var `effect'_`trt'
        local trtlistnoref `trtlistnoref' `trt'
    }
    di "Found treatments: `ref' (ref) `trtlistnoref'"

    // create treatment name macros
    local dim 0
    foreach name in `ref' `trtlistnoref' {
        local trtname`name' `name'
        local trtnames `trtnames' trtname`name'
        local ++dim
    }

    // check we have the required `variance'* variables
    tempvar Smin 
    qui gen `Smin' = . // will hold the study min covariance - large value suggests augmented
    foreach trt1 in `trtlistnoref' {
        qui replace `Smin' = min(`Smin',`variance'_`trt1'_`trt1')
        foreach trt2 in `trtlistnoref' {
            local thiscov
            foreach possvar in `variance'_`trt1'_`trt2' `variance'_`trt2'_`trt1' {
                cap confirm var `possvar'
                if !_rc local thiscov `possvar'
            }
            if mi("`thiscov'") {
                di as error "Covariance of `effect'_`trt1' and `effect'_`trt2' not found"
                exit 498
            }
        }
    }
    summ `Smin', meanonly
    local Sminmin = r(min)

    // designs
    local trtlistsort `ref' `trtlistnoref'
    local trtlistsort : list sort trtlistsort
    qui gen `design'=""
    tempvar hastrt
    foreach trt in `trtlistsort' {
    	if "`trt'"!="`ref'" qui gen `hastrt' = !mi(`effect'_`trt')
        else qui gen `hastrt' = `Smin' < `mult'*`Sminmin'
    	qui replace `design' = `design' + " `trt'" if `hastrt'
        drop `hastrt'
    }

    // maxarms
    tempvar narms
    gen `narms' = wordcount(`design')
    summ `narms', meanonly
    local maxarms = r(max)    
    
    // store as characteristics
    local format augmented

} // end of importing augmented data

else if "`format'"=="standard" {
    * names for variables
    local y `effect'
    local S `variance'
	local contrast `contrast'
    local names stderr trtdiff design t1 t2 component // names to be read or defaulted, as in network_setup
    if mi("`genprefix'`gensuffix'") local genprefix _ // default
    foreach name in `names' {
        local `name' `genprefix'`name'`gensuffix'
    }

	// check y variables and count dimensions
	local dim 0
	set tracedepth 1
	set trace off
	while 1 {
		local ++dim
		cap confirm numeric var `y'_`dim'
		if _rc {
			local dim = `dim'-1
			continue, break
		}
	}
	if `dim'==0 {
		di as error "Variable `y'_1 not found"
		exit 498
	}
	
    // identify treatments and reference, if not specified
	// identify designs
	// check contrast variables
	tempvar strpos trt1 trt2 
	qui gen `design' = ""
	forvalues d=1/`dim' {
		local var `contrast'_`d'
		gen `strpos' = strpos(`var',"-")
		cap assert `strpos' | mi(`var')
		if _rc {
			di as error "Error: values of `var' are not of format trt - trt"
			l `var' if !(`strpos' | mi(`var'))
			exit 498
		}
		qui gen `trt1' = trim(substr(`var',1,`strpos'-1))
		qui gen `trt2' = trim(substr(`var',`strpos'+1,.))
		foreach t in 2 1 { // backward order to start with first trt
			if !regexm(`trt`t'',"^[0-9a-zA-Z]+$") {
				di as error "Error: treatment " as res `t' as error " in contrast " as res "`var'" as error " includes non-alphanumeric characters"
				l `var' if !regexm(`trt`t'',"^[0-9a-zA-Z]+$") & !mi(`trt`t'')
				exit 498
			}
			qui levelsof `trt`t'', local(trtlevels) clean
			if mi("`trtlist'") local trtlist2 : list trtlist2 | trtlevels
			if mi("`ref'") local ref = `trt`t''[1]
			if `t'==1 | `d'==1 qui replace `design' = `design' + cond(!mi(`design',`trt`t'')," ","") + `trt`t''
			else {
				cap assert word(`design',1) == `trt`t'' if !mi(`var')
				if _rc {
					di as error "Error: second treatments in contrasts don't match:" _c
					l `contrast'_1 `var' if word(`design',1) != `trt`t'' & !mi(`var')
					exit 498
				}
			}
		}
		drop `strpos' `trt1' `trt2' 
	}
	if mi("`trtlist'") local trtlist `trtlist2'
	local trtlistnoref : list trtlist - ref
    di "Found treatments: `ref' (ref) `trtlistnoref'"

    // check we have the required `variance'* variables
    forvalues d1=1/`dim' {
		forvalues d2=`d1'/`dim' {
            cap confirm var `S'_`d1'_`d2'
            if _rc {
                di as error "Covariance `S'_`d1'_`d2' not found"
                exit 498
            }
        }
    }

    // maxarms
    tempvar narms
    gen `narms' = wordcount(`design')
    summ `narms', meanonly
    local maxarms = r(max)    
    
    // store as characteristics
    local format standard

} // end of importing standard data

// check for ambiguous designs - in network setup and network import
qui levelsof `design', local(designs)
foreach des1 of local designs {
	foreach des2 of local designs {
		if "`des1'"=="`des2'" continue
		if subinstr("`des1'"," ","",.) == subinstr("`des2'"," ","",.) {
			di as error "Sorry, designs " as result "`des1'" as error " and " as result "`des2'" as error " are too similar."
			di as error "To avoid possible problems in fitting inconsistency models, please change your treatment codes."
			exit 498
		}
	}
}

// GENERATE STUDY COMPONENTS
network_components, design(`design') trtlist(`ref' `trtlistnoref') gen(`component')
assert !mi(`component')
local ncomponents = r(ncomponents)
if `ncomponents'==1 {
	drop `component'
	local component
}
else {
    di as error "Warning: network is disconnected (see variable _component)"
}

// FINISH OFF FOR ALL FORMATS
local allthings allthings studyvar design ref trtlistnoref maxarms measure dim format ///
    d n y S stderr contrast t1 t2 trtdiff testcons_type testcons_stat testcons_df testcons_p ///
    metavars consistency_fitted inconsistency_fitted plot_location ncomponents
foreach thing in `allthings' {
    char _dta[network_`thing'] ``thing''
}

cap estimates drop consistency
cap estimates drop inconsistency 

restore, not 

end
