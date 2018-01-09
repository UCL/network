/*
*! UNDER DEVELOPMENT - version 1.3.0 - Ian White 17aug2017
*! UNDER DEVELOPMENT - version 1.3.1 - Ian White 29aug2017
28-29aug2017: 
	change varnames to reflect paper:
	move scalars representing priors into model file (but not N etc.)
	wrote help file and tidied up options
8dec2016 nmabayes2.ado 

TO DO:
    find better way to de-augment the data
    does it work for multiple chains?
    update to wbs*.ado
    allow trials with >3 arms in LA [see notes]
    optionally output all differences (e.g. C-B)
    slicker names for graphs - requires parsing `trace2' to detect cgoptions()
	make it store name for future reference - but not as rclass (which is where wbstats are returned)
	make it work for post-estimation - problem is that model is lost? store as global?
	drop gen.inits()?

NOTE
	sigC is scalar, SigmaC is matrix
	in non-common heterogeneity models, sigmascale is the scalar that multiplies 
		the default variance matrix implied by the inverse Wishart prior

PROBLEMS
    in LA and LAplus, sometimes winbugs runs but ends with TRAP 60 (postcondition violated)
		and doesn't output to CODA 
		- I think this is due to working in dropbox
		- not found working on hard disk
	can't set seed in winbugs batch mode

MODEL PARMS
LA:     alphaA[study] muC[treat] deltaC[arm] + sigC
LAplus: alphaA[study] muC[treat] deltaC[study,treat] + sigC or invSigmaC[,]
CB:     LAplus +  muA1 inv2sigA1
AB:     muA[treat]            etaA[study,treat]

*/

prog def network_bayes
syntax, [ ///
	name(string) model(string) noCOMmonhet PRIORonly /// model specification
	seed(int 0) BUrnin(int 1000) UPdates(int 1000) THin(int 1) /// MCMC 
	WINBUGSdir(string) quit noTImer subdir /// WinBUGS
	sigCprior(string) sigAprior(string) sigA1prior(string) /// prior
	sigcprior(string) sigaprior(string) siga1prior(string) /// unallowed prior 
    alphaAprec(string) muAprec(string) muA1prec(string) muCprec(string)  /// prior
    alphaaprec(string) muaprec(string) mua1prec(string) mucprec(string)  /// prior
	df(string) Sigmascale(string) /// prior 
	parms(string) ac AC2(string) notrace TRACE2(string) DENsity DENsity2(string) clear nostats /// output statistics
	debug  /// undocumented options
	]

if mi("`name'") local name network_bayes
if mi("`files'") local files subdir
assert inlist("`files'","current","subdir")
if "`subdir'"=="subdir" {
	cap mkdir `name'
	cap cd `name'
	if _rc {
		di as error "Failed to create subdirectory; changing to files(current)"
		local subdir
	}
	else cap cd ..
}
if "`subdir'"=="subdir" local subdir `name'/
local pwd `=c(pwd)'

local modellist LA LAplus CB AB
if !`: list model in modellist' {
    di as error "Model `model' not yet allowed"
    exit 198
}
* recode commonhet as 0/1
local commonhet = mi("`commonhet'") 
if inlist("`model'", "LA") & !`commonhet' {
	di as error "Option nocommonhet not allowed with model LA"
	exit 198
}
* don't accept options with lowercase C/A
foreach arg in sigCprior sigAprior sigA1prior alphaAprec muAprec muA1prec muCprec {
	local lowerarg = lower("`arg'")
	if !mi("``lowerarg''") {
		di as error "Option `lowerarg'() ignored - please use `arg'()"
		exit 198
	}
}
if mi("`quit'") local timer notimer
local hassigA  = "`model'"=="AB" & `commonhet'
local hassigA1 = "`model'"=="CB" 

// LOAD SAVED NETWORK PARAMETERS
foreach thing in `_dta[network_allthings]' {
	local `thing' : char _dta[network_`thing']
}

// OUTPUT SOME INFORMATION
local col2 as result _col(31)
di as text _n "*** Network bayes (Warning: program under development) ***"

// START ANALYSIS
preserve
foreach type in data scalars inits model script coda log {
	local `type'file = subinstr(`"`pwd'/`subdir'`name'_`type'"',"\","/",.)
}

if "`model'"!="" {
	* PREPARE AND OUTPUT PRIOR PARAMETERS
	di as text "Model:" `col2' "`model'"
	di as text "Heterogeneity:" `col2' cond(`commonhet',"Common","Non-common")
	* which prior parameters are needed
	local hasalphaAprec = inlist("`model'","LA","LAplus") 
	local hasmuA1prec   = inlist("`model'","CB") 
	local hassigA1prior = inlist("`model'","CB") 
	local hasmuAprec    = inlist("`model'","AB")
	local hasmuCprec    = !inlist("`model'","AB")
	local hassigCprior  = `commonhet'
	local hassigmascale = !`commonhet'
	local hasdf         = !`commonhet'
	local hassigAprior = inlist("`model'","AB") & `commonhet'
	* defaults for prior parameters
	foreach parm in alphaAprec muA1prec muAprec muCprec {
		local `parm'default 0.001
		if !mi("``parm''") confirm number ``parm''
	}
	foreach parm in sigA1prior sigCprior sigAprior {
		local `parm'default dunif(0,10)
	}
	local sigmascaledefault 1
	local NTm1 : word count `trtlistnoref'
	local dfdefault = `NTm1' + inlist("`model'","AB")
	* set up prior parameters and output
	foreach parm in alphaAprec muA1prec sigA1prior muAprec muCprec sigCprior sigmascale df sigAprior {
		if `has`parm'' {
			if mi("``parm''") local `parm' ``parm'default'
			di as text "`parm':" `col2' "``parm''" _c
			if "``parm''" == "``parm'default'" di " (default)" _c
			di
		}
		else if !mi("``parm''") {
			di as error "Option `parm'(``parm'') ignored
			local `parm'
		}
	}

	* OPEN FILES
	foreach type in data scalars inits model script {
		local `type'file ``type'file'.txt
		cap file close `type'
		qui file open `type' using "``type'file'", write text replace
	}
    * GET DATA IN DESIRED FORMAT
    if "`format'" != "standard" network convert standard
    if "`outcome'" == "count" {
		local vars `d' `n'
		if "`measure'"=="Log odds ratio" local link logit
		else exit 497
	}
    else if "`outcome'" == "quantitative" local vars `mean' `sd' `n'
    else exit 498
    * de-augment
    if "`outcome'" == "count" {
        tempvar isaug
        gen `isaug'=0
        forvalues i=0/`=`maxarms'-1' {
            qui replace `isaug'=1 if `d'`i' != round(`d'`i',1)
        }
        forvalues i=0/`=`maxarms'-1' {
            qui replace `d'`i' = floor(`d'`i') if `isaug'
            qui replace `n'`i' = `n'`i' - 1    if `isaug'
        }
        drop `isaug'
    }
	* PREPARE FOR RESHAPE
    local keep `studyvar'
    tempvar trtvar trtnum
    forvalues i=0/`=`maxarms'-1' {
    	foreach thing of local vars {
            local keep `keep' `thing'`i'
        }
        qui gen `trtvar'`i' = word(`design',`i'+1)
        local keep `keep' `trtvar'`i'
    }
	keep `keep'
    local i 0
    foreach trt in `ref' `trtlistnoref' {
        local ++i
    	local trt`i' `trt'
        cap confirm number `trt'
        if _rc local string string
    }
	local NT `i'
    tempvar arm
    qui reshape long `vars' `trtvar', i(`studyvar') j(`arm') `string'
    drop `arm'
    qui drop if mi(`trtvar')
    if !mi("`string'") {
    	qui gen `trtnum' = .
    	forvalues i = 1/`NT' {
    		qui replace `trtnum' = `i' if `trtvar' == "`trt`i''"
    	}
    	drop `trtvar'
        rename `trtnum' `trtvar'
    }
    if "`outcome'" == "count" local vars2 r n
    else if "`outcome'" == "quantitative" local vars2 mean sd n
	rename (`studyvar' `trtvar' `vars') (s t `vars2')
	sort s t
    if "`model'"=="LA" {
    	egen b=min(t), by(s)
    	by s: gen m = _n
    	cap assert m<=3
    	if _rc {
    		di as error "Your data has a trial with >3 arms, which this LA model code can't yet handle"
    		exit 498
    	}
        local lavars b m
    }
	* # ARMS, # STUDIES,# TREATMENTS
    local N = _N
	qui summ s
	local NS = r(max)
    * WRITE DATA OUT IN WINBUGS FORMAT
    forvalues i=0/`N' {
        foreach var in s t `vars2' `lavars' {
            if `i'==0 file write data %10s "`var'[] " 
            else file write data %9.0f "`=`var'[`i']' " 
        }
        file write data _n
    }
    file write data "END" _n

	// WRITE MODEL
	local fwm  file write model
	local fwm0 file write model _col(1)
	local fwm1 file write model _col(5)
	local fwm2 file write model _col(9)
	local fwm3 file write model _col(13)
	local commonword = cond(`commonhet',"COMMON","NON-COMMON")
	`fwm0' "## NMA MODEL `model' WITH `commonword' HETEROGENEITY" _n
	if "`prior'"=="prior" `fwm0' "## PRIOR ONLY" _n
	`fwm0' "## WRITTEN BY NETWORK_BAYES.ADO AT `=c(current_time)' ON `=c(current_date)' " _n
	`fwm0' "## In this code:" _n
	if "`outcome'" == "count" `fwm0' "##   Data s[], r[], n[] represent Stata variables `studyvar', `d', `n'" _n
	if "`outcome'" == "quantitative" `fwm0' "##   Data s[], mean[], sd[], n[] represent Stata variables `studyvar', `mean', `sd', `n'" _n
    `fwm0' "##   Scalars N=`N', NS=`NS', NT=`NT' are #arms, #studies, #treatments" _n
	
	`fwm0' _n "model {" _n
	
	`fwm1' _n "## LINK DATA TO THETA" _n
	`fwm1' "for (o in 1:N) {" _n
	if "`outcome'"=="count" `fwm2' "r[o] ~ dbin(theta[o],n[o])" _n
	if "`outcome'"=="quantitative" {			
		`fwm2' "mean[o] ~ dnorm(theta[o], prec[o])" _n
		`fwm2' "prec[o] <- n[o]/pow(sd[o],2)" _n
    }
    `fwm1' "}" _n

	`fwm1' _n "## MODEL THETA" _n
	`fwm1' "for (o in 1:N) {" _n
	if !mi("`link'") `fwm2' "`link'(theta[o]) <- "
	else `fwm2' "theta[o] <- "
	if "`prior'"=="prior" `fwm' "0*(" 
	if "`model'"=="LA" {
		`fwm' "alphaA[s[o]] + deltaC[o]*(1-equals(t[o],b[o]))" 
    }
	else if "`model'"=="AB" {
		`fwm' "muA[t[o]] + etaA[s[o],t[o]]" 
    }
	else {
		`fwm' "alphaA[s[o]] + deltaC[s[o],t[o]]" 
    }
	if "`prior'"=="prior" `fwm' ")" 
	`fwm' _n
    `fwm1' "}" _n
	
	`fwm1' _n "## MODEL STUDY MAIN EFFECTS" _n
	if inlist("`model'","LA","LAplus") {
		`fwm1' "for (i in 1:NS) {" _n
		`fwm2' "alphaA[i] ~ dnorm(0, `alphaAprec')" _n
		`fwm1' "}" _n
	}
	else if "`model'"=="CB" {
		`fwm1' "for (i in 1:NS) {" _n
		`fwm2' "alphaA[i] ~ dnorm(muA1, inv2sigA1)" _n
		`fwm1' "}" _n
		`fwm1' "muA1 ~ dnorm(0, `muA1prec')" _n
		`fwm1' "inv2sigA1 <- 1/pow(sigA1,2)" _n
		`fwm1' "sigA1 ~ `sigA1prior'" _n
	}
	else if "`model'"=="AB" {
		`fwm1' "## (none)" _n
	}

	if "`model'"!="AB" {
		`fwm1' _n "## MODEL OVERALL TREATMENT EFFECTS" _n
		`fwm1' "muC[1] <- 0" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "muC[k] ~ dnorm(0, `muCprec')" _n
		`fwm1' "}" _n
	}
	else {
		`fwm1' _n "## MODEL OVERALL TREATMENT MEANS" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "muA[k] ~ dnorm(0, `muAprec')" _n
		`fwm2' "# useful summaries" _n
		`fwm2' "muC[k] <- muA[k] - muA[1]" _n
		`fwm1' "}" _n
	}

	`fwm1' _n "## MODEL HETEROGENEITY EFFECTS" _n
	if "`model'"=="LA" {
		`fwm1' "for (o in 1:N) {" _n
		`fwm2' "deltaC[o] ~ dnorm(md[o], taud[o])" _n
		`fwm2' "md[o] <- muC[t[o]] - muC[b[o]] + equals(m[o],3)*sw[o]" _n
		`fwm2' "taud[o] <- (1+equals(m[o],3)/3) / pow(sigC, 2)" _n
    	`fwm1' "}" _n
    	`fwm1' "sw[1] <- 0" _n
    	`fwm1' "for (o in 2:N) {" _n
        `fwm2' "sw[o] <- (deltaC[o-1] - md[o-1])/2" _n
    	`fwm1' "}" _n
    }
	else if "`model'"=="AB" {
	    `fwm1' "for (i in 1:NS) { " _n
        `fwm2' "etaA[i,1:NT] ~ dmnorm(zero[1:NT], invSigmaA[1:NT,1:NT])" _n
    	`fwm1' "}" _n
	    `fwm1' "for (k in 1:NT) { " _n
        `fwm2' "zero[k] <- 0" _n
    	`fwm1' "}" _n
	}
	else {
	    `fwm1' "for (i in 1:NS) { " _n
        `fwm2' "deltaC[i,1] <- 0" _n
        `fwm2' "deltaC[i,2:NT] ~ dmnorm(muC[2:NT], invSigmaC[1:NT-1,1:NT-1])" _n
    	`fwm1' "}" _n
    }

	`fwm1' _n "## PRIOR FOR HETEROGENEITY VARIANCE" _n
	if "`model'"=="LA" {
        `fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
	}
	else if inlist("`model'","LAplus","CB") & `commonhet' {
        `fwm1' "for (k in 1:NT-1) {" _n
        `fwm2' "for (l in 1:NT-1) {" _n
        `fwm3' "invSigmaC[k,l] <- 2 * (equals(k,l) - 1/NT) / pow(sigC, 2)" _n // invsigCsq*inv(P)
		`fwm3' "## this is the inverse of sigC^2 * P" _n
	    `fwm3' "## where P is (NT-1)x(NT-1) matrix of diagonal 1's" _n
	    `fwm3' "## and off-diagonal 0.5's" _n
        `fwm2' "}" _n
        `fwm1' "}" _n
        `fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
    }
	else if inlist("`model'","LAplus","CB") & !`commonhet' {
		if `df'==0 {
			di as text "Taking df=NT-1"
			local df = `NT'-1
		}
		if `df'<`NT'-1 {
			di as error "`model' model requires df>#treatments-1
			exit 498
		}
		`fwm1' "invSigmaC[1:NT-1,1:NT-1] ~ dwish(Pscale[1:NT-1,1:NT-1], `df')" _n
		`fwm1' "    ## E[invSigmaC] = inv(`sigmascale'*P)" _n
		`fwm1' "for (k in 1:NT-1) {" _n
		`fwm2' "for (l in 1:NT-1) {" _n
		`fwm3' "Pscale[k,l] <- `sigmascale' * `df' * (equals(k,l)+1) / 2" _n 
		`fwm3' "    ## Pscale = `sigmascale'*`df'*P" _n 
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "" _n
		`fwm1' "# useful summaries" _n
		`fwm1' "SigmaC[1:NT-1,1:NT-1] <- inverse(invSigmaC[1:NT-1,1:NT-1])" _n
		`fwm1' "sigC[1,1] <- 0" _n
		`fwm1' "for (k in 2:NT) {" _n
		`fwm2' "sigC[k,1] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "sigC[1,k] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "for (l in 2:NT) {" _n
		`fwm3' "sigC[k,l] <- sqrt(SigmaC[k-1,k-1]+SigmaC[l-1,l-1]-2*SigmaC[k-1,l-1])" _n
		`fwm3' "## SD of heterogeneity for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
	}
	else if inlist("`model'","AB") & `commonhet' {
        `fwm1' "for (k in 1:NT) {" _n
        `fwm2' "for (l in 1:NT) {" _n
        `fwm3' "invSigmaA[k,l] <- (2/pow(sigC,2)) * ( equals(k,l) - (2*pow(sigA,2))"
		`fwm3' "/ (2*NT*pow(sigA,2)+pow(sigC,2)) )" _n
		`fwm3' "## exchangeable correlation structure" _n
        `fwm2' "}" _n
        `fwm1' "}" _n
        `fwm1' "sigA ~ `sigAprior' ## SD of heterogeneity" _n
        `fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
	}
	else if inlist("`model'","AB") & !`commonhet' {
		if `df'==0 {
			di as text "Taking df=NT"
			local df = `NT'
		}
		if `df'<`NT' {
			di as error "`model' model requires df>#treatments
			exit 498
		}
		`fwm1' "invSigmaA[1:NT,1:NT] ~ dwish(scaleid[1:NT,1:NT], `df')" _n
		`fwm1' "    ## E[invSigmaA] = (2/`sigmascale') * I" _n
		`fwm1' "    ## E[SigmaA] = (`sigmascale'/2) * (`df'/(`df'-NT)) * I" _n
		`fwm1' "    ## mode[SigmaA] = (`sigmascale'/2) * (`df'/(`df'+NT)) * I" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "scaleid[k,l] <- `sigmascale' * `df' * equals(k,l) / 2" _n
		`fwm3' "    ## scaleid = (`sigmascale'/2) * `df' * I" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "" _n
		`fwm1' "# useful summaries" _n
		`fwm1' "SigmaA[1:NT,1:NT] <- inverse(invSigmaA[1:NT,1:NT])" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "sigC[k,l] <- sqrt(SigmaA[k,k]+SigmaA[l,l]-2*SigmaA[k,l])" _n
		`fwm3' "    ## SD of heterogeneity for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
	}
	else exit 496

    * WRITE SCALARS OUT IN WINBUGS FORMAT
	file write scalars "list(" _n
    file write scalars "N=" (`N') ", NS=" (`NS') ", NT=" (`NT')
//     if "`model'"!="AB" {
// 		file write scalars ", muCprec=" (`muCprec') 
// 	}
	file write scalars _n ")" _n
	`fwm' _n "}" _n

    * WRITE INITS OUT IN WINBUGS FORMAT
	file write inits "list(" _n
	local hasinits 0
	* init for sigC
    if `commonhet' {
    	local 0, `sigCprior'
		syntax, [dunif(string) *]
		local sigC 1
		if !mi("`dunif'") {
			tokenize "`dunif'", parse(",")
			if `1'>1 | `3'<1 local sigC = (`1' + `3')/2
		}
    	file write inits "sigC=" (`sigC')
		local hasinits 1
    }
	* init for sigA and sigA1
    if `hassigA' | `hassigA1' { 
		if `hassigA'  local sigAname sigA
		if `hassigA1' local sigAname sigA1
    	local 0, ``sigAname'prior'
		syntax, [dunif(string) *]
		local `sigAname' 1
		if !mi("`dunif'") {
			tokenize "`dunif'", parse(",")
			if `1'>1 | `3'<1 local `sigAname' = (`1' + `3')/2
		}
    	file write inits "`sigAname'=" (``sigAname'')
		local hasinits 1
    }
	* inits for deltaC
	if `hasinits' file write inits "," _n
	if "`model'"=="AB" {
		file write inits "etaA=structure(" 
		file write inits _col(5) ".Data=c("
		forvalues i=1/`NS' {
			forvalues j=1/`NT' {
				file write inits (0)
				if `i'<`NS' | `j'<`NT' file write inits ","
				else file write inits ")," 
			}
		}
		file write inits _col(5) ".Dim=c(`NS',`NT')" _n
		file write inits ")"
	}
	else if "`model'" == "LA" {
		file write inits "deltaC=c("
		forvalues i=1/`N' {
			file write inits (0)
			if `i'<`N' file write inits ","
			else file write inits ")"
		}
	}
	else {
		file write inits "deltaC=structure(" 
		file write inits _col(5) ".Data=c("
		forvalues i=1/`NS' {
			forvalues j=1/`NT' {
				if `j'==1 file write inits "NA"
				else file write inits (0)
				if `i'<`NS' | `j'<`NT' file write inits ","
				else file write inits ")," 
			}
		}
		file write inits _col(5) ".Dim=c(`NS',`NT')" _n
		file write inits ")"
	}
	* inits for alphaA/muA
	local parm = cond("`model'"=="AB", "muA", "alphaA")
	local length = cond("`model'"=="AB", `NT', `NS')
	file write inits "," _n
	file write inits "`parm'=c("
	forvalues i=1/`length' {
		file write inits (0)
		if `i'<`length' file write inits ","
		else file write inits ")"
	}
	* inits for muC
	if "`model'"!="AB" {
		file write inits "," _n
		file write inits "muC=c("
		forvalues i=1/`NT' {
			if `i'==1 file write inits "NA"
			else file write inits (0)
			if `i'<`NT' file write inits ","
			else file write inits ")"
		}
	}
	
	file write inits _n ")" _n
	
	* PARAMETERS OF INTEREST
	local set muC sigC
	if `hassigA' local set `set' sigA
	if `hassigA1' local set `set' sigA1
	
    * WRITE WINBUGS SCRIPT
    file write script "## SCRIPT TO FIT NMA MODEL `model' WITH `commonword' HETEROGENEITY" _n
	if "`prior'"=="prior" file write script  "## PRIOR ONLY" _n
	file write script "## WRITTEN BY NETWORK_BAYES.ADO AT `=c(current_time)' ON `=c(current_date)' " _n(2)
    file write script "display('log')" _n
    file write script "check('`modelfile'')" _n
    file write script "data('`datafile'')" _n
    file write script "data('`scalarsfile'')" _n
    file write script "compile(1)" _n
    file write script "inits(1,'`initsfile'')" _n
    file write script "gen.inits()" _n
    file write script "update(`burnin')" _n
    foreach setparm of local set {
		file write script "set(`setparm')" _n
	}
    file write script "thin.samples(`thin')" _n
    file write script "update(`updates')" _n
    file write script "coda(*,'`codafile'')" _n
    file write script "save('`logfile'.txt')" _n
    if !mi("`quit'") file write script "quit()" _n
    else file write script _n
	
	di as text "Writing files ..." _c
	foreach type in data scalars inits model script {
		file close `type'
		di as text _col(19) "`type' to `subdir'`name'_`type'.txt"
	}
	
    * ERASE PREVIOUS RESULTS
    cap erase `pwd'/`subdir'`name'_codaIndex.txt
    if !_rc forvalues i=1/99 {
        cap erase `pwd'/`subdir'`name'_coda`i'.txt
        if _rc continue, break
    }
    
    * RUN WINBUGS
	di as text "Running Winbugs ... " _c
	if "`timer'"!="notimer" {
		timer clear 99
		timer on 99
	}
	if mi("`winbugsdir'") local winbugsdir C:\WinBUGS14\
	shell "`winbugsdir'WinBUGS14.exe" /PAR "`scriptfile'"
	
    cap confirm file `pwd'/`subdir'`name'_codaIndex.txt
    if _rc {
        di as error "Winbugs run failed"
        exit 498
    }
	di as text "finished" _c
	if "`timer'"!="notimer" {
		timer off 99
		qui timer list 99
		di as text " (run time " as result r(t99) as text " seconds)"
		timer clear 99
	}
	else di


    // READ CODA DATA
    wbcoda, root(`pwd'/`subdir'`name'_coda) clear
    * rename contrasts
    if !mi("`debug'") summ
    forvalues i = 2/`NT' { // assumes muC is a NT-1 vector
    	rename muC_`i' diff_`trt`i''_`ref'
    	label var diff_`trt`i''_`ref' "Mean diff `trt`i'' vs `ref'"
    }
    if "`model'"=="AB" & !`commonhet' {
        drop muC_1 
		if !`commonhet' drop sigC_1_1
    }
    if !`commonhet' { // assumes sigC is a NT by NT matrix
        forvalues i = 1/`NT' {
            forvalues j = 1/`NT' {
                if `i'==1 & `j'==1 continue
                rename sigC_`i'_`j' sigC_`trt`i''_`trt`j''
                label var sigC_`trt`i''_`trt`j'' "Het SD `trt`j'' vs `trt`i''"
                if `j'<=`i' drop sigC_`trt`i''_`trt`j''
            }
        }
    }
    else label var sigC "Het SD (all contrasts)"
    if `hassigA'  rename sigA  sigA_`ref'
    if `hassigA1' rename sigA1 sigA_`ref'
    if `hassigA' | `hassigA1' label var sigA_`ref' "SD of reference mean"
	label data "network bayes results: burnin `burnin', updates `updates', thin `thin', model `model' `commonhet'"
    qui save `subdir'`name'_sample, replace
    di as text "Bugs output saved in `subdir'`name'_sample.dta"
}
else {
	cap use `subdir'`name'_sample, clear
	if _rc {
		di as error "No model statement, and no saved results found in `subdir'`name'_sample"
		exit 498
	}
}

// OUTPUT RESULTS
di as text "" _c // just gets font right for next
if missing("`parms'") {
    if `commonhet' local parms diff_* sigC
    else local parms diff_* sigC_*
}
if `hassigA' | `hassigA1' local parms `parms' sigA_`ref'
if mi("`stats'") wbstats `parms'
if !mi("`ac'`ac2'") wbac `parms', `ac2'
if mi("`trace'`trace2'") wbtrace `parms' // default is to show the trace
else if !mi("`trace2'") wbtrace `parms', cgoptions(`trace2')
if !mi("`density'`density2'") wbdensity `parms', `density2'

// RESTORE OR NOT
if !mi("`clear'") {
	restore, not
	di as text "Bugs output is now loaded into memory"
}

end
