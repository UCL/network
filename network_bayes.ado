/*
*! version 1.4 - Ian White - 15mar2018
v1.4 15mar2018: 
	changing parameterisation to match paper:
		W(fR,nu) not W(nu*fR,nu)
	allow prior to be specified for either sigA or sigA2
		but always monitor sigA2
		same for sigC, logsigAmean, logsigCmean
		muA1 now renamed muA
27-28nov2017
	rhoprior(dbeta()) gives sensible starting value.
	tidied up fastac(), fasttrace() and their help entries.
23nov2017
	AB NCH model has mean of inv(SigmaA) proportional to P(rho), not I.
		This aims to uncouple prior for arm heterog from that for contrast heterog.
		New logsigAmean(#) option.
22nov2017
	better label for samples data
	outputs sigA parms for AB NCH model
20nov2017 - runs as post-estimation w/o network setup
version 1.3.3 - Ian White - 13oct2017-18oct2017
	changed subdir and  files options to new savedir(dir[,create])
	changed sigmascale option to logsigCmean = prior mean of log(sigC)
		- more interpretable
		->  DO: also allow user to set prior mean of SigmaC[i,i]
	fast trace and AC plots
	still used from winbugs suite: wbcoda, wbstats, wbdensity 
version 1.3.2 - Ian White - 30aug2017
version 1.3.2.1 - 9oct2017
	ensure studyvar is numeric and 1,2,3,...
	to do this, it sorts on studyvar
version 1.3.2 - 30aug2017
	correct the ABcommonhet model
version 1.3.1 - 28-29aug2017: 
	change varnames to reflect paper:
	move scalars representing priors into model file (but not N etc.)
	wrote help file and tidied up options
version 1.3.0 - Ian White 17aug2017
8dec2016 nmabayes2.ado 

TO DO
    how can I output credible limits?
	improve post-estimation
		- currently requires network data
		- errors with different models
		- report all parms saved
	find better way to de-augment the data
    does it work for multiple chains?
    update to wbs*.ado
    allow trials with >3 arms in LA [see notes]
    optionally output all differences (e.g. C-B)
    slicker names for graphs - requires parsing `trace2' to detect cgoptions()
	make it store name for future reference - but not as rclass (which is where wbstats are returned)
	drop gen.inits() - FIND  OUT WHY IT'S NEEDED FOR MODEL(CB3)
	need to save results for replay mode (fails for ABc model)
	utility to look at logsigC instead of sigC?
	should default name = model?
	wbac produces nasty plots
	change model names to match paper
	consider using fast tracei.ado
	allow direct specification of mean of inverse Wisharts (rather than via mean of logsigC)

NOTE
	sigC2 is scalar (with prior sigC2prior)
	SigmaC is matrix (with prior proportional to exp(logsigC2mean)

PROBLEMS
    in LA and LAplus, sometimes winbugs runs but ends with TRAP 60 (postcondition violated)
		and doesn't output to CODA 
		- I think this is due to working in dropbox
		- not found working on hard disk
	can't set seed in winbugs batch mode

*/

prog def network_bayes

* some prior options require upper case: check for use of lower case
local prior_options_respect_case /// 
	alphaAprec muAprec muCprec sigAprior sigA2prior ///
		/// priors for main effects
	sigCprior sigC2prior rhoprior logsigCmean logsigC2mean ///
	logsigAmean logsigA2mean df ///
		// priors for heterogeneity variances
foreach option of local prior_options_respect_case {
	local prior_options `prior_options' `option'(string)
	local loweroption = lower("`option'")
	if "`loweroption'" == "`option'" continue	
	syntax, [`loweroption'(string) *]
	if !missing("``loweroption''") {
		di as error "Option `loweroption'() ignored - please use `option'()"
		exit 198
	}
}

* main parsing

syntax, [ ///
	name(name) model(string) noCOMmonhet PRIORonly /// model specification
	seed(int 0) BUrnin(int 1000) UPdates(int 1000) THin(int 1) dryrun /// MCMC 
	WINBUGSdir(string) parms(string) Quitbugs noTImer SAVedir(string) /// WinBUGS
    `prior_options' /// prior options (from above)
	ac AC2(string) notrace TRACE2(string) DENsity DENsity2(string) nostats clear /// output options
	debug /// undocumented options
	]

if mi("`name'") local name network_bayes

* where files are stored: savedir = stata name of directory, filepath = full path
if !mi("`savedir'") {
	local wd = c(pwd)
	local 0 `savedir'
	syntax anything, [Create]
	if !mi("`create'") {
		cap cd `anything'
		if _rc {
			mkdir `anything'
			di as text "Directory `anything' created"
			qui cd `anything'
		}
	}
	else qui cd `anything'
	local filepath = c(pwd)
	qui cd `wd'
	local savedir `anything'
	local endsavedir = substr("`savedir'",length("`savedir'"),1)
	if "`endsavedir'" == "\" local savedir = substr("`savedir'",1,length("`savedir'")-1)+"/"
	else if "`endsavedir'" != "/" local savedir `savedir'/
}
else {
	local filepath = c(pwd)
}
local filepath `filepath'/

local modellist CB1 CB2 CB3 AB
if !`: list model in modellist' {
    di as error "Model `model' not yet allowed"
    exit 198
}
* recode commonhet as 0/1
local commonhet = mi("`commonhet'") 
local commonhetname = cond(`commonhet',"Common","Non-common")
local commonhetabb = cond(`commonhet',"CH","NCH")
if inlist("`model'", "CB1") & !`commonhet' {
	di as error "Option nocommonhet not allowed with model CB1"
	exit 198
}
if mi("`quitbugs'") local timer notimer
* parms to monitor
local setsigA2  = ("`model'"=="AB" & `commonhet') | "`model'"=="CB3"
local setSigmaA = "`model'"=="AB" & !`commonhet'

if !mi("`prioronly'") {
	local trace notrace
	local burnin 0
	local thin 1
}
* check for heterogeneity priors on both SD and variance scales
foreach option2 in sigA2prior logsigA2mean sigC2prior logsigC2mean {
	local option = subinstr("`option2'","2","",1)
	if !mi("``option''") & !mi("``option2''") {
		di as error "Can't have both `option'() and `option2'()
		exit 198
	}
	if !mi("``option2''") local  sig12priors `sig12priors' ``option2''
	else local sig12priors `sig12priors' ``option''
	* sig12priors lists the chosen parameters
}
// END OF PARSING

// LOAD SAVED NETWORK PARAMETERS
foreach thing in `_dta[network_allthings]' {
	local `thing' : char _dta[network_`thing']
}
local NT = `dim'+1

// OUTPUT SOME INFORMATION
local col2 as result _col(31)
di as text _n "*** Network bayes (Warning: program under development) ***"

// START ANALYSIS
preserve
foreach type in data scalars inits model script coda log {
	local `type'file = subinstr(`"`filepath'`name'_`type'"',"\","/",.)
}

if "`model'"!="" {
	if mi("`allthings'") {
		di as error "Data are not in network format: use network setup|import"
		exit 459
	}
	* PREPARE AND OUTPUT PRIOR PARAMETERS
	di as text "Model:" `col2' "`model'"
	di as text "Heterogeneity:" `col2' "`commonhetname'"
	di as text "Sampling from:" `col2' cond("`prioronly'"=="","Posterior","Prior")
	* parameters
	local modelparms alphaA muA muC sigA sigC logsigA logsigC df rho
	* which parameters are used
	local hasalphaA  = inlist("`model'","CB1","CB2") 
	local hasmuA     = inlist("`model'","CB3","AB") 
	local hasmuC     = !inlist("`model'","AB")
	local hassigA    = inlist("`model'","CB3")
	local hassigC    = `commonhet'
	local haslogsigA = inlist("`model'","AB") & !`commonhet'
	local haslogsigC = !`commonhet'
	local hasdf      = !`commonhet'
	local hasrho     = inlist("`model'","AB") & `commonhet'
	* how their priors are specified
	local alphaAspec  prec  
	local muAspec     prec     
	local muCspec     prec     
	local sigAspec    prior   
	local sigCspec    prior   
	local logsigAspec mean 
	local logsigCspec mean 
	local dfspec      // yes, blank
	local rhospec     prior    
	* has<parm>=2 if priors specified for  squared form
	foreach parm in sigA sigC {
		if `has`parm'' & !mi("``parm'2``parm'spec''") local has`parm' = 2 
	}
	* for prior means, just code the double
	if !mi("`logsigA2mean'") local logsigAmean = `logsigA2mean'/2
	if !mi("`logsigC2mean'") local logsigCmean = `logsigC2mean'/2
	* defaults for prior parameters
	local alphaAdefault   0.001
	local muAdefault      0.001 
	local muCdefault      0.001
	local sigAdefault     dunif(0,10)
	local sigCdefault     dunif(0,10)
	local logsigCdefault  0
	local logsigAdefault  0
	local dfdefault       = `NT' - 1 + inlist("`model'","AB")
	local rhodefault      dunif(0,1)
	* set up prior parameters and output
	foreach parm of local modelparms {
		local spec ``parm'spec'
		local default ``parm'default'
		if `has`parm'' {
			local two = cond(`has`parm''==2,"2","")
			if !mi("`debug'") {
				di "Parameter `parm': spec=`spec', default= `default', two=`two'
			}
			if mi("``parm'`two'`spec''") local `parm'`two'`spec' `default'
			di as text "`parm'`two'`spec':" `col2' "``parm'`two'`spec''" _c
			if "``parm'`spec''" == "`default'" di " (default)" _c
			di
		}
		else if !mi("``parm'`spec''") {
			di as error "Option `parm'`spec'(``parm'`spec'') ignored
			local `parm'`spec'
		}
		else if !mi("``parm'2`spec''") {
			di as error "Option `parm'2`spec'(``parm'2`spec'') ignored
			local `parm'2`spec'
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
	* ensure studyvar is numeric 1,2,3,...
	tempvar studyvar2
	sort `studyvar'
	gen `studyvar2' = sum(`studyvar'!=`studyvar'[_n-1])
	drop `studyvar'
	rename `studyvar2' `studyvar'
	
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
	else qui destring `trtvar', replace

    if "`outcome'" == "count" local vars2 r n
    else if "`outcome'" == "quantitative" local vars2 mean sd n
	rename (`studyvar' `trtvar' `vars') (s t `vars2')
	sort s t
    if "`model'"=="CB1" {
    	egen b=min(t), by(s)
    	by s: gen m = _n
    	summ m, meanonly
		local mmax = r(max)
		if `mmax'>2 local lavars b m
		else local lavars b 
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
	local commonhetnameupper = upper("`commonhetname'")
	`fwm0' "## NMA MODEL `model' WITH `commonhetnameupper' HETEROGENEITY" _n
	if "`prioronly'"=="prioronly" `fwm0' "## PRIOR ONLY" _n
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
	if "`prioronly'"=="prioronly" `fwm' "0*(" 
	if "`model'"=="CB1" {
		`fwm' "alphaA[s[o]] + deltaC[o]*(1-equals(t[o],b[o]))" 
    }
	else if "`model'"=="AB" {
		`fwm' "muA[t[o]] + etaA[s[o],t[o]]" 
    }
	else {
		`fwm' "alphaA[s[o]] + deltaC[s[o],t[o]]" 
    }
	if "`prioronly'"=="prioronly" `fwm' ")" 
	`fwm' _n
    `fwm1' "}" _n
	
	`fwm1' _n "## MODEL STUDY MAIN EFFECTS" _n
	if inlist("`model'","CB1","CB2") {
		`fwm1' "for (i in 1:NS) {" _n
		`fwm2' "alphaA[i] ~ dnorm(0, `alphaAprec')" _n
		`fwm1' "}" _n
	}
	else if "`model'"=="CB3" {
		`fwm1' "for (i in 1:NS) {" _n
		`fwm2' "alphaA[i] ~ dnorm(muA, invsigA2)" _n
		`fwm1' "}" _n
		`fwm1' "muA ~ dnorm(0, `muAprec')" _n
		`fwm2' "## muA is overall mean for reference arm" _n
		`fwm1' "invsigA2 <- 1/sigA2" _n
		if `hassigA'==2 {
			`fwm1' "sigA2 ~ `sigA2prior'" _n
		}
		else {
			`fwm1' "sigA2 <- pow(sigA,2)" _n
			`fwm1' "sigA ~ `sigAprior'" _n
		}
	}
	else if "`model'"=="AB" {
		`fwm1' "## (none)" _n
	}

	if "`model'"!="AB" {
		`fwm1' _n "## MODEL OVERALL TREATMENT EFFECTS" _n
		`fwm1' "muC[1] <- 0" _n
		`fwm1' "for (k in 2:NT) {" _n
		`fwm2' "muC[k] ~ dnorm(0, `muCprec')" _n
		`fwm1' "}" _n
	}
	else {
		`fwm1' _n "## MODEL OVERALL TREATMENT MEANS" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "muA[k] ~ dnorm(0, `muAprec')" _n
		`fwm2' "## useful summaries" _n
		`fwm2' "muC[k] <- muA[k] - muA[1]" _n
		`fwm1' "}" _n
	}


	`fwm1' _n "## MODEL HETEROGENEITY EFFECTS" _n
	if "`model'"=="CB1" {
		`fwm1' "for (o in 1:N) {" _n
		`fwm2' "deltaC[o] ~ dnorm(md[o], taud[o])" _n
		if `mmax'==2 `fwm2' "md[o] <- muC[t[o]] - muC[b[o]]" _n
		else         `fwm2' "md[o] <- muC[t[o]] - muC[b[o]] + offset[o]" _n
		if `mmax'==2 `fwm2' "taud[o] <- 1/sigC2" _n
		else {
			`fwm2' "taud[o] <- (1/sigC2) * (1 + step(m[o]-3)*(1-2/m[o]))" _n
			`fwm2' "## includes variance correction for multi-arm studies" _n
		}
    	`fwm1' "}" _n
		if `mmax'>2 {
			`fwm1' "## mean correction for multi-arm studies" _n
			`fwm1' "offset[1] <- 0" _n
			`fwm1' "offset[2] <- 0" _n
			`fwm1' "for (o in 3:N) {" _n
			`fwm2' "offset[o] <- step(m[o]-3) * ( (m[o]-2)*offset[o-1] + (deltaC[o-1]-muC[t[o-1]]+muC[b[o-1]]) ) /(m[o]-1)" _n
			`fwm1' "}" _n
		}
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
	if inlist("`model'","CB1") & `commonhet' {
        if `hassigC'==2 `fwm1' "sigC2 ~ `sigC2prior' ## SD of heterogeneity" _n
        else {
			`fwm1' "sigC2 <- pow(sigC,2)" _n
			`fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
		}
	}
	else if inlist("`model'","CB2","CB3") & `commonhet' {
        `fwm1' "for (k in 1:NT-1) {" _n
        `fwm2' "for (l in 1:NT-1) {" _n
        `fwm3' "invSigmaC[k,l] <- 2 * (equals(k,l) - 1/NT) / sigC2" _n // invsigC2*inv(P)
		`fwm3' "## this is the inverse of sigC2 * P" _n
	    `fwm3' "## where P is (NT-1)x(NT-1) matrix of diagonal 1's" _n
	    `fwm3' "## and off-diagonal 0.5's" _n
        `fwm2' "}" _n
        `fwm1' "}" _n
         if `hassigC'==2 `fwm1' "sigC2 ~ `sigC2prior' ## SD of heterogeneity" _n
        else {
			`fwm1' "sigC2 <- pow(sigC,2)" _n
			`fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
		}
   }
	else if inlist("`model'","CB2","CB3") & !`commonhet' {
		if `df'==0 {
			di as text "Taking df=NT-1"
			local df = `NT'-1
		}
		if `df'<`NT'-1 {
			di as error "`model' model requires df>#treatments-1
			exit 498
		}
		local sigmascale = 2 * exp( 2*`logsigCmean' + digamma((`df'-`NT'+2)/2))
		`fwm1' "SigmaC.nu <- `df'" _n
		`fwm1' "SigmaC.f <- `sigmascale'" _n
		`fwm1' "for (k in 1:NT-1) {" _n
		`fwm2' "for (l in 1:NT-1) {" _n
		`fwm3' "SigmaC.R[k,l] <- SigmaC.f * (equals(k,l)+1) / 2" _n 
		`fwm3' "    ## SigmaC.R = SigmaC.f * P(1/2)" _n 
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "invSigmaC[1:NT-1,1:NT-1] ~ dwish(SigmaC.R[1:NT-1,1:NT-1], SigmaC.nu)" _n
		`fwm1' "    ## E[invSigmaC] = inv((SigmaC.f/SigmaC.nu) * P(1/2))" _n
		`fwm1' "" _n
		`fwm1' "## useful summaries" _n
		`fwm1' "SigmaC[1:NT-1,1:NT-1] <- inverse(invSigmaC[1:NT-1,1:NT-1])" _n
		`fwm1' "sigC2[1,1] <- 0" _n
		`fwm1' "for (k in 2:NT) {" _n
		`fwm2' "sigC2[k,1] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "sigC2[1,k] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "for (l in 2:NT) {" _n
		`fwm3' "sigC2[k,l] <- sqrt(SigmaC[k-1,k-1]+SigmaC[l-1,l-1]-2*SigmaC[k-1,l-1])" _n
		`fwm3' "## SD of heterogeneity for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
	}
	else if inlist("`model'","AB") & `commonhet' {
        `fwm1' "for (k in 1:NT) {" _n
        `fwm2' "for (l in 1:NT) {" _n
        `fwm3' "invSigmaA[k,l] <- (2/sigC2) * ( equals(k,l) - rho / (1-rho+NT*rho) )" _n
		`fwm3' "## exchangeable correlation structure" _n
        `fwm2' "}" _n
        `fwm1' "}" _n
        `fwm1' "rho ~ `rhoprior' ## correlation in compound symmetrical SigmaA" _n
        if `hassigC'==2 `fwm1' "sigC2 ~ `sigC2prior' ## SD of heterogeneity" _n
        else {
			`fwm1' "sigC2 <- pow(sigC,2)" _n
			`fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
		}
		`fwm1' "## useful summaries" _n
		`fwm1' "sigA2 <- sigC2 / (2*(1-rho))" _n
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
		local sigmascale = 2 * exp(2*`logsigAmean' + digamma((`df'-`NT'+1)/2))
		local r = 1 - 0.5*exp(2*`logsigCmean' - 2*`logsigAmean')
		`fwm1' "SigmaA.nu <- `df'" _n
		`fwm1' "SigmaA.f <- `sigmascale'" _n
		`fwm1' "SigmaA.r <- `r'" _n
		`fwm1' "invSigmaA[1:NT,1:NT] ~ dwish(SigmaA.R[1:NT,1:NT], SigmaA.nu)" _n
		`fwm1' "    ## E[invSigmaA] = inv((SigmaA.f/SigmaA.nu) * P(SigmaA.r))" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "SigmaA.R[k,l] <- SigmaA.f * ((1-SigmaA.r)*equals(k,l)+SigmaA.r)" _n
		`fwm3' "    ## SigmaA.R = SigmaA.f * P(SigmaA.r)" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "" _n
		`fwm1' "## useful summaries" _n
		`fwm1' "SigmaA[1:NT,1:NT] <- inverse(invSigmaA[1:NT,1:NT])" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "sigA2[k] <- SigmaA[k,k]" _n // new 2017-11-22
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "sigC2[k,l] <- SigmaA[k,k]+SigmaA[l,l]-2*SigmaA[k,l]" _n
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
	* inits for parms with arbitrary prior
	* middle of a uniform, otherwise 1
    foreach parm in sigC sigA rho {
		if !`has`parm'' continue
		local parm2 = cond(`has`parm''==2,"`parm'2","`parm'")
		if `hasinits' file write inits "," _n
    	local 0, ``parm2'prior'
		syntax, [dunif(string) dbeta(string) *]
		local `parm2' 1
		if !mi("`dunif'") {
			tokenize "`dunif'", parse(",")
			if `1'>=1 | `3'<=1 local `parm2' = (`1' + `3')/2
		}
		if !mi("`dbeta'") {
			tokenize "`dbeta'", parse(",")
			local `parm2' = `1' / (`1' + `3')
		}
    	file write inits "`parm2'=" (``parm2'')
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
	else if "`model'" == "CB1" {
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
	local extraparms `parms'
	local parms muC sigC2
	if `setSigmaA' | `setsigA2'  local parms `parms' sigA2
	local allparms : list parms | extraparms

    * WRITE WINBUGS SCRIPT
    file write script "## SCRIPT TO FIT NMA MODEL `model' WITH `commonword' HETEROGENEITY" _n
	if "`prioronly'"=="prioronly" file write script  "## PRIOR ONLY" _n
	file write script "## WRITTEN BY NETWORK_BAYES.ADO AT `=c(current_time)' ON `=c(current_date)' " _n(2)
    file write script "display('log')" _n
    file write script "check('`modelfile'')" _n
    file write script "data('`datafile'')" _n
    file write script "data('`scalarsfile'')" _n
    file write script "compile(1)" _n
    file write script "inits(1,'`initsfile'')" _n
    file write script "gen.inits()" _n
    file write script "update(`burnin')" _n
    foreach setparm of local allparms {
		file write script "set(`setparm')" _n
	}
    file write script "thin.samples(`thin')" _n
    file write script "update(`updates')" _n
    file write script "coda(*,'`codafile'')" _n
    file write script "save('`logfile'.txt')" _n
    if !mi("`quitbugs'") file write script "quit()" _n
    else file write script _n
	
	di as text "Writing files ..." _c
	foreach type in data scalars inits model script {
		file close `type'
		di as text _col(19) "`type' to `savedir'`name'_`type'.txt"
	}
	
	if "`dryrun'"=="dryrun" {
		di "End of dry run. Files are written but WinBUGS has not run."
		exit
	}
	
    * ERASE PREVIOUS RESULTS
    cap erase `filepath'`name'_codaIndex.txt
    if !_rc forvalues i=1/99 {
        cap erase `filepath'`name'_coda`i'.txt
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
	
    cap confirm file `filepath'`name'_codaIndex.txt
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

*set tracedepth 1
*set trace on

    // READ CODA DATA
    wbcoda, root(`filepath'`name'_coda) clear
    * rename contrasts
    if !mi("`debug'") summ
    forvalues i = 2/`NT' { // assumes muC is a NT-1 vector
    	rename muC_`i' diff_`trt`i''_`ref'
    	label var diff_`trt`i''_`ref' "Mean diff `trt`i'' vs `ref'"
    }
    if "`model'"=="AB" & !`commonhet' {
        drop muC_1 sigC2_1_1
    }
    if !`commonhet' { // assumes sigC2 is a NT by NT matrix
        forvalues i = 1/`NT' {
            forvalues j = 1/`NT' {
                if `i'==1 & `j'==1 continue
                rename sigC2_`i'_`j' sigC2_`trt`i''_`trt`j''
                label var sigC2_`trt`i''_`trt`j'' "Het SD, contrast `trt`j'' vs `trt`i''"
                if `j'<=`i' drop sigC2_`trt`i''_`trt`j''
            }
        }
    }
    else label var sigC2 "Het SD (all contrasts)"

    if `setsigA2' {
		rename sigA2 sigA2_`ref'
		label var sigA2_`ref' "Het SD, arm `trt1'"
	}
    if `setSigmaA' {
		forvalues i = 1/`NT' {
			rename sigA2_`i' sigA2_`trt`i''
			label var sigA2_`trt`i'' "Het var, arm `trt`i''"
		}
	}
	local sampledfrom = cond("`prioronly'"=="","posterior","prior")
	label data "Sample from `sampledfrom': model `model' `commonhetabb'; burnin `burnin', thin `thin'"
    qui save `savedir'`name'_sample, replace
    di as text "Bugs output saved in `savedir'`name'_sample.dta"
}
else {
	cap use `savedir'`name'_sample, clear
	if _rc {
		di as error "No model statement, and no saved results found in `savedir'`name'_sample"
		exit 498
	}
	di as text "Using results stored in " as result "`savedir'`name'_sample.dta"
}

// OUTPUT RESULTS
di as text "" _c // just gets font right for next
* identify parms of interest
if `commonhet' local parms diff_* sigC2
else local parms diff_* sigC2_*
if `setsigA2' local parms `parms' sigA2_`ref'
if `setSigmaA' local parms `parms' sigA2_*
local parms : list parms | extraparms
* do results
if mi("`stats'") wbstats `parms'
if !mi("`ac'`ac2'") fastac `parms', `ac2'
if mi("`trace'") fasttrace, `trace2'
if !mi("`density'`density2'") wbdensity `parms', `density2'

// RESTORE OR NOT
if !mi("`clear'") {
	restore, not
	di as text "Bugs output is now loaded into memory"
}

end

***************************************************************

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

***************************************************************

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

***************************************************************
