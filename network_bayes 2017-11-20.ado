/*
*! UNDER DEVELOPMENT 
20nov2017 - runs as post-estimation w/o network setup
*! version 1.3.3 - Ian White - 13oct2017 18oct2017
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
	find better way to de-augment the data
    does it work for multiple chains?
    update to wbs*.ado
    allow trials with >3 arms in LA [see notes]
    optionally output all differences (e.g. C-B)
    slicker names for graphs - requires parsing `trace2' to detect cgoptions()
	make it store name for future reference - but not as rclass (which is where wbstats are returned)
	drop gen.inits() - FIND  OUT WHY IT'S NEEDED FOR MODEL(CB)
	need to save results for replay mode (fails for ABc model)
	utility to look at logsigC instead of sigC?
	should default name = model?
	wbac produces nasty plots
	change model names to match paper
	consider using fast tracei.ado
	allow direct specification of mean of inverse Wisharts (rather than via mean of logsigC)

NOTE
	sigC is scalar (with prior sigCprior)
	SigmaC is matrix (with prior proportional to exp(logsigCmean)

PROBLEMS
    in LA and LAplus, sometimes winbugs runs but ends with TRAP 60 (postcondition violated)
		and doesn't output to CODA 
		- I think this is due to working in dropbox
		- not found working on hard disk
	can't set seed in winbugs batch mode

*/

prog def network_bayes
syntax, [ ///
	name(string) model(string) noCOMmonhet PRIORonly /// model specification
	seed(int 0) BUrnin(int 1000) UPdates(int 1000) THin(int 1) dryrun /// MCMC 
	WINBUGSdir(string) parms(string) quit noTImer SAVedir(string) /// WinBUGS
    alphaAprec(string) muA1prec(string) sigA1prior(string) muCprec(string) muAprec(string)  /// priors for main effects
    alphaaprec(string) mua1prec(string) siga1prior(string) mucprec(string) muaprec(string)  /// unallowed prior
	sigCprior(string) rhoprior(string) logsigCmean(string) df(string) /// prior for heterogeneity variances
	sigcprior(string)                  								  /// unallowed prior 
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

local modellist LA LAplus CB AB
if !`: list model in modellist' {
    di as error "Model `model' not yet allowed"
    exit 198
}
* recode commonhet as 0/1
local commonhet = mi("`commonhet'") 
local commonhetname = cond(`commonhet',"Common","Non-common")
if inlist("`model'", "LA") & !`commonhet' {
	di as error "Option nocommonhet not allowed with model LA"
	exit 198
}
* don't accept options with lowercase C/A
foreach arg in sigCprior sigA1prior alphaAprec muAprec muA1prec muCprec {
	local lowerarg = lower("`arg'")
	if !mi("``lowerarg''") {
		di as error "Option `lowerarg'() ignored - please use `arg'()"
		exit 198
	}
}
if mi("`quit'") local timer notimer
local hassigA  = "`model'"=="AB" & `commonhet'
local hassigA1 = "`model'"=="CB" 

if !mi("`prioronly'") {
	local trace notrace
	local burnin 0
	local thin 1
}
// END OF PARSING

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
	* which prior parameters are needed
	local hasalphaAprec = inlist("`model'","LA","LAplus") 
	local hasmuA1prec   = inlist("`model'","CB") 
	local hassigA1prior = inlist("`model'","CB") 
	local hasmuAprec    = inlist("`model'","AB")
	local hasmuCprec    = !inlist("`model'","AB")
	local hassigCprior  = `commonhet'
	local haslogsigCmean = !`commonhet'
	local hasdf         = !`commonhet'
	local hasrhoprior = inlist("`model'","AB") & `commonhet'
	* defaults for prior parameters
	foreach parm in alphaAprec muA1prec muAprec muCprec {
		local `parm'default 0.001
		if !mi("``parm''") confirm number ``parm''
	}
	local sigA1priordefault dunif(0,10)
	local sigCpriordefault  dunif(0,10)
	local rhopriordefault   dunif(0,1)
	local logsigCmeandefault 0
	local NTm1 : word count `trtlistnoref'
	local dfdefault = `NTm1' + inlist("`model'","AB")
	* set up prior parameters and output
	foreach parm in alphaAprec muA1prec sigA1prior muAprec muCprec ///
		sigCprior logsigCmean df rhoprior {
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
    if "`model'"=="LA" {
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
	if "`model'"=="LA" {
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
		`fwm1' "for (k in 2:NT) {" _n
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
		if `mmax'==2 `fwm2' "md[o] <- muC[t[o]] - muC[b[o]]" _n
		else         `fwm2' "md[o] <- muC[t[o]] - muC[b[o]] + offset[o]" _n
		if `mmax'==2 `fwm2' "taud[o] <- pow(sigC, -2)" _n
		else {
			`fwm2' "taud[o] <- pow(sigC, -2) * (1 + step(m[o]-3)*(1-2/m[o]))" _n
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
		local sigmascale = exp(2*`logsigCmean' + digamma((`df'-`NT'+2)/2))*2/`df'
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
*		`fwm3' "invSigmaA[k,l] <- (2/pow(sigC,2)) * ( equals(k,l) - (2*pow(sigA,2))"
*		`fwm3' "/ (2*NT*pow(sigA,2)+pow(sigC,2)) )" _n
        `fwm3' "invSigmaA[k,l] <- 2 * pow(sigC,-2) * ( equals(k,l) - rho / (1-rho+NT*rho) )" _n
		`fwm3' "## exchangeable correlation structure" _n
        `fwm2' "}" _n
        `fwm1' "}" _n
*       `fwm1' "sigA ~ `sigAprior' ## SD of heterogeneity" _n
        `fwm1' "sigC ~ `sigCprior' ## SD of heterogeneity" _n
        `fwm1' "rho ~ `rhoprior' ## correlation in compound symmetrical SigmaA" _n
		`fwm1' "# useful summaries" _n
		`fwm1' "sigA <- sigC / sqrt(2*(1-rho))"
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
		local sigmascale = exp(2*`logsigCmean' + digamma((`df'-`NT'+1)/2)) / `df'
		`fwm1' "invSigmaA[1:NT,1:NT] ~ dwish(scaleid[1:NT,1:NT], `df')" _n
		`fwm1' "    ## E[invSigmaA] = (1/`sigmascale') * I" _n
		`fwm1' "    ## E[SigmaA] = (`sigmascale') * (`df'/(`df'-NT)) * I" _n
		`fwm1' "    ## mode[SigmaA] = (`sigmascale') * (`df'/(`df'+NT)) * I" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "scaleid[k,l] <- `sigmascale' * `df' * equals(k,l)" _n
		`fwm3' "    ## scaleid = `sigmascale' * `df' * I" _n
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
	* inits for parms with arbitrary prior
	* middle of a uniform, otherwise 1
    foreach parm in sigC sigA1 rho {
		if !`has`parm'prior' continue
		if `hasinits' file write inits "," _n
    	local 0, ``parm'prior'
		syntax, [dunif(string) *]
		local `parm' 1
		if !mi("`dunif'") {
			tokenize "`dunif'", parse(",")
			if `1'>=1 | `3'<=1 local `parm' = (`1' + `3')/2
		}
    	file write inits "`parm'=" (``parm'')
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
	local extraparms `parms'
	local parms muC sigC
	if `hassigA' local parms `parms' sigA
	if `hassigA1' local parms `parms' sigA1
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
    if !mi("`quit'") file write script "quit()" _n
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


    // READ CODA DATA
    wbcoda, root(`filepath'`name'_coda) clear
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
	label data "network bayes results: burnin `burnin', updates `updates', thin `thin', model `model', `commonhetname' het"
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
if `commonhet' local parms diff_* sigC
else local parms diff_* sigC_*
if `hassigA' | `hassigA1' local parms `parms' sigA_`ref'
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
syntax [anything] [using/] [if] [in], [title(passthru) note(passthru) *]
preserve
if !mi("`using'") qui use `using', clear
if !mi("`anything'") qui keep order `anything'
if !mi("`if'`in'") qui keep `if' `in'
rename (*) (value*)
rename valueorder order
qui reshape long value, i(order) j(parm) string
line value order, by(parm, yrescale note("") `title' `note') xtitle("Samples") ytitle("") `options'
end

***************************************************************

prog def fastac
// quick trace
syntax [anything] [using/] [if] [in], ///
	[title(passthru) note(passthru) maxlag(int 40) SEParate *]
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
	forvalues lag=1/`maxlag' {
		qui replace `lagvar'=`parm'[_n-`lag']
		qui corr `parm' `lagvar'
		qui replace corr`parm'=r(rho) in `lag'
	}
	local corrlist `corrlist' corr`parm'
	label  var corr`parm' "`parm'"
}
qui keep in 1/`maxlag'
keep `corrlist'
gen lag=_n
if mi("`separate'") {
line `corrlist' lag, `title' `note' ///
	xtitle("Lags") ytitle("Autocorrelation") `options' ///
	yscale(range(0 1)) ylabel(0(0.2)1)
}
else {
qui reshape long corr, i(lag) j(parm) string
line corr lag, by(parm, note("") `title' `note') ///
	xtitle("Lags") ytitle("Autocorrelation") `options' ///
	yscale(range(0 1))
}
end

***************************************************************
