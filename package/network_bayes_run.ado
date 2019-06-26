prog def network_bayes_run

syntax, Reference(string) [name(name) SAVedir(string) /// options for both run and output
	mkdir study(string) treatment(string) heterogeneity(string) PRIORonly /// model specification
	seed(int 0) BUrnin(int 1000) UPdates(int 1000) THin(int 1) dryrun /// MCMC 
	WINBUGSdir(string) Quitbugs noTImer /// WinBUGS
	PARMs(string)] // rename?

local mainreference `reference'

* expand abbreviations for `reference'
local 0 , `reference'
cap syntax , [Studyspecific NOne NEtworkwide] // none MUST go before networkwide - else gets read as nonetworkwide
if !_rc {
	local reference `studyspecific' `networkwide' `none'
	cap assert wordcount("`reference'") == 1
}
if _rc {
	di as error "Syntax: reference(Studyspecific|NEtworkwide|NOne)"
	exit _rc
}

* where files are stored: savedir = stata name of directory, filepath = full path
if !mi("`savedir'") {
	local wd = c(pwd)
	if !mi("`mkdir'") {
		cap cd `savedir'
		if _rc {
			mkdir `savedir'
			di as text "Directory `savedir' created"
			qui cd `savedir'
		}
	}
	else qui cd `savedir'
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

if mi("`quitbugs'") local timer notimer

if !mi("`prioronly'") {
	local trace notrace
	local burnin 0
	local thin 1
}

// PARSE MODEL OPTIONS AND OUTPUT

local col1 as text _col(6)
local col2 as result _col(36)
di as text _n "*** Network bayes (Warning: program under development) ***"

di as text "Model form: reference is" `col2' "`reference'"

* parse study intercepts
if mi("`study'") local study fixed
local 0 `study'
syntax anything, [*]
if "`anything'"=="fixed" {
	cap noi syntax anything, [prior(string)]
	iferrorexit "Error in treatment() option"
	if mi("`prior'") local prior dnorm(0,0.01)
	local studyprior `prior'
	di as text "Study effects" `col2' "fixed, prior ~`prior'"
}
else if "`anything'"=="random" {
	cap noi syntax anything, [meanprior(string) precprior(string)]
	iferrorexit "Error in treatment() option"
	if mi("`meanprior'") local meanprior dnorm(0,0.01)
	if mi("`precprior'") local precprior ???
	local studymeanprior `meanprior'
	local studyprecprior `precprior'
	di as text "Study effects" `col2' "random, ~ N(study.mean,study.prec)"
	di `col1' "prior for study.mean:" `col2' "~ `meanprior'"
	di `col1' "prior for study.prec:" `col2' "~ `precprior'"
}
else if "`anything'"=="none" {
	syntax anything, 
	di as text "Study effects" `col2' "None"
}
else {
	di as error "study(`anything') is not allowed
	exit 198
}
local study `anything'

* parse treatment effects
if mi("`treatment'") local treatment fixed
local 0 `treatment'
syntax anything, [*]
if "`anything'"=="fixed" {
	cap noi syntax anything, [prior(string)]
	iferrorexit "Error in treatment() option"
	if mi("`prior'") local prior dnorm(0,0.01)
	local treatmentprior `prior'
	di as text "Treatment effects" `col2' "fixed, prior ~ `prior'"
}
else if "`anything'"=="random" {
	cap noi syntax anything, [meanprior(string) precprior(string)]
	iferrorexit "Error in treatment() option"
	if mi("`meanprior'") local meanprior dnorm(0,0.01)
	if mi("`precprior'") local precprior ???
	local treatmentmeanprior `meanprior'
	local treatmentprecprior `precprior'
	di as text "Treatment effects" `col2' "random, ~ N(treatment.mean,treatment.prec)"
	di `col1' "prior for treatment.mean:" `col2' "~ `meanprior'"
	di `col1' "prior for treatment.prec:" `col2' "~ `precprior'"
}
else if "`anything'"=="none" {
	syntax anything, 
	di as text "Treatment effects" `col2' "None"
}
else {
	di as error "treatment(`anything') is not allowed
	exit 198
}
local treatment `anything'

* parse heterogeneity
if mi("`heterogeneity'") local heterogeneity common
local 0 `heterogeneity'
syntax anything, [*]
if "`anything'"=="none" {
	cap noi syntax anything, 
	iferrorexit "Error in heterogeneity() option"
	di as text "Heterogeneity variance:" `col2' "none"
}
else if "`anything'"=="common" {
	cap noi syntax anything, [prior(string)]
	iferrorexit "Error in heterogeneity() option"
	if mi("`prior'") local prior ???
	local hetprior `prior'
	di as text "Heterogeneity variance:" `col2' "common"
	di `col1' "prior for het. variance:" `col2' "~ `hetprior'"
}
else if "`anything'"=="trtspecific" {
	cap noi syntax anything, [fixed random prior(string)]
	iferrorexit "Error in heterogeneity() option"
	di as text "Heterogeneity variance:" `col2' "structured, treatment-specific model"
	di as error "Not yet implemented"
	exit 498
}
else if "`anything'"=="unstructured" {
	cap noi syntax anything, [prior(string)]
	iferrorexit "Error in heterogeneity() option"
	if mi("`prior'") local prior ???
	local hetprior `prior'
	di as text "Heterogeneity variance:" `col2' "Unstructured"
	di `col1' "prior for het. variance matrix:" `col2' "~ `hetprior'"
}
else {
	di as error "heterogeneity(`anything') is not allowed
	exit 198
}
local heterogeneity `anything'

// END OF PARSING


// LOAD SAVED NETWORK PARAMETERS 
foreach thing in `_dta[network_allthings]' {
	local `thing' : char _dta[network_`thing']
}
if mi("`allthings'") {
	di as error "Data are not in network format: use network setup|import"
	exit 459
}
local NT = `dim'+1




// PREPARE TO WRITE BUGS FILES
foreach type in data scalars inits model script coda log {
	local `type'file = subinstr(`"`filepath'`name'_`type'"',"\","/",.)
}
foreach type in data scalars inits model script { // files to be written by -file write-
	local `type'file ``type'file'.txt
	cap file close `type'
	qui file open `type' using "``type'file'", write text replace
}


// WRITE DATA TO FILE IN WINBUGS FORMAT

* GET DATA IN DESIRED FORMAT
preserve
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
if "`reference'"=="studyspecific" { // extra variables: ref trt b and order m
// -> change b to ref and m to arm?
	egen b = min(t), by(s)
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

* WRITE DATA TO FILE
forvalues i=0/`N' {
	foreach var in s t `vars2' `lavars' {
		if `i'==0 file write data %10s "`var'[] " 
		else file write data %9.0f "`=`var'[`i']' " 
	}
	file write data _n
}
file write data "END" _n

* END OF WRITING DATA TO FILE


// WRITE MODEL
local fwm  file write model
local fwm0 file write model _col(1)
local fwm1 file write model _col(5)
local fwm2 file write model _col(9)
local fwm3 file write model _col(13)
`fwm0' "## NETWORK META-ANALYSIS MODEL" _n
if "`prioronly'"=="prioronly" `fwm0' "## PRIOR ONLY" _n
`fwm0' "## WRITTEN BY NETWORK_BAYES.ADO AT `=c(current_time)' ON `=c(current_date)' " _n
`fwm0' "## In this code:" _n
if "`outcome'" == "count" `fwm0' "##   Data s[], r[], n[] represent Stata variables `studyvar', `d', `n'" _n
if "`outcome'" == "quantitative" `fwm0' "##   Data s[], mean[], sd[], n[] represent Stata variables `studyvar', `mean', `sd', `n'" _n
if "`reference'"=="studyspecific" {
	`fwm0' "##   b[] is study-specific reference treatment" 
	if `mmax'>2 `fwm0' " and m[] is study group" 
	`fwm0' _n
}
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


`fwm1' _n "## MODEL FORM" _n
`fwm1' "for (o in 1:N) {" _n
if !mi("`link'") `fwm2' "`link'(theta[o]) <- "
else `fwm2' "theta[o] <- "
if "`prioronly'"=="prioronly" `fwm' "0*(" 
if "`reference'"=="studyspecific" {
	`fwm' "alphaA[s[o]] + deltaC[o]*(1-equals(t[o],b[o]))" 
}
else if "`reference'" == "networkwide" {
	`fwm' "alphaA[s[o]] + deltaC[s[o],t[o]]" 
}
else if "`reference'"=="none" & "`study'"=="fixed" {
	`fwm' "alphaA[s[o]] + muC[t[o]] + etaA[s[o],t[o]]" 
}
else if "`reference'"=="none" {
	`fwm' "muA[t[o]] + etaA[s[o],t[o]]" 
}
else exit 494
if "`prioronly'"=="prioronly" `fwm' ")" 
`fwm' _n
`fwm1' "}" _n


`fwm1' _n "## MODEL STUDY INTERCEPTS" _n
if "`study'"=="fixed" {
	`fwm1' "for (i in 1:NS) {" _n
	`fwm2' "alphaA[i] ~ `studyprior'" _n
	`fwm1' "}" _n
}
else if "`study'"=="random" {
	`fwm1' "for (i in 1:NS) {" _n
	`fwm2' "alphaA[i] ~ dnorm(alphaA.mean, alphaA.prec)" _n
	`fwm1' "}" _n
	`fwm1' "alphaA.mean ~ `studymeanprior'" _n
	`fwm2' "## alphaA.mean is overall mean for reference arm" _n
	`fwm1' "alphaA.prec ~ `studyprecprior')" _n
	`fwm2' "## alphaA.prec is inverse variance for reference arm" _n
}
else if "`study'"=="" {
	`fwm1' "## (none)" _n
}
else exit 494


`fwm1' _n "## MODEL OVERALL TREATMENT PARAMETERS" _n

if "`reference'"!="none" { // contrasts
	`fwm1' "muC[1] <- 0" _n
	`fwm1' "for (k in 2:NT) {" _n
	if "`treatment'"=="fixed" `fwm2' "muC[k] ~ `treatmentprior'" _n
	else if "`treatment'"=="random" {
		`fwm2' "muC[k] ~ dnorm(muC.mean, muC.prec)" _n
		`fwm2' "muC.mean ~ `treatmentmeanprior'" _n
		`fwm2' "muC.prec ~ `treatmentprecprior'" _n
	}
	`fwm1' "}" _n
}
else { // arm-level means
	`fwm1' "for (k in 1:NT) {" _n
	if "`treatment'"=="fixed" `fwm2' "muA[k] ~ `treatmentprior'" _n
	else if "`treatment'"=="random" {
		`fwm2' "muA[k] ~ dnorm(muA.mean, muA.prec)" _n
		`fwm2' "muA.mean ~ `treatmentmeanprior'" _n
		`fwm2' "muA.prec ~ `treatmentprecprior'" _n
	}
	`fwm2' "## useful summaries" _n
	`fwm2' "muC[k] <- muA[k] - muA[1]" _n
	`fwm1' "}" _n
}


`fwm1' _n "## MODEL HETEROGENEITY EFFECTS" _n
if "`reference'"=="studyspecific" {
	`fwm1' "for (o in 1:N) {" _n
	if "`heterogeneity'"=="none" {
		`fwm2' "deltaC[o] <- muC[t[o]] - muC[b[o]]" _n
	}
	else if "`heterogeneity'"=="common" {
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
}
else if "`reference'"=="networkwide" {
	`fwm1' "for (i in 1:NS) { " _n
	`fwm2' "deltaC[i,1] <- 0" _n
	`fwm2' "deltaC[i,2:NT] ~ dmnorm(muC[2:NT], invSigmaC[1:NT-1,1:NT-1])" _n
	`fwm1' "}" _n
}
else if "`reference'"=="none" {
	`fwm1' "for (i in 1:NS) { " _n
	`fwm2' "etaA[i,1:NT] ~ dmnorm(zero[1:NT], invSigmaA[1:NT,1:NT])" _n
	`fwm1' "}" _n
	`fwm1' "for (k in 1:NT) { " _n
	`fwm2' "zero[k] <- 0" _n
	`fwm1' "}" _n
}
else exit 493

file close _all
exit 1

if "`heterogeneity'" {
	`fwm1' _n "## PRIOR FOR COMMON HETEROGENEITY VARIANCE" _n
	if inlist("`model'","1CB") {
	}
	else if inlist("`model'","2CB","3CB") {
		`fwm1' "for (k in 1:NT-1) {" _n
		`fwm2' "for (l in 1:NT-1) {" _n
		`fwm3' "invSigmaC[k,l] <- 2 * (equals(k,l) - 1/NT) / sigC2" _n // invsigC2*inv(P)
		`fwm3' "## this is the inverse of sigC2 * P" _n
		`fwm3' "## where P is (NT-1)x(NT-1) matrix of diagonal 1's" _n
		`fwm3' "## and off-diagonal 0.5's" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
   }
	else if inlist("`model'","2AB") {
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "invSigmaA[k,l] <- 2 * equals(k,l) / sigC2" _n 
		`fwm2' "}" _n
		`fwm1' "}" _n
   }
	else if inlist("`model'","4AB") {
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "invSigmaA[k,l] <- (2/sigC2) * ( equals(k,l) - rho / (1-rho+NT*rho) )" _n
		`fwm3' "## exchangeable correlation structure" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "rho ~ `rhoprior' ## correlation in compound symmetrical SigmaA" _n
	}
	else di as error "Program error: model not coded"
	if `hassigC'==2 `fwm1' "sigC2 ~ `sigC2prior' ## contrast heterogeneity variance" _n
	else {
		`fwm1' "sigC2 <- pow(sigC,2)" _n
		`fwm1' "sigC ~ `sigCprior' ## contrast heterogeneity SD" _n
	}
	if inlist("`model'","4AB") | `hassigC'==2 `fwm1' "## useful summaries" _n
	if inlist("`model'","4AB") `fwm1' "sigA <- sqrt(sigC2 / (2*(1-rho))) ## arm heterogeneity SD" _n
	if `hassigC'==2 `fwm1' "sigC <- sqrt(sigC2)" _n
}
else {
	`fwm1' _n "## PRIOR FOR NON-COMMON HETEROGENEITY VARIANCE" _n
	if inlist("`model'","2CB","3CB") {
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
		`fwm1' "sigC[1,1] <- 0" _n
		`fwm1' "for (k in 2:NT) {" _n
		`fwm2' "sigC[k,1] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "sigC[1,k] <- sqrt(SigmaC[k-1,k-1])" _n
		`fwm2' "for (l in 2:NT) {" _n
		`fwm3' "sigC[k,l] <- sqrt(SigmaC[k-1,k-1]+SigmaC[l-1,l-1]-2*SigmaC[k-1,l-1])" _n
		`fwm3' "## contrast heterogeneity variance for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n 
	}
	else if inlist("`model'","2AB") {
		if `df'==0 {
			di as text "Taking df=NT"
			local df = `NT'
		}
		if `df'<`NT' {
			di as error "`model' model requires df>#treatments
			exit 498
		}
		local sigmascale = exp(2*`logsigCmean' + digamma((`df'-`NT'+1)/2))
		`fwm1' "invSigmaA[1:NT,1:NT] ~ dwish(SigmaA.R[1:NT,1:NT], SigmaA.nu)" _n
		`fwm1' "SigmaA.nu <- `df'" _n
		`fwm1' "SigmaA.f <- `sigmascale'" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "SigmaA.R[k,l] <- SigmaA.f * equals(k,l)" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
		`fwm1' "" _n
		`fwm1' "## useful summaries" _n
		`fwm1' "SigmaA[1:NT,1:NT] <- inverse(invSigmaA[1:NT,1:NT])" _n
		`fwm1' "for (k in 1:NT) {" _n
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "sigC[k,l] <- sqrt(SigmaA[k,k]+SigmaA[l,l]-2*SigmaA[k,l])" _n
		`fwm3' "    ## contrast heterogeneity SD for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
	}
	else if inlist("`model'","4AB") {
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
		`fwm2' "sigA[k] <- sqrt(SigmaA[k,k]) ## arm heterogeneity SD for k" _n 
		`fwm2' "for (l in 1:NT) {" _n
		`fwm3' "sigC[k,l] <- sqrt(SigmaA[k,k]+SigmaA[l,l]-2*SigmaA[k,l])" _n
		`fwm3' "    ## contrast heterogeneity SD for k vs l" _n
		`fwm2' "}" _n
		`fwm1' "}" _n
	}
	else di as error "Program error: model not coded"
}
`fwm0' _n "}" _n

* WRITE SCALARS OUT IN WINBUGS FORMAT
file write scalars "list(N=" (`N') ", NS=" (`NS') ", NT=" (`NT') ")" _n

* WRITE INITS OUT IN WINBUGS FORMAT
file write inits "list(" _n
* inits for parms with arbitrary prior
* middle of a uniform, otherwise 1
foreach parm in sigC sigA rho {
	if !`has`parm'' continue
	local parm2 = cond(`has`parm''==2,"`parm'2","`parm'")
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
	file write inits "`parm2'=" (``parm2'') "," _n
}
* inits for alphaA/muA
if `hasalphaA' writeinits alphaA, dim(`NS')
if `hasmuA' {
	if "`model'"=="3CB" writeinits muA
	else if "`model'"=="4AB" writeinits muA, dim(`NT')
}
* inits for muC
if `hasmuC' writeinits muC, dim(`NT') na(1)
* end of inits
* inits for etaA
if `hasetaA' writeinits etaA, dim(`NS',`NT') 
* inits for deltaC
if `hasdeltaC' {
	if "`model'" == "1CB" writeinits deltaC, dim(`N')
	else writeinits deltaC, dim(`NS',`NT') na(.,1) 
}
* inits for invSigmaA: to be done (not needed?)
file write inits ")" _n

* PARAMETERS OF INTEREST
local extraparms `parms'
local parms muC sigC
if `setSigmaA' | `setsigA' local parms `parms' sigA
local setparms : list parms | extraparms

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
foreach setparm of local setparms {
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

cap confirm file `"`filepath'`name'_codaIndex.txt"'
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
	rename muC_`i' muC_`trt`i''_`ref'
	label var muC_`trt`i''_`ref' "Mean diff `trt`i'' vs `ref'"
}
if "`model'"=="4AB" {
	drop muC_1 
	if !`commonhet' drop sigC_1_1
}
if !`commonhet' { // assumes sigC is a NT by NT matrix
	forvalues i = 1/`NT' {
		forvalues j = 1/`NT' {
			if `i'==1 & `j'==1 & "`model'"!="2AB" continue
			rename sigC_`i'_`j' sigC_`trt`i''_`trt`j''
			label var sigC_`trt`i''_`trt`j'' "Het SD, contrast `trt`j'' vs `trt`i''"
			if `j'<=`i' drop sigC_`trt`i''_`trt`j''
		}
	}
}
else label var sigC "Het SD (all contrasts)"

if `setsigA' {
	if "`model'"=="4AB" {
		label var sigA "Het SD" // leave as sigA for model 4
	}
	else if "`model'"=="3CB" {
		rename sigA sigA_`ref'  
		label var sigA_`ref' "Het SD, arm `trt1'"
	}
	else di as error "Program error"
}
if `setSigmaA' {
	forvalues i = 1/`NT' {
		rename sigA_`i' sigA_`trt`i''
		label var sigA_`trt`i'' "Het SD, arm `trt`i''"
	}
}
local sampledfrom = cond("`prioronly'"=="","posterior","prior")
label data "Sample from `sampledfrom': model `model' `commonhetabb'; burnin `burnin', thin `thin'"
qui save `savedir'`name'_sample, replace
di as text "Bugs output saved in `savedir'`name'_sample.dta"

end

prog def iferrorexit
if _rc {
	di as error `0'
	exit _rc
}
end
