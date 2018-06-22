/*
Cscript for network bayes 
Updated 20nov2017 & 22jun2018
RUN IN NETWORK SCRIPTS DIRECTORY
*/

set more off
do network_adopath
pda

which network
which network_bayes


local opts updates(1000) name(cscript) quit savedir(c:\temp\bugsfiles) clear

set tracedepth 2
set trace off
local pm -1
local psd .5
local muCprec 1

// CHECK PRIORS: CH MODELS
set seed 467164

foreach model in CB1 CB2 AB2 CB3 AB4 {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') commonhet ///
		muCprec(`muCprec') ///
		prioronly sigCprior(dnorm(`pm',`=1/`psd'^2')) `opts'
	ttest sigC==`pm'
	assert r(p)>0.001
	sdtest sigC==`psd'
	assert r(p)>0.001
	if "`model'"!="AB4" {
		ttest muC_B_A==0
		assert r(p)>0.001
		sdtest muC_B_A==`=1/sqrt(`muCprec')'
		assert r(p)>0.001
	}
	exit
}

// CHECK PRIORS: NCH MODELS
set seed 571894198
foreach model in CB2 AB2 CB3 AB4 {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') nocommonhet ///
		prioronly logsigCmean(`pm') `opts'
	gen logsigC = log(sigC_A_B)
	ttest logsigC==`pm'
	assert r(p)>0.001
}

local opts burnin(50000) updates(50000) thin(5) name(cscript) quit savedir(c:\temp\bugsfiles) clear

// CHECK SMOKING RESULTS: CH MODELS
set seed 571894198
foreach model in CB1 CB2 AB2 CB3 AB4 {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') commonhet `opts'
	wbstats muC_C_A
	assert abs(r(mn1)-0.8)<0.1
}

// CHECK SMOKING RESULTS: NCH MODELS
set seed 571894198
foreach model in CB2 AB2 CB3 AB4 {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') nocommonhet `opts'
	wbstats muC_C_A
	assert abs(r(mn1)-0.8)<0.1
}

