/*
Cscript for network bayes 
Updated 20nov2017 & 22jun2018
3may2019 - add directory and log file
RUN IN NETWORK SCRIPTS DIRECTORY
*/


local networkdir c:\ado\ian\network\
cd `networkdir'scripts
log using `networkdir'testlogs\network_bayes_cscript.log

set more off
do network_adopath
pda

which network
which network_bayes


local opts updates(10000) name(cscript) quit savedir(c:\temp\bugsfiles) clear

set tracedepth 2
set trace off
local pm -1
local psd .5
local muCprec 1

// CHECK PRIORS: CH MODELS
foreach model in 1CB 2CB 2AB 3CB 4AB {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') commonhet ///
		muCprec(`muCprec') muAprec(`=`muCprec'*2') ///
		prioronly sigCprior(dlnorm(`pm',`=1/`psd'^2')) `opts'
	gen logsigC = log(sigC)
	ttest logsigC==`pm'
	assert r(p)>0.001
	sdtest logsigC==`psd'
	assert r(p)>0.001
	ttest muC_B_A==0
	assert r(p)>0.001
	sdtest muC_B_A==`=1/sqrt(`muCprec')'
	assert r(p)>0.001
}

// CHECK PRIORS: NCH MODELS
foreach model in 2CB 2AB 3CB 4AB {
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
foreach model in 1CB 2CB 2AB 3CB 4AB {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') commonhet `opts'
	wbstats muC_C_A
	assert abs(r(mn1)-0.8)<0.1
}

// CHECK SMOKING RESULTS: NCH MODELS
foreach model in 2CB 2AB 3CB 4AB {
	use "smoking.dta", clear
	qui network setup d n, studyvar(study) trtvar(trt)
	network bayes, model(`model') nocommonhet `opts'
	wbstats muC_C_A
	assert abs(r(mn1)-0.8)<0.1
}

log close
