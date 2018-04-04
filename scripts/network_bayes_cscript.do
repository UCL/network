/*
Cscript for network bayes 
Updated 20nov2017
*/

set more off
adopath ++ c:\ado\network\
adopath ++BASE
adopath ++SITE
pda

which network

use "C:\ado\network\smoking.dta", clear
network setup d n, studyvar(study) trtvar(trt)

local opts name(cscript) quit savedir(temp, create)

set tracedepth 2
set trace off

network bayes, model(LA) `opts' 

local opts name(cscript) quit savedir(temp)

network bayes, model(LA) `opts' sigCprior(dlnorm(-1.005,1.487))

network bayes, notrace ac savedir(temp)

network bayes, model(LA) `opts' sigCprior(dlnorm(-1.005,1.487)) burnin(10000) updates(10000) thin(10)

local opts `opts' notrace

network bayes, model(LAplus) sigCprior(dlnorm(-1.005,1.487)) commonhet `opts' 

network bayes, model(LAplus) logsigCmean(-1) nocommonhet `opts'  

network bayes, model(CB) sigCprior(dlnorm(-1.005,1.487)) commonhet `opts' 

network bayes, model(CB) logsigCmean(-1) nocommonhet `opts' 

network bayes, model(AB) sigCprior(dlnorm(-1.005,1.487)) commonhet `opts' 

network bayes, model(AB) logsigCmean(-1) nocommonhet `opts' 

