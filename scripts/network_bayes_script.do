/*
Script to support network_bayes help file
Updated 20nov2017
*/

use http://www.ucl.ac.uk/~rmjwiww/stata/meta/smoking, clear

network setup d n, studyvar(study) trtvar(trt)

* Simple analysis using LA model:

network bayes, model(LA)

network bayes, model(LA) quit

network bayes, model(LA) quit sigCprior(dlnorm(-1.005,1.487))

network bayes, notrace ac 

network bayes, model(LA) quit sigCprior(dlnorm(-1.005,1.487)) burnin(10000) updates(10000) thin(10)

network bayes, model(LAplus) nocommonhet quit logsigCmean(-1.005)

network bayes, model(LAplus) nocommonhet quit logsigCmean(-1.005) prioronly

preserve
use LAplus_sample, clear
gen logsigC = log(sigC_A_B)
ci mean logsigC
restore

network bayes, model(AB) quit sigCprior(dlnorm(-1.005,1.487)) 

network bayes, model(AB) nocommonhet quit logsigCmean(-1.005)

