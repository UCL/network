pda
use "C:\copyofN\ABCB\analysis\hyp18\hyp186a.dta", clear
network setup d n, studyvar(study)
#delimit ;
network bayes, 
	model(CB) 
	nocommonhet 
	logsigC2mean(-1.56) 
	logsigAmean(0) 
	sigAprior(dlnorm(0,0.8)) 
	sigC2prior(dlnorm(-3.02,0.2922)) 
	alphaAprec(.03)
	muAprec(4)
	muCprec(.005)
	rhoprior(dbeta(25,1)) 
	savedir(c:\temp) 
	clear  
	burnin(0) updates(1000) thin(1)   
	prioronly 
;
#delimit cr


logvar sig*
*gen rho = 1 - sigC2/sigA2/2
summ
