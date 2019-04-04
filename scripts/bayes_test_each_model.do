/* network bayes: test each model 
Draw from the priors as an easy way to test the models
Use simplified priors 
*/
pda
use "C:\copyofN\ABCB\analysis\hyp18\hyp186a.dta", clear
network setup d n, studyvar(study)
save c:\temp\network_bayes_test, replace

local opts savedir(c:\temp\bugsfiles) burnin(0) updates(1000) thin(1) quit clear

#delimit ;

* Model 1CB/CH;
foreach prior in sigCprior(dlnorm(-1,4)) sigC2prior(dlnorm(-2,1)) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(1CB) name(model1CB) commonhet prioronly 
	`prior' `opts'
;
cap gen sigC=sqrt(sigC2);
cap gen sigC2=sigC^2;
for var sig*: gen logX=log(X);
summ logsigC logsigC2;
};


* Model 2CB/CH;
#delimit ;
foreach prior in sigCprior(dlnorm(-1,4)) sigC2prior(dlnorm(-2,1)) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(2CB) name(model2CB_CH) commonhet prioronly 
	`prior' `opts'
;
cap gen sigC=sqrt(sigC2);
cap gen sigC2=sigC^2;
for var sig*: gen logX=log(X);
summ logsigC logsigC2;
};


* Model 2CB/NCH;
#delimit ;
foreach prior in logsigCmean(-1) logsigC2mean(-2) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(2CB) name(model2CB_NCH) nocommonhet prioronly 
	`prior' `opts'
;
for var sig*: gen logX=log(X);
summ log*;
};


* Model 2AB/CH;
#delimit ;
foreach prior in sigCprior(dlnorm(-1,4)) sigC2prior(dlnorm(-2,1)) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(2AB) name(model2AB_CH) commonhet prioronly 
	`prior' `opts'
;
cap gen sigC=sqrt(sigC2);
cap gen sigC2=sigC^2;
for var sig*: gen logX=log(X);
summ logsig*;
};


* Model 2AB/NCH;
#delimit ;
foreach prior in logsigCmean(-1) logsigC2mean(-2) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(2AB) name(model2AB_NCH) nocommonhet prioronly 
	`prior' `opts'
;
cap gen sigC=sqrt(sigC2);
cap gen sigC2=sigC^2;
for var sig*: gen logX=log(X);
summ logsig*;
};


* Model 3CB/CH;
#delimit ;
foreach prior in sigCprior(dlnorm(-1,4)) sigC2prior(dlnorm(-2,1)) {;
foreach priorA in sigAprior(dlnorm(-0.5,4)) sigA2prior(dlnorm(-1,1)) {;

use c:\temp\network_bayes_test, clear;
network bayes, model(3CB) name(model3CB_CH) commonhet prioronly 
	`prior' `priorA' `opts'
;
for var sig*: gen logX=log(X);
summ logsig*;
};
};


* Model 3CB/NCH;
#delimit ;
foreach prior in logsigCmean(-1) logsigC2mean(-2) {;
foreach priorA in sigAprior(dlnorm(-0.5,4)) sigA2prior(dlnorm(-1,1)) {;

use c:\temp\network_bayes_test, clear;
network bayes, model(3CB) name(model3CB_NCH) nocommonhet prioronly 
	`prior' `priorA' `opts'
;
for var sig*: gen logX=log(X);
summ logsig*;
};
};


* Model 4AB/CH;
#delimit ;
foreach prior in sigCprior(dlnorm(-1,4)) sigC2prior(dlnorm(-2,1)) {;
foreach priorrho in rhoprior(dbeta(60,2)) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(4AB) name(model4AB_CH) commonhet prioronly 
	`prior' `priorrho' `opts' 
;
for var sig*: gen logX=log(X);
summ log*;
};
};


* Model 4AB/NCH;
#delimit ;
foreach prior in logsigCmean(-1) logsigC2mean(-2) {;
foreach priorA in logsigAmean(-0.5) logsigA2mean(-1) {;
use c:\temp\network_bayes_test, clear;
network bayes, model(4AB) name(model4AB_NCH) nocommonhet prioronly 
	`prior' `priorA' `opts' 
;
for var sig*: gen logX=log(X);
summ log*;
};
};
