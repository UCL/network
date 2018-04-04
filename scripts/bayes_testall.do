/*
Test all network bayes models using default priors
IW 15mar2018
*/
pda
local models CB1 CB2 CB3 AB
local hets commonhet nocommonhet 
local priors /*prioronly*/ noprioronly
local data 6a

cap mkdir temp

foreach model of local models {
foreach het of local hets {
foreach prior of local priors {

	if "`model'"=="CB1" & "`het'"=="nocommonhet" continue
	di _new(5) as input ///
		"Data `data' analysed by `model' `het', `prior'" ///
		_n

	clear
	use "C:\copyofN\ABCB\analysis\hyp18\hyp18`data'.dta"
	qui network setup d n, studyvar(study)

	local ch = substr("`het'",1,1)
	local pr = substr("`prior'",1,1)
	local name hyp18_run3_`data'_`model'_`ch'_`pr'

	if "`prior'"=="prioronly" local opts burnin(0) updates(10000) thin(1) 
	else local opts burnin(10000) updates(100000) thin(10) 

	network bayes, name(`name') model(`model') `het' ///
		savedir(c:\temp) ///
		clear quit dryrun ///
		trace(name(`name',replace)) `prior' `opts'
	
	*logvar sig*
	*summ log*

}
}
}
