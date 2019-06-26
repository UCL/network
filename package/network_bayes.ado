/*
*! Ian White # Experimental # 26jun2019
26jun2019: complete revision with new syntax
*/

prog def network_bayes

// MAIN PARSING

local opts name(name) replay SAVedir(passthru) // options for both run and output
local runopts /// options for run 
	mkdir Reference(string) study(string) treatment(string) heterogeneity(string) PRIORonly /// model specification
	seed(int 0) BUrnin(int 1000) UPdates(int 1000) THin(int 1) dryrun /// MCMC 
	WINBUGSdir(string) Quitbugs noTImer /// WinBUGS
	PARMs(string) // rename?
local outputopts /// options for output
	ac AC2(string) notrace TRACE2(string) DENsity DENsity2(string) nostats clear /// output options
	debug // undocumented options

* pre-parse
syntax, [`opts' `runopts' `outputopts'] // checks valid syntax
syntax, [`opts' `runopts' *] 
local outputoptvalues `options'	// identifies output options
syntax, [`opts' `outputopts' *]
local runoptvalues `options'	// identifies run options

syntax, [`opts' *]
if mi("`name'") local name network_bayes
if mi("`replay'") { // run + output
	network_bayes_run, name(`name') `savedir' `runoptvalues'
	network_bayes_output, name(`name') `savedir' `outputoptvalues'
}
else { //output only
	if !mi(`"`runoptvalues'"') {
		di as error "Options not allowed with replay: `runoptvalues'
		exit 198
	}
	network_bayes_output, name(`name') `savedir' `outputoptvalues'
}

end
