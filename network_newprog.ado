/*
Template for new network suite member
Must add <newprog> to network.ado under 
	local subcmds0
or
	local subcmds1
*/

program define network_newprog

// PARSE
syntax

// LOAD SAVED NETWORK PARAMETERS
foreach thing in `_dta[network_allthings]' {
	local `thing' : char _dta[network_`thing']
}

// DO THE WORK

end
