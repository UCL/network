// weights.do - can we express netowkr results as weighted sums of the data?
// for indirect comparisons, surely weights don't sum to 1?

use http://www.mrc-bsu.cam.ac.uk/IW_Stata/meta/thromb.dta, clear
network setup r n, studyvar(study) trtvar(treat)
network meta c, 
qui mvmeta, bos(w)
mvmeta

set trace off
local ests : rownames wt_matrix, 
local studs : roweq wt_matrix, 
local studs : subinstr local studs "Study" "", all
local wtsum 0 
local wtestsum 0
forvalues i=1/`=rowsof(wt_matrix)' {
	local est = word(`"`ests'"',`i')
	local study = word(`"`studs'"',`i')
	local wt = wt_matrix[`i',1]
	local estval = `est'[`study']
	if !mi(`estval') {
		* di as text "`est'" ///
			_col(11) "Study " `study' ///
			_col(24) as result %6.2f 100*`wt' ///
			_col(39) "`est'[`study'] = " `estval'
		local wtsum = `wtsum' + `wt'
		local wtestsum = `wtestsum' + `wt'*`estval'
	}
}
lincom [_y_B] _cons
di `wtsum'
di `wtestsum'
di `wtestsum' / `wtsum'

