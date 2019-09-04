/* 
compute EB estimates after mvmeta estimation
not suitable for meta-regression, since it assumes the first elements of e(b) are the means
IW 4sep2019
could be worked into a post-estimation command

Note: if Sigma is singular, EB estimates may look bad
	as e.g. after running the default mvmeta b V, bscov(uns)
	see study 31 where S is also near-singular
*/

* net get st0156, from(http://www.stata-journal.com/software/sj9-1)
use FSCstage1, clear
mvmeta b V, bscov(exch 0.5)

mat ydata=e(ydata)
mat Sdata=e(Sdata)
mat Sigma=e(Sigma)
mat mu=e(b)
mat mu=mu[1,1..e(dims)]

forvalues i=1/`=e(N)' {
	local first = e(dims)*(`i'-1)+1
	local last = e(dims)*`i'
	mat yi = ydata[`i',1...]
	local studyname : rowname yi
	mat Si = Sdata[`first'..`last',1...]
	mat roweq Si = ""
	mat l yi, title(Data for study `i')
	mat l Si, title(Variance matrix for study `i')
	mat eb = (yi*syminv(Si)+mu*syminv(Sigma)) * syminv(syminv(Si)+syminv(Sigma))
	mat rownames eb = "`studyname'"
	mat ebvar = syminv(syminv(Si)+syminv(Sigma))
	mat l eb, title(EB estimates for study `i')
	mat l ebvar, title(EB variance matrix for study `i')
}
