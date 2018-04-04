prog def readbugs
syntax anything, [dir(string) saving(string) replace clear]
if !mi("`dir'") local dir `dir'/
if mi("`saving'") {
	local clear clear
	tempfile saving
}
else local hassaving yes
local filelength 0
foreach name of local anything {
	local filelength = max(`filelength',length("`name'"))
}
tempname post
postfile `post' str`filelength' file str12 parm n mean se sd median lower upper using `saving', `replace'
foreach name of local anything {
	use `dir'`name'_sample, clear
	foreach parm of varlist _all {
	if "`parm'"=="order" continue
	cap wbstats `parm'
	if _rc post `post' ("`name'") ("`parm'") (.) (.) (.) (.) (.) (.) (.)
	else   post `post' ("`name'") ("`parm'") (r(n1)) (r(mn1)) (r(se1)) (r(sd1)) (r(md1)) (r(lb1)) (r(ub1)) 
	}
}
postclose `post'

if !mi("`hassaving'") di as text "Results saved in `saving'"

if !mi("`clear'") {
	use `saving', clear
	di as text "Results are now loaded into memory"
}
end
