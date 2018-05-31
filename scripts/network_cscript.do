/* CERTIFICATION SCRIPT FOR NETWORK.ADO
REQUIRES: MVMETA, METAREG
31may2018: added checks of matrix output of network meta, and hairy trtcodes
6apr2018:  added network compare
13mar2017: CTU file locations; added connectedness check
21dec2015: corrected treat to trt for smoking data (now follows SJ article)
8jun2015:  added other effect measures
25may2015: added network meta i, luades and network rank
15may2015: force option added to network meta in pairs format
2apr2015:  data sets now assumed to be in current directory:
    smoking.dta
    thromb.dta
    coronary artery disease pairwise.dta
11jul2014: fails with long names
*/
cls
cscript network
set trace off
set more off
pause on
prog drop _all
local mvmetadir c:\ado\ian\mvmeta\
adopath ++ `mvmetadir'package
local networkdir c:\ado\ian\network\
adopath ++ `networkdir'package
cd `networkdir'scripts

which mvmeta
which network

// check network setup

use smoking, clear
keep study trt d n

network setup d n, studyvar(stud) trtvar(trt) format(aug) ref(Self help)
l if study<=3

// check network convert
network convert pairs
l if study<=3

network convert stan
l if study<=3
save z, replace

network convert aug, large(1E5)
l if study<=3

network convert pairs
l if study<=3

network convert stan
l if study<=3

network convert aug, large(1E5)
l if study<=3

network convert pairs
l if study<=3

network convert stan
l if study<=3

cf _all using z, verbose


// check network meta: aug & std, multi-arm trials
use smoking, clear
keep study trt d n
network setup d n, studyvar(stud) trtvar(trt) format(aug) ref(Self help)
network table

network meta c, print(bscov)
local tau = [tau]_b[_cons]

network compare
network compare, eform format(%6.2f) saving(z) replace level(99)
preserve
network compare, clear
restore


network convert st
network meta c, print(bscov)
local reldiff = 100*([tau]_b[_cons]/`tau'-1)
di "reldiff = " `reldiff' "% (" [tau]_b[_cons] " vs. " `tau' ")"
if abs(`reldiff')>0.01 exit 99


network convert aug
network meta c, print(bscov)
local reldiff = 100*([tau]_b[_cons]/`tau'-1)
di "reldiff = " `reldiff' "% (" [tau]_b[_cons] " vs. " `tau' ")"
if abs(`reldiff')>0.01 exit 99


network meta i, print(bscov)
local tau = [tau]_b[_cons]

network convert st
network meta i, print(bscov)
local reldiff = 100*([tau]_b[_cons]/`tau'-1)
di "reldiff = " `reldiff' "% (" [tau]_b[_cons] " vs. " `tau' ")"
if abs(`reldiff')>0.01 exit 99
	// tau = 0.55296504


network convert aug
network meta i, print(bscov)
local reldiff = 100*([tau]_b[_cons]/`tau'-1)
di "reldiff = " `reldiff' "% (" [tau]_b[_cons] " vs. " `tau' ")"
if abs(`reldiff')>0.01 exit 99


* change reference treatment
use smoking, clear
keep study trt d n
network setup d n, studyvar(stud) trtvar(trt) format(aug) ref(Individual counselling)
network meta i, print(bscov)
local reldiff = 100*([tau]_b[_cons]/`tau'-1)
di "reldiff = " `reldiff' "% (" [tau]_b[_cons] " vs. " `tau' ")"
if abs(`reldiff')>0.01 exit 99


// check network meta: aug, std & pairs, only two-arm trials
use "coronary artery disease pairwise", clear
network import, tr(t1 t2) eff(logOR) study(study) stderr(se) 

network meta c, z

network conv st
network meta c, nounc print(bscov)
network compare

network conv aug
network meta c, nounc print(bscov)

network conv pairs
network meta i, z

network conv st
network meta i, nounc print(bscov)

network conv aug
network meta i, nounc print(bscov)


// forest plot

use "coronary artery disease pairwise", clear
network import, tr(t1 t2) eff(logOR) study(study) stderr(se) measure(mean diff)
network conv aug
network meta c, 
network compare
network meta i, 
network forest, msize(*0.2)


// Invented data to test network setup for quantitative data

clear
input study str1 trt sbp sbpsd count
1 A 50 10 100
1 B 60 10 100
1 C 62 10 100
2 A 70 10 100
2 B 75 10 100
3 A 60 10  25
3 C 55 10  25
end
l
network setup sbp sbpsd count, stud(study) trt(trt) smd
list study _y* _S*, noo sepby(_design)
network table
network query
network meta c, force
network compare

// Check everything works for all formats and refcats

foreach format in augmented standard pairs {
	foreach ref in "No contact" "Self help" "Individual counselling" "Group counselling" {
		di as input "Loop 1: format `format', ref `ref'"
		use smoking, clear
		keep study trt d n
		network setup d n, studyvar(stud) trtvar(trt) format(`format') ref(`ref')
		network table
		network pattern
		local force = cond("`format'"=="pairs","force","")
		network meta c, i2 `force'
		network meta i, `force'
		network query
		network forest, name(forest_`format', replace)
		if "`format'"=="augmented" network meta i, luades `force'
		if "`format'"=="augmented" network sidesplit B C, show nosymm 
		if "`format'"=="augmented" network sidesplit all, tau
		network unset
	}
}


// Check it also handles long study and treatment names
foreach format in augmented standard pairs {
	di as input "Loop 2: format `format'"
	use smoking, clear
	decode trt, gen(trtstring)
	drop trt
	rename trtstring trt
	gen stud="Study " + string(study)
	keep stud trt d n
	network setup d n, studyvar(stud) trtvar(trt) format(`format') 
	network table
	network pattern
	network map, circle(9) centre improve(2) name(map`format'`ref'circle, replace) loc(M) replace listloc 
	network map, square(3) improve(2) name(map`format'`ref'square, replace) trtcodes
	network map, tri(4) improve(2) name(map`format'`ref'triangular, replace) title(The triangular one)
	network map, random(9) improve(2) name(map`format'`ref'random, replace)
	local force = cond("`format'"=="pairs","force","")
	network meta c, i2 `force'
	network compare
	network meta i, `force'
	network que
	network forest, debug name(forest_`format', replace)
}


// More forest plots - formats should agree
* Load and analyse the smoking data
foreach format in pairs aug st {
	di as input "Loop 3: format `format'"
	use smoking, clear
	keep study trt d n
	network setup d n, studyvar(stud) trtvar(trt) format(`format')
	local opts ms(S) xsize(9.8) ysize(7.3) columns(xtile) ///
		xline(0,lcol(gray)) contrastpos(2)
	network forest, title(Smoking network) `opts' ///
		name(smoke_forest1_`format', replace) 	
	local force = cond("`format'"=="pairs","force","")
	network meta c, `force'
	network meta i, `force'
	network forest, title(Smoking network) `opts' ///
		name(smoke_forest2_`format', replace) eform group(type) xlabel(0.1 1 10 100)
}

* Load and analyse the thrombolytics data (metareg fails for pairs format - why?)
foreach format in aug st {
	di as input "Loop 4: format `format'"
	use thromb, clear
	keep study treat r n
	network setup r n, studyvar(stud) trtvar(treat) format(`format')
	local opts ms(Sh) xsize(9.8) ysize(7.3) columns(xtile) ///
		xline(0,lcol(gray)) contrastpos(-1.5) ncolumns(3)
	network forest, title(Thrombolytics network) `opts' ///
		name(thromb_forest1_`format', replace) 
	local force = cond("`format'"=="pairs","force","")
	network meta c, `force'
	network compare
	network meta i, `force'
	network forest, title(Thrombolytics network) `opts' ///
		name(thromb_forest2_`format', replace) 
}

* Check if and in
use thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) 
network meta i if study>10
local test1 = r(chi2)
keep if study>10
network meta i
local test2 = r(chi2)
di `test1', `test2'
assert abs(`test2'/`test1'-1) < 1E-10 

* Check rank
network meta c
network rank min, rep(50) seed(41087) cum all bar saving(z) replace mcse ///
	title(Rank bars) name(rank1, replace) legend(row(2))
network rank min, meanrank clear line predict trtcodes ///
	ylabel(0 25 50) title(Rankogram) name(rank2, replace)

* Check network import from augmented format, and play with prefixes
use thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) genprefix(AA)
network meta i
local test1 = r(chi2)
network unset
drop AAdesign
rename study myid
rename (AAy*) (b*)
rename (AAS*) (V*)
network import, studyvar(myid) effect(b) variance(V) ref(A)
network meta i
local test2 = r(chi2)
di `test1', `test2'
assert abs(`test2'/`test1'-1) < 1E-10 

* Check other effect measures
foreach measure in rr or rd hr {
	use thromb, clear
	keep study treat r n
    network setup r n, studyvar(stud) trtvar(treat) `measure'
    network meta c
	network compare
    network meta i
    network forest, name(`measure', replace)
} 

* Check with >26 treatments
clear
set obs 30
gen study = _n
gen trt1 = 0
gen trt2 = _n
reshape long trt, i(study) j(arm)
drop arm
gen n = 100
gen d =  20
network setup d n, study(study) trt(trt)


// Check connectedness routines
clear
input trt1	trt2
1	2
2	3
2	5
4	5
6	7
7	8
end
gen trial = _n
reshape long trt, i(tri) 
drop _j
gen n = 10
gen d = 5

network setup d n, studyvar(trial) trtvar(trt) numcodes
matrix list network_adjacency // the adjacency matrix
matrix list network_indirect_connection // the indirect connection matrix
matrix list network_distance // the distance matrix
matrix list network_components // assigns treatments to components
assert rowsof(network_components) == 8
assert colsof(network_components) == 2

// Check variances aren't deleted when one trtcode starts with reference trtcode
clear
input trial str10 trt n d
1 "A" 100 10
1 "AB" 100 10
end
network setup d n, study(trial) trt(trt) nocode
l _S_AB_AB


// Check matrices output by network meta
// Uses tough nested treatment codes
// 31may2018

clear
set obs 18
gen study = _n
gen trt1 = cond(study<=12,1,7)
gen trt2 = cond(study<=12,mod(_n-1,6)+2,mod(_n-1,6)+1)
reshape long trt, i(study) j(arm)
gen n=100
gen r=20+study+5*trt
label def trt 1 "AA" 2 "AAB" 3 "AABC" 4 "BC" 5 "B" 6 "BD" 7 "A" 
label val trt trt
drop arm
l, sepby(study)
network setup r n , study(study) trt(trt) nocode

network meta c, fixed
mat nc = _network_consistency
network meta i, fixed
mat ni = _network_inconsistency
foreach format in standard pairs augmented {
	network convert `format'
	network meta c
	assert mreldif(nc,_network_consistency)<1E-6
	network meta i
	assert mreldif(ni,_network_inconsistency)<1E-6
	// worst is about 5E-7
}

// Check setup aborts when designs are potentially ambiguous
// 31may2018
pda
clear
input trial trt n d
1 1 100 10
1 2 100 10
1 3 100 10
1 4 100 10
1 5 100 10
2 2 100 10
2 3 100 10
3 4 100 10
3 5 100 10
end
label def trt 1 "A" 2 "B" 3 "CD" 4 "BC" 5 "D"
label val trt trt
cap network setup d n, study(trial) trt(trt) nocode
assert _rc==498
