use h:\meta\network\ado\thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) 
encode _design, gen(design)
keep st n* design
reshape long n, i(study) j(trt) string
drop if mi(n)
encode trt, gen(treat)
* consistency model
qui reg n i.study i.treat
di e(rank)
* DBT inconsistency model
qui reg n i.study i.treat#i.design
di e(rank)
* full heterogeneity model
qui reg n i.study##i.treat
di e(rank)
