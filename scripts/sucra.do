/*
Demonstrate SUCRA
*/
pda
use thromb, clear
keep study treat r n
network setup r n, studyvar(study) trtvar(treat) nocodes 
network meta i

network rank min, all line cum seed(61606) clear ///
	name(cumrank, replace) meanrank format(%6.3f)
format _Pbest %6.2f
scatter _Pbest _Treat if _Rank==19, ytitle(SUCRA) ///
	title(Thrombolytics network: SUCRAs for 28 studies) ///
	name(sucra, replace) xlabel(0/7,value)
