// Thromb, dropping study 1
* this is challenging with ref(B) because the first B design is B-E
* and there are no other E trials
prog drop _all
set more off
include thromb_bestmaps.do
foreach ref in A B C D E F G H {
use thromb, clear
keep study treat r n
qui network setup r n, ref(`ref') studyvar(study) trtvar(treat) nocodes 
qui network meta i, luades
nlcom ref`ref': [tau]_cons
}
exit
network meta i, luades
network forest, list ms(Oh) name(forestLA,replace)
network meta i
network forest, list ms(Oh) name(forestDBT,replace)
