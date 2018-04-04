pda
use thromb, clear
network setup r n, studyvar(study) trtvar(trt) genprefix(X)
network query
network convert aug
network query
network convert pairs
network query
network convert standard
network query
exit
network meta c
network rank min
* should always be: y S stderr contrast t1 t2 trtdiff
* but not always displayed!
