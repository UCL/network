/* 
error_4020.do
an error on network map or network forest (occurs at the graph statement)
*/
use thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) 
network convert standard
network meta c, fixed
network meta i, fixed
cap noi network map
cap noi network forest
