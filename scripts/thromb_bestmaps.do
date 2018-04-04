* Best network maps for thrombolytics data

#delimit ;
mat A=(
3,3,9\
5,4,3\
1,2,9\
4.33,3,6\
6,4.5,3\
3,3.67,9\
5,2,3\
1,4,9)
;
#delimit cr
mat rownames A = A B C D E F G H
mat colnames A = x y labelpos

/*
use thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) nocodes
network map, loc(A) name(mapall, replace) title("Thrombolytics network") 

use thromb, clear
keep study treat r n
drop if inlist(study,1,2)
network setup r n, studyvar(stud) trtvar(treat) nocodes
network map, loc(A) name(map2arm, replace) title("Thrombolytics network, 2-arm studies only") 
*/
