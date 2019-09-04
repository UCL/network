/*
Small test script for network table
USer needs to check the treatments are correctly ordered in each case
*/

use thromb, clear
network setup r n, studyvar(study) trtvar(treat) numcodes
network table, trtcodes
network table
more

use thromb, clear
network setup r n, studyvar(study) trtvar(treat) 
network table, trtcodes
network table
more

use thromb, clear
encode treat, gen(trt)
drop treat
label def trt 1 "SK" 2 "AtPA" 3 "tPA" 6 "Ret"  5 "Ten"  4 "SK+tPA"  7 "UK"  8 "ASPAC", modify
network setup r n, studyvar(study) trtvar(trt) numcodes
network table, trtcodes
network table
more

use thromb, clear
encode treat, gen(trt)
drop treat
label def trt 1 "SK" 2 "AtPA" 3 "tPA" 6 "Ret"  5 "Ten"  4 "SK+tPA"  7 "UK"  8 "ASPAC", modify
network setup r n, studyvar(study) trtvar(trt) 
network table, trtcodes
network table


