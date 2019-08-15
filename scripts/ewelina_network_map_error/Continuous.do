 * Data set up from mean sd N effect estimate (W)MD
   network setup mean sd n, studyvar(studyid) trtvar(trt) md ref(NOFE)
   
   br studyid _design
   ///Fochi 1985 (row 10) design C G  the comparison ICSAC vs IFS
   
 * Network description 
   network map, list
   
 * Labels changed to simple codes 
replace trt= "A" if trt=="FASG"
replace trt= "B" if trt=="ICARB"
replace trt= "C" if trt=="ICSAC"
replace trt= "D" if trt=="IFA"
replace trt= "E" if trt=="IFF"
replace trt= "F" if trt=="IFG"
replace trt= "G" if trt=="IFS"
replace trt= "H" if trt=="IMISCA"
replace trt= "I" if trt=="IPMCX"
replace trt= "J" if trt=="IVIDX"
replace trt= "K" if trt=="IVIFCM"
replace trt= "L" if trt=="IVIPMCX"
replace trt= "M" if trt=="IVISU"
replace trt= "N" if trt=="LAC"
replace trt= "O" if trt=="NOFE"

 * Data set up from mean sd N effect estimate (W)MD
   network setup mean sd n, studyvar(studyid) trtvar(trt) md ref(O)
   
   br studyid _design
   ///Fochi 1985 (row 10) design C G

   * Network description 
   network map, list
   
