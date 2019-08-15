/* 
Bug in network map 
Discovered by Ewelina R, July 2019
Arises when reference is not the first treatment

to look out for: C (ICSAC) is compared with G (IFS) 
	in study "Fochi 1985" 
*/

// default ref trt
use "Continuous outcome arm format.dta", clear

network setup mean sd n, studyvar(studyid) trtvar(trt) md 
l studyid _design
   
network map, list trtcodes name(codes1,replace) debug
network map, list name(names1,replace) debug 
mat l _network_map_location

// alternative ref trt
use "Continuous outcome arm format.dta", clear

network setup mean sd n, studyvar(studyid) trtvar(trt) md ref(NOFE)

network map, list trtcodes name(codes2,replace) debug 
network map, list name(names2,replace) debug 
mat l _network_map_location

// alternative ref trt
use "Continuous outcome arm format.dta", clear

network setup mean sd n, studyvar(studyid) trtvar(trt) md ref(IFS)

network map, list trtcodes name(codes3,replace) debug 
network map, list name(names3,replace) debug 
mat l _network_map_location

/*
Treatment codes (regardless of reference) are
   A:                                  FASG
   B:                                  ICARB
   C:                                  ICSAC
   D:                                  IFA
   E:                                  IFF
   F:                                  IFG
   G:                                  IFS
   H:                                  IMISCA
   I:                                  IPMCX
   J:                                  IVIDX
   K:                                  IVIFCM
   L:                                  IVIPMCX
   M:                                  IVISU
   N:                                  LAC
   O:                                  NOFE
*/
