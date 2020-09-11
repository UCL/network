/* 
2nd error found in Paris NMA course 4-6 Dec 2019:
	problems with network sidesplit
Acute Mania data: continuous data, arm-level 
*/
clear
import excel "C:\Users\rmjwiww\Dropbox\NMA course 2019\Practicals\datasets\Continuous arm-level\Cipriani 2011_501231_Acute Mania (mean change).xlsx", sheet("Sheet1") firstrow
drop t
network setup mean sd n, study(S) trt(T) ref(Placebo)
network map, name(AcuteMania,replace)

local opt fixed // only for speed 

* block 1: matsize too small, mvmeta will fail
set matsize 400

* unhelpful error if matsize is too small
cap noi network sidesplit A F, `opt'

* and unhelpful missing values
network sidesplit all, `opt'

* block 2: matsize too small, mvmeta will succeed
set matsize 4000

* unhelpful error if no direct evidence
cap noi network sidesplit A D, `opt'

* successful counterparts of analyses that failed above
* to show they can work
network sidesplit A F, `opt'
network sidesplit all, `opt'

