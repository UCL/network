// extracted from Anna's NMA course practical 3
// network forest gives error 4020 "not an array" after -graph twoway-
// IW 26/5/2022

import excel "..\datasets\psoriasis class data.xlsx", sheet("Sheet1") firstrow clear

//Prepare the data in the necessary format
network setup rPASI90C nC, stud(unique_id) trt(treatclass) rr nocodes ref(PBO)

network map, name(map,replace)

//Evaluate incoherence globally
network meta c
network meta i
network forest, name(forest,replace)

