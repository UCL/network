// extracted from Anna's NMA course practical 3
// network forest gives error 4020 "not an array" after -graph twoway-
// IW 26/5/2022

// HOW I FOUND IT

import excel "C:\Users\rmjwiww\Dropbox\NMA course 2022\Practicals\psoriasis class data.xlsx", sheet("Sheet1") firstrow clear

network setup rPASI90C nC, stud(unique_id) trt(treatclass) rr nocodes ref(PBO)

network meta c
network meta i
network forest

// SHOWING IT'S VERY BASIC, FOLLOW WITH

network forest, name(forest,replace) debug nograph clear
scatter diff se // FAILS

// BUT NOW I DID -CLEAR ALL- AND EVERYTHING GOES OK AGAIN
