/* Demonstrate that network forest can now work even
   when changes have been made since fitting the network meta models
*/
prog drop _all
set more off
use http://www.mrc-bsu.cam.ac.uk/IW_Stata/meta/thromb.dta, clear
network setup r n, studyvar(study) trtvar(treat)
network meta i
network meta c
network convert standard
network forest, name(tf1, replace) trtcodes 
network forest, name(tf2, replace) trtcodes eform 
