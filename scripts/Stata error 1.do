/* 
1st error found in Paris NMA course 4-6 Dec 2019:
-network setup- fails ?because of hyphens in Study
NOW FIXED!!!
*/
pda
clear
import excel "C:\Users\rmjwiww\Dropbox\NMA course 2019\Practicals\datasets\Binary arm-level\501194_Akshintala 2013_verified.xlsx", sheet("Sheet1") firstrow
drop StudyID treat
network setup r n, study(Study) trt(Treatment)
