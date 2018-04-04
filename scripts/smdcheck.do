pda
clear
input study str1 trt sbp sbpsd count
1 A 50 10 100
1 B 60 20 200
1 C 62 10 100
2 A 70 10 100
2 B 75 20 200
3 A 60 10  25
3 C 55 20  50
end
set trace off
network setup sbp sbpsd count, stud(study) trt(trt) smd 
l study - countC, noo
list study _y* _S*, noo 
