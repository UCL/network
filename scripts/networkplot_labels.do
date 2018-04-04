// awkwardness with networkplot if|in, labels()
pda
use thromb, clear
keep study treat r n
network setup r n, studyvar(stud) trtvar(treat) numcodes format(pairs)
networkplot _t1 _t2 if study>10, name(map0, replace) 
networkplot _t1 _t2 if study>10, name(map0A, replace) loc(A) 
networkplot _t1 _t2 if study>10, name(map1, replace) label("A" "B" "C" "E" "F" "G" "H")
networkplot _t1 _t2 if study>10, name(map2, replace) label("A" "B" "C" "D" "E" "F" "G" "H")
networkplot _t1 _t2 if study>10, name(map1A, replace) loc(A) label("A" "B" "C" "E" "F" "G" "H")
networkplot _t1 _t2 if study>10, name(map2A, replace) loc(A) label("A" "B" "C" "D" "E" "F" "G" "H")
keep if study>10
mat B = A[1..3,1...] \ A[5...,1...]
networkplot _t1 _t2, name(map3, replace) label("A" "B" "C" "E" "F" "G" "H")
networkplot _t1 _t2, name(map4, replace)
networkplot _t1 _t2, name(map3A, replace) loc(A) label("A" "B" "C" "E" "F" "G" "H")
networkplot _t1 _t2, name(map4A, replace) loc(A) 
networkplot _t1 _t2, name(map3B, replace) loc(B) label("A" "B" "C" "E" "F" "G" "H")
networkplot _t1 _t2, name(map4B, replace) loc(B) 
network table
network query, t
