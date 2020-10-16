/* network map error demonstrated in smoking data: 
import seems to require treat() to be specified in a very specific way
IW 22sep2020
*/

use http://www.homepages.ucl.ac.uk/~rmjwiww/stata/meta/smoking, clear
network setup d n, studyvar(stud) trtvar(trt)
network convert pairs
keep study _t* _y _st
rename (_*) (*)
network unset
save smokpairs, replace

// this version succeeds
use smokpairs, clear
network import, studyvar(study) treat(t1 t2) effect(y) stderr(stderr)
network map, debug

// this version fails
use smokpairs, clear
network import, studyvar(study) treat(t2 t1) effect(y) stderr(stderr)
network map, debug



// this version succeeds
use smokpairs, clear
rename (t1 t2) (experi contro)
network import, studyvar(study) treat(experi contro) effect(y) stderr(stderr)
network map, debug

use smokpairs, clear
rename (t1 t2) (experi contro)
network import, studyvar(study) treat(contro experi) effect(y) stderr(stderr)
network map, debug
