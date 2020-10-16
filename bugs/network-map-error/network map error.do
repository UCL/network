/* apparent error in network map
*/

myadopath network
cd "C:\ado\ian\network\bugs\network-map-error"

use "NMA-OS-ID changed-Component1.dta", clear
l id_name intervention2 intervention1 logoshr selogoshr
keep id_name intervention2 intervention1 logoshr selogoshr
*rename (id_name intervention2 intervention1 logoshr selogoshr) (study t1 t2 logOR se)
network import, studyvar(id_name) treat(intervention2 intervention1) effect(logoshr) stderr(selogoshr)
cap noi network map, debug

use "NMA-OS-ID changed-Component1.dta", clear
keep id_name intervention2 intervention1 logoshr selogoshr
network import, studyvar(id_name) treat(intervention1 intervention2) effect(logoshr) stderr(selogoshr)
cap noi network map, debug

exit 1

pda
use "NMA-OS-ID changed-Component1.dta", clear
keep id_name intervention2 intervention1 logoshr selogoshr
network import, studyvar(id_name) treat(intervention2 intervention1) effect(logoshr) stderr(selogoshr)
l
pause on
network_mapi

exit 1
pda
use "NMA-OS-ID changed-Component1.dta", clear
keep id_name intervention2 intervention1 logoshr selogoshr
network import, studyvar(id_name) treat(intervention2 intervention1) effect(logoshr) stderr(selogoshr)
l
networkplot 

exit 1


pda
use "NMA-OS-ID changed-Component1.dta", clear
l id_name intervention2 intervention1 logoshr selogoshr
*keep id_name intervention2 intervention1 logoshr selogoshr
network import, studyvar(id_name) treat(intervention2 intervention1) effect(logoshr) stderr(selogoshr)
network meta c, fixed 
netleague,eform 


exit 1

pda 
use "NMA-OS-ID changed-Component1.dta", clear
l id_name intervention2 intervention1 logoshr selogoshr
keep id_name intervention2 intervention1 logoshr selogoshr
network import, studyvar(id_name) treat(intervention2 intervention1) effect(logoshr) stderr(selogoshr)
network convert augmented
network meta c, fixed 
netleague,eform 


pda
use smokpairs, clear
network import, studyvar(study) treat(t2 t1) effect(y) stderr(stderr)
cap noi network map, debug
use smokpairs, clear
network import, studyvar(study) treat(t1 t2) effect(y) stderr(stderr)
cap noi network map, debug
