* a clean Stata run

local place laptop // MODIFY
if "`place'"=="laptop" {
	local dropbox "C:\Users\Ian White\Dropbox"
	local test c:\stata\test
}
else if "`place'"=="work" {
	local dropbox "C:\Users\ian\Dropbox"
	local test h:\stata\test
}
else exit 497

cap noi adopath -PERSONAL // or sysdir set PERSONAL rubbish
cap noi adopath -OLDPLACE // or sysdir set OLDPLACE rubbish
cap noi adopath - "H:\ado\progver\"
cap noi adopath - "H:\ado\progver"

cd `test'
!del /s /q *.* 
	// /s=+subdirectories; /q=without confirm
sysdir set PLUS `test'
prog drop _all

* mvmeta files to be tested
net from "`dropbox'/ado/mvmeta" 
* net from http://www.mrc-bsu.cam.ac.uk/IW_Stata/beta    // external website
* net from Q:\www\htdocs\BSUsite\Software\pub\software\stata\beta // internal website
net install mvmeta

* network files to be tested
net from "`dropbox'/ado/network/packages" // laptop
net install network

* other packages needed for mvmeta
ssc install sencode
ssc install coefplot

* check versions
which mvmeta
which network

cd "`dropbox'/ado/mvmeta"
do mvmeta_cscript

* other packages needed for network
ssc install metareg
net install network_graphs, from(http://www.mtm.uoi.gr)

cd "`dropbox'/ado/network/scripts"
do network_cscript

