cd "C:\Users\Ian White\Dropbox\network_v1.1"
import excel using SanchezAspirinNMA.xlsx, clear firstrow
network setup r n, study(study) trt(treat)
network meta c
network meta i
