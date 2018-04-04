pda
use http://www.mrc-bsu.cam.ac.uk/IW_Stata/meta/smoking, clear
keep study treat d n
network setup d n, studyvar(stud) trtvar(treat)

network meta c
local pbestopt max in 1, rep(1000) zero stripprefix(_y_) zeroname(A) all id(study) note(Smoking network: consistency model)
mvmeta, noest pbest(`pbestopt' bar name(pbestbar, replace) title(Bar graph of ranks) legend(row(1)))
mvmeta, noest pbest(`pbestopt' bar cum name(pbestbarcum, replace) title(Cumulative bar graph of ranks) legend(row(1)))
mvmeta, noest pbest(`pbestopt' line name(pbestline, replace) title(Line graph of ranks))
mvmeta, noest pbest(`pbestopt' line cum name(pbestlinecum, replace) title(Cumulative line graph of ranks))

network meta i
local pbestopt max, rep(1000) zero stripprefix(_y_) zeroname(A) all id(study) note(Smoking network: inconsistency model)
mvmeta, noest pbest(`pbestopt' bar name(ipbestbar, replace) title(Bar graph of ranks) legend(row(1)))
mvmeta, noest pbest(`pbestopt' bar cum name(ipbestbarcum, replace) title(Cumulative bar graph of ranks) legend(row(1)))
mvmeta, noest pbest(`pbestopt' line name(ipbestline, replace) title(Line graph of ranks))
mvmeta, noest pbest(`pbestopt' line cum name(ipbestlinecum, replace) title(Cumulative line graph of ranks))
