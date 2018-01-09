{smcl}
{* *! version 1.3.1 29 Aug 2017}{...}
{vieweralsosee "Main network help page" "network"}{...}
{vieweralsosee "" "--"}{...}
{viewerjumpto "Syntax" "network_bayes##syntax"}{...}
{viewerjumpto "Description" "network_bayes##description"}{...}
{viewerjumpto "Remarks" "network_bayes##remarks"}{...}
{viewerjumpto "Examples" "network_bayes##examples"}{...}
{viewerjumpto "Requirements" "network_bayes##Requirements"}{...}
{title:Title}

{phang}{bf:network bayes} {hline 2} Perform Bayesian network meta-analysis using WinBUGS


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}{cmdab:network bayes}
[{cmd:,}
{it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Model specification}
{synopt:{opt name(string)}}Gives a name for the analysis. 
Default is "network_bayes".{p_end}
{synopt:{opt model(string)}}Specifies the model: LA, LAplus, CB or AB. 
If {opt model()} is not specified then {cmd:network bayes} enters replay mode:
it attempts to load and analyse samples from a previous run with the same {cmd:name}.{p_end}
{synopt:{opt nocom:monhet}}Specifies that model has different heterogeneity variances
for different treatment contrasts. 
Default is the same (common) heterogeneity variance for all treatment contrasts.{p_end}
{synopt:{opt prior:only}}Draws a sample from  the prior, ignoring the data.{p_end}

{syntab:MCMC options}
{synopt:{opt seed(#)}}Sets seed for MCMC sampling - HAS NO EFFECT.{p_end}
{synopt:{opt bu:rnin(#)}}Sets length of burnin. Default is 1000.{p_end}
{synopt:{opt up:dates(#)}}Sets number of updates. Default is 1000.{p_end}
{synopt:{opt th:in(#)}}Sets thinning of the chain. Default is 1 (no thinning).
Thinning does not change the number of updates performed 
and reduces the number of updates in the final sample.{p_end}
{synopt:{opt dryrun}}Writes WinBUGS files but does not run them. Useful in debugging.{p_end}

{syntab:WinBUGS options}
{synopt:{opt winbugs:dir(string)}}Specifies location of winbugs14.exe.
Default is c:\winbugs14.{p_end}
{synopt:{opt parms(string)}}The parameters to be monitored in the WinBUGS run. 
Default is to monitor the overall means and heterogeneities of all contrasts.{p_end}
{synopt:{opt quit}}Closes WinBUGS at the end of the run. This is helpful if the run was successful 
and unhelpful if the run failed.{p_end}
{synopt:{opt noTImer}}Suppresses timing the WinBUGS run.{p_end}
{synopt:{opt sav:edir(dirname[,create])}}Writes files to the named directory. 
Suboption {cmd:create} causes the directory to be created if it does not exist.{p_end}

{syntab:Prior specification: study and treatment main effects}
{synopt:{opt alphaAprec(#)}}In LA and LAplus models: 
the prior variance for the study-specific intercepts alphaA. 
The full prior is a Normal distribution with mean 0 and the specified variance.
Default is 0.001.{p_end}
{synopt:{opt muA1prec(#)}}In CB models: the prior variance for the mean muA1
of the study-specific intercepts alphaA. 
The full prior is a Normal distribution with mean 0 and the specified variance.
Default is 0.001.{p_end}
{synopt:{opt sigA1prior(#)}}In CB models: the full prior (in BUGS language) for the standard deviation sigA1
of the study-specific intercepts alphaA. Default is dunif(0,10).{p_end}
{synopt:{opt logsigAmean(#)}}In AB NCH models: the prior mean 
for the log of the arm heterogeneity variance in all arms.
Default value is 0.{p_end}
{synopt:{opt muCprec(#)}}In LA, LAplus and CB models: the prior variance for 
the overall mean treatment effects muC[k]. 
The full priors are Normal distributions with mean 0 and the specified variance.
Default is 0.001.{p_end}
{synopt:{opt muAprec(#)}}In AB models: the prior variance for the means muA[k]
of the study-specific intercepts alphaA[k]. 
The full priors are Normal distributions with mean 0 and the specified variance.
Default is 0.001.{p_end}

{syntab:Prior specification: heterogeneity variance}
{synopt:{opt sigCprior(string)}}In common-heterogeneity models: the full prior (in BUGS language) 
for the heterogeneity standard deviation sigC. (Note: the standard deviation, not the variance.)
Default value is dunif(0,10).{p_end}
{synopt:{opt rhoprior(string)}}In common-heterogeneity AB models: 
the full prior (in BUGS language) for rho, the correlation in the compound-symmetric heterogeneity matrix SigmaA. 
This is the between-studies correlation of treatment-specific means.
Default value is dunif(0,1).{p_end}
{synopt:{opt logsigCmean(#)}}In non-common-heterogeneity models: the prior mean 
for the log of the heterogeneity variance of any contrast.
Default value is 0.{p_end}
{synopt:{opt df(#)}}In non-common-heterogeneity models: 
the degrees of freedom of the inverse Wishart prior
for matrix SigmaC (in LAplus and CB models) or SigmaA (in AB models). 
Default value is the dimension of the matrix, i.e. #treatments-1 in LAplus and CB models or #treatments in AB models.{p_end}

{syntab:Output options, also available in replay mode}
{synopt:{opt ac}[{it:(options)}]}Graphs the autocorrelations for all monitored parameters, 
using a fast alternative to {help wbac}. 
{it:options} may be 
{cmdab:lag:s(#)} giving the maximum lag (default is 40),
{cmdab:sep:arate} giving a separate panel for each parameter (default is to overlay them),
and any {help twoway options:options for graph twoway}.{p_end}
{synopt:{opt notrace}}Suppresses the trace plots of all monitored parameters.{p_end}
{synopt:{opt trace}({it:options})}Specifies options for the trace plots:
may be any {help twoway_options:options for graph twoway}.{p_end}
{synopt:{opt den:sity}[{it:(options)}]}Draws density plots for all monitored parameters 
using {help wbdensity}. {it:options} are options for {help wbdensity}. {p_end}
{synopt:{opt nostats}}Suppresses the default summaries of the posterior distributions of all monitored parameters.{p_end}
{synopt:{opt clear}}Clears the current data and loads the sample into memory.{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}


{marker description}{...}
{title:Description}

{pstd}{cmd:network_bayes} performs a Bayesian analysis of a network meta-analysis model using winbugs.
It does this in the following steps:{p_end}
{phang2}1. Write out the model and data in Winbugs format, 
together with a suitable initial values file and script.{p_end}
{phang2}2. Call WinBUGS, which must be installed in the user's computer. WinBUGS draws a sample from the posterior 
and writes the results out to files in CODA format.{p_end}
{phang2}3. Read the samples from the CODA format files and store them in Stata format.{p_end}
{phang2}4. Summarise the samples.{p_end}
{pstd}In replay mode, steps 1-3 are omitted.

{pstd}The file names are formed from the model name followed by an underscore and 
"model.txt", "data.txt", "inits.txt", "scalars.txt", "script.txt" (for the model files);
"log.txt", "coda1.txt", "codaIndex.txt" (for the Winbugs output files);
and "sample.dta" (for the results file in Stata format).


{marker models}{...}
{title:Models}

{pstd}The LA model describes the observed data using study-specific baselines.
Each study has an intercept (fixed parameter) and one or more treatment effects (random parameters). 

{pstd}The LAplus model describes all the potential data using a common reference treatment
but is equivalent to the LA model.

{pstd}The CB model has the study intercepts as random parameters, 
uncorrelated with the treatment effect parameters.

{pstd}The AB model additionally allows the study intercepts to be correlated with the treatment effects.
The model is parameterised with each study having a set of treatment-specific means (random parameters). 

{pstd}Most NMA models assume that the heterogeneity variance is the same for all treatment contrasts [Salanti, 2008].
This is the default for {cmd:network bayes}.  
However there are non-common-heterogeneity options for LAplus, CB  and AB models.
Extending the LA model for non-common-heterogeneity is awkward [Lu & Ades 2009] and has not been 
implemented; for this choice the LAplus model should be used.
The non-common-heterogeneity model model the variance-covariance matrix 
using an inverse Wishart prior Sigma^-1 ~ W(R,nu):

{phang2}For the LAplus and CB models, Sigma is the variance-covariance matrix of the treatment contrasts,
of dimension #treatments-1, and 
R is a scalar multiple of the "P" matrix (ones on the diagonal, halves off the diagonal). 

{phang2}For the AB model, Sigma is the variance-covariance matrix of the treatment arms,
of dimension #treatments, and 
R is a scalar multiple of the identity matrix.

{pstd}The above formulations make the models invariant to a different choice of reference treatment and to 
re-ordering of the treatments. 
The user specifies the scalar by specifying the prior mean of the log 
heterogeneity variance for any treatment contrast.


{marker examples}{...}
{title:Examples}

{pin}. {stata "use http://www.ucl.ac.uk/~rmjwiww/stata/meta/smoking, clear"}

{pin}. {stata "network setup d n, studyvar(study) trtvar(trt)"}

{pstd}Simple analysis using LA model:

{pin}. {stata "network bayes, model(LA)"}

{pstd}Note that you have to close the WinBUGS window manually. 
In future analyses we use the quit option to do this automatically.

{pin}. {stata "network bayes, model(LA) quit"}

{pstd}Now we incorporate an informative prior for sigC. 
{help network_bayes##Turner++12:Turner et al (2012)} propose priors
depending on outcome and contrast types.
Here the outcome is "subjective" and the contrast is "non-pharmacological" so the 
appropriate prior for log(sigC^2) is a normal distribution with mean -2.01 and SD 1.64. 
This imples for log(sigC) a normal distribution with mean -1.005 and SD 0.82.
To specify this prior in winbugs notation we also need to know the precision (1/0.82)^2 = 1.487.

{pin}. {stata "network bayes, model(LA) quit sigCprior(dlnorm(-1.005,1.487))"}

{pstd}Mixing may be poor. Explore autocorrelations:

{pin}. {stata "network bayes, notrace ac"}

{pstd}Yes, autocorrelation is substantial up to lag 5-20. 
We increase the burnin and number of updates, and thin the chain to reduce autocorrelation:

{pin}. {stata "network bayes, model(LA) quit sigCprior(dlnorm(-1.005,1.487)) burnin(10000) updates(10000) thin(10)"}

{pstd}We now show some different models.
For clarity, in the commands below, we omit the burnin(), updates() and thin() options that should  be
used to give adequate precision.
First the LAplus model with non-common heterogeneity:

{pin}. {stata "network bayes, model(LAplus) nocommonhet quit logsigCmean(-1.005)"}

{pstd}But have we used the correct prior? To  check, we can draw from the prior:

{pin}. {stata "network bayes, model(LAplus) nocommonhet quit logsigCmean(-1.005) prioronly"}

{pstd}From this we see that the prior mean for each heterogeneity SD is about 1. 
But this does not tell us about the prior mean for the log heterogeneity SD.
We can find this by loading the saved samples and computing the sampled values of the log heterogeneity SD:

{pin}. {stata "preserve"}{p_end}
{pin}. {stata "use network_bayes_sample, clear"}{p_end}
{pin}. {stata "gen logsigC = log(sigC_A_B)"}{p_end}
{pin}. {stata "summ logsigC"}{p_end}
{pin}. {stata "restore"}{p_end}

{pstd}Note how we have not specified the prior SD of the log heterogeneity SD,
because this is instead implied by the inverse Wishart model.
It turns out to be about 1, so that our prior is slightly less informative 
in the non-common-heterogeneity model than in the common-heterogeneity model.

{pstd}Finally we fit the AB model with common heterogeneity:

{pin}. {stata "network bayes, model(AB) quit sigCprior(dlnorm(-1.005,1.487))"}

{pstd}and the AB model with non-common heterogeneity:

{pin}. {stata "network bayes, model(AB) nocommonhet quit logsigCmean(-1.005)"}


{marker Requirements}{...}
{title:Requirements}

{phang} Requires {browse "https://www.mrc-bsu.cam.ac.uk/software/bugs/":WinBUGS} to be installed onto the user's computer.

{phang} Requires John Thompson's winbugs suite to be installed into your ado directory. 
Currently the best way to do this appears to be to download 
{browse "http://www2.le.ac.uk/departments/health-sciences/research/gen-epi/Progs/winbugsfromstata/wbfiles.zip":wbfiles.zip} 
and unzip it into your ado directory.
Please see {browse "http://www2.le.ac.uk/departments/health-sciences/research/gen-epi/Progs/winbugs-from-stata":John Thompson's winbugs-from-Stata page} 
for more information.


{title:References}{marker refs}

{phang}{marker Turner++12}Turner RM, Davey J, Clarke MJ, Thompson SG, Higgins JPT.
Predicting the extent of heterogeneity in meta-analysis, 
using empirical data from the Cochrane Database of Systematic Reviews. 
International Journal of Epidemiology 2012; 41: 818–827.
{browse "http://ije.oxfordjournals.org/content/41/3/818.short"}


{p}{helpb network: Return to main help page for network}

