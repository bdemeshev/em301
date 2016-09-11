set more off

log using sem14.log, text replace

/*	*****************************************************/
/*     	File Name:	sem14.do					        */
/*     	Date:   	January 14, 2013				    */
/*      Author: 	Elena S. Vakulenko					*/
/*      Purpose:	Logit and Probit models             */
/*      Input File:	sem14.dta							*/
/*      Output File:	sem14.log					    */	
/*	*****************************************************/


sum

*Logit estimation
logit agree age nonint mw14 meduc adjinc nsibs fpro cath so urb

*Probit estimation
probit agree age nonint mw14 meduc adjinc nsibs fpro cath so urb
est store prob1

gen adjinc2=adjinc^2
gen adjinc_age=adjinc*age

probit agree age age2 nonint mw14 meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb
est store prob2

*LR test
lrtest prob1 prob2

*Wald test
testparm  adjinc2 adjinc_age age2

testparm  adjinc2 adjinc_age 

probit agree age age2 nonint mw14 meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb, vce(robust)

probit agree age age2 nonint  meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb if mw14==0
probit agree age age2 nonint  meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb if mw14==1


foreach i of var  age age2 nonint  meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb {
gen `i'_m=`i'*mw14
}

probit agree age age2 nonint  meduc adjinc adjinc2 adjinc_age nsibs fpro cath so urb mw14 *_m

testparm mw14 *_m

probit agree age nonint mw14 meduc adjinc nsibs fpro cath so urb
probit agree age mw14 meduc adjinc nsibs fpro cath so urb
probit agree age mw14 meduc adjinc nsibs fpro cath urb

*Prediction
predict agree_f, pr

hist agree_f
sum agree
tabulate agree

*Testing
estat classification
estat gof   
lroc
lsens

estat classification, cutoff(0.2) all

probit agree age mw14 meduc adjinc nsibs fpro cath so urb

*Marginal effects
margins,  dydx(age mw14 meduc adjinc nsibs fpro cath so urb)

*Prediction at point
margins, at( age=25 mw14=0 meduc=12 (mean) adjinc nsibs=2 fpro=0 cath=1 so=1 urb=1)

*Marginal effects at point
margins,  dydx(age mw14 meduc adjinc nsibs fpro cath so urb) at( age=25 mw14=0 meduc=12 (mean) adjinc nsibs=2 fpro=0 cath=1 so=1 urb=1)

log close

*clear
*exit
