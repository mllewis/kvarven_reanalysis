cd "" 

putexcel set "Dataset - Adjustment.xls", modify

putexcel A1="Original", nformat(number_d2)
putexcel B1="RE", nformat(number_d2)
putexcel C1="RE-se", nformat(number_d2)
putexcel D1="TF", nformat(number_d2)
putexcel E1="TF-se", nformat(number_d2)
putexcel F1="PET", nformat(number_d2)
putexcel G1="PET-se", nformat(number_d2)
putexcel H1="PEESE", nformat(number_d2)
putexcel I1="PEESE-se", nformat(number_d2)

import delimited "Meta\Roth.csv", clear

putexcel A2="Oppenheimer et al. (2009)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B2=(r(theta)), nformat(number_d2)
putexcel C2=(r(se)), nformat(number_d2)

meta trimfill

putexcel D2=(r(table)[2,2]), nformat(number_d2)
putexcel E2=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F2=(_b[_cons]), nformat(number_d2)
putexcel G2=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H2=(_b[_cons]), nformat(number_d2)
putexcel I2=(_se[_cons]), nformat(number_d2)


import delimited "Meta\Kühberger.csv", clear

putexcel A3="Tversky & Kahneman (1981)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B3=(r(theta)), nformat(number_d2)
putexcel C3=(r(se)), nformat(number_d2)

meta trimfill

putexcel D3=(r(table)[2,2]), nformat(number_d2)
putexcel E3=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F3=(_b[_cons]), nformat(number_d2)
putexcel G3=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H3=(_b[_cons]), nformat(number_d2)
putexcel I3=(_se[_cons]), nformat(number_d2)


import delimited "Meta\Miles.csv", clear

putexcel A4="Husnu & Crisp (2010)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B4=(r(theta)), nformat(number_d2)
putexcel C4=(r(se)), nformat(number_d2)

meta trimfill

putexcel D4=(r(table)[2,2]), nformat(number_d2)
putexcel E4=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F4=(_b[_cons]), nformat(number_d2)
putexcel G4=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H4=(_b[_cons]), nformat(number_d2)
putexcel I4=(_se[_cons]), nformat(number_d2)


import delimited "Meta\Schimmack.csv", clear

putexcel A5="Schwarz et al. (1991)", nformat(number_d2)

meta set q seq, random(dlaird)
meta summarize

putexcel B5=(r(theta)), nformat(number_d2)
putexcel C5=(r(se)), nformat(number_d2)

meta trimfill

putexcel D5=(r(table)[2,2]), nformat(number_d2)
putexcel E5=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress seq

putexcel F5=(_b[_cons]), nformat(number_d2)
putexcel G5=(_se[_cons]), nformat(number_d2)

meta regress varq

putexcel H5=(_b[_cons]), nformat(number_d2)
putexcel I5=(_se[_cons]), nformat(number_d2)



import delimited "Meta\Felts.csv", clear

putexcel A6="Hauser et al. (2007)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B6=(r(theta)), nformat(number_d2)
putexcel C6=(r(se)), nformat(number_d2)

meta trimfill

putexcel D6=(r(table)[2,2]), nformat(number_d2)
putexcel E6=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F6=(_b[_cons]), nformat(number_d2)
putexcel G6=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H6=(_b[_cons]), nformat(number_d2)
putexcel I6=(_se[_cons]), nformat(number_d2)



import delimited "Meta\Henriksson.csv", clear

putexcel A7="Critcher & Gilovich (2008)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B7=(r(theta)), nformat(number_d2)
putexcel C7=(r(se)), nformat(number_d2)

meta trimfill

putexcel D7=(r(table)[2,2]), nformat(number_d2)
putexcel E7=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F7=(_b[_cons]), nformat(number_d2)
putexcel G7=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H7=(_b[_cons]), nformat(number_d2)
putexcel I7=(_se[_cons]), nformat(number_d2)



import delimited "Meta\Kivikangas.csv", clear

putexcel A8="Graham et al. (2009)", nformat(number_d2)

meta set d se, random(dlaird)
meta summarize

putexcel B8=(r(theta)), nformat(number_d2)
putexcel C8=(r(se)), nformat(number_d2)

meta trimfill

putexcel D8=(r(table)[2,2]), nformat(number_d2)
putexcel E8=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress se

putexcel F8=(_b[_cons]), nformat(number_d2)
putexcel G8=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H8=(_b[_cons]), nformat(number_d2)
putexcel I8=(_se[_cons]), nformat(number_d2)




import delimited "Meta\Rabelo.csv", clear

putexcel A9="Jostmann et al. (2009)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B9=(r(theta)), nformat(number_d2)
putexcel C9=(r(se)), nformat(number_d2)

meta trimfill

putexcel D9=(r(table)[2,2]), nformat(number_d2)
putexcel E9=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F9=(_b[_cons]), nformat(number_d2)
putexcel G9=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H9=(_b[_cons]), nformat(number_d2)
putexcel I9=(_se[_cons]), nformat(number_d2)



import delimited "Meta\Blanken.csv", clear


putexcel A10="Monin & Miller (2001)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B10=(r(theta)), nformat(number_d2)
putexcel C10=(r(se)), nformat(number_d2)

meta trimfill

putexcel D10=(r(table)[2,2]), nformat(number_d2)
putexcel E10=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F10=(_b[_cons]), nformat(number_d2)
putexcel G10=(_se[_cons]), nformat(number_d2)

meta regress var

putexcel H10=(_b[_cons]), nformat(number_d2)
putexcel I10=(_se[_cons]), nformat(number_d2)


import delimited "Meta\Meissner.csv", clear


putexcel A11="Schooler & Engstler-Schooler1990", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B11=(r(theta)), nformat(number_d2)
putexcel C11=(r(se)), nformat(number_d2)

meta trimfill

putexcel D11=(r(table)[2,2]), nformat(number_d2)
putexcel E11=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F11=(_b[_cons]), nformat(number_d2)
putexcel G11=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H11=(_b[_cons]), nformat(number_d2)
putexcel I11=(_se[_cons]), nformat(number_d2)



import delimited "Meta\Hagger.csv", clear


putexcel A12="Sripada et al. (2014)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B12=(r(theta)), nformat(number_d2)
putexcel C12=(r(se)), nformat(number_d2)

meta trimfill

putexcel D12=(r(table)[2,2]), nformat(number_d2)
putexcel E12=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F12=(_b[_cons]), nformat(number_d2)
putexcel G12=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H12=(_b[_cons]), nformat(number_d2)
putexcel I12=(_se[_cons]), nformat(number_d2)




import delimited "Meta\Rand.csv", clear


putexcel A13="Rand et al. (2012)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B13=(r(theta)), nformat(number_d2)
putexcel C13=(r(se)), nformat(number_d2)

meta trimfill

putexcel D13=(r(table)[2,2]), nformat(number_d2)
putexcel E13=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F13=(_b[_cons]), nformat(number_d2)
putexcel G13=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H13=(_b[_cons]), nformat(number_d2)
putexcel I13=(_se[_cons]), nformat(number_d2)





import delimited "Meta\Coles.csv", clear


putexcel A14="Strack et al. (1988)", nformat(number_d2)

meta set es se, random(dlaird)
meta summarize

putexcel B14=(r(theta)), nformat(number_d2)
putexcel C14=(r(se)), nformat(number_d2)

meta trimfill

putexcel D14=(r(table)[2,2]), nformat(number_d2)
putexcel E14=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress se

putexcel F14=(_b[_cons]), nformat(number_d2)
putexcel G14=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H14=(_b[_cons]), nformat(number_d2)
putexcel I14=(_se[_cons]), nformat(number_d2)




import delimited "Meta\DeCoster.csv", clear


putexcel A15="Srull & Wyer (1979)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B15=(r(theta)), nformat(number_d2)
putexcel C15=(r(se)), nformat(number_d2)

meta trimfill

putexcel D15=(r(table)[2,2]), nformat(number_d2)
putexcel E15=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F15=(_b[_cons]), nformat(number_d2)
putexcel G15=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H15=(_b[_cons]), nformat(number_d2)
putexcel I15=(_se[_cons]), nformat(number_d2)




import delimited "Meta\Belle.csv", clear


putexcel A16="Mazar et al. (2008)", nformat(number_d2)

meta set d sed, random(dlaird)
meta summarize

putexcel B16=(r(theta)), nformat(number_d2)
putexcel C16=(r(se)), nformat(number_d2)

meta trimfill

putexcel D16=(r(table)[2,2]), nformat(number_d2)
putexcel E16=((r(table)[2,4] - r(table)[2,3])/(2*1.96)), nformat(number_d2)

meta regress sed

putexcel F16=(_b[_cons]), nformat(number_d2)
putexcel G16=(_se[_cons]), nformat(number_d2)

meta regress v

putexcel H16=(_b[_cons]), nformat(number_d2)
putexcel I16=(_se[_cons]), nformat(number_d2)

import excel "Dataset - Adjustment.xls", sheet("Sheet1") firstrow case(lower) clear

gen pet_z=pet/petse

gen petpeese=peese if pet_z>=1.96
replace petpeese=pet if pet_z<1.96

gen petpeesese=peesese if pet_z>=1.96
replace petpeesese=petse if pet_z<1.96



export excel using "Adjustment", firstrow(variables) replace
gen id=_n
save "Adjustment", replace

import excel "Dataset.xls", sheet("Dataset") firstrow case(lower) clear

gen id=_n

save "Dataset", replace

merge 1:1 id using "Adjustment.dta"

gen diff_re = re - replication_s
gen diff_tf = tf - replication_s
gen diff_petpeese = petpeese - replication_s
gen diff_threepsm = threepsm - replication_s

gen sed_re = sqrt(rese^2 + ser^2)
gen sed_tf = sqrt(tfse^2 + ser^2)
gen sed_petpeese = sqrt(petpeesese^2 + ser^2)
gen sed_threepsm = sqrt(threepsmse^2 + ser^2)

graph drop _all
set scheme s1mono

*Find Z-score to determine if the replication was statistically significant or not*
gen zscorer=abs(replication_s/ser)

//5 %-level
gen rsignif=1 if zscorer>=1.959964
replace rsignif=0 if zscorer<1.959964
replace rsignif=0 if replication_s<=0

//0.5 %-level
gen rsignif05=1 if zscorer>=2.807
replace rsignif05=0 if zscorer<2.807
replace rsignif05=0 if replication_s<=0

*Find relative effect between the meta-analyses and the replications*
sum replication_s
sum meta_s

*Average if all metas use RE*
sum re

*Wilcoxon test*
signrank diff = 0

*Tau correlation analysis*
*gen tau=sqrt(tausqrm)
spearman diff tau
pwcorr diff tau, sig
sum tau

*Analysis for original studies*
sum effecto
sum meta_s
gen diffo=effecto - meta_s
gen sediffo=sqrt((sem)^2 + (seo)^2)
meta set diffo sediffo, random(dlaird)
meta summarize

*Analysis figure 3*
putexcel set "Table 6.xls", modify
putexcel A1="Test", nformat(number_d2)
putexcel B1="Difference", nformat(number_d2)
putexcel C1="CI - L 5%", nformat(number_d2)
putexcel D1="CI - U 5%", nformat(number_d2)
putexcel E1="SE", nformat(number_d2)
putexcel F1="N", nformat(number_d2)
putexcel G1="Z", nformat(number_d2)
putexcel H1="p-value", nformat(number_d2)
putexcel I1="Mean difference", nformat(number_d2)

putexcel A2="Main", nformat(number_d2)
putexcel A3="Published studies", nformat(number_d2)
putexcel A4="Excluding studies that included the replication", nformat(number_d2)
putexcel A5="Included original", nformat(number_d2)
putexcel A6="Carter", nformat(number_d2)
putexcel A7="Loyalty", nformat(number_d2)
putexcel A8="Sanctity", nformat(number_d2)
putexcel A9="Excluding cohens q", nformat(number_d2)
putexcel A10="Only unconverted", nformat(number_d2)
putexcel A11="Only converted", nformat(number_d2)
putexcel A12="ML", nformat(number_d2)
putexcel A13="RRR", nformat(number_d2)
putexcel A14="Sig 5", nformat(number_d2)
putexcel A15="Not sig 5", nformat(number_d2)
putexcel A16="Sig 05", nformat(number_d2)
putexcel A17="Not sig 05", nformat(number_d2)


meta set diff sed, random(dlaird)
***RE***
*Main*
meta summarize
putexcel B2=(r(theta)), nformat(number_d2)
putexcel C2=(r(ci_lb)), nformat(number_d2)
putexcel D2=(r(ci_ub)), nformat(number_d2)
putexcel E2=(r(se)), nformat(number_d2)
putexcel F2=(r(N)), nformat(number_d2)
putexcel G2=(r(z)), nformat(number_d2)
putexcel H2=(r(p)), nformat(number_d2)
*Published studies*
meta summarize if publi==1
putexcel B3=(r(theta)), nformat(number_d2)
putexcel C3=(r(ci_lb)), nformat(number_d2)
putexcel D3=(r(ci_ub)), nformat(number_d2)
putexcel E3=(r(se)), nformat(number_d2)
putexcel F3=(r(N)), nformat(number_d2)
putexcel G3=(r(z)), nformat(number_d2)
putexcel H3=(r(p)), nformat(number_d2)
*Excluding studies that included the replication*
meta summarize if replication_inc==0
putexcel B4=(r(theta)), nformat(number_d2)
putexcel C4=(r(ci_lb)), nformat(number_d2)
putexcel D4=(r(ci_ub)), nformat(number_d2)
putexcel E4=(r(se)), nformat(number_d2)
putexcel F4=(r(N)), nformat(number_d2)
putexcel G4=(r(z)), nformat(number_d2)
putexcel H4=(r(p)), nformat(number_d2)
*Included original*
meta summarize if originalinc==1
putexcel B5=(r(theta)), nformat(number_d2)
putexcel C5=(r(ci_lb)), nformat(number_d2)
putexcel D5=(r(ci_ub)), nformat(number_d2)
putexcel E5=(r(se)), nformat(number_d2)
putexcel F5=(r(N)), nformat(number_d2)
putexcel G5=(r(z)), nformat(number_d2)
putexcel H5=(r(p)), nformat(number_d2)
*Using Carter*
preserve
replace diff=0.39 if original=="Sripada et al. (2014)"
replace sed=0.072514423 if original=="Sripada et al. (2014)"
meta set diff sed
meta summarize
putexcel B6=(r(theta)), nformat(number_d2)
putexcel C6=(r(ci_lb)), nformat(number_d2)
putexcel D6=(r(ci_ub)), nformat(number_d2)
putexcel E6=(r(se)), nformat(number_d2)
putexcel F6=(r(N)), nformat(number_d2)
putexcel G6=(r(z)), nformat(number_d2)
putexcel H6=(r(p)), nformat(number_d2)
restore
*Using Loyalty*
preserve
replace diff=0.55707583 if original=="Graham et al. (2009)"
replace sed=0.057183299 if original=="Graham et al. (2009)"
meta set diff sed
meta summarize
putexcel B7=(r(theta)), nformat(number_d2)
putexcel C7=(r(ci_lb)), nformat(number_d2)
putexcel D7=(r(ci_ub)), nformat(number_d2)
putexcel E7=(r(se)), nformat(number_d2)
putexcel F7=(r(N)), nformat(number_d2)
putexcel G7=(r(z)), nformat(number_d2)
putexcel H7=(r(p)), nformat(number_d2)
restore
*Using Sanctity*
preserve
replace diff=0.55707583 if original=="Graham et al. (2009)"
replace sed=0.24767027 if original=="Graham et al. (2009)"
meta set diff sed
meta summarize
putexcel B8=(r(theta)), nformat(number_d2)
putexcel C8=(r(ci_lb)), nformat(number_d2)
putexcel D8=(r(ci_ub)), nformat(number_d2)
putexcel E8=(r(se)), nformat(number_d2)
putexcel F8=(r(N)), nformat(number_d2)
putexcel G8=(r(z)), nformat(number_d2)
putexcel H8=(r(p)), nformat(number_d2)
restore
*Excluding Schwartz*
meta set diff sed
meta summarize if original!="Schwarz et al. (1991)"
putexcel B9=(r(theta)), nformat(number_d2)
putexcel C9=(r(ci_lb)), nformat(number_d2)
putexcel D9=(r(ci_ub)), nformat(number_d2)
putexcel E9=(r(se)), nformat(number_d2)
putexcel F9=(r(N)), nformat(number_d2)
putexcel G9=(r(z)), nformat(number_d2)
putexcel H9=(r(p)), nformat(number_d2)
*ML*
meta summarize if ml==1
putexcel B12=(r(theta)), nformat(number_d2)
putexcel C12=(r(ci_lb)), nformat(number_d2)
putexcel D12=(r(ci_ub)), nformat(number_d2)
putexcel E12=(r(se)), nformat(number_d2)
putexcel F12=(r(N)), nformat(number_d2)
putexcel G12=(r(z)), nformat(number_d2)
putexcel H12=(r(p)), nformat(number_d2)
*RRR*
meta summarize if ml==0
putexcel B13=(r(theta)), nformat(number_d2)
putexcel C13=(r(ci_lb)), nformat(number_d2)
putexcel D13=(r(ci_ub)), nformat(number_d2)
putexcel E13=(r(se)), nformat(number_d2)
putexcel F13=(r(N)), nformat(number_d2)
putexcel G13=(r(z)), nformat(number_d2)
putexcel H13=(r(p)), nformat(number_d2)
*sig 5%*
meta summarize if rsignif==1
putexcel B14=(r(theta)), nformat(number_d2)
putexcel C14=(r(ci_lb)), nformat(number_d2)
putexcel D14=(r(ci_ub)), nformat(number_d2)
putexcel E14=(r(se)), nformat(number_d2)
putexcel F14=(r(N)), nformat(number_d2)
putexcel G14=(r(z)), nformat(number_d2)
putexcel H14=(r(p)), nformat(number_d2)
*not sig 5%*
meta summarize if rsignif==0
putexcel B15=(r(theta)), nformat(number_d2)
putexcel C15=(r(ci_lb)), nformat(number_d2)
putexcel D15=(r(ci_ub)), nformat(number_d2)
putexcel E15=(r(se)), nformat(number_d2)
putexcel F15=(r(N)), nformat(number_d2)
putexcel G15=(r(z)), nformat(number_d2)
putexcel H15=(r(p)), nformat(number_d2)
*sig 0.5%*
meta summarize if rsignif05==1
putexcel B16=(r(theta)), nformat(number_d2)
putexcel C16=(r(ci_lb)), nformat(number_d2)
putexcel D16=(r(ci_ub)), nformat(number_d2)
putexcel E16=(r(se)), nformat(number_d2)
putexcel F16=(r(N)), nformat(number_d2)
putexcel G16=(r(z)), nformat(number_d2)
putexcel H16=(r(p)), nformat(number_d2)
*not sig 0.5%*
meta summarize if rsignif05==0
putexcel B17=(r(theta)), nformat(number_d2)
putexcel C17=(r(ci_lb)), nformat(number_d2)
putexcel D17=(r(ci_ub)), nformat(number_d2)
putexcel E17=(r(se)), nformat(number_d2)
putexcel F17=(r(N)), nformat(number_d2)
putexcel G17=(r(z)), nformat(number_d2)
putexcel H17=(r(p)), nformat(number_d2)

*Main*
sum diff
putexcel I2=(r(mean)), nformat(number_d2)
*Published studies*
sum diff if publi==1
putexcel I3=(r(mean)), nformat(number_d2)
*Excluding studies that included the replication*
sum diff if replication_inc==0
putexcel I4=(r(mean)), nformat(number_d2)
*Included original*
sum diff if originalinc==1
putexcel I5=(r(mean)), nformat(number_d2)
*Using Carter*
preserve
replace diff=0.39 if original=="Sripada et al. (2014)"
replace sed=0.072514423 if original=="Sripada et al. (2014)"
sum diff 
putexcel I6=(r(mean)), nformat(number_d2)
restore
*Using Loyalty*
preserve
replace diff=0.55707583 if original=="Graham et al. (2009)"
replace sed=0.057183299 if original=="Graham et al. (2009)"
sum diff 
putexcel I7=(r(mean)), nformat(number_d2)
restore
*Using Sanctity*
preserve
replace diff=0.55707583 if original=="Graham et al. (2009)"
replace sed=0.24767027 if original=="Graham et al. (2009)"
sum diff 
putexcel I8=(r(mean)), nformat(number_d2)
restore
*Excluding Schwartz*
sum diff if original!="Schwarz et al. (1991)"
putexcel I9=(r(mean)), nformat(number_d2)
*ML*
sum diff if ml==1
putexcel I12=(r(mean)), nformat(number_d2)
*RRR*
sum diff if ml==0
putexcel I13=(r(mean)), nformat(number_d2)
*sig 5%*
sum diff if rsignif==1
putexcel I14=(r(mean)), nformat(number_d2)
*not sig 5%*
sum diff if rsignif==0
putexcel I15=(r(mean)), nformat(number_d2)
*sig 0.5%*
sum diff if rsignif05==1
putexcel I16=(r(mean)), nformat(number_d2)
*not sig 0.5%*
sum diff if rsignif05==0
putexcel I17=(r(mean)), nformat(number_d2)

***Table 1***
putexcel set "Table 1.xls", modify
putexcel A1="Method", nformat(number_d2)
putexcel A2="Random Effects", nformat(number_d2)
putexcel A3="PET-PEESE", nformat(number_d2)
putexcel A4="3PSM", nformat(number_d2)
putexcel A5="Trim & Fill", nformat(number_d2)

***False Positive & Negative***

gen z_re=re/rese
gen z_petpeese=petpeese/petpeesese
gen z_threepsm=threepsm/threepsmse
gen z_tf=tf/tfse

*False positive*
gen fpos5_re=0 if rsignif==0 & z_re<1.96 & z_re!=. & original!="Schwarz et al. (1991)"
replace fpos5_re=1 if rsignif==0 & z_re>=1.96 & z_re!=. & original!="Schwarz et al. (1991)"
gen fpos5_petpeese=0 if rsignif==0 & z_petpeese<1.96 & z_petpeese!=. & original!="Schwarz et al. (1991)"
replace fpos5_petpeese=1 if rsignif==0 & z_petpeese>=1.96 & z_petpeese!=. & original!="Schwarz et al. (1991)"
gen fpos5_threepsm=0 if rsignif==0 & z_threepsm<1.96 & z_threepsm!=. & original!="Schwarz et al. (1991)"
replace fpos5_threepsm=1 if rsignif==0 & z_threepsm>=1.96 & z_threepsm!=. & original!="Schwarz et al. (1991)"
gen fpos5_tf=0 if rsignif==0 & z_tf<1.96 & z_tf!=. & original!="Schwarz et al. (1991)"
replace fpos5_tf=1 if rsignif==0 & z_tf>=1.96 & z_tf!=. & original!="Schwarz et al. (1991)"

gen fpos05_re=0 if rsignif05==0 & z_re<2.807 & z_re!=. & original!="Schwarz et al. (1991)"
replace fpos05_re=1 if rsignif05==0 & z_re>=2.807 & z_re!=. & original!="Schwarz et al. (1991)"
gen fpos05_petpeese=0 if rsignif05==0 & z_petpeese<2.807 & z_petpeese!=. & original!="Schwarz et al. (1991)"
replace fpos05_petpeese=1 if rsignif05==0 & z_petpeese>=2.807 & z_petpeese!=. & original!="Schwarz et al. (1991)"
gen fpos05_threepsm=0 if rsignif05==0 & z_threepsm<2.807 & z_threepsm!=. & original!="Schwarz et al. (1991)"
replace fpos05_threepsm=1 if rsignif05==0 & z_threepsm>=2.807 & z_threepsm!=. & original!="Schwarz et al. (1991)"
gen fpos05_tf=0 if rsignif05==0 & z_tf<2.807 & z_tf!=. & original!="Schwarz et al. (1991)"
replace fpos05_tf=1 if rsignif05==0 & z_tf>=2.807 & z_tf!=. & original!="Schwarz et al. (1991)"

*False negative*
gen fneg5_re=1 if rsignif==1 & z_re<1.96 & z_re!=. & original!="Schwarz et al. (1991)"
replace fneg5_re=0 if rsignif==1 & z_re>=1.96 & z_re!=. & original!="Schwarz et al. (1991)"
gen fneg5_petpeese=1 if rsignif==1 & z_petpeese<1.96 & z_petpeese!=. & original!="Schwarz et al. (1991)"
replace fneg5_petpeese=0 if rsignif==1 & z_petpeese>=1.96 & z_petpeese!=. & original!="Schwarz et al. (1991)"
gen fneg5_threepsm=1 if rsignif==1 & z_threepsm<1.96 & z_threepsm!=. & original!="Schwarz et al. (1991)"
replace fneg5_threepsm=0 if rsignif==1 & z_threepsm>=1.96 & z_threepsm!=. & original!="Schwarz et al. (1991)"
gen fneg5_tf=1 if rsignif==1 & z_tf<1.96 & z_tf!=. & original!="Schwarz et al. (1991)"
replace fneg5_tf=0 if rsignif==1 & z_tf>=1.96 & z_tf!=. & original!="Schwarz et al. (1991)"

gen fneg05_re=1 if rsignif05==1 & z_re<2.807 & z_re!=. & original!="Schwarz et al. (1991)"
replace fneg05_re=0 if rsignif05==1 & z_re>=2.807 & z_re!=. & original!="Schwarz et al. (1991)"
gen fneg05_petpeese=1 if rsignif05==1 & z_petpeese<2.807 & z_petpeese!=. & original!="Schwarz et al. (1991)"
replace fneg05_petpeese=0 if rsignif05==1 & z_petpeese>=2.807 & z_petpeese!=. & original!="Schwarz et al. (1991)"
gen fneg05_threepsm=1 if rsignif05==1 & z_threepsm<2.807 & z_threepsm!=. & original!="Schwarz et al. (1991)"
replace fneg05_threepsm=0 if rsignif05==1 & z_threepsm>=2.807 & z_threepsm!=. & original!="Schwarz et al. (1991)"
gen fneg05_tf=1 if rsignif05==1 & z_tf<2.807 & z_tf!=. & original!="Schwarz et al. (1991)"
replace fneg05_tf=0 if rsignif05==1 & z_tf>=2.807 & z_tf!=. & original!="Schwarz et al. (1991)"

putexcel B1="False Positive Rate 5%", nformat(number_d2)
putexcel C1="False Positive Rate 0.5%", nformat(number_d2)
putexcel D1="False Negative Rate 5%", nformat(number_d2)
putexcel E1="False Negative Rate 0.5%", nformat(number_d2)

sum fpos5_re
putexcel B2=(r(mean)), nformat(number_d2)
sum fpos05_re
putexcel C2=(r(mean)), nformat(number_d2)
sum fneg5_re
putexcel D2=(r(mean)), nformat(number_d2)
sum fneg05_re
putexcel E2=(r(mean)), nformat(number_d2)

sum fpos5_petpeese
putexcel B3=(r(mean)), nformat(number_d2)
sum fpos05_petpeese
putexcel C3=(r(mean)), nformat(number_d2)
sum fneg5_petpeese
putexcel D3=(r(mean)), nformat(number_d2)
sum fneg05_petpeese
putexcel E3=(r(mean)), nformat(number_d2)

sum fpos5_threepsm
putexcel B4=(r(mean)), nformat(number_d2)
sum fpos05_threepsm
putexcel C4=(r(mean)), nformat(number_d2)
sum fneg5_threepsm
putexcel D4=(r(mean)), nformat(number_d2)
sum fneg05_threepsm
putexcel E4=(r(mean)), nformat(number_d2)

sum fpos5_tf
putexcel B5=(r(mean)), nformat(number_d2)
sum fpos05_tf
putexcel C5=(r(mean)), nformat(number_d2)
sum fneg5_tf
putexcel D5=(r(mean)), nformat(number_d2)
sum fneg05_tf
putexcel E5=(r(mean)), nformat(number_d2)


*Mean Meta-Replication Difference; Random Effects*
putexcel F1="Mean Meta-Replication Difference; Random Effects", nformat(number_d2)
putexcel G1="Mean Meta-Replication Difference; Random Effects, Z-value", nformat(number_d2)
putexcel M1="Mean Meta-Replication Difference; Random Effects, P-value", nformat(number_d2)
putexcel N1="Mean Meta-Replication Difference; Random Effects, CI 5% lb", nformat(number_d2)
putexcel O1="Mean Meta-Replication Difference; Random Effects, CI 5% ub", nformat(number_d2)

meta set diff_re sed_re, random(dlaird)
meta summarize
putexcel F2=(r(theta)), nformat(number_d2)
putexcel G2=(r(z)), nformat(number_d2)
putexcel M2=(r(p)), nformat(number_d2)
putexcel N2=(r(ci_lb)), nformat(number_d2)
putexcel O2=(r(ci_ub)), nformat(number_d2)


meta set diff_petpeese sed_petpeese, random(dlaird)
meta summarize
putexcel F3=(r(theta)), nformat(number_d2)
putexcel G3=(r(z)), nformat(number_d2)
putexcel M3=(r(p)), nformat(number_d2)
putexcel N3=(r(ci_lb)), nformat(number_d2)
putexcel O3=(r(ci_ub)), nformat(number_d2)

meta set diff_threepsm sed_threepsm, random(dlaird)
meta summarize
putexcel F4=(r(theta)), nformat(number_d2)
putexcel G4=(r(z)), nformat(number_d2)
putexcel M4=(r(p)), nformat(number_d2)
putexcel N4=(r(ci_lb)), nformat(number_d2)
putexcel O4=(r(ci_ub)), nformat(number_d2)

meta set diff_tf sed_tf, random(dlaird)
meta summarize
putexcel F5=(r(theta)), nformat(number_d2)
putexcel G5=(r(z)), nformat(number_d2)
putexcel M5=(r(p)), nformat(number_d2)
putexcel N5=(r(ci_lb)), nformat(number_d2)
putexcel O5=(r(ci_ub)), nformat(number_d2)

*Mean Meta-Replication Difference; Unweighted*
putexcel H1="Mean Meta-Replication Difference; Unweighted", nformat(number_d2)
sum diff_re
putexcel H2=(r(mean)), nformat(number_d2)

sum diff_petpeese
putexcel H3=(r(mean)), nformat(number_d2)

sum diff_threepsm
putexcel H4=(r(mean)), nformat(number_d2)

sum diff_tf
putexcel H5=(r(mean)), nformat(number_d2)

*Overestimation factor*
putexcel I1="Overestimation factor", nformat(number_d2)
sum replication_s

gen of_re=re/0.1551067
sum of_re
putexcel I2=(r(mean)), nformat(number_d2)

gen of_petpeese=petpeese/0.1551067
sum of_petpeese
putexcel I3=(r(mean)), nformat(number_d2)

gen of_threepsm=threepsm/0.1551067
sum of_threepsm
putexcel I4=(r(mean)), nformat(number_d2)

gen of_tf=tf/0.1551067
sum of_tf
putexcel I5=(r(mean)), nformat(number_d2)

*Root Mean Squared Error*
putexcel J1="Root Mean Squared Error", nformat(number_d2)
*Random Effects*
gen se_re=diff_re^2
label var se_re "Squared Error Random Effects"
egen mse_re=mean(se_re)
gen rmse_re=sqrt(mse_re)
sum rmse_re
putexcel J2=(r(mean)), nformat(number_d2)

*PET-PEESE*
gen se_petpeese=diff_petpeese^2
label var se_petpeese "Squared Error PET-PEESE"
egen mse_petpeese=mean(se_petpeese)
gen rmse_petpeese=sqrt(mse_petpeese)
sum rmse_petpeese
putexcel J3=(r(mean)), nformat(number_d2)

*Trim and fill*
gen se_tf=diff_tf^2
label var se_tf "Squared Error Trim & Fill"
egen mse_tf=mean(se_tf)
gen rmse_tf=sqrt(mse_tf)
sum rmse_tf
putexcel J5=(r(mean)), nformat(number_d2)

*3PSM*
gen se_threepsm=diff_threepsm^2
label var se_threepsm "Squared Error 3PSM"
egen mse_threepsm=mean(se_threepsm)
gen rmse_threepsm=sqrt(mse_threepsm)
sum rmse_threepsm
putexcel J4=(r(mean)), nformat(number_d2)

*MDE*
putexcel K1="MDE 5%", nformat(number_d2)
putexcel L1="MDE 0.5%", nformat(number_d2)

gen mde5_re=rese*2.8
gen mde05_re=rese*3.647

gen mde5_petpeese=petpeesese*2.8
gen mde05_petpeese=petpeesese*3.647

gen mde5_tf=tfse*2.8
gen mde05_tf=tfse*3.647

gen mde5_threepsm=threepsmse*2.8
gen mde05_threepsm=threepsmse*3.647

sum mde5_re
putexcel K2=(r(mean)), nformat(number_d2)
sum mde05_re
putexcel L2=(r(mean)), nformat(number_d2)

sum mde5_petpeese
putexcel K3=(r(mean)), nformat(number_d2)
sum mde05_petpeese
putexcel L3=(r(mean)), nformat(number_d2)

sum mde5_threepsm
putexcel K4=(r(mean)), nformat(number_d2)
sum mde05_threepsm
putexcel L4=(r(mean)), nformat(number_d2)

sum mde5_tf
putexcel K5=(r(mean)), nformat(number_d2)
sum mde05_tf
putexcel L5=(r(mean)), nformat(number_d2)

*Regresion testing for difference between studies that replicated and those who did not*
meta set diff sed
meta regress rsignif
eststo reg5
meta regress rsignif05
eststo reg05
esttab reg5 reg05 using"regresjon.rtf", se nostar replace
esttab reg5 reg05 using"regresjon.rtf", p(3) nostar bracket append

***Table 9-12***
***Random Effects***
gen pv_re=2*(1-normal(abs(z_re)))
gen z5_re=1.96*rese
gen cil5_re=re-z5_re
gen ciu5_re=re+z5_re

gen z05_re=2.807*rese
gen cil05_re=re-z05_re
gen ciu05_re=re+z05_re

label var id "ID"
label var original "Original study"
label var re "Estimate"
label var rese "Standard Error"
label var z_re "Z-value"
label var pv_re "p-value"
label var cil5_re "CI - lower bound 5%"
label var ciu5_re "CI - upper bound 5%"
label var cil05_re "CI - lower bound 0.5%"
label var ciu05_re "CI - upper bound 0.5%"
label var mde5_re "MDE 5%"
label var mde05_re "MDE 0.5%"



export excel id original re rese z_re pv_re cil5_re ciu5_re cil05_re ciu05_re mde5_re mde05_re using "Table SI 9.xls", firstrow(varlabels) replace

putexcel set "Table SI 9.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)


***PET-PEESE***
gen pv_petpeese=2*(1-normal(abs(z_petpeese)))
gen z5_petpeese=1.96*petpeesese
gen cil5_petpeese=petpeese-z5_petpeese
gen ciu5_petpeese=petpeese+z5_petpeese

gen z05_petpeese=2.807*petpeesese
gen cil05_petpeese=petpeese-z05_petpeese
gen ciu05_petpeese=petpeese+z05_petpeese

label var id "ID"
label var original "Original study"
label var petpeese "Estimate"
label var petpeesese "Standard Error"
label var z_petpeese "Z-value"
label var pv_petpeese "p-value"
label var cil5_petpeese "CI - lower bound 5%"
label var ciu5_petpeese "CI - upper bound 5%"
label var cil05_petpeese "CI - lower bound 0.5%"
label var ciu05_petpeese "CI - upper bound 0.5%"
label var mde5_petpeese "MDE 5%"
label var mde05_petpeese "MDE 0.5%"



export excel id original petpeese petpeesese z_petpeese pv_petpeese cil5_petpeese ciu5_petpeese cil05_petpeese ciu05_petpeese mde5_petpeese mde05_petpeese using "Table SI 10.xls", firstrow(varlabels) replace

putexcel set "Table SI 10.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)


***3PSM***
gen pv_threepsm=2*(1-normal(abs(z_threepsm)))
gen z5_threepsm=1.96*threepsmse
gen cil5_threepsm=threepsm-z5_threepsm
gen ciu5_threepsm=threepsm+z5_threepsm

gen z05_threepsm=2.807*threepsmse
gen cil05_threepsm=threepsm-z05_threepsm
gen ciu05_threepsm=threepsm+z05_threepsm

label var id "ID"
label var original "Original study"
label var threepsm "Estimate"
label var threepsmse "Standard Error"
label var z_threepsm "Z-value"
label var pv_threepsm "p-value"
label var cil5_threepsm "CI - lower bound 5%"
label var ciu5_threepsm "CI - upper bound 5%"
label var cil05_threepsm "CI - lower bound 0.5%"
label var ciu05_threepsm "CI - upper bound 0.5%"
label var mde5_threepsm "MDE 5%"
label var mde05_threepsm "MDE 0.5%"



export excel id original threepsm threepsmse z_threepsm pv_threepsm cil5_threepsm ciu5_threepsm cil05_threepsm ciu05_threepsm mde5_threepsm mde05_threepsm using "Table SI 11.xls", firstrow(varlabels) replace

putexcel set "Table SI 11.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)

***Trim & Fill***
gen pv_tf=2*(1-normal(abs(z_tf)))
gen z5_tf=1.96*tfse
gen cil5_tf=tf-z5_tf
gen ciu5_tf=tf+z5_tf

gen z05_tf=2.807*tfse
gen cil05_tf=threepsm-z05_tf
gen ciu05_tf=threepsm+z05_tf


label var id "ID"
label var original "Original study"
label var tf "Estimate"
label var tfse "Standard Error"
label var z_tf "Z-value"
label var pv_tf "p-value"
label var cil5_tf "CI - lower bound 5%"
label var ciu5_tf "CI - upper bound 5%"
label var cil05_tf "CI - lower bound 0.5%"
label var ciu05_tf "CI - upper bound 0.5%"
label var mde5_tf "MDE 5%"
label var mde05_tf "MDE 0.5%"



export excel id original tf tfse z_tf pv_tf cil5_tf ciu5_tf cil05_tf ciu05_tf mde5_tf mde05_tf using "Table SI 12.xls", firstrow(varlabels) replace

putexcel set "Table SI 12.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)



***Table 13-16***
***Random Effects***
gen z_re_diff=diff_re/sed_re

gen pv_re_diff=2*(1-normal(abs(z_re_diff)))

gen z5_re_diff=1.96*sed_re
gen cil5_re_diff=diff_re-z5_re_diff
gen ciu5_re_diff=diff_re+z5_re_diff

gen z05_re_diff=2.807*sed_re
gen cil05_re_diff=diff_re-z5_re_diff
gen ciu05_re_diff=diff_re+z5_re_diff

gen mde5_re_diff=sed_re*2.8
gen mde05_re_diff=sed_re*3.65

label var id "ID"
label var original "Original study"
label var diff_re "Difference"
label var sed_re "Standard Error"
label var z_re_diff "Z-value"
label var pv_re_diff "p-value"
label var cil5_re_diff "CI - lower bound 5%"
label var ciu5_re_diff "CI - upper bound 5%"
label var cil05_re_diff "CI - lower bound 0.5%"
label var ciu05_re_diff "CI - upper bound 0.5%"
label var mde5_re_diff "MDE 5%"
label var mde05_re_diff "MDE 0.5%"



export excel id original diff_re sed_re z_re_diff pv_re_diff cil5_re_diff ciu5_re_diff cil05_re_diff ciu05_re_diff mde5_re_diff mde05_re_diff using "Table SI 13.xls", firstrow(varlabels) replace

putexcel set "Table SI 13.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)


***PET-PEESE***
gen z_petpeese_diff=diff_petpeese/sed_petpeese

gen pv_petpeese_diff=2*(1-normal(abs(z_petpeese_diff)))

gen z5_petpeese_diff=1.96*sed_petpeese
gen cil5_petpeese_diff=diff_petpeese-z5_petpeese_diff
gen ciu5_petpeese_diff=diff_petpeese+z5_petpeese_diff

gen z05_petpeese_diff=2.807*sed_petpeese
gen cil05_petpeese_diff=diff_petpeese-z5_petpeese_diff
gen ciu05_petpeese_diff=diff_petpeese+z5_petpeese_diff

gen mde5_petpeese_diff=sed_petpeese*2.8
gen mde05_petpeese_diff=sed_petpeese*3.65

label var id "ID"
label var original "Original study"
label var diff_petpeese "Difference"
label var sed_petpeese "Standard Error"
label var z_petpeese_diff "Z-value"
label var pv_petpeese_diff "p-value"
label var cil5_petpeese_diff "CI - lower bound 5%"
label var ciu5_petpeese_diff "CI - upper bound 5%"
label var cil05_petpeese_diff "CI - lower bound 0.5%"
label var ciu05_petpeese_diff "CI - upper bound 0.5%"
label var mde5_petpeese_diff "MDE 5%"
label var mde05_petpeese_diff "MDE 0.5%"



export excel id original diff_petpeese sed_petpeese z_petpeese_diff pv_petpeese_diff cil5_petpeese_diff ciu5_petpeese_diff cil05_petpeese_diff ciu05_petpeese_diff mde5_petpeese_diff mde05_petpeese_diff using "Table SI 14.xls", firstrow(varlabels) replace

putexcel set "Table SI 14.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)


***3PSM***
gen z_threepsm_diff=diff_threepsm/sed_threepsm

gen pv_threepsm_diff=2*(1-normal(abs(z_threepsm_diff)))

gen z5_threepsm_diff=1.96*sed_threepsm
gen cil5_threepsm_diff=diff_threepsm-z5_threepsm_diff
gen ciu5_threepsm_diff=diff_threepsm+z5_threepsm_diff

gen z05_threepsm_diff=2.807*sed_threepsm
gen cil05_threepsm_diff=diff_threepsm-z5_threepsm_diff
gen ciu05_threepsm_diff=diff_threepsm+z5_threepsm_diff

gen mde5_threepsm_diff=sed_threepsm*2.8
gen mde05_threepsm_diff=sed_threepsm*3.65

label var id "ID"
label var original "Original study"
label var diff_threepsm "Difference"
label var sed_threepsm "Standard Error"
label var z_threepsm_diff "Z-value"
label var pv_threepsm_diff "p-value"
label var cil5_threepsm_diff "CI - lower bound 5%"
label var ciu5_threepsm_diff "CI - upper bound 5%"
label var cil05_threepsm_diff "CI - lower bound 0.5%"
label var ciu05_threepsm_diff "CI - upper bound 0.5%"
label var mde5_threepsm_diff "MDE 5%"
label var mde05_threepsm_diff "MDE 0.5%"



export excel id original diff_threepsm sed_threepsm z_threepsm_diff pv_threepsm_diff cil5_threepsm_diff ciu5_threepsm_diff cil05_threepsm_diff ciu05_threepsm_diff mde5_threepsm_diff mde05_threepsm_diff using "Table SI 15.xls", firstrow(varlabels) replace

putexcel set "Table SI 15.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)

***Trim & Fill***
gen z_tf_diff=diff_tf/sed_tf

gen pv_tf_diff=2*(1-normal(abs(z_tf_diff)))

gen z5_tf_diff=1.96*sed_tf
gen cil5_tf_diff=diff_tf-z5_tf_diff
gen ciu5_tf_diff=diff_tf+z5_tf_diff

gen z05_tf_diff=2.807*sed_tf
gen cil05_tf_diff=diff_tf-z5_tf_diff
gen ciu05_tf_diff=diff_tf+z5_tf_diff

gen mde5_tf_diff=sed_tf*2.8
gen mde05_tf_diff=sed_tf*3.65

label var id "ID"
label var original "Original study"
label var diff_tf "Difference"
label var sed_tf "Standard Error"
label var z_tf_diff "Z-value"
label var pv_tf_diff "p-value"
label var cil5_tf_diff "CI - lower bound 5%"
label var ciu5_tf_diff "CI - upper bound 5%"
label var cil05_tf_diff "CI - lower bound 0.5%"
label var ciu05_tf_diff "CI - upper bound 0.5%"
label var mde5_tf_diff "MDE 5%"
label var mde05_tf_diff "MDE 0.5%"



export excel id original diff_tf sed_tf z_tf_diff pv_tf_diff cil5_tf_diff ciu5_tf_diff cil05_tf_diff ciu05_tf_diff mde5_tf_diff mde05_tf_diff using "Table SI 16.xls", firstrow(varlabels) replace

putexcel set "Table SI 16.xls", modify

putexcel B2="Oppenheimer et al. (2009)20", nformat(number_d2)
putexcel B3="Tversky & Kahneman (1981)39 ", nformat(number_d2)
putexcel B4="Husnu & Crisp (2010)8 ", nformat(number_d2)
putexcel B5="Schwarz et al. (1991)28", nformat(number_d2)
putexcel B6="Hauser et al. (2007)6 ", nformat(number_d2)
putexcel B7="Critcher & Gilovich (2008)1 ", nformat(number_d2)
putexcel B8="Graham, Haidt & Nosek (2009)4", nformat(number_d2)
putexcel B9="Jostmann et al. (2009)11 ", nformat(number_d2)
putexcel B10="Monin & Miller (2001)18", nformat(number_d2)
putexcel B11="Schooler & Engstler-Schooler (1990)25", nformat(number_d2)
putexcel B12="Sripada et al. (2014)30 ", nformat(number_d2)
putexcel B13="Rand et al. (2012)22", nformat(number_d2)
putexcel B14="Strack et al. (1988)36", nformat(number_d2)
putexcel B15="Srull & Wyer (1979)34 ", nformat(number_d2)
putexcel B16="Mazar et al. (2008)14", nformat(number_d2)



****Figures*****
*Figure 4, comparing Meta and Original*

replace original="Overall" in 18

meta set diffo sediffo, random(dlaird)
meta summarize
replace diffo=0.09 in 18
replace sediffo=0.09/1.2 in 18

mkmat diffo if effecto!=3.01, matrix(diffeo) nomissing rownames(original)
mkmat sediffo if effecto!=3.01, matrix(diffeose) nomissing rownames(original)

matrix Differenceo=diffeo'
matrix Differenceose=diffeose'



coefplot (matrix(Differenceo), se(Differenceose) xline(0) l(99.5 95) ciopts(recast(rcap rcap) lcol(gray gs4)) msymbol(s) mcolor(gs4) xscale(range(-0.5,1)) xlabel(-1(0.25)1.5, nogrid)), subtitle("Difference (Original study - Meta-analysis)", size(medium) margin(medium) justification(center) color(black) bcolor(white) bmargin(top_bottom)) graphregion(margin(zero)) name(fig4) plotregion(margin(small)) fxsize(95) byopts(compact cols(1))
graph display fig4, scale(0.61) ysize(4.72440945) xsize(3.46456693) 

graph save "fig4" "fig4.gph", replace


*Figure 2, panel A*
mkmat meta_s, matrix(metaa) nomissing rownames(original) 
mkmat sem, matrix(standardm) nomissing rownames(original)

matrix Estimates=metaa'
matrix standarderrorm=standardm'

mkmat replication_s, matrix(repl) nomissing rownames(original)
mkmat ser, matrix(standardr) nomissing rownames(original)

matrix replicatione=repl'
matrix standarderrorr=standardr'

*Figure 2, panel B*

mkmat diff, matrix(diffepun) nomissing rownames(original)
mkmat sed, matrix(diffepunse) nomissing rownames(original)

matrix Differencepun=diffepun'
matrix Differencepunse=diffepunse'

*Figure 2*


coefplot (matrix(Estimates), se(standarderrorm) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol(gray gs4)) msymbol(s) mcolor(gs4) label(Meta-Analysis) xlabel(-0.5(0.25)1.25, nogrid)) ///
(matrix(replicatione), se(standarderrorr) pstyle(p4) l(99.5 95) ciopts(recast(rcap rcap) lcol(gray gs4)) label(Replication) msymbol(s) mfcolor(white) mcolor(gs4) xlabel(, nogrid)), xline(0) ///
subtitle("Estimates", size(medium) margin(medium) justification(center) color(black) bcolor(white) bmargin(top_bottom)) graphregion(margin(zero)) plotregion(margin(small)) legend(rows(2) position(left)) ///
 || (matrix(Differencepun), se(Differencepunse) xline(0) l(99.5 95) ciopts(recast(rcap rcap) lcol(gray gs4)) msymbol(s) mcolor(gs4) xscale(range(-0.5,1)) ///
xlabel(-0.5(0.25)1.25, nogrid)), subtitle("Difference", size(medium) margin(medium) justification(center) color(black) bcolor(white) bmargin(top_bottom)) name(fig2) graphregion(margin(zero)) ///
plotregion(margin(small)) fxsize(95) byopts(compact cols(1))

graph display fig2, scale(0.3) ysize(8.26771654) xsize(7.08661417)

graph save "fig2" "fig2.gph", replace


*Figure 5*
graph drop _all

mkmat re, matrix(metaar) nomissing rownames(original) 
mkmat rese, matrix(standardmr) nomissing rownames(original)

matrix Estimatesr=metaar'
matrix standarderrormr=standardmr'

mkmat tf, matrix(metaat) nomissing rownames(original) 
mkmat tfse, matrix(standardmt) nomissing rownames(original)

matrix Estimatest=metaat'
matrix standarderrormt=standardmt'

mkmat petpeese, matrix(metaap) nomissing rownames(original) 
mkmat petpeesese, matrix(standardmp) nomissing rownames(original)

matrix Estimatesp=metaap'
matrix standarderrormp=standardmp'

mkmat threepsm, matrix(metaath) nomissing rownames(original) 
mkmat threepsmse, matrix(standardmth) nomissing rownames(original)

matrix Estimatesth=metaath'
matrix standarderrormth=standardmth'

coefplot (matrix(replicatione), se(standarderrorr) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol(gray gs4)) msymbol(s) mfcolor(white) mcolor(gs4) label(Replication) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(Estimatesr), se(standarderrormr) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("215 64 13" "215 64 13")) msymbol(d) mcolor("215 64 13") label(Random Effects) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(Estimatesp), se(standarderrormp) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("247 177 56" "247 177 56")) msymbol(t) mcolor("247 177 56") label(PET-PEESE) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(Estimatesth), se(standarderrormth) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("178 171 210" "178 171 210")) msymbol(r) mcolor("178 171 210") label(3PSM) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(Estimatest), se(standarderrormt) pstyle(p4) l(99.5 95) ciopts(recast(rcap rcap) lcol("94 60 153" "94 60 153")) label(Trim and Fill) msymbol(s) mcolor("94 60 153") xlabel(, nogrid)), xline(0) ///
subtitle("Estimates", size(medium) margin(medium) justification(center) color(black) bcolor(white) bmargin(top_bottom)) graphregion(margin(zero)) plotregion(margin(small)) name(fig5) fxsize(95) legend(rows(2) position(6)) ///

graph display fig5, scale(0.25) ysize(7.87401575) xsize(7.08661417) 

graph save "fig5" "fig5.gph", replace

*Figure 6*
graph drop _all

*Analysis figure 4*
replace original="Overall" in 18

meta set diff_re sed_re, random(dlaird)
meta summarize
replace diff_re=0.265 in 18
replace sed_re=0.265/5.68 in 18

meta set diff_tf sed_tf, random(dlaird)
meta summarize
replace diff_tf=0.228 in 18
replace sed_tf=0.228/5.05 in 18

meta set diff_petpeese sed_petpeese, random(dlaird)
meta summarize
replace diff_petpeese=0.039 in 18
replace sed_petpeese=0.039/0.59 in 18

meta set diff_threepsm sed_threepsm, random(dlaird)
meta summarize
replace diff_threepsm=0.234 in 18
replace sed_threepsm=0.234/5.49 in 18

mkmat diff_re, matrix(diffre) nomissing rownames(original) 
mkmat sed_re, matrix(diffsere) nomissing rownames(original)

matrix REdiff=diffre'
matrix REdiffSE=diffsere'

mkmat diff_tf, matrix(difftf) nomissing rownames(original) 
mkmat sed_tf, matrix(diffsetf) nomissing rownames(original)

matrix TFdiff=difftf'
matrix TFdiffSE=diffsetf'

mkmat diff_petpeese, matrix(diffpp) nomissing rownames(original) 
mkmat sed_petpeese, matrix(diffsepp) nomissing rownames(original)

matrix PPdiff=diffpp'
matrix PPdiffSE=diffsepp'

mkmat diff_threepsm, matrix(diff3PSM) nomissing rownames(original) 
mkmat sed_threepsm, matrix(diffse3PSM) nomissing rownames(original)

matrix PSMdiff=diff3PSM'
matrix PSMdiffSE=diffse3PSM'

coefplot (matrix(REdiff), se(REdiffSE) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("215 64 13" "215 64 13")) msymbol(d) mcolor("215 64 13") label(Random Effects) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(PPdiff), se(PPdiffSE) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("247 177 56" "247 177 56")) msymbol(t) mcolor("247 177 56") label(PET-PEESE) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(PSMdiff), se(PSMdiffSE) pstyle(p3)l(99.5 95) ciopts(recast(rcap rcap) lcol("178 171 210" "178 171 210")) msymbol(r) mcolor("178 171 210") label(3PSM) xlabel(-1.25(0.25)1.5, nogrid)) ///
(matrix(TFdiff), se(TFdiffSE) pstyle(p4) l(99.5 95) ciopts(recast(rcap rcap) lcol("94 60 153" "94 60 153")) label(Trim and Fill) msymbol(s) mcolor("94 60 153") xlabel(, nogrid)), xline(0) ///
subtitle("Differences", size(medium) margin(medium) justification(center) color(black) bcolor(white) bmargin(top_bottom)) graphregion(margin(zero)) plotregion(margin(small)) name(fig6) fxsize(95) legend(rows(2) position(6)) ///


graph display fig6, scale(0.25) ysize(7.87401575) xsize(7.08661417) 

graph save "fig6" "fig6.gph", replace

*Figure 3*
graph drop _all
import excel "Table 6.xls", sheet("Sheet1") firstrow case(lower) clear


mkmat diff if se!=0, matrix(Effectd) nomissing rownames(test) 
mkmat se if se!=0, matrix(SEd) nomissing rownames(test)

matrix Effect=Effectd'
matrix SE=SEd'

matrix list Effect

coefplot (matrix(Effect), se(SE) xline(0) l(99.5 95) ciopts(recast(rcap rcap)) xscale(range(-0.1,0.65)) xlabel(-0.1(0.05)0.65) lcol(gray gs4) msymbol(s) mcolor(gs4) name(fig3))

graph display fig3, scale(0.61)  ysize(4.72440945) xsize(3.46456693) 

graph save "fig3" "fig3.gph", replace

***Create SI table 6***
import excel "Table 6.xls", sheet("Sheet1") firstrow case(lower) clear
gen mde5=se*2.8
gen mde05=se*3.647

label var mde5 "MDE 5%"
label var mde05 "MDE 0.5%"

gen z_se05=se*2.807
gen ci_l05=difference-z_se05
gen ci_u05=difference+z_se05

label var ci_l05 "CI - L 0.5%"
label var ci_u05 "CI - U 0.5%"

export excel using "Table 6.xls", firstrow(varlabels) replace
