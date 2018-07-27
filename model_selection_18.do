** Diego F. Leal, Anthony Paik, Steven Boutcher
** Data processing do-file
** Status and Conformity Revisited: Corporate Law Firms and Isomorphism in Pro Bono Ties.
** Code written by Diego F. Leal, UMass-Amherst (www.diegoleal.info)
** Last revision: 07/26/2018

clear
pwd

capture log close
log using "C:\Users\diego\Dropbox\DLeal\USC\papers\Probono\probono_2018\probono_output_data_files\model_selection_logfile.smcl", replace

cd "C:\Users\diego\Dropbox\DLeal\USC\papers\Probono\probono_2018\probono_output_data_files"
set more off
version 13
use finalData.dta

*Jaccard Index 0-100
sum jaccardIndex
gen jaccard100 = jaccardIndex * 100
sum jaccard100

order jaccard100, after(dyad)

*isolates
gen isolate_1 = 0
replace isolate_1 = 1 if degree_1 == 0

gen isolate_2 =0
replace isolate_2 = 1 if degree_2 == 0

gen hasIsolate = 0
replace hasIsolate = 1 if isolate_1 == 1 | isolate_2 == 1

*encoding string variables
encode location_1, gen(city1)
encode location_2, gen(city2)

order location_1 location_2, after(firm_2)

*generating a variable to register whether or not firms are in different cities
egen differentCity =diff(location_1 location_2)

*ordering the variables
order city1 city2, after(location_2)

*generating a variable to register whether or not firms are in the same city, NYC gets a special code
gen sameCity = .
order sameCity, after(city2)
replace sameCity = 1 if differentCity == 0
replace sameCity = 0 if differentCity == 1
replace sameCity = 2 if nyc_2 == 1 & nyc_1 ==1

*checking the variable revenue
sum gross_revenue_1 gross_revenue_2
recode gross_revenue_1 gross_revenue_2 (0=.)
sum gross_revenue_1 gross_revenue_2

*revenue difference 
gen rev_diff=abs(gross_revenue_1-gross_revenue_2)
sum rev_diff

order rev_diff, after(gross_revenue_2)

*log revenue difference
gen logrev_diff=ln(rev_diff + 1) // + 1 bc ln(0) undefined
sum rev_diff logrev_diff


*squared revenue difference
gen rev_diffsq=rev_diff*rev_diff
sum rev_diff rev_diffsq

*profits and profit_diff
gen profit_1=ppp_1*num_eq_partners_1
gen profit_2=ppp_2*num_eq_partners_2
gen profit_diff=abs(profit_1-profit_2)
sum profit_1 profit_2 profit_diff
gen profit_diffsq = profit_diff^2

order profit_1 profit_2 profit_diff, after(rev_diff) 

*ranking and ranking squared
gen rank_diff=abs(amlaw200_rank_1-amlaw200_rank_2)
sum rank_diff
gen rank_diffsq=rank_diff^2
sum rank_diffsq

*coordinator (0=neither have; 1=one firm has; 2=both have)
gen coord=.
destring coordinator1yes_2, replace
destring coordinator1yes_1, replace
replace coord = 0 if coordinator1yes_1 == 0 | coordinator1yes_2 == 0
replace coord=1 if coordinator1yes_1==1 | coordinator1yes_2==1
replace coord=2 if coordinator1yes_1==1 & coordinator1yes_2==1
replace coord=. if coordinator1yes_1==. | coordinator1yes_2==.
tab coord,m

order coordinator1yes_1 coordinator1yes_2 coord, after(profit_diff)

*gender and racial minority
gen gender_diff=abs(percentfemale_1-percentfemale_2)
gen race_diff=abs(percentminority_1-percentminority_2)
sum gender_diff race_diff

order gender_diff, after(percentfemale_2)
order race_diff, after(percentminority_2)

*size (number of attorneys)
gen size_diff=abs(num_attorneys_1-num_attorneys_2)
sum size_diff
gen size_diffsq = size_diff^2
sum size_diffsq

order size_diff, after (num_attorneys_2)

*pro Bono hours (average hours per lawyer and total hours)
gen pbtotal_diff=abs(total_hours_1-total_hours_2)
gen pbavg_diff=abs(avg_hrs_per_lawyer_1-avg_hrs_per_lawyer_2)
sum pbtotal_diff

order pbtotal_diff, after(total_hours_2) 

*squared revenue difference
gen pbavg_diffsq=pbavg_diff*pbavg_diff
sum pbavg_diff pbavg_diffsq

*age difference 
gen age_diff=abs(age_1-age_2)
sum age_diff
gen age_diffsq = age_diff^2
sum age_diffsq

order age_diff,after(age_2)

*difference in # of practice areas 
gen practiceArea_diff=abs(practiceAreas_1 - practiceAreas_2)
sum practiceArea_diff
gen practiceArea_diffsq = practiceArea_diff^2
sum practiceArea_diffsq

order practiceArea_diff,after(practiceAreas_2)

********************************************************************************************************************************************
********************************************************
* TESTING THE BEST CUT-OFF POINTS BASED ON VAULT DATA  *
********************************************************


********************************************
********************************************
*** Vault 1-25 / 26-50 / 51-75 76/100 ******
********************************************
********************************************

*vault
gen voult_diff=abs(vault2005_1 - vault2005_2)
sum voult_diff
gen voult_diffsq=voult_diff^2
sum voult_diffsq

*Vault catogorical rank
gen catrankVoult_1=vault2005_1
recode catrankVoult_1 0=. 1/25=1 26/75=2 76/100=3
tab catrankVoult_1

gen catrankVoult_2=vault2005_2
recode catrankVoult_2 0=. 1/25=1 26/75=2 76/100=3
tab catrankVoult_2

list catrankVoult_1 vault2005_1

gen difVoult=abs(catrankVoult_1-catrankVoult_2)
tab difVoult

gen newrankVoult=0 if catrankVoult_1==1 & catrankVoult_2==1
replace newrankVoult=1 if catrankVoult_1==2 & catrankVoult_2==2
replace newrankVoult=2 if catrankVoult_1==3 & catrankVoult_2==3
replace newrankVoult=3 if difVoult ==1
replace newrankVoult=4 if difVoult ==2 

tab newrankVoult

reg jaccardIndex i.newrankVoult 
predict student, rstudent
eststo m1
fitstat, saving (m1)   // suport for m1

**************************************
**************************************
*** Vault 1-10 / 11-90 / 91-100 ******
**************************************
**************************************

*Vault catogorical rank
gen catrankVoult10_1=vault2005_1
recode catrankVoult10_1 0=. 1/10=1 11/90=2 91/100=3  
tab catrankVoult10_1

gen catrankVoult10_2=vault2005_2
recode catrankVoult10_2 0=. 1/10=1 11/90=2 91/100=3  
tab catrankVoult10_2

gen difVoult10=abs(catrankVoult10_1-catrankVoult10_2)
tab difVoult10

gen newrankVoult10=0 if catrankVoult10_1==1 & catrankVoult10_2==1
replace newrankVoult10=1 if catrankVoult10_1==2 & catrankVoult10_2==2
replace newrankVoult10=2 if catrankVoult10_1==3 & catrankVoult10_2==3
replace newrankVoult10=3 if difVoult10 ==1 
replace newrankVoult10=4 if difVoult10 ==2 

tab newrankVoult10

reg jaccardIndex i.newrankVoult10
predict student10, rstudent
eststo m2
fitstat, saving (m2) using (m1) //support for m1


**************************************
**************************************
*** Vault 1-40 / 41-60 / 61-100 *****
**************************************
**************************************

*Vault catogorical rank
gen catrankVoult40_1=vault2005_1
recode catrankVoult40_1 0=. 1/40=1 41/60=2 61/100=3  
tab catrankVoult40_1

gen catrankVoult40_2=vault2005_2
recode catrankVoult40_2 0=. 1/40=1 41/60=2 61/100=3
tab catrankVoult40_2
gen difVoult40=abs(catrankVoult40_1-catrankVoult40_2)
tab difVoult40

gen newrankVoult40=0 if catrankVoult40_1==1 & catrankVoult40_2==1
replace newrankVoult40=1 if catrankVoult40_1==2 & catrankVoult40_2==2
replace newrankVoult40=2 if catrankVoult40_1==3 & catrankVoult40_2==3
replace newrankVoult40=3 if difVoult40 ==1 
replace newrankVoult40=4 if difVoult40 ==2 
tab newrankVoult40

reg jaccardIndex i.newrankVoult40
predict student40, rstudent
eststo m3
fitstat, saving (m3) using (m1) //support for m1


**************************************
**************************************
*** Vault 1-20 / 21-80 / 81-100 ******
**************************************
**************************************

*Vault catogorical rank
gen catrankVoult20_1=vault2005_1
recode catrankVoult20_1 0=. 1/20=1 21/80=2 81/100=3  
tab catrankVoult20_1

gen catrankVoult20_2=vault2005_2
recode catrankVoult20_2 0=. 1/20=1 21/80=2 81/100=3  
tab catrankVoult20_2

gen difVoult20=abs(catrankVoult20_1-catrankVoult20_2)
tab difVoult20

gen newrankVoult20=0 if catrankVoult20_1==1 & catrankVoult20_2==1
replace newrankVoult20=1 if catrankVoult20_1==2 & catrankVoult20_2==2
replace newrankVoult20=2 if catrankVoult20_1==3 & catrankVoult20_2==3
replace newrankVoult20=3 if difVoult20 ==1 
replace newrankVoult20=4 if difVoult20 ==2 

tab newrankVoult20

reg jaccardIndex i.newrankVoult20
predict student20, rstudent
eststo m4
fitstat, saving (m4) using (m1) //support for m1

**************************************
**************************************
*** Vault 1-30 / 31-75 / 76-100 ******
**************************************
**************************************

*Vault catogorical rank
gen catrankVoult30_1=vault2005_1
recode catrankVoult30_1 0=. 1/30=1 31/70=2 71/100=3  
tab catrankVoult30_1

gen catrankVoult30_2=vault2005_2
recode catrankVoult30_2 0=. 1/30=1 31/70=2 71/100=3 
tab catrankVoult30_2

gen difVoult30=abs(catrankVoult30_1-catrankVoult30_2)
tab difVoult30

gen newrankVoult30=0 if catrankVoult30_1==1 & catrankVoult30_2==1
replace newrankVoult30=1 if catrankVoult30_1==2 & catrankVoult30_2==2
replace newrankVoult30=2 if catrankVoult30_1==3 & catrankVoult30_2==3
replace newrankVoult30=3 if difVoult30 ==1 
replace newrankVoult30=4 if difVoult30 ==2 

tab newrankVoult30

reg jaccardIndex i.newrankVoult30
predict student30, rstudent
eststo m5
fitstat, saving (m5) using (m1) //support for m1


*******************************************************************************
*** Modeling isomorphism using the ranking with best fit: 1-25/26-75/76-100
********************************************************************************

*reordering the data


rename newrankVoult  vault

*clear fit stats
eststo clear

reg jaccard100 i.vault
test 0.vault = 1.vault
test 1.vault = 2.vault
test 2.vault = 3.vault // no difference
test 3.vault = 4.vault
fitstat, saving (m1)

reg jaccard100 i.vault pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff i.sameCity
test 0.vault = 1.vault
test 1.vault = 2.vault
test 2.vault = 3.vault // no difference
test 3.vault = 4.vault
fitstat, saving (m2)


****** FINAL MODELS****
eststo clear
*including practice area and practice area sq
set more off
eststo: reg jaccard100 i.vault if vault != . & pbavg_diff != . & logrev_diff != . & size_diff != . & age_diff != . & gender_diff != . & race_diff != . & practiceArea_diff != . & sameCity != .
fitstat, saving (m1)
eststo: reg jaccard100 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff i.sameCity if vault != .
fitstat, saving (m2)
eststo: reg jaccard100 i.vault pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff i.sameCity
fitstat, saving (m3) using (m2)
esttab, r2
eststo clear

*******************************
*sensitivity analyses
*******************************



**************************************************
****************************
* to see best evidence on the 137 firms with AM LAw Rev data run sensitivityAmLaw.do
**************************************************
***************************************************

*generate binary version of Vault
tab vault, gen(vaultVar)
tab sameCity, gen(sameCityDum)
*4) confrim results based on inv log coefficient
reg inveLogIndex vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff i.sameCity

keep inveLogIndex firm_1 firm_2 dyad jaccard100 vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3

*recode missing values as 9999
foreach var of varlist jaccard100 vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3{
recode `var' (. = 9999)
}

*recode missing values in the DV
reg jaccard100 vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3 if jaccard100 != 9999

drop if jaccard100 == 9999 // no missing values

*diferent models
reg jaccard100 vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3 if jaccard100 != 9999
reg jaccard100 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3 if jaccard100 != 9999
reg jaccard100 vaultVar2 vaultVar3 vaultVar4 vaultVar5 if jaccard100 != 9999
reg inveLogIndex vaultVar2 vaultVar3 vaultVar4 vaultVar5 pbavg_diff logrev_diff size_diff age_diff gender_diff race_diff practiceArea_diff sameCityDum2 sameCityDum3 if jaccard100 != 9999

*sort data set
sort firm_2 firm_1

*export
export delimited using "C:\Users\diego\Dropbox\DLeal\USC\papers\Probono\probono_2018\probono_output_data_files", replace
export delimited dyad jaccard100 firm_2 firm_1 size_diff gender_diff race_diff age_diff practiceArea_diff inveLogIndex logrev_diff pbavg_diff vaultVar2 vaultVar3 vaultVar4 vaultVar5 sameCityDum2 sameCityDum3 using "dataAmLawRev_18", replace



log close




















































