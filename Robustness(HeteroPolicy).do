

* 0001. Matching

foreach t of numlist 2013 / 2014 {

cd "$fold1"

use "CNTSD_ETS.dta", clear

// 保留相应年份的样本进行PSM（去除不在相应年份enrolled的ETS企业）
drop if regu == 1 & ReguStart_year != `t'

// 剔除首期样本从ETS公布后才开始的企业，因为pre-policy的观测不足
bysort id_unified: drop if year[1] >= 2011

// 剔除末期样本在ETS实行前就结束的企业，因为post-policy的观测不足
gsort id_unified -year
bysort id_unified: drop if year[1] <= `t'-1

sort id_unified year



// Set number marks for regions and industries (in order to match within the same region or industry)

// PS: some firms changed names and industries over time but now we just make them unified for later matching
// But this replace only for matching, not applied to other scenarios
gsort id_unified -year
by id_unified: replace name_unified = name_unified[_n-1] if name_unified[_n-1] != ""
by id_unified: replace IND2_name = IND2_name[_n-1] if IND2_name[_n-1] != ""
gsort id_unified -IND2_name
by id_unified: replace IND2_name = IND2_name[_n-1] if IND2_name == "" & IND2_name[_n-1] != ""
sort id_unified year

encode IND2_name, gen(industry_num)
encode province, gen(province_num)

sort id_unified year




gen CLratio = capital / employ

foreach C of varlist employ wage sale output capital CE CEOutput Emiss EmissSale EmissOutput CLratio {
	replace `C' = log(1+`C') 
}

keep id_unified year regu region industry_num province_num /*
*/ firm_age employ wage sale output capital /*
*/ CE CEOutput Emiss EmissSale EmissOutput

reshape wide firm_age employ wage sale output capital /*
*/ CE CEOutput Emiss EmissSale EmissOutput, i(id_unified regu industry_num province_num region) j(year)



*** Rigorous matching: matching by various missing value scenarios, respectively
// Any firms with three consecutive missing value (in the middle) in pre-ETS periods won't be put into PSM



**** 3 covariates

local x1 = "Emiss"
local x2 = "EmissOutput"
local x3 = "CE"

// (a) No missing value in two years
global covariate1 `x1'2009 `x1'2010 `x2'2009 `x2'2010 `x3'2009 `x3'2010
// (b) 2009 missing
global covariate2 `x1'2010 `x2'2010 `x3'2010
// (c) 2010 missing
global covariate3 `x1'2009 `x2'2009 `x3'2009





gen Treat = .
gen Weight = .
gen id_match = ""

levelsof province_num, local(prv_num)
levelsof industry_num, local(ind_num)

egen indpilot_num = group(industry_num) if region == 1
levelsof indpilot_num, local(indpilot)

egen indprv_num = group(industry_num province_num)
levelsof indprv_num, local(indprv)




// Mahalanobis

cd "$fold3"

// calculate propensity score
preserve

logit regu $covariate1 if Emiss2009 != . & Emiss2010 != .
predict pscore if e(sample), pr

gen pscore_ind = industry_num*10 + pscore

keep if pscore_ind != .

keep id_unified pscore_ind

rename id_unified id_pscore

save pscore_cov1.dta, replace

restore


preserve

logit regu $covariate2 if Emiss2009 == . & Emiss2010 != .
predict pscore if e(sample), pr

gen pscore_ind = industry_num*10 + pscore

keep if pscore_ind != .

keep id_unified pscore_ind

rename id_unified id_pscore

save pscore_cov2.dta, replace

restore


preserve

logit regu $covariate3 if Emiss2009 != . & Emiss2010 == .
predict pscore if e(sample), pr

gen pscore_ind = industry_num*10 + pscore

keep if pscore_ind != .

keep id_unified pscore_ind

rename id_unified id_pscore

save pscore_cov3.dta, replace

restore



recast str22 id_match

gen MissType = 1 if Emiss2009 != . & Emiss2010 != .
replace MissType = 2 if Emiss2009 == . & Emiss2010 != .
replace MissType = 3 if Emiss2009 != . & Emiss2010 == .
drop if MissType == .

foreach v of varlist $covariate1 {
	gen `v'_NoMiss = `v'
	replace `v'_NoMiss = 0 if `v'_NoMiss == .
}


local x1 = "Emiss"
local x2 = "EmissOutput"
local x3 = "CE"

global covariate `x1'2009_NoMiss `x1'2010_NoMiss `x2'2009_NoMiss `x2'2010_NoMiss `x3'2009_NoMiss `x3'2010_NoMiss


mahapick $covariate, idvar(id_unified) treated(regu) genfile(MahMatch_temp) replace /*
*/ matchon(industry_num MissType) sliceby(MissType) prime_id(treat_id) score scorevar(Mahscore)



preserve


use MahMatch_temp.dta, clear

keep if _matchnum != 0

rename id_unified control_id
rename treat_id id_unified

save MahMatch_forTreat.dta, replace


duplicates tag control_id, gen(dup)

gen weight_con = dup + 1

duplicates drop control_id, force

keep control_id weight_con Mahscore

rename control_id id_unified

save MahMatch_forControl.dta, replace


restore



// For treated units
gen MahDist = .

merge 1:1 id_unified using MahMatch_forTreat.dta

replace Treat = 1 if _merge == 3
replace Weight = 1 if _merge == 3
replace id_match = control_id if _merge == 3
replace MahDist = Mahscore if _merge == 3

drop if _merge == 2
drop _merge

drop control_id _matchnum Mahscore


// For control units
merge 1:1 id_unified using MahMatch_forControl.dta

replace Treat = 0 if _merge == 3
replace Weight = weight_con if _merge == 3

drop if _merge == 2
drop _merge

drop weight_con Mahscore



// Merge with propensity score
gen pscore_treat = .
gen pscore_control = .

// for treatment
gen id_pscore = id_unified if Treat == 1

merge m:1 id_pscore using pscore_cov1.dta

replace pscore_treat = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge

drop pscore_ind

merge m:1 id_pscore using pscore_cov2.dta

replace pscore_treat = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge
drop pscore_ind

merge m:1 id_pscore using pscore_cov3.dta

replace pscore_treat = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge
drop pscore_ind


// for control
drop id_pscore
gen id_pscore = id_match if Treat == 1

merge m:1 id_pscore using pscore_cov1.dta
replace pscore_control = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge
drop pscore_ind

merge m:1 id_pscore using pscore_cov2.dta
replace pscore_control = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge
drop pscore_ind

merge m:1 id_pscore using pscore_cov3.dta
replace pscore_control = pscore_ind if _merge == 3

drop if _merge == 2
drop _merge
drop pscore_ind

drop id_pscore


gen psdiff = abs(pscore_treat - pscore_control)


// caliper on propensity score difference (1.0/0.5/0.25)
gen DropPS = 1 if psdiff > 1.0 & psdiff != .

preserve

keep if DropPS == 1

keep id_match

capture duplicates drop id_match, force

rename id_match id_unified

save DropPS_control.dta, replace

restore

merge 1:1 id_unified using DropPS_control.dta

replace DropPS = 1 if _merge == 3

drop if _merge == 2
drop _merge



// caliper on Mahalanobis distance difference
gen DropMah = 1 if MahDist > 1.0 & MahDist != .

preserve

keep if DropMah == 1

keep id_match

capture duplicates drop id_match, force

rename id_match id_unified

save DropMah_control.dta, replace

restore

merge 1:1 id_unified using DropMah_control.dta

replace DropMah = 1 if _merge == 3

drop if _merge == 2
drop _merge


erase pscore_cov1.dta
erase pscore_cov2.dta
erase pscore_cov3.dta
erase MahMatch_temp.dta
erase MahMatch_forTreat.dta
erase MahMatch_forControl.dta
erase DropPS_control.dta
erase DropMah_control.dta

sort id_unified


// Drop samples with distances larger than caliper
drop if DropPS == 1
drop if DropMah == 1



rename Treat Treat`t'
rename Weight Weight`t'

keep if Treat`t' != .	// 提取匹配成功的treatment & control group样本
keep id_unified Treat`t' Weight`t' id_match


cd "$fold3"

save MatchMah_`t'.dta, replace


}







* 0002. Combine PSM samples together

cd "$fold3"

use MatchMah_2013.dta, clear

append using MatchMah_2014.dta


// Combine same firms appearing in different-year PSM samples into one obs
sort id_unified Treat2013
by id_unified: replace Treat2013 = Treat2013[_n-1] if Treat2013 == . & Treat2013[_n-1] != .
by id_unified: replace Weight2013 = Weight2013[_n-1] if Weight2013 == . & Weight2013[_n-1] != .

sort id_unified Treat2014
by id_unified: replace Treat2014 = Treat2014[_n-1] if Treat2014 == . & Treat2014[_n-1] != .
by id_unified: replace Weight2014 = Weight2014[_n-1] if Weight2014 == . & Weight2014[_n-1] != .

// drop firm duplicates
duplicates drop id_unified, force


cd "$fold3"

save MatchMah.dta, replace





















* 0003. Regression Section

cd "$fold1"

use "CNTSD_ETS.dta", clear


** (1) Merge PSM matched samples
cd "$fold3"

merge m:1 id_unified using "MatchMah.dta"

drop if _merge == 2
drop _merge


sort id_unified year



** (2) Variable post in different settings

* post for DDD (announcement)
gen post_annc = 1 if year >= 2011 & year < 2013
replace post_annc = 0 if post_annc == .


* post for DID (implementation)
gen post_implt = 1 if year >= ReguStart_year
replace post_implt = 0 if post_implt == .





* post for PSM+DID (implementation)

** I. Duplicate some firms as control units for different treatment years
expand 2 if Treat2013 != . & Treat2014 != ., gen(Dup_MultiYear)


// Mark PSM samples
sort id_unified year Dup_MultiYear

gen Dup_MultiYear_D = Dup_MultiYear
gsort id_unified -Dup_MultiYear_D year
by id_unified: replace Dup_MultiYear_D = Dup_MultiYear_D[_n-1] if Dup_MultiYear_D[_n-1] != .



// For control firms used in multiple treatment years, allocate one duplicate set for each treatment year
sort id_unified Dup_MultiYear year

// for Dup_MultiYear
gen PSM2013 = 1 if (Treat2013 != . & Treat2014 != . & Dup_MultiYear == 0)
gen PSM2014 = 1 if (Treat2013 != . & Treat2014 != . & Dup_MultiYear == 1)

// for non-Dup_MultiYear
replace PSM2013 = 1 if Treat2013 != . & Dup_MultiYear_D == 0
replace PSM2014 = 1 if Treat2014 != . & Dup_MultiYear_D == 0

gen PSM = 1 if PSM2013 == 1 | PSM2014 == 1


// duplicates only counted into analysis when using PSM+DID
// when other analyses they shouldn't be included
replace post_annc = . if Dup_MultiYear == 1
replace post_implt = . if Dup_MultiYear == 1


// drop the weight for Dup_MultiYear samples, otw will affect Dup_MultiUnit process

// Duplicates in 2013 & 2014
// For duplicates
replace Treat2013 = . if Dup_MultiYear == 1 & (Treat2013 != . & Treat2014 != .)
replace Weight2013 = . if Dup_MultiYear == 1 & (Weight2013 != . & Weight2014 != .)

// For originals
replace Treat2014 = . if Dup_MultiYear == 0 & Dup_MultiYear_D == 1 & (Treat2013 != . & Treat2014 != .)
replace Weight2014 = . if Dup_MultiYear == 0 & Dup_MultiYear_D == 1 & (Weight2013 != . & Weight2014 != .)


** II. Duplicate firms as control units matching multiple treated units
expand 2 if Weight2013 == 2 | Weight2014 == 2, gen(Dup_MultiUnit2)
expand 3 if Weight2013 == 3 | Weight2014 == 3, gen(Dup_MultiUnit3)
expand 4 if Weight2013 == 4 | Weight2014 == 4, gen(Dup_MultiUnit4)
expand 5 if Weight2013 == 5 | Weight2014 == 5, gen(Dup_MultiUnit5)
expand 6 if Weight2013 == 6 | Weight2014 == 6, gen(Dup_MultiUnit6)
expand 7 if Weight2013 == 7 | Weight2014 == 7, gen(Dup_MultiUnit7)
expand 8 if Weight2013 == 8 | Weight2014 == 8, gen(Dup_MultiUnit8)
expand 9 if Weight2013 == 9 | Weight2014 == 9, gen(Dup_MultiUnit9)
expand 10 if Weight2013 == 10 | Weight2014 == 10, gen(Dup_MultiUnit10)


gen Dup_MultiUnit = 1 if Dup_MultiUnit2 == 1 | Dup_MultiUnit3 == 1 | Dup_MultiUnit4 == 1 | Dup_MultiUnit5 == 1 /*
*/ | Dup_MultiUnit6 == 1 | Dup_MultiUnit7 == 1 | Dup_MultiUnit8 == 1 | Dup_MultiUnit9 == 1 | Dup_MultiUnit10 == 1
replace Dup_MultiUnit = 0 if Dup_MultiUnit == .


gen Dup_MultiUnit_D = Dup_MultiUnit
gsort id_unified -Dup_MultiUnit_D year
by id_unified: replace Dup_MultiUnit_D = Dup_MultiUnit_D[_n-1] if Dup_MultiUnit_D[_n-1] != .

sort id_unified Dup_MultiUnit year

// these duplicates only counted into analysis when using PSM+DID
// when other analyses they shouldn't be included
replace post_annc = . if Dup_MultiUnit == 1
replace post_implt = . if Dup_MultiUnit == 1



** 2013
gen post_impltPSM = 1 if year >= 2013 & Treat2013 != .
replace post_impltPSM = 0 if year < 2013 & Treat2013 != .

** 2014
replace post_impltPSM = 1 if year >= 2014 & Treat2014 != .
replace post_impltPSM = 0 if year < 2014 & Treat2014 != .


** DDD post for PSM
gen post_anncPSM = 1 if post_impltPSM != . & (year >= 2011 & year < 2013 & Treat2013 != .)
*gen post_anncPSM = 1 if post_impltPSM != . & year >= 2011 & year < 2013 & (Treat2013 != . | Treat2014 != .)
replace post_anncPSM = 1 if post_impltPSM != . & (year >= 2011 & year < 2014 & Treat2014 != .)
replace post_anncPSM = 0 if post_impltPSM != . & post_anncPSM == .









** (3) Regression
set matsize 11000, perm


replace id_unified = id_unified +  "-DupMultiYear" if Dup_MultiYear == 1



sort id_unified Dup_MultiUnit year

replace id_unified = id_unified +  "-DupMultiUnit2" if Dup_MultiUnit2 == 1


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit3) if Dup_MultiUnit3 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit31" if Dup_MultiUnit3 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit32" if Dup_MultiUnit3 == 1 & dup_Num == 2
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit4) if Dup_MultiUnit4 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit41" if Dup_MultiUnit4 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit42" if Dup_MultiUnit4 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit43" if Dup_MultiUnit4 == 1 & dup_Num == 3
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit5) if Dup_MultiUnit5 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit51" if Dup_MultiUnit5 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit52" if Dup_MultiUnit5 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit53" if Dup_MultiUnit5 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit54" if Dup_MultiUnit5 == 1 & dup_Num == 4
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit6) if Dup_MultiUnit6 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit61" if Dup_MultiUnit6 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit62" if Dup_MultiUnit6 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit63" if Dup_MultiUnit6 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit64" if Dup_MultiUnit6 == 1 & dup_Num == 4
replace id_unified = id_unified +  "-DupMultiUnit65" if Dup_MultiUnit6 == 1 & dup_Num == 5
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit7) if Dup_MultiUnit7 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit71" if Dup_MultiUnit7 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit72" if Dup_MultiUnit7 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit73" if Dup_MultiUnit7 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit74" if Dup_MultiUnit7 == 1 & dup_Num == 4
replace id_unified = id_unified +  "-DupMultiUnit75" if Dup_MultiUnit7 == 1 & dup_Num == 5
replace id_unified = id_unified +  "-DupMultiUnit76" if Dup_MultiUnit7 == 1 & dup_Num == 6
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit8) if Dup_MultiUnit8 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit81" if Dup_MultiUnit8 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit82" if Dup_MultiUnit8 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit83" if Dup_MultiUnit8 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit84" if Dup_MultiUnit8 == 1 & dup_Num == 4
replace id_unified = id_unified +  "-DupMultiUnit85" if Dup_MultiUnit8 == 1 & dup_Num == 5
replace id_unified = id_unified +  "-DupMultiUnit86" if Dup_MultiUnit8 == 1 & dup_Num == 6
replace id_unified = id_unified +  "-DupMultiUnit87" if Dup_MultiUnit8 == 1 & dup_Num == 7
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit9) if Dup_MultiUnit9 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit91" if Dup_MultiUnit9 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit92" if Dup_MultiUnit9 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit93" if Dup_MultiUnit9 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit94" if Dup_MultiUnit9 == 1 & dup_Num == 4
replace id_unified = id_unified +  "-DupMultiUnit95" if Dup_MultiUnit9 == 1 & dup_Num == 5
replace id_unified = id_unified +  "-DupMultiUnit96" if Dup_MultiUnit9 == 1 & dup_Num == 6
replace id_unified = id_unified +  "-DupMultiUnit97" if Dup_MultiUnit9 == 1 & dup_Num == 7
replace id_unified = id_unified +  "-DupMultiUnit98" if Dup_MultiUnit9 == 1 & dup_Num == 8
drop dup_Num


bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit10) if Dup_MultiUnit10 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit101" if Dup_MultiUnit10 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit102" if Dup_MultiUnit10 == 1 & dup_Num == 2
replace id_unified = id_unified +  "-DupMultiUnit103" if Dup_MultiUnit10 == 1 & dup_Num == 3
replace id_unified = id_unified +  "-DupMultiUnit104" if Dup_MultiUnit10 == 1 & dup_Num == 4
replace id_unified = id_unified +  "-DupMultiUnit105" if Dup_MultiUnit10 == 1 & dup_Num == 5
replace id_unified = id_unified +  "-DupMultiUnit106" if Dup_MultiUnit10 == 1 & dup_Num == 6
replace id_unified = id_unified +  "-DupMultiUnit107" if Dup_MultiUnit10 == 1 & dup_Num == 7
replace id_unified = id_unified +  "-DupMultiUnit108" if Dup_MultiUnit10 == 1 & dup_Num == 8
replace id_unified = id_unified +  "-DupMultiUnit109" if Dup_MultiUnit10 == 1 & dup_Num == 9
drop dup_Num


drop Dup_MultiUnit2 Dup_MultiUnit3 Dup_MultiUnit4 Dup_MultiUnit5 Dup_MultiUnit6 Dup_MultiUnit7 Dup_MultiUnit8 Dup_MultiUnit9 Dup_MultiUnit10






sort id_unified year

egen id = group(id_unified)	// string variable can't be used for xtset
move id id_unified
xtset id year 	// xtset for calculating difference and collapse

* remove the firms starting after 2011 (not enough pre-policy observations)
bysort id_unified: drop if year[1] >= 2011

* remove the firms ending before 2010 (not enough post-policy observations)
gsort id_unified -year
bysort id_unified: drop if year[1] <= 2010

sort id_unified year



* Dependent variables

** (1) Main:
foreach v of varlist Emiss EmissOutput EmissSale {
	gen log`v' = log(`v')		
}


** (2) Abatement:
foreach v of varlist CE CEOutput EmissCE {
	gen log`v' = log(`v')		
}

*** fuel switching to different energy
gen CoalCERatio = CE_coal / CE
gen CoalEmisRatio = Emiss_coal / Emiss

gen OilCERatio = CE_oil / CE
gen OilEmisRatio = Emiss_oil / Emiss

gen GasCERatio = (CE_NG) / CE
gen GasEmisRatio = (Emiss_NG) / Emiss

gen ElecCERatio = CE_elec / CE
gen ElecEmisRatio = Emiss_elec / Emiss


foreach v of varlist Coal CoalCERatio CoalEmisRatio /*
*/ Oil OilCERatio OilEmisRatio GasCERatio GasEmisRatio /*
*/ Electric ElecCERatio ElecEmisRatio {
	gen log`v' = log(1+`v') 
}




** (3) Econ:
foreach v of varlist output sale employ wage capital valueadd invest  {
	gen log`v' = log(`v') 
}


replace export = 0 if export == .
gen logexport = log(1+export)


*** Other economic indicators
gen CapLab = capital / employ
gen OutputLab = output / employ
gen OutputCap = output / capital
gen WageLab = wage / employ

foreach v of varlist CapLab OutputLab OutputCap WageLab {
	gen log`v' = log(`v')
}



* Independent variables: market performance
// take log
foreach v of varlist price {
	gen log`v' = log(1+`v')		
}





/*
// 将控制变量取对数
foreach C of varlist firm_age employ sale output capital {
	gen log`C' = log(1+`C') 
}

*global control firm_age invest employ sale output capital
global control logfirm_age logemploy logsale logoutput logcapital
*/

* Standarize the period for PSM
// Treat2013
gen period = 0 if year == 2013 & Treat2013 != .
replace period = 1 if year == 2014 & Treat2013 != .
replace period = 2 if year == 2015 & Treat2013 != .

replace period = -1 if year == 2012 & Treat2013 != .
replace period = -2 if year == 2011 & Treat2013 != .
replace period = -3 if year == 2010 & Treat2013 != .
replace period = -4 if year == 2009 & Treat2013 != .
replace period = -5 if year == 2008 & Treat2013 != .

// Treat2014
replace period = 0 if year == 2014 & Treat2014 != .
replace period = 1 if year == 2015 & Treat2014 != .

replace period = -1 if year == 2013 & Treat2014 != .
replace period = -2 if year == 2012 & Treat2014 != .
replace period = -3 if year == 2011 & Treat2014 != .
replace period = -4 if year == 2010 & Treat2014 != .
replace period = -5 if year == 2009 & Treat2014 != .
replace period = -6 if year == 2008 & Treat2014 != .




* Linear trend
sort id_unified year

// Some firms changed their sectors over time so might be cause "panels are not nested within clusters"
// We temporarily make every firm in the same sector in all years, but the solution should be unifying sectors
gsort id_unified -year
by id_unified: replace IND2_name = IND2_name[_n-1] if IND2_name[_n-1] != ""
sort id_unified year

egen stderror = group(IND2_name)
egen stderror_secprv = group(IND2_name province)
egen stderror_sec4 = group(IND4_name)


global dep_var_main logEmiss logEmissOutput

global dep_var_abate logCE logCEOutput logEmissCE GasCERatio

global dep_var_econ logemploy logcapital logCapLab logwage loginvest logvalueadd logoutput logexport logOutputLab logOutputCap TFP_OP TFP_LP



// For linear trends & fixed effects
gen TimeTrend = year - 2006
egen Sec = group(IND2_name)
egen Pro = group(province)

gen SecTrend = Sec * TimeTrend
gen ProTrend = Pro * TimeTrend

egen SecYear = group(IND2_name year)
egen ProYear = group(province year)

drop if year <= 2008



* Mass-based and Rate-based allocation

// Extract matched control firms
// mass-based
preserve

keep if PSM == 1
keep if MassBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen MassBaseGroup = 1 if PSM == 1 & MassBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace MassBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp



// rate-based
preserve

keep if PSM == 1
keep if RateBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore


gen RateBaseGroup = 1 if PSM == 1 & RateBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace RateBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp


replace MassBaseGroup = 0 if PSM == 1 & MassBaseGroup != 1
replace RateBaseGroup = 0 if PSM == 1 & RateBaseGroup != 1







* Robustness: Policy Heterogeneity based upon Observable Carbon Market Performance
** (1) carbon price
foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.logprice##c.RateBaseGroup c.region##c.post_anncPSM /*
	*/ , absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "ChinaETSEmis_Mah_Policy.xls", excel keep(logprice c.logprice#c.RateBaseGroup c.region#c.post_anncPSM) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
}



** (2) turnover rate
foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.turnover##c.RateBaseGroup c.region##c.post_anncPSM /*
	*/ , absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "ChinaETSEmis_Mah_Policy.xls", excel keep(turnover c.turnover#c.RateBaseGroup c.region#c.post_anncPSM) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
}


