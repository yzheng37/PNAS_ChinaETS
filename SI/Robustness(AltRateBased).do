

* 1. Matching

foreach t of numlist 2013 / 2014 {

use "CNTSD_ETS.dta", clear

drop if regu == 1 & ReguStart_year != `t'

bysort id_unified: drop if year[1] >= 2011

gsort id_unified -year
bysort id_unified: drop if year[1] <= `t'-1

sort id_unified year


encode IND2_name, gen(industry_num)

sort id_unified year

foreach C of varlist sale output CE CEOutput Emiss EmissOutput {
	replace `C' = log(`C') 
}

keep id_unified year regu region industry_num sale output CE CEOutput Emiss EmissOutput

reshape wide sale output CE CEOutput Emiss EmissOutput, i(id_unified regu industry_num region) j(year)


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

levelsof industry_num, local(ind_num)



recast str22 id_match

gen MissType = 1 if Emiss2009 != . & Emiss2010 != .
replace MissType = 2 if Emiss2009 == . & Emiss2010 != .
replace MissType = 3 if Emiss2009 != . & Emiss2010 == .
drop if MissType == .

foreach v of varlist $covariate1 {
	gen `v'_NoMiss = `v'
	replace `v'_NoMiss = 0 if `v'_NoMiss == .
}

global covariate `x1'2009_NoMiss `x1'2010_NoMiss `x2'2009_NoMiss `x2'2010_NoMiss `x3'2009_NoMiss `x3'2010_NoMiss


mahapick $covariate, idvar(id_unified) treated(regu) genfile(MahMatch_temp) replace /*
*/ matchon(industry_num MissType) sliceby(MissType) prime_id(treat_id) score scorevar(Mahscore)


// record matched id
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
merge 1:1 id_unified using MahMatch_forTreat.dta

replace Treat = 1 if _merge == 3
replace Weight = 1 if _merge == 3
replace id_match = control_id if _merge == 3

drop if _merge == 2
drop _merge

drop control_id _matchnum


// For control units
merge 1:1 id_unified using MahMatch_forControl.dta

replace Treat = 0 if _merge == 3
replace Weight = weight_con if _merge == 3

drop if _merge == 2
drop _merge

drop weight_con


erase MahMatch_temp.dta
erase MahMatch_forTreat.dta
erase MahMatch_forControl.dta

sort id_unified

rename Treat Treat`t'
rename Weight Weight`t'

keep if Treat`t' != .
keep id_unified Treat`t' Weight`t' id_match


save MatchMah_`t'.dta, replace


}


// Combine matched samples together

use MatchMah_2013.dta, clear

append using MatchMah_2014.dta

// Combine same firms appearing in different-year matched samples into one obs
sort id_unified Treat2013
by id_unified: replace Treat2013 = Treat2013[_n-1] if Treat2013 == . & Treat2013[_n-1] != .
by id_unified: replace Weight2013 = Weight2013[_n-1] if Weight2013 == . & Weight2013[_n-1] != .

sort id_unified Treat2014
by id_unified: replace Treat2014 = Treat2014[_n-1] if Treat2014 == . & Treat2014[_n-1] != .
by id_unified: replace Weight2014 = Weight2014[_n-1] if Weight2014 == . & Weight2014[_n-1] != .

// drop firm duplicates
duplicates drop id_unified, force

save MatchMah.dta, replace






* 2. Analysis

// Merge matched matched samples
use "CNTSD_ETS.dta", clear

merge m:1 id_unified using "MatchMah.dta"

drop if _merge == 2
drop _merge

sort id_unified year

gen Match = 1 if Treat2013 != . | Treat2014 != .


// Duplicate some firms as control units for different treatment years
expand 2 if Treat2013 != . & Treat2014 != ., gen(Dup_MultiYear)

// Duplicate firms as control units matching multiple treated units
expand 2 if Weight2013 == 2 | Weight2014 == 2, gen(Dup_MultiUnit2)
expand 3 if Weight2013 == 3 | Weight2014 == 3, gen(Dup_MultiUnit3)

gen Dup_MultiUnit = 1 if Dup_MultiUnit2 == 1 | Dup_MultiUnit3 == 1


// post for implementation (2013)
gen post_implt = 1 if year >= 2013 & Treat2013 != .
replace post_implt = 0 if year < 2013 & Treat2013 != .

// post for implementation (2014)
replace post_implt = 1 if year >= 2014 & Treat2014 != .
replace post_implt = 0 if year < 2014 & Treat2014 != .


// post for announcement
gen post_annc = 1 if post_implt != . & (year >= 2011 & year < 2013 & Treat2013 != .)
replace post_annc = 1 if post_implt != . & (year >= 2011 & year < 2014 & Treat2014 != .)
replace post_annc = 0 if post_implt != . & post_annc == .



// unique id for duplicated firms
replace id_unified = id_unified +  "-DupMultiYear" if Dup_MultiYear == 1

sort id_unified Dup_MultiUnit year

replace id_unified = id_unified +  "-DupMultiUnit2" if Dup_MultiUnit2 == 1

bysort id_unified year: gen dup_Num = sum(Dup_MultiUnit3) if Dup_MultiUnit3 == 1
sort id_unified Dup_MultiUnit year dup_Num

replace id_unified = id_unified +  "-DupMultiUnit31" if Dup_MultiUnit3 == 1 & dup_Num == 1
replace id_unified = id_unified +  "-DupMultiUnit32" if Dup_MultiUnit3 == 1 & dup_Num == 2
drop dup_Num

drop Dup_MultiYear Dup_MultiUnit2 Dup_MultiUnit3


sort id_unified year
egen id = group(id_unified)
move id id_unified
xtset id year

// Variables
// Main:
foreach v of varlist Emiss EmissOutput {
	gen log`v' = log(`v')		
}

global dep_var_main logEmiss logEmissOutput


sort id_unified year

egen stderror = group(IND2_name)

gen TimeTrend = year - 2006

egen Sec = group(IND2_name)
egen Pro = group(province)

gen SecTrend = Sec * TimeTrend
gen ProTrend = Pro * TimeTrend

egen SecYear = group(IND2_name year)
egen ProYear = group(province year)


* Mass-based and Rate-based allocation
// Extract matched control firms
// mass-based
preserve

keep if Match == 1
keep if MassBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen MassBaseGroup = 1 if Match == 1 & MassBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace MassBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

// rate-based
preserve

keep if Match == 1
keep if RateBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore


gen RateBaseGroup = 1 if Match == 1 & RateBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace RateBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

replace MassBaseGroup = 0 if Match == 1 & MassBaseGroup != 1
replace RateBaseGroup = 0 if Match == 1 & RateBaseGroup != 1





* Robustness: Alternative mass-based & rate-based Classifications

** (1) Alt. Classification 1: drop GD&CQ 
preserve

keep if Match == 1
keep if regu == 1 & MassBase_GDCQdrop == . & RateBase_GDCQdrop == . 
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace MassBase_GDCQdrop = . if _merge == 3
replace RateBase_GDCQdrop = . if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

gen GDCQdrop = 1 if Match == 1 & (MassBase_GDCQdrop == . | RateBase_GDCQdrop == .)

foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.post_implt##c.regu##c.RateBaseGroup c.region##c.post_annc if Match == 1 & GDCQdrop != 1, absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_AltRateBase.xls", excel keep(c.post_implt#c.regu c.post_implt#c.regu#c.RateBaseGroup c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y, GDCQdrop, Y)
}



** (2) Alt. Classification 2: exogenous & endogenous
// exogenous group
preserve

keep if Match == 1
keep if ExoBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen ExoBaseGroup = 1 if Match == 1 & ExoBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace ExoBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

// endogenous group
preserve

keep if Match == 1
keep if EndoBase == 1
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore


gen EndoBaseGroup = 1 if Match == 1 & EndoBase == 1

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace EndoBaseGroup = 1 if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

replace ExoBaseGroup = 0 if Match == 1 & ExoBaseGroup != 1
replace EndoBaseGroup = 0 if Match == 1 & EndoBaseGroup != 1

foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.post_implt##c.regu##c.EndoBaseGroup c.region##c.post_annc if Match == 1, absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_AltRateBase.xls", excel keep(c.post_implt#c.regu c.post_implt#c.regu#c.EndoBaseGroup c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
}




** (3) Alt. Classification 3: drop mass-based endogenous
preserve

keep if Match == 1
keep if regu == 1 & MassBase_ExcepDrop == . & RateBase_ExcepDrop == . 
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace MassBase_ExcepDrop = . if _merge == 3
replace RateBase_ExcepDrop = . if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

gen ExcepDrop = 1 if Match == 1 & (MassBase_ExcepDrop == . | RateBase_ExcepDrop == .)


foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.post_implt##c.regu##c.RateBaseGroup c.region##c.post_annc if Match == 1 & ExcepDrop != 1, absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_AltRateBase.xls", excel keep(c.post_implt#c.regu c.post_implt#c.regu#c.RateBaseGroup c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y, DropOBExcep, Y)
}



** (4) Alt. Classification 4: only grandfathering
preserve

keep if Match == 1
keep if regu == 1 & MassBase_GF == . & RateBase_GF == . 
duplicates drop id_unified, force
keep id_match
duplicates drop id_match, force
drop if id_match == ""
rename id_match id_match_temp

save "id_match.dta", replace

restore

gen id_match_temp = substr(id_unified, 1, 15)

merge m:1 id_match_temp using "id_match.dta"

replace MassBase_GF = . if _merge == 3
replace RateBase_GF = . if _merge == 3

drop if _merge == 2
drop _merge
erase "id_match.dta"

drop id_match_temp

gen NonGF = 1 if Match == 1 & (MassBase_GF == . | RateBase_GF == .)

foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.post_implt##c.regu##c.RateBaseGroup c.region##c.post_annc if Match == 1 & NonGF != 1, absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_AltRateBase.xls", excel keep(c.post_implt#c.regu c.post_implt#c.regu#c.RateBaseGroup c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y, GF, Y)
}

