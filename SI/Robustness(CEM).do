
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

levelsof industry_num, local(ind_num)


gen MissType = 1 if Emiss2009 != . & Emiss2010 != .
replace MissType = 2 if Emiss2009 == . & Emiss2010 != .
replace MissType = 3 if Emiss2009 != . & Emiss2010 == .
drop if MissType == .

set seed 13585

levelsof industry_num, local(group)

imb $covariate1 if MissType == 1, treatment(regu)
cem industry_num (`group') $covariate1 if MissType == 1, treatment(regu) k2k autocuts(sturges)
replace Treat = 1 if regu == 1 & cem_matched == 1 & Treat == .
replace Treat = 0 if regu == 0 & cem_matched == 1 & Treat == .
replace Weight = cem_weights if Weight == .

imb $covariate2 if MissType == 2, treatment(regu)
cem industry_num (`group') $covariate2 if MissType == 2, treatment(regu) k2k autocuts(sturges)
replace Treat = 1 if regu == 1 & cem_matched == 1 & Treat == .
replace Treat = 0 if regu == 0 & cem_matched == 1 & Treat == .
replace Weight = cem_weights if Weight == .

imb $covariate3 if MissType == 3, treatment(regu)
cem industry_num (`group') $covariate3 if MissType == 3, treatment(regu) k2k autocuts(sturges)
replace Treat = 1 if regu == 1 & cem_matched == 1 & Treat == .
replace Treat = 0 if regu == 0 & cem_matched == 1 & Treat == .
replace Weight = cem_weights if Weight == .


rename Treat Treat`t'
rename Weight Weight`t'

keep if Treat`t' != .
keep id_unified Treat`t' Weight`t'


save MatchCEM_`t'.dta, replace

}



// Combine matched samples together

use MatchCEM_2013.dta, clear

append using MatchCEM_2014.dta


// Combine same firms appearing in different-year matched samples into one obs
sort id_unified Treat2013
by id_unified: replace Treat2013 = Treat2013[_n-1] if Treat2013 == . & Treat2013[_n-1] != .
by id_unified: replace Weight2013 = Weight2013[_n-1] if Weight2013 == . & Weight2013[_n-1] != .

sort id_unified Treat2014
by id_unified: replace Treat2014 = Treat2014[_n-1] if Treat2014 == . & Treat2014[_n-1] != .
by id_unified: replace Weight2014 = Weight2014[_n-1] if Weight2014 == . & Weight2014[_n-1] != .

// drop firm duplicates
duplicates drop id_unified, force


save MatchCEM.dta, replace







* 2. Analysis

// Merge matched matched samples
use "CNTSD_ETS.dta", clear

merge m:1 id_unified using "MatchCEM.dta"

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



* Robustness Checks on Alternative Matching Methods (CEM)
foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.regu##c.post_implt c.region##c.post_annc if Match == 1, absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_CEM.xls", excel keep(c.regu#c.post_implt c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
}

