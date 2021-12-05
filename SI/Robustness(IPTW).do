

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


gen MissType = 1 if Emiss2009 != . & Emiss2010 != .
replace MissType = 2 if Emiss2009 == . & Emiss2010 != .
replace MissType = 3 if Emiss2009 != . & Emiss2010 == .
drop if MissType == .


logit regu $covariate1 if MissType == 1
predict pscore if e(sample), pr
gen pscore_ind = industry_num*10 + pscore

psmatch2 regu, pscore(pscore_ind) neighbor(1) common caliper(0.5)
replace Treat = 1 if _treated == 1 & _weight != . & _weight != 0 & _support == 1
replace Treat = 0 if _treated == 0 & _weight != . & _weight != 0 & _support == 1
replace Weight = _weight if _weight != . & _weight != 0 & _support == 1

// Weighting (IPTW)
gen IPTW = 1 / pscore if _treated == 1 & Emiss2009 != . & Emiss2010 != .
replace IPTW = 1 / (1 - pscore) if _treated == 0 & Emiss2009 != . & Emiss2010 != .


drop pscore pscore_ind




logit regu $covariate2 if MissType == 2
predict pscore if e(sample), pr
gen pscore_ind = industry_num*10 + pscore

psmatch2 regu, pscore(pscore_ind) neighbor(1) common caliper(0.5)
replace Treat = 1 if _treated == 1 & _weight != . & _weight != 0 & _support == 1
replace Treat = 0 if _treated == 0 & _weight != . & _weight != 0 & _support == 1
replace Weight = _weight if _weight != . & _weight != 0 & _support == 1

// Weighting (IPTW)
replace IPTW = 1 / pscore if _treated == 1 & Emiss2009 == . & Emiss2010 != .
replace IPTW = 1 / (1 - pscore) if _treated == 0 & Emiss2009 == . & Emiss2010 != .

drop pscore pscore_ind



logit regu $covariate3 if MissType == 3
predict pscore if e(sample), pr
gen pscore_ind = industry_num*10 + pscore

psmatch2 regu, pscore(pscore_ind) neighbor(1) common caliper(0.5)
replace Treat = 1 if _treated == 1 & _weight != . & _weight != 0 & _support == 1
replace Treat = 0 if _treated == 0 & _weight != . & _weight != 0 & _support == 1
replace Weight = _weight if _weight != . & _weight != 0 & _support == 1

// Weighting (IPTW)
replace IPTW = 1 / pscore if _treated == 1 & Emiss2009 != . & Emiss2010 == .
replace IPTW = 1 / (1 - pscore) if _treated == 0 & Emiss2009 != . & Emiss2010 == .

drop pscore pscore_ind


rename Treat Treat`t'
rename Weight Weight`t'
rename IPTW IPTW`t'

keep id_unified Treat`t' Weight`t' IPTW`t'


save MatchIPTW_`t'.dta, replace

}




// Combine matched samples together

use MatchIPTW_2013.dta, clear

append using MatchIPTW_2014.dta


// Combine same firms appearing in different-year matched samples into one obs
sort id_unified Treat2013
by id_unified: replace Treat2013 = Treat2013[_n-1] if Treat2013 == . & Treat2013[_n-1] != .
by id_unified: replace Weight2013 = Weight2013[_n-1] if Weight2013 == . & Weight2013[_n-1] != .
by id_unified: replace IPTW2013 = IPTW2013[_n-1] if IPTW2013 == . & IPTW2013[_n-1] != .

sort id_unified Treat2014
by id_unified: replace Treat2014 = Treat2014[_n-1] if Treat2014 == . & Treat2014[_n-1] != .
by id_unified: replace Weight2014 = Weight2014[_n-1] if Weight2014 == . & Weight2014[_n-1] != .
by id_unified: replace IPTW2014 = IPTW2014[_n-1] if IPTW2014 == . & IPTW2014[_n-1] != .

// drop firm duplicates
duplicates drop id_unified, force

save MatchIPTW.dta, replace






* 2. Analysis

// Merge matched matched samples
use "CNTSD_ETS.dta", clear

merge m:1 id_unified using "MatchIPTW.dta"

drop if _merge == 2
drop _merge

sort id_unified year


// post for implementation
gen post_implt = 1 if year >= ReguStart_year
replace post_implt = 0 if post_implt == .

// post for announcement
gen post_annc = 1 if year >= 2011 & year < 2013
replace post_annc = 0 if post_annc == .


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

egen IPTW = rowmean(IPTW2013 IPTW2014)
drop if IPTW == .

gen TimeTrend = year - 2006

egen Sec = group(IND2_name)
egen Pro = group(province)

gen SecTrend = Sec * TimeTrend
gen ProTrend = Pro * TimeTrend

egen SecYear = group(IND2_name year)
egen ProYear = group(province year)



* Robustness Checks on Alternative Matching Methods (IPTW)
foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.regu##c.post_implt c.region##c.post_annc [pweight=IPTW] , absorb(id year ProTrend SecTrend) vce(cluster stderror)
	outreg2 using "Robust_IPTW.xls", excel keep(c.regu#c.post_implt c.region#c.post_annc) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
}


