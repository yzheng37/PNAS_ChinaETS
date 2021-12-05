

* 1. Matching

foreach t of numlist 2013 / 2014 {

use "CNTSD_ETS_with0708.dta", clear

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
use "CNTSD_ETS_with0708.dta", clear

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






* Dynamic effects

** 2007-2015
gen ETS2007 = (year == 2007)
gen ETS2008 = (year == 2008)
gen ETS2009 = (year == 2009)
gen ETS2010 = (year == 2010)
gen ETS2011 = (year == 2011)
gen ETS2012 = (year == 2012)
gen ETS2013 = (year == 2013)
gen ETS2014 = (year == 2014)
gen ETS2015 = (year == 2015)

// reference period: 2010 (one period before ETS announcement)
gen baseline = 0

foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.regu#c.ETS2007 c.regu#c.ETS2008 c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015 if Match == 1, absorb(id year ProTrend SecTrend) vce(cluster stderror) level(95)

	estimate store dynamic
	
	coefplot /*
	*/ (dynamic, ciopts(recast(rarea) color(gs13)) msymbol(Oh) msize(large) mlcolor(blue) mlwidth(medthick) legend(off) /*
	*/ keep(c.regu#c.ETS2007 c.regu#c.ETS2008 c.regu#c.ETS2009  baseline c.regu#c.ETS2011)) /*
	*/ (dynamic, ciopts(recast(rarea) color(gs10)) msymbol(O) msize(large) mcolor(blue) mlcolor(white) mlwidth(medium) legend(off) /*
	*/ keep(c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013))/* 
	*/ (dynamic, ciopts(recast(rarea) color(gs7)) msymbol(S) msize(large) mcolor(blue) mlcolor(white) mlwidth(medium) legend(off) /*
	*/ keep(c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015))/*  
	*/, coeflabels(c.regu#c.ETS2007 = "-4" c.regu#c.ETS2008 = "-3" c.regu#c.ETS2009 = "-2" baseline = "-1" c.regu#c.ETS2011 = "0" c.regu#c.ETS2012 = "1" c.regu#c.ETS2013 = "2" c.regu#c.ETS2014 = "3" c.regu#c.ETS2015 = "4") /*
	*/ order(c.regu#c.ETS2007 c.regu#c.ETS2008 c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015) /*
	*/ omitted offset(0) xlabel(,angle(0)) ylabel(#9, angle(0)) vertical levels(95) color(blue) scheme(s1color) /*
	*/ yscale(range(-0.5(0.1)0.4)) plotregion(lwidth(none)) graphregion(margin(small)) title(`Y', size(large)) xtitle("Period") ytitle("") drop(cons_)

	graph save Dynamic_`Y'.gph, replace
	
	outreg2 using "DynamicEffect.xls", excel keep(c.regu#c.ETS2007 c.regu#c.ETS2008 c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year linear trend, Y, Industry-Year linear trend, Y)
	
}

// Combine all dynamic effect graphs
graph combine "Dynamic_logEmiss.gph" /*
*/ "Dynamic_logEmissOutput.gph" /*
*/ , rows(2) cols(1) title("", size(medium)) iscale(0.6) xsize(10) ysize(10) scheme(s1color) graphregion(margin(zero)) saving("Dynamic_07to15.gph", replace)
	
erase "Dynamic_logEmiss.gph"
erase "Dynamic_logEmissOutput.gph"



** 2009-2015
drop if year <= 2008

foreach Y of varlist $dep_var_main {
	quietly reghdfe `Y' c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015 if Match == 1, absorb(id year ProTrend SecTrend) vce(cluster stderror) level(95)
	
	estimate store dynamic
	
	coefplot /*
	*/ (dynamic, ciopts(recast(rarea) color(gs13)) msymbol(Oh) msize(large) mlcolor(blue) mlwidth(medthick) legend(off) /*
	*/ keep(c.regu#c.ETS2009 baseline c.regu#c.ETS2011)) /*
	*/ (dynamic, ciopts(recast(rarea) color(gs10)) msymbol(O) msize(large) mcolor(blue) mlcolor(white) mlwidth(medium) legend(off) /*
	*/ keep(c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013))	/*
	*/ (dynamic, ciopts(recast(rarea) color(gs7)) msymbol(S) msize(large) mcolor(blue) mlcolor(white) mlwidth(medium) legend(off) /*
	*/ keep(c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015))/* 
	*/ , coeflabels(c.regu#c.ETS2009 = "-2" baseline = "-1" c.regu#c.ETS2011 = "0" c.regu#c.ETS2012 = "1" c.regu#c.ETS2013 = "2" c.regu#c.ETS2014 = "3" c.regu#c.ETS2015 = "4") /*
	*/ order(c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015) /*
	*/ omitted offset(0) xlabel(, angle(0)) ylabel(#9, angle(0)) vertical levels(95) color(blue) scheme(s1color) yscale(range(-0.5(0.1)0.4)) plotregion(lwidth(none)) graphregion(margin(small)) title(`Y', size(large)) xtitle("Period") ytitle("") drop(cons_)

	graph save Dynamic_`Y'.gph, replace
	
	outreg2 using "DynamicEffect.xls", excel keep(c.regu#c.ETS2009 baseline c.regu#c.ETS2011 c.regu#c.ETS2012 c.regu#c.ETS2013 c.regu#c.ETS2014 c.regu#c.ETS2015) dec(3) /*
	*/ addtext(Firm FE, Y, Year FE, Y, Province-Year FE, Y, Industry-Year FE, Y)
	
}

// Combine all dynamic effect graphs
graph combine "Dynamic_logEmiss.gph" /*
*/ "Dynamic_logEmissOutput.gph" /*
*/ , rows(2) cols(1) title("", size(medium)) iscale(0.6) xsize(10) ysize(10) scheme(s1color) graphregion(margin(zero)) saving("DynamicNew_09to15.gph", replace)

erase "Dynamic_logEmiss.gph"
erase "Dynamic_logEmissOutput.gph"






// Combine dynamic effect graphs (09to15 + 07to15)
graph combine "Dynamic_09to15.gph" /*
*/ "Dynamic_07to15.gph" /*
*/ , rows(1) cols(2) title("", size(medium)) iscale(1) xsize(15) ysize(10)  scheme(s1color) graphregion(margin(zero)) saving("Dynamic.gph", replace)

	
