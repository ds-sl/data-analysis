// ---------------------------- // 
*		CHAPTER 1
// ---------------------------- //


* ---------------------------------------------------------------------------- *
* 				   	            F1.0 & F1.0supp                                *
*                      Global adult and total population                       *
* ---------------------------------------------------------------------------- * 

use "$workdatawir/world_2021.dta", clear
keep if inlist(widcode, "npopul999i", "npopul992i")

gen bot50 = value*.5/1e9
gen mid40 = value*.4/1e9  
gen top10 = value*.1/1e6  
gen top1  = value*.01/1e6 

rename value total
label var total "Total"
label var bot50 "Bottom 50%"
label var mid40 "Middle 40%"
label var top10 "Top 10%"
label var top1  "Top 1%"
drop total
replace widcode = "Total Population" if widcode == "npopul999i" 
replace widcode = "Adult Population" if widcode == "npopul992i" 

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.0") sheetmod cell(A2) first(varl) 


* ---------------------------------------------------------------------------- *
* 				   	           F1.1 & F1.1supp                                 *
*            Global Income & Wealth inequality by population group             *
* ---------------------------------------------------------------------------- *

use if inlist(iso, "WO") & year == 2021 ///
	using $wid, clear

drop currency iso year 
keep if inlist(widcode, "sptinc992j", "shweal992j") ///
	  & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100")

reshape wide value, i(widcode) j(p) string

renvars value*, pred(5)
label var p0p50   "Bottom 50%"
label var p50p90  "Middle 40%"
label var p90p100 "Top 10%"
label var p99p100 "Top 1%"
label var widcode " "

replace widcode = "Income" if widcode == "sptinc992j" 
replace widcode = "Wealth" if widcode == "shweal992j" 
gsort widcode

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.1") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.1")
putexcel (B3:E4), nformat(percent)
preserve
	drop p99p100
	expand 2 in l, gen(new)
	replace widcode = "Total population (7.8 billion)" if new
	replace p0p50   = .5 if new
	replace p50p90  = .4 if new
	replace p90p100 = .1 if new
	drop new
	generate order = 1 if widcode == "Total population (7.8 billion)"
    replace order = 2  if widcode == "Income"
	replace order = 3  if widcode == "Wealth"
	gsort order
	drop order
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.1supp") sheetmod cell(A2) first(varl)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.1supp")
	putexcel (B3:D5), nformat(percent)
restore


* ---------------------------------------------------------------------------- *
* 				   	                   T1.1                                    *
*                 Income & Wealth averages by population group                 *
* ---------------------------------------------------------------------------- *

use if inlist(iso, "WO") & year == 2021 ///
	using $wid, clear

keep if inlist(widcode, "anninc992i", "aptinc992j","tptinc992j", "ahweal992j", "thweal992j") ///
	  & inlist(p, "p0p100", "p0p50", "p50p90", "p90p100", "p99p100", "p99.9p100", "p50p51")
	  
drop currency iso year 
keep if inlist(widcode, "aptinc992j", "tptinc992j", "ahweal992j", "thweal992j") ///
	  & inlist(p, "p0p100", "p0p50", "p50p90", "p90p100", "p99p100", "p99.9p100", "p50p51")
	  
replace value = round(value, 100)
reshape wide value, i(p) j(widcode) string
renvars value*, pred(5)

replace thweal992j = . if p == "p0p100"
replace tptinc992j = . if p == "p0p100"

gen aa = thweal992j if p == "p50p51" // Middle 40%
egen t50 = mode(aa)
replace thweal992j = t50 if p == "p50p90"
drop aa t50

gen aa = tptinc992j if p == "p50p51" // Middle 40%
egen t50 = mode(aa)
replace tptinc992j = t50 if p == "p50p90"
drop aa t50

drop if p == "p50p51"

label var ahweal992j "Avg. wealth per adult (PPP €)"
label var aptinc992j "Avg. annual income per adult (PPP €)"
label var thweal992j "Wealth threshold (PPP €)"
label var tptinc992j "Income threshold (PPP €)"

label var p    " "
generate order = 1 if p == "p0p100"
replace order = 2  if p == "p0p50"
replace order = 3  if p == "p50p90"
replace order = 4  if p == "p90p100"
replace order = 5  if p == "p99p100"
replace order = 6  if p == "p99.9p100"
gsort order

replace p = "Total Population" if p == "p0p100"
replace p = "Bottom 50%"       if p == "p0p50"
replace p = "Middle 40%"       if p == "p50p90"
replace p = "Top 10%"          if p == "p90p100"
replace p = "Top 1%"           if p == "p99p100"
replace p = "Top 0.1%"         if p == "p99.9p100"
order p aptinc992j tptinc992j ahweal992j thweal992j
drop order

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-T1.1") sheetmod cell(A2) first(varl)


* ---------------------------------------------------------------------------- *
* 				   	        F1.2a, F1.2b & F1.2supp                            *
*                 Average income & wealth across world regions                 *
* ---------------------------------------------------------------------------- *
 
use if (inlist(iso, "WO", "XF", "QE", "QP", "XL") | inlist(iso, "XN", "XR", "XS", "QL")) & ///
	inlist(widcode, "ahweal992j", "anninc992i") & year == 2021 & p == "p0p100" ///
	using $wid, clear

replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"           if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

rename iso RegionWID
drop year p currency
reshape wide value, i(RegionWID) j(widcode) string
renvars value*, pred(5)

generate aa = anninc992i if RegionWID == "WO"
egen ainc_wo = mode(aa)
drop aa

generate aa = ahweal992j if RegionWID == "WO"
egen aweal_wo = mode(aa)
drop aa

drop if RegionWID == "WO"

generate average_inc  = anninc992i/ainc_wo
generate average_weal = ahweal992j/aweal_wo

*    F1.2a
preserve
	gsort average_inc
	keep RegionWID average_inc 
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.2a") sheetmod cell(A2) first(varl)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.2a")
	putexcel (B3:B10), nformat(percent)
restore
*    F1.2b
preserve
	gsort average_weal
	keep RegionWID average_weal
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.2b") sheetmod cell(A2) first(varl)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.2b")
	putexcel (B3:B10), nformat(percent)
restore
*    F1.2supp
preserve
	gsort anninc992i
	keep RegionWID anninc992i 
	replace anninc992i = round(anninc992i, 100)
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.2supp") sheetmod cell(A2) first(varl)
restore

preserve
	gsort ahweal992j
	keep RegionWID ahweal992j 
	replace ahweal992j = round(ahweal992j, 100)
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.2supp") sheetmod cell(D2) first(varl)
restore


* ---------------------------------------------------------------------------- *
* 				   	                  F1.3                                     *
*                  Global income shares by population group                    *
* ---------------------------------------------------------------------------- *

use if widcode == "sptinc992j" & inlist(iso, "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") ///
		& year == 2021 & inlist(p, "p0p50", "p50p90", "p90p100") using $wid, clear
replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"           if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

drop widcode currency 

reshape wide value, i(iso) j(p) string
renvars value*, predrop(5)
label var p0p50   "Bottom 50%"
label var p50p90  "Middle 40%"
label var p90p100 "Top 10%"

order year iso 
gsort p90p100
export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.3") sheetmod cell(A1) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.3")
putexcel (C2:E9), nformat(percent)

tempfile F1p3
save `F1p3', replace



* ---------------------------------------------------------------------------- *
* 				   	           F1.4 & F1.4supp                                 *
*                      Income gaps across world regions                        *
* ---------------------------------------------------------------------------- *

use if widcode == "adiinc992j" & inlist(p, "p0p50", "p90p100", "p99p100") ///
using "$workdatawir/regional-aggregates-posttax-2.dta", clear
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "top1"  if p == "p99p100"

reshape wide value, i(iso) j(p) string
renvars value*, predrop(5)
gen T10B50post = top10/(bot50)
keep iso T10B50post
tempfile T10B50postregions
save `T10B50postregions', replace

use if widcode == "aptinc992j" & inlist(iso, "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") & ///
       year == 2021 & inlist(p, "p0p50", "p90p100", "p99p100") using $wid, clear

drop widcode currency 
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "top1"  if p == "p99p100"

reshape wide value, i(iso) j(p) string
renvars value*, predrop(5)
gen T10B50 = top10/(bot50) 
gen T1B50  = top1/(bot50)  

merge 1:1 iso using `T10B50postregions', nogen
replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

drop top10 bot50 top1 

gen decreaseprepost = (T10B50-T10B50post)/T10B50

order year iso T10B50 T10B50post T1B50 decreaseprepost
sort T10B50

preserve 
keep year iso T10B50 T1B50
export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.4") sheetmod cell(A1) first(var)
restore 

keep year iso T10B50 T10B50post decreaseprepost


tempfile incomegaps
save `incomegaps', replace


* ---------------------------------------------------------------------------- *
* 				   	                  F1.5                                     *
*                         Income gaps across the world                         *
* ---------------------------------------------------------------------------- *

import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code shortname / iso isoname
keep iso isoname 
tempfile isoname 
save `isoname', replace

use if widcode == "rptinc992j" & (strpos(iso, "X") != 1 | iso == "XI") & (strpos(iso, "O") != 1 | iso == "OM") & ///
      (strpos(iso, "Q") != 1 | iso == "QA") & strpos(iso, "-") == 0 & iso != "WO" & ///
       year >2019 & p == "p0p100" using $wid, clear


merge m:1 iso using `isoname', nogen keep(master matched) keepusing(isoname)

bys iso: egen maxyear = max(year)
keep if year == maxyear 
rename value T10B50
keep year iso isoname T10B50


tempfile F1p5
save `F1p5', replace
drop iso 

order year isoname T10B50
export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.5") sheetmod cell(A1) first(var)

*** export data for R code generating maps
 
//pre-tax income
use "$wid", clear
keep if year >=2020 & inlist(widcode, "aptinc992j", "sptinc992j") & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100")
bys iso: egen maxyear = max(year)
keep if year==maxyear 
drop maxyear currency
save "$workdatawir/income_map.dta", replace 

//post-tax income
use "$wid", clear
keep if year>=2018 & inlist(widcode, "sdiinc992j") & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100")
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "top1"  if p == "p99p100"
replace p = "mid40" if p == "p50p90"
ren value share
bys iso: egen maxyear = max(year)
keep if year==maxyear 
drop maxyear year widcode currency
save "$workdatawir/posttax_map.dta", replace

//region index
import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code shortname / ISO name_region
keep ISO name_region 
save "$workdatawir/index_region.dta", replace


* ---------------------------------------------------------------------------- *
* 				   	             F1.6a & F1.6b                                 *
*               Top 10% and Bottom 50% shares across the world                 *
* ---------------------------------------------------------------------------- *

import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code shortname / iso isoname
keep iso isoname 
tempfile isoname 
save `isoname', replace

 
use if widcode == "sptinc992j" & (strpos(iso, "X")!=1 | iso == "XI") & ///
      (strpos(iso, "O")!=1 | iso == "OM") & (strpos(iso, "Q")!=1 | iso == "QA") & ///
       strpos(iso, "-")==0 & iso!="WO" & year >2019 & inlist(p, "p0p50", "p90p100") using $wid, clear
merge m:1 iso using `isoname', nogen keep(master matched) keepusing(isoname)

bys iso: egen maxyear = max(year)
keep if year == maxyear 
drop maxyear

replace value = value
drop widcode currency 
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
reshape wide value, i(isoname) j(p) string
renvars value*, predrop(5)

keep year isoname bot50 top10
order year isoname bot50 top10
export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.6") sheetmod cell(A1) first(var)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.6")
putexcel (C2:D178), nformat(percent)


* ---------------------------------------------------------------------------- *
* 			                          F1.7                                     *
*                  Density of global income distribution                       *
* ---------------------------------------------------------------------------- *

use if widcode == "npopul992i" & year == 2021 & inlist(iso, "QE", "QL", "XB", "XF", "XL", "XN", "XR", "XS") using $wid, clear
ren value npopul992i

egen totpop = total(npopul992i)
replace npopul992i = npopul992i/totpop

reshape wide npopul992i, i(year) j(iso) string
 
keep year npopul992i*
 
tempfile population
save `population', replace

use if inlist(widcode, "aptinc992j", "ahweal992j") & year == 2021 & inlist(iso, "QE", "QL", "XB", "XF", "XL", "XN", "XR", "XS") using $wid, clear
reshape wide value, i(iso year p) j(widcode) string
renvars value*, predrop(5)

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)

keep if inlist(n, 1000)

drop p p_max n currency
rename p_min p

sort iso year p

merge m:1 year using `population', nogen

range dinc 2.5 14 200
gen expdinc = exp(dinc)
gen loginc = log(aptinc992j)

gen aptinc992jm = aptinc992j/12

range dincm 0 11 200
gen expdincm = exp(dincm)
gen logincm = log(aptinc992jm)

range dweal 1.5 16 200
gen expdweal = exp(dweal)
gen logweal = log(ahweal992j)

foreach c in QE QL XB XF XL XN XR XS{
	kdensity loginc if iso == "`c'", gen(dinc`c' inc`c') at(dinc) nograph
	replace inc`c' = inc`c'*npopul992i`c'[1]
	
	kdensity logincm if iso == "`c'", gen(dincm`c' incm`c') at(dincm) nograph
	replace incm`c' = incm`c'*npopul992i`c'[1]
	
	kdensity logweal if iso == "`c'", gen(dweal`c' weal`c') at(dweal) nograph
	replace weal`c' = weal`c'*npopul992i`c'[1]
}


* monthly income
preserve 
keep dincm expdincm incm* 
renvars incm*, predrop(4)

replace expdincm = round(expdincm,.1)
replace expdincm = round(expdincm,1) if expdincm>=1
replace expdincm = round(expdincm,5) if expdincm>=20
replace expdincm = round(expdincm,10) if expdincm>=100
replace expdincm = round(expdincm,50) if expdincm>=200
replace expdincm = round(expdincm,100) if expdincm>=1000
replace expdincm = round(expdincm,500) if expdincm>=2000
replace expdincm = round(expdincm,1000) if expdincm>=10000
replace expdincm = round(expdincm,5000) if expdincm>=20000

label var XF "Sub-Saharan Africa" 
label var XL "Latin America" 
label var XN "MENA" 
label var XR "Russia & Central Asia" 
label var QE "Europe" 
label var XB "North America"
label var QL "East Asia"
label var XS "South & South-East Asia"

order d expd XB QE QL XR XN XL XS XF

line XB QE QL XR XN XL XS XF d

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.7") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.7")
putexcel (C3:J202), nformat(percent)
restore

tempfile density
save `density', replace


* ---------------------------------------------------------------------------- *
* 			                         F1.8                                      *
*                Geographical decomposition of income groups                   *
* ---------------------------------------------------------------------------- *


use if inlist(widcode, "aptinc992j") using $wid, clear
drop if missing(value)
keep if inlist(iso, "QE", "XB", "XR", "XL") ///
	  | inlist(iso, "XF", "XN", "CN", "IN")

// Parse percentiles
generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

replace p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)$")

replace p_max = 1000*100 if missing(p_max)

replace p_max = p_min + 1000 if missing(p_max) & inrange(p_min, 0, 98000)
replace p_max = p_min + 100  if missing(p_max) & inrange(p_min, 99000, 99800)
replace p_max = p_min + 10   if missing(p_max) & inrange(p_min, 99900, 99980)
replace p_max = p_min + 1    if missing(p_max) & inrange(p_min, 99990, 99999)

replace p = "p" + string(round(p_min/1e3, 0.001)) + "p" + string(round(p_max/1e3, 0.001)) if !missing(p_max)

// Keep only g-percentiles
generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency
rename p_min p
duplicates drop iso year p widcode, force
sort iso year widcode p

reshape wide value, i(iso year p) j(widcode) string

rename valueaptinc992j a

preserve 
	use if widcode == "xlceup999i" & inlist(iso, "CN", "IN") & year == 2021 using $wid, clear
	drop p widcode currency year
	rename value ppp
	
	tempfile ppp
	save `ppp'
restore 
merge m:1 iso using `ppp', nogen

replace a = a/ppp if !missing(ppp)
drop ppp

keep if year == 2021


// rank and compute new bracket for each region
preserve 
	use if widcode == "npopul992i" & year == 2021 using $wid, clear
	keep if inlist(iso, "QE", "XB", "XR", "XL") ///
		  | inlist(iso, "XF", "XN", "CN", "IN")
	drop p widcode currency
	rename value npopul992i
	
	tempfile npopul992i
	save `npopul992i'
restore 
merge m:1 iso year using `npopul992i', nogen
preserve 
	use "~/Dropbox/WIR2022/Data/work-data/OtherAsia-ptinc-hweal", clear
	keep year iso p ai npopul992i
	rename ai a
	keep if year == 2021
	
	tempfile OAsia
	save `OAsia'
restore
append using `OAsia'
drop n

// recompute size of the bracket
gen n=0.01
replace n=0.001 if p>=99000 & p<99900
replace n=0.0001 if p>=99900 & p<99990
replace n=0.00001 if p>=99990

generate pop = n*npopul992i

gsort year -a
bys year : generate rank = sum(pop)
bys year : replace rank = 1e5*(1 - rank/rank[_N])

egen bracket = cut(rank), at(0(1000)99000 99100(100)99900 99910(10)99990 99991(1)99999 200000)

collapse (sum) pop, by(year bracket iso)
bys year bracket : egen tot = sum(pop)
gen share = (pop/tot)*100
// gen share = (pop/tot)
keep iso year bracket share 
reshape wide share, i( year bracket) j(iso) string
ds year bracket, not
foreach l in `r(varlist)' {
replace `l' = 0 if missing(`l')
}

bys year : gen perc = _n
tsset year perc
ds year bracket, not

foreach x in `r(varlist)' {
	lowess `x' perc, bwidth(.125) gen(`x'_sm)
}


ren bracket p 


keep p share*sm
foreach i in XF CN IN XL XN XR QE XB OA{
	replace share`i'_sm = share`i'_sm/100
}
label var shareIN_sm "India" 
label var shareOA_sm "Other Asia" 
label var shareCN_sm "China" 
label var shareXF_sm "SSA" 
label var shareXL_sm "Latin America" 
label var shareXN_sm "MENA" 
label var shareXR_sm "Russia & Central Asia" 
label var shareQE_sm "Europe" 
label var shareXB_sm "North America & Oceania"

* just for correct graduations on graph
replace p = p/1000
replace p = . if mod(_n-1,10)!=0 

replace p = round(floor(p),.1) if p>99 & p<99.91 
replace p = round(floor(p*10)/10,.01) if p>=99.91 & p<99.991 
replace p = round(floor(p*100)/100,.001) if p>=99.991 

order p shareIN_sm shareOA_sm shareCN_sm shareXF_sm shareXL_sm shareXN_sm shareXR_sm shareQE_sm shareXB_sm    

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.8") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.8")
putexcel (B3:J129), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	            F1.9a & F1.9b                                  *
*         Redistribution and income inequality across world regions            *
* ---------------------------------------------------------------------------- *

use `incomegaps', clear

preserve 
	keep year iso T10B50 T10B50post 
	order year iso T10B50post T10B50
	gsort T10B50

	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.9a") sheetmod cell(A1) first(var)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.9a")
	putexcel (E2:E9), nformat(percent)
restore
preserve 
	keep year iso T10B50 decreaseprepost
	gsort T10B50
	
	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.9b") sheetmod cell(A1) first(var)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.9b")
	putexcel (D2:D9), nformat(percent)
restore


* ---------------------------------------------------------------------------- *
* 				   	                 F1.10                                     *
*              Inequality before and after taxes across the world              *
* ---------------------------------------------------------------------------- *

use "$workdatawir/posttax2018.dta", clear

gsort iso year -p
by iso year  : generate ts = sum(posttaxshr)
bys iso year : generate bs = 1 - ts

gen bot50 = bs if p == 0.5
gen top10 = ts if p == 0.9
bys iso year (bot50): replace bot50 = bot50[1]
bys iso year (top10): replace top10 = top10[1]

gen T10B50post = top10/(bot50/5)
duplicates drop iso, force

gen avgincome = mnninc2019EUR/npopul992i

keep iso T10B50post avgincome

merge 1:1 iso using `F1p5', nogen keep(matched)
drop if T10B50post == . 

gen decreaseprepost = (T10B50-T10B50post)/T10B50

drop iso year 
gen selected = isoname if strpos("China Japan Germany France United Kingdom Italy Argentina Brazil Chile Colombia Mexico Algeria Egypt Australia Canada USA Kenya Nigeria South Africa Indonesia India Russian Federation",isoname)!=0 

replace selected = "Russia" if isoname == "Russian Federation"
drop if isoname == "Oman"
order selected isoname T10B50 T10B50post avgincome decreaseprepost 

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.10") sheetmod cell(A1) first(var)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.10")
putexcel (F2:F152), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                 F1.11                                     *
*                   Wealth inequality across world regions                     *
* ---------------------------------------------------------------------------- *

use if widcode == "shweal992j" & inlist(iso, "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") & ///
       year == 2021 & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100") using $wid, clear
// merge m:1 iso using `regions', nogen keep(master matched) keepusing(regionWID)
replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

drop widcode currency 
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "mid40" if p == "p50p90"
replace p = "top1"  if p == "p99p100"
replace value = round(value, 0.0001) 

reshape wide value, i(iso) j(p) string
renvars value*, predrop(5)
label var bot50 "Bottom 50%"
label var mid40 "Middle 40%"
label var top10 "Top 10%"

gen T10B50 = top10/(bot50/5)

order year iso top1 bot50 mid40 top10 

preserve 
	keep year iso bot50 mid40 top10 
	sort top10

	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.11") sheetmod cell(A1) first(varl)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.11")
	putexcel (C2:E9), nformat(percent)
restore 

* ---------------------------------------------------------------------------- *
* 				   	                 F1.12                                     *
*        Concentration of wealth: the top 1% share across world regions        *
* ---------------------------------------------------------------------------- *

preserve
	keep year iso top1
	sort top1

	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.12") sheetmod cell(A1) first(var)
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.12")
	putexcel (C2:C9), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 				   	                 F1.13                                     *
*                       Wealth gaps across world regions                       *
* ---------------------------------------------------------------------------- *
 
use if widcode == "shweal992j" & inlist(iso, "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") & ///
       year == 2021 & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100") using $wid, clear
replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

drop widcode currency 
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "mid40" if p == "p50p90"
replace p = "top1"  if p == "p99p100"
replace value = round(value, 0.0001) 

reshape wide value, i(iso) j(p) string
renvars value*, predrop(5)
label var bot50 "Bottom 50%"
label var mid40 "Middle 40%"
label var top10 "Top 10%"

gen T10B50 = top10/(bot50/5)
keep year iso T10B50
sort T10B50

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.13") sheetmod cell(A1) first(var)


* ---------------------------------------------------------------------------- *
* 			                 F1.14a, F1.14b & F1.14c                           *
*      Top 10%, Bottom 50% and Middle 40% wealth shares across the world       *
* ---------------------------------------------------------------------------- *

import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code shortname / iso isoname
keep iso isoname 
tempfile isoname 
save `isoname', replace

use if widcode == "shweal992j" & (strpos(iso, "X")!=1 | iso == "XI") & ///
      (strpos(iso, "O")!=1 | iso == "OM") & (strpos(iso, "Q")!=1 | iso == "QA") & ///
       strpos(iso, "-")==0 & iso!="WO" & year >2019 & inlist(p, "p0p50", "p90p100", "p50p90", "p99p100") using $wid, clear
merge m:1 iso using `isoname', nogen keep(master matched) keepusing(isoname)

bys iso: egen maxyear = max(year)
keep if year == maxyear 
drop maxyear

drop widcode currency 
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "top1"  if p == "p99p100"
replace p = "mid40" if p == "p50p90"
reshape wide value, i(isoname) j(p) string
renvars value*, predrop(5)
gen T10B50 = top10/(bot50/5)

keep year isoname top1 bot50 mid40 top10 T10B50
order year isoname top1 bot50 mid40 top10 T10B50

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.14") sheetmod cell(A1) first(var)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.14")
putexcel (C2:F175), nformat(percent)

*** export data for R code generating maps

use "$wid", clear
keep if year==2021 & inlist(widcode, "shweal992j") & inlist(p, "p0p50", "p50p90", "p90p100", "p99p100")
replace p = "bot50" if p == "p0p50"
replace p = "top10" if p == "p90p100"
replace p = "top1" if p == "p99p100"
replace p = "mid40" if p == "p50p90"
bys iso: egen maxyear = max(year)
keep if year==maxyear 
drop maxyear currency widcode
order p iso year value
save "$workdatawir/wealth_map.dta", replace 



* ---------------------------------------------------------------------------- *
* 			                          F1.15                                    *
*            Density of global household wealth distribution                   *
* ---------------------------------------------------------------------------- *

use `density', replace
keep dweal expdweal weal* 
renvars weal*, predrop(4)

replace expdweal = round(expdweal,10) 
replace expdweal = round(expdweal,50) if expdweal>=200
replace expdweal = round(expdweal,100) if expdweal>=1000
replace expdweal = round(expdweal,500) if expdweal>=2000
replace expdweal = round(expdweal,5000) if expdweal>=10000
replace expdweal = round(expdweal,50000) if expdweal>=100000
replace expdweal = round(expdweal,500000) if expdweal>=1000000

label var XF "Sub-Saharan Africa" 
label var XL "Latin America" 
label var XN "MENA" 
label var XR "Russia & Central Asia" 
label var QE "Europe" 
label var XB "North America"
label var QL "East Asia"
label var XS "South & South-East Asia"

order d expd XB QE XR XN XL XF QL XS

line XB QE XR XN XL XF QL XS d

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.15") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.15")
putexcel (C3:J202), nformat(percent)


* ---------------------------------------------------------------------------- *
* 			                         F1.16                                     *
*                Geographical decomposition of wealth groups                   *
* ---------------------------------------------------------------------------- *


use if inlist(widcode, "ahweal992j","xlceup999i","npopul992i") & year == 2021 using $wid, clear
drop if missing(value)
keep if inlist(iso, "QE", "XB", "XR", "XL") ///
	  | inlist(iso, "XF", "XN", "CN", "IN")
reshape wide value, i(iso year p) j(widcode) string
renvars value*, predrop(5)

bys iso year (xlceup999i): replace xlceup999i = xlceup999i[1]
bys iso year (npopul992i): replace npopul992i = npopul992i[1]

// Parse percentiles
generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

replace p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)$")

replace p_max = 1000*100 if missing(p_max)

replace p_max = p_min + 1000 if missing(p_max) & inrange(p_min, 0, 98000)
replace p_max = p_min + 100  if missing(p_max) & inrange(p_min, 99000, 99800)
replace p_max = p_min + 10   if missing(p_max) & inrange(p_min, 99900, 99980)
replace p_max = p_min + 1    if missing(p_max) & inrange(p_min, 99990, 99999)

replace p = "p" + string(round(p_min/1e3, 0.001)) + "p" + string(round(p_max/1e3, 0.001)) if !missing(p_max)

// Keep only g-percentiles
generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency
rename (p_min ahweal992j) (p a)
duplicates drop iso year p, force
sort iso year p

replace a = a/xlceup999i if xlceup999i!=1
drop xlceup999i

// rank and compute new bracket for each region

preserve 
	use "~/Dropbox/WIR2022/Data/work-data/OtherAsia-ptinc-hweal", clear
	keep year iso p aw npopul992i
	rename aw a
	keep if year == 2021
	
	tempfile OAsia
	save `OAsia'
restore
append using `OAsia'

bys p (n): replace n = n[1]
replace n = n/1e5
generate pop = n*npopul992i

gsort year -a
bys year : generate rank = sum(pop)
// replace p = p/10
bys year : replace rank = 1e5*(1 - rank/rank[_N])

egen bracket = cut(rank), at(0(1000)99000 99100(100)99900 99910(10)99990 99991(1)99999 200000) 

collapse (sum) pop, by(year bracket iso)
// bys iso year : egen average_top = mean(pop) if inrange(bracket, 99990, 99999)
preserve
	keep if inrange(bracket, 99990, 99999)
	gsort iso year bracket
	by iso year : generate n = cond(_N == _n, 100000 - bracket, bracket[_n + 1] - bracket)

	collapse (mean) pop [pw=n], by(iso)
	gen year = 2021
	gen bracket = 99990
	
	tempfile aggregated_top
	save `aggregated_top'
restore
drop if inrange(bracket, 99990, 99999) 
append using `aggregated_top'
gsort iso year bracket

bys year bracket : egen tot = sum(pop)
gen share = (pop/tot)*100

keep iso year bracket share 
reshape wide share, i( year bracket) j(iso) string
ds year bracket, not
foreach l in `r(varlist)' {
	replace `l' = 0 if missing(`l')
}

bys year : gen perc = _n
tsset year perc
ds year bracket, not

foreach x in `r(varlist)' {
	lowess `x' perc, bwidth(.125) gen(`x'_sm)
}

ren bracket p 

keep p share*sm
foreach i in XF CN IN XL XN XR QE XB OA{
	replace share`i'_sm = share`i'_sm/100
}
label var shareIN_sm "India" 
label var shareOA_sm "Other Asia" 
label var shareCN_sm "China" 
label var shareXF_sm "SSA" 
label var shareXL_sm "Latin America" 
label var shareXN_sm "MENA" 
label var shareXR_sm "Russia & Central Asia" 
label var shareQE_sm "Europe" 
label var shareXB_sm "North America & Oceania"

* just for correct graduations on graph
replace p = p/1000
replace p = . if mod(_n-1,10)!=0 

replace p = round(floor(p),.1) if p>99 & p<99.91 
replace p = round(floor(p*10)/10,.01) if p>=99.91 & p<99.991 
replace p = round(floor(p*100)/100,.001) if p>=99.991 

order p shareIN_sm shareOA_sm shareCN_sm shareXF_sm shareXL_sm shareXN_sm shareXR_sm shareQE_sm shareXB_sm    

export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-F1.16") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-F1.16")
putexcel (B3:J129), nformat(percent)




* ---------------------------------------------------------------------------- *
* 			                        FB1.1                                      *
*           Impact of the Covid-19 recession across world regions              *
* ---------------------------------------------------------------------------- *


import excel "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
ren (code shortname region5) (iso isoname region)
tempfile regions
save `regions', replace

use if inlist(widcode, "mnninc999i", "npopul992i", "npopul999i", "xlceux999i", "xlceup999i") & ///
inrange(year, 2019, 2021) using $wid, clear

reshape wide value, i(iso year) j(widcode) string 
renvars value*, predrop(5)
drop currency p

merge m:1 iso using `regions', keepusing(isoname region) keep(master matched) nogen


gen xlceup2021 = xlceup if year==2021
bys iso (xlceup2021): replace xlceup2021 = xlceup2021[1]
replace xlceup999i = xlceup2021 
drop xlceup2021

gen mnninc_ppp = mnninc999i/xlceup

collapse (sum) npopul999i npopul992i mnninc_ppp if region!="", by(year region)

preserve 
	collapse (sum) npopul999i npopul992i mnninc_ppp, by(year)
	gen region = "World"
	tempfile world 
	save `world', replace
restore 

append using `world'

sort region year
by region: gen rec1920 = (mnninc_ppp-mnninc_ppp[_n-1]) if _n==2
by region: gen rec1921 = (mnninc_ppp-mnninc_ppp[_n-2]) if _n==3

gen rec1920_w = rec1920[26]
gen rec1921_w = rec1921[27]

by region: gen share_rec1920 = rec1920/rec1920_w if _n==2

by region: gen pct_rec1920 = (mnninc_ppp-mnninc_ppp[_n-1])/mnninc_ppp[_n-1] if _n==2
by region: gen pct_rec1921 = (mnninc_ppp-mnninc_ppp[_n-2])/mnninc_ppp[_n-2] if _n==3

replace region = "MENA" if strpos(region, "Middle")!=0

preserve 
	keep region year pct_rec1920 pct_rec1921
	bys region (pct_rec1920): replace pct_rec1920 = pct_rec1920[1]
	drop if pct_rec1921 == . 

	label var region "Region name"
	label var pct_rec1920 "NI growth rate (%)"
	label var pct_rec1921 "NI growth rate (%)"

	gsort -pct_rec1920


	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-FB1.1", replace) firstrow(varl) sheetmod cell(A1) 
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-FB1.1")
	putexcel (C2:D10), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 			                        FB1.2                                      *
*          Share of 2020 global recession captured by world region             *
* ---------------------------------------------------------------------------- *


preserve 
	keep region share_rec1920
	label var share_rec "Share of recession"
	drop if share_rec==.
	drop if region == "World"
	gsort -share_rec1920

	export excel using "$export/WIR2022-Chapter1.xlsx", sheet("data-FB1.2", replace) firstrow(varl) sheetmod cell(A1) 
	putexcel set "$export/WIR2022-Chapter1.xlsx", modify sheet("data-FB1.2")
	putexcel (B2:B10), nformat(percent)
restore 




