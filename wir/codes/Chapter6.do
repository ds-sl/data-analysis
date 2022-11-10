// ---------------------------- // 
*		CHAPTER 6
// ---------------------------- //


* ---------------------------------------------------------------------------- *
* 				   	            F6.1, F6.2 & T6.1                              *
*                         Global emissions since 1850                          *
* ---------------------------------------------------------------------------- * 

* See data from Chancel (2021).


* ---------------------------------------------------------------------------- *
* 				   	                  T6.2                                     *
*                        Global per capita carbon budget                       *
* ---------------------------------------------------------------------------- * 

* See data from Chancel (2021).


* ---------------------------------------------------------------------------- *
* 				   	                  F6.3a                                    *
*                        Global per capita carbon budget                       *
* ---------------------------------------------------------------------------- *

* territorial emissions 
use "$workdatawir/carbon.dta", clear
keep iso year ntghg
keep if year == 2019
bys iso: keep if _n == 1
ren ntghg entghg999i

tempfile tghg
save `tghg', replace

use if inlist(widcode, "npopul999i", "lpfghg999i") & inlist(year,2020,2019) & p == "p0p100" using $wid, clear  
reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
ren lpfghg999i enfghg999i
bys iso (year): replace enfghg999i = enfghg999i[1]
bys iso (year): replace npopul999i = npopul999i[1]
keep if year == 2019

merge 1:1 iso year using `tghg', nogen 

joinby iso using "$workdatawir/country-codes.dta", unmatched(master) 
drop if v2 == "" | v2 == "the World"

replace enfghg = enfghg*npop/1e6

collapse (sum) enfghg entghg (sum) npop, by(regionWID year)

drop if regionWID==""

gen fcap=enfghg*1e6/npop
gen tcap=entghg*1e6/npop

gsort - year regionWID
drop if regionWID==""

keep regionWID tcap fcap 

expand 2 if region == "SSEAsia"

replace region = "World" if _n == _N
replace fcap   = 6.56    if _n == _N
replace tcap   = 6.56    if _n == _N


gen id = 1 
replace id = 0 if region == "World"
expand 3 if region == "World", gen(new)
gsort id -new
replace region = "sust1C" if _n == 2
replace region = "sust2C" if _n == 3
replace id = 1 if region != "World"

replace fcap = 1.1 if _n == 2
replace fcap = 3.4 if _n == 3

sort id fcap 


drop new

gen pctavg = fcap/fcap[1]
gen pct2c  = fcap/3.4
gen diff   = fcap/tcap-1

replace regionWID = "Sustainable level (1.5°C)" if regionWID == "sust1C"
replace regionWID = "Sustainable level (2°C)"   if regionWID == "sust2C"
replace regionWID = "Sub-Saharan Africa" 		if regionWID == "SSAfrica"
replace regionWID = "South South-East Asia" 	if regionWID == "SSEAsia"
replace regionWID = "Latin America" 			if regionWID == "LatinAmerica"
replace regionWID = "North America" 			if regionWID == "NorthAmerica"
replace regionWID = "East Asia" 				if regionWID == "EastAsia"
replace regionWID = "Russia & Central Asia"     if regionWID == "RussiaCentralAsia"


preserve 
keep region fcap
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.3") sheetmod cell(A2) first(varl)
restore 

* ---------------------------------------------------------------------------- *
* 				   	                  F6.3b                                    *
*                        Historical and current emissions                      *
* ---------------------------------------------------------------------------- *

* See data from Chancel (2021).


* ---------------------------------------------------------------------------- *
* 				   	                  T6.3                                     *
*                   Average per capita emissions by world region               *
* ---------------------------------------------------------------------------- *

preserve 
keep region fcap pctavg pct2c
drop if strpos(region,"Sust")!=0

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-T6.3") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-T6.3")
putexcel (C3:C11), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 				   	                  T6.4                                     *
*                  Carbon footprints vs territorial emissions                  *
* ---------------------------------------------------------------------------- *

preserve 
keep region fcap tcap diff
drop if strpos(region,"Sust")!=0

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-T6.4") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-T6.4")
putexcel (D3:D11), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 				                 F6.4a & F6.4b                                 *
*                    Emissions by group across world regions                   *
* ---------------------------------------------------------------------------- *

use if inlist(p,"p0p50","p50p90","p90p100") & widcode=="apfghg999i" & iso!="WO" & (iso!="OAsia" & iso != "QP") & year==2019 using "$workdatawir/regional-aggregates-ghg.dta", clear
reshape wide value, i(iso year) j(p) string
renvars value*, predrop(5)

ren (p0 p50 p90) (bot50 mid40 top10)
keep iso bot50 mid40 top10
sort top10

replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        	if iso == "XB"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"            	if iso == "QL"

renvars bot50 mid40 top10, prefix("value")
reshape long value, i(iso) j(group "bot50" "mid40" "top10")
replace group = "Bottom 50%" if group == "bot50" 
replace group = "Middle 40%" if group == "mid40" 
replace group = "Top 10%"    if group == "top10" 

bys iso: gen top10 = value[3]
sort top10 group
drop top10

replace iso = "" if group != "Bottom 50%"

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.4") sheetmod cell(A2) first(varl)


* ---------------------------------------------------------------------------- *
* 				                 F6.5a & F6.5b                                 *
*         Global carbon inequality: emission levels and shares by group        *
* ---------------------------------------------------------------------------- *

use if inlist(p,"p0p100","p0p20","p20p50", "p0p50","p50p90","p90p100","p99p100") & inlist(widcode,"apfghg999i","spfghg999i","tpfghg999i") & iso=="WO" & year==2019 using "$workdatawir/regional-aggregates-ghg.dta", clear
reshape wide value, i(iso p) j(widcode) string
renvars value*, predrop(5)

keep p a s t

replace p = "Full population" 	if p == "p0p100"
replace p = "Bottom 50%" 		if p == "p0p50"
replace p = "Middle 40%" 		if p == "p50p90"
replace p = "Top 10%" 			if p == "p90p100"
replace p = "Top 1%" 			if p == "p99p100"

 
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.5") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.5")
putexcel (C3:C7), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  T6.5                                     *
*                      Carbon emissions by emitters group                      *
* ---------------------------------------------------------------------------- *

use if widcode == "npopul999i" & iso == "WO" & inlist(year,1990,2021) using $wid, clear
ren value npopul999i 
keep year npopul999i

tempfile worldpop
save `worldpop', replace

use if strpos("p0p100 p0p50 p20p100 p30p100 p50p90 p90p100 p99p100 p99.9p100 p99.99p100 p99.999p100 p50p51", p) & p != "p0p1" & inlist(widcode, "apfghg999i", "tpfghg999i", "spfghg999i") & iso=="WO" & inlist(year,1990,2019) using "$workdatawir/regional-aggregates-ghg.dta", clear
reshape wide value, i(iso year p) j(widcode) string
renvars value*, predrop(5)
ren (apfghg999i tpfghg999i spfghg999i) (fcap t s)

bys iso year (p): replace t = t[5] if p == "p50p90"
bys iso year (p): replace t = 0    if p == "p0p50"
bys iso year (p): replace t = 0    if p == "p20p100"

bys iso year (p): replace s = 1 - s if p == "p20p100"
bys iso year (p): replace s = s[2] - s[3] if p == "p30p100"
drop if p == "p50p51"

bys iso year (p): replace fcap = s*100*fcap[1]/20 if p == "p20p100"
bys iso year (p): replace fcap = s*100*fcap[1]/30 if p == "p30p100"

replace p = "p0p20"  if p == "p20p100"
replace p = "p30p50" if p == "p30p100"

replace year = 2021 if year == 2019
merge m:1 year using `worldpop', nogen 

reshape wide fcap s t npop, i(p) j(year)  
keep p fcap* npop* s* t* 

sort fcap1990
gen id = _n
replace id = 0 if p == "p0p100"
replace id = 0.5 if p == "p0p50"
sort id

cap drop pop
gen pop = 1-real(substr(p,2,length(p)-5))/100
replace pop = .2 if p == "p0p20"
replace pop = .3 if p == "p30p50"
replace pop = .4 if p == "p50p90"
replace pop = .5 if p == "p0p50"
gen ftot1990 = fcap1990*pop*npopul999i1990/1e9
gen ftot2021 = fcap2021*pop*npopul999i2021/1e9

gen pop2021  = pop*npopul999i2021/1e6

gen g = ftot2021/ftot1990 - 1
gen gcap = fcap2021/fcap1990 - 1
gen sg = (ftot2021-ftot1990)/(ftot2021[1]-ftot1990[1])

preserve 
keep p pop2021 fcap2021 t2021 s2021
order p pop2021 fcap2021 t2021 s2021

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-T6.5") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-T6.5")
putexcel (E3:E10), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 				   	                  T6.6                                     *
*                        Emissions growth and inequality                       *
* ---------------------------------------------------------------------------- *

preserve 
drop if inlist(p, "p0p20", "p30p50")
keep p fcap* ftot* gcap g sg
order p fcap* ftot* gcap g sg

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-T6.6") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-T6.6")
putexcel (F3:H10), nformat(percent)
restore 


* ---------------------------------------------------------------------------- *
* 				   	                  F6.6                                     *
*                             Carbon elephant curve                            *
* ---------------------------------------------------------------------------- *

use if inlist(widcode,"apfghg999i") & iso=="WO" & inlist(year,1980,1990,2019) using "$workdatawir/regional-aggregates-ghg.dta", clear

ren value apfghg999i
keep year p apfghg999i

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)

keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max
rename p_min p

reshape wide a, i(p) j(year)

gen g=apfghg999i2019/apfghg999i1990-1

replace g = g[4] if _n<4

lowess g p ,bwidth(.1) gen(sg)

gen perc = _n

replace p=p/1000

keep p sg
replace p = 99.9 if p > 99 & p <= 99.9
replace p = 99.99 if p > 99.9 & p <= 99.99
replace p = 99.999 if p > 99.99 

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.6") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.6")
putexcel (B3:B129), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F6.7                                     *
*                     Top 1% and Bottom 50% emission shares                    *
* ---------------------------------------------------------------------------- *

use if inlist(p,"p0p50","p99p100") & inlist(widcode,"spfghg999i") & iso=="WO" & inrange(year,1990,2019) using "$workdatawir/regional-aggregates-ghg.dta", clear

expand 2 if year == 2019, gen(new)
replace year = 2020 if new == 1
drop new 

reshape wide value, i(year) j(p) string
renvars value*, predrop(5)

ren (p0 p99) (bot50 top1)
keep year bot50 top1

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.7") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.7")
putexcel (B3:C33), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F6.8                                     *
*                 Within and between countries carbon inequality               *
* ---------------------------------------------------------------------------- *

clear all
tempfile combined 
save `combined', replace emptyok

use if inrange(year,1990,2019) & inlist(widcode,"lpfghg999i","npopul999i") using $wid, clear
reshape wide value, i(iso year p) j(widcode) string 
renvars value*, predrop(5)
bys iso year (npopul999i): replace npopul999i = npopul999i[1]

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency 
rename p_min p

gen pop = n*npopul999i/1e5
encode iso, gen(iso_)
replace lpfghg999i = 0 if lpfghg999i < 0 

forvalue y = 1990/2019{
	preserve 
	keep if year == `y'
	
	theildeco lpfghg999i [aw = pop], byg(iso_)
	
	gen theil = `r(Theil)'
	gen between = `r(between_T)'
	gen within = `r(within_T)'
	
	duplicates drop between, force
	replace between = between/theil
	replace within = within/theil
	
	keep year between within
	
	append using `combined'
	save `combined', replace
	restore
}

use `combined', clear

sort year 

label var between "Between countries"
label var within  "Within countries"

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.8") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.8")
putexcel (B3:C32), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F6.9a                                    *
*               Geographical composition of global emitter groups              *
* ---------------------------------------------------------------------------- *

use if inlist(widcode, "apfghg999i") & inlist(year,2019) & inlist(iso,"XF","QE","XB","XL","XN","XR","OAsia") using "$workdatawir/regional-aggregates-ghg.dta", clear  

ren value lpfghg999i
keep iso p lpfghg999i

tempfile regions 
save `regions', replace

use if inlist(widcode, "lpfghg999i") & inlist(year,2019) & inlist(iso,"IN","CN") using $wid, clear  

ren value lpfghg999i
keep iso p lpfghg999i

append using `regions'

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)

keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max 
rename p_min p

sort iso p


preserve 
	use if widcode == "npopul999i" & year == 2019 using $wid, clear
	keep if inlist(iso, "QE", "XB", "XR", "XL", "XS") ///
		  | inlist(iso, "XF", "XN", "CN", "IN", "QL")
	drop p widcode currency year
	rename value npopul999i
	
	tempfile npopul999i
	save `npopul999i'
restore 
merge m:1 iso using `npopul999i', nogen


gen popchinaindia = npop if (iso == "CN" | iso == "IN") & p == 0 
gen popasia 	  = npop if iso == "XS" | iso == "QL"

egen popasia_       = total(popasia)
egen popchinaindia_ = total(popchinaindia)

replace npopul = popasia_-popchinaindia_ if iso == "OAsia"
drop pop*
drop if lpfghg == . 


generate pop = n*npopul999i

gsort -lpfghg
generate rank = sum(pop)
replace rank = 1e5*(1 - rank/rank[_N])

egen bracket = cut(rank), at(0(1000)99000 99100(100)99900 99910(10)99990 99991(1)99999 200000)

collapse (sum) pop, by(bracket iso)
bys bracket : egen tot = sum(pop)
gen share = (pop/tot)*100
keep iso bracket share 
reshape wide share, i(bracket) j(iso) string
ds bracket, not
foreach l in `r(varlist)' {
replace `l' = 0 if missing(`l')
}


// Smooth China and p30-p45
gen a=(shareCN+shareOAsia)/2
replace shareCN=a if bracket>=30000 & bracket<45000
replace shareOAsia=a if bracket>=30000 & bracket<45000
drop a


gen perc = _n
tsset perc
ds bracket, not

set graphics off
foreach x in `r(varlist)' {
	lowess `x' perc, bwidth(.1) gen(`x'_sm)
	replace `x'_sm = `x' if bracket<2000
}
set graphics on

label var shareXF_sm "Sub-Saharan Africa" 
label var shareCN_sm "China" 
label var shareIN_sm "India" 
label var shareXL_sm "Latin America" 
label var shareXN_sm "MENA" 
label var shareXR_sm "Russia & Central Asia" 
label var shareQE_sm "Europe" 
label var shareXB_sm "North America & Oceania"
label var shareOAsia_sm "Other Asia"

ren bracket p 

keep p share*sm
foreach i in XF CN IN XL XN XR QE XB OAsia{
	replace share`i'_sm = share`i'_sm/100
}

* just for correct graduations on graph
replace p = p/1000
replace p = . if mod(_n-1,10)!=0 

replace p = round(floor(p),.1) if p>99 & p<99.91 
replace p = round(floor(p*10)/10,.01) if p>=99.91 & p<99.991 
replace p = round(floor(p*100)/100,.001) if p>=99.991 

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.9a") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.9a")
putexcel (B3:J128), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F6.9b                                    *
*               Density of the global carbon emission distribution             *
* ---------------------------------------------------------------------------- *

use if widcode == "npopul999i" & year == 2019 & inlist(iso, "QE", "QL", "XB", "XF", "XL", "XN", "XR", "XS") using $wid, clear
ren value npopul999i

egen totpop = total(npopul999i)
replace npopul999i = npopul999i/totpop

reshape wide npopul999i, i(year) j(iso) string
 
keep year npopul999i*
 
tempfile population
save `population', replace

use if !inlist(iso, "OAsia", "WO", "QP") & widcode == "apfghg999i" & year == 2019 using "$workdatawir/regional-aggregates-ghg.dta", clear
ren value apfghg999i

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)

keep if inlist(n, 1000)

drop p p_max widcode n
rename p_min p

sort iso year p

merge m:1 year using `population', nogen

range d -4 6 150
gen expd = exp(d)
gen loga = log(apfghg999i)

foreach c in QE QL XB XF XL XN XR XS{
	kdensity loga if iso == "`c'", gen(d`c' x`c') at(d) nograph
	replace x`c' = x`c'*npopul999i`c'[1]
}

keep d expd x* 
renvars x*, predrop(1)

label var XF "Sub-Saharan Africa" 
label var XL "Latin America" 
label var XN "MENA" 
label var XR "Russia & Central Asia" 
label var QE "Europe" 
label var XB "North America & Oceania"
label var QL "East Asia"
label var XS "South & South-East Asia"

order d expd XB XR QE XL XN QL XS XF

export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.9b") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter6.xlsx", modify sheet("data-F6.9b")
putexcel (C3:J152), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                F6.10abcd                                  *
*        Emissions reduction targets in the US, France, India and China        *
* ---------------------------------------------------------------------------- *

use if inlist(widcode, "lpfghg999i") & inlist(year,2019) & inlist(iso,"IN","CN","FR","US") & inlist(p,"p0p100","p0p50","p50p90","p90p100") using $wid, clear  

ren value a 
keep iso p a 

gen proj = 4.8 
replace proj = 3.7 if iso == "IN"
replace proj = 10 if iso == "US"
replace proj = 10 if iso == "CN"
bys iso: gen change = proj-a
bys iso: gen pctchange = 100*(proj/a-1)
gen label = "Increase: "+string(round(change,.1))+" tonnes per capita ("+string(round(pctchange,1))+"%)"
replace label = "Reduction: "+substr(string(round(change,.1)),2,length(string(round(change,.1)))-1)+" tonnes per capita ("+string(round(pctchange,1))+"%)" if change<0
bys iso (p): gen ghg = "Average GHG emissions: "+string(round(a[1],.1))+" tonnes per person per year"
bys iso (p): gen projlabel = "On average, emissions are projected to increase by "+string(round(change[1],.1))+" tonnes per capita by 2030"
bys iso (p): replace projlabel = "On average, emissions are projected to increase by "+substr(string(round(change[1],.1)),2,length(string(round(change[1],.1)))-1)+" tonnes per capita by 2030" if change[1]<0

// reshape wide a proj change pctchange label ghg projlabel, i(p) j(iso) string

replace p = "Full population" 	if p == "p0p100"
replace p = "Bottom 50%" 		if p == "p0p50"
replace p = "Middle 40%" 		if p == "p50p90"
replace p = "Top 10%" 			if p == "p90p100"
 
label var a 2021
label var proj 2030

 
preserve
keep if iso == "US"
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.10a") sheetmod cell(A2) first(varl)
restore 

preserve
keep if iso == "FR"
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.10b") sheetmod cell(A2) first(varl)
restore 

preserve
keep if iso == "IN"
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.10c") sheetmod cell(A2) first(varl)
restore 

preserve
keep if iso == "CN"
export excel using "$export/WIR2022-Chapter6.xlsx", sheet("data-F6.10d") sheetmod cell(A2) first(varl)
restore 


* ---------------------------------------------------------------------------- *
* 				   	               T6.7 & T6.8                                 *
*              Climate policies and pollution top-up on wealth tax             *
* ---------------------------------------------------------------------------- * 

* See data from Chancel (2021).




