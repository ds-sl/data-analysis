// ---------------------------- // 
*		CHAPTER 3
// ---------------------------- //


	
* ---------------------------------------------------------------------------- *
* 				   	                  F3.1                                     *
*                Global, public and private wealth-income ratios               *
* ---------------------------------------------------------------------------- *

use if iso == "WO" & inrange(year, 1995, 2020) using $wid, clear
keep if inlist(widcode, "wwealn999i", "wwealp999i", "wwealg999i") 
drop currency p iso 

reshape wide value, i(year) j(widcode) string
renvars value*, predrop(5)


export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.1") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.1")
putexcel (B3:D28), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                 F3.1supp                                  *
*                      National wealth across world regions                    *
* ---------------------------------------------------------------------------- *

use if (inlist(iso, "XF", "QE", "QP", "XL") | inlist(iso, "XN", "XR", "XS", "QL")) using $wid, clear
keep if inlist(widcode, "wwealn999i", "wwealp999i", "wwealg999i") & inrange(year, 1995, 2020) 
drop currency p
reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
renvars wwealn999i wwealg999i wwealp999i \ KYnweal KYgweal KYpweal

replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"           if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "Eastern Asia"            if iso == "QL"

replace iso = "LatinAmerica"      if iso == "Latin America"
replace iso = "NorthAmerica"      if iso == "North America"
replace iso = "RussiaCentralAsia" if iso == "Russia & Central Asia"
replace iso = "SSAfrica"          if iso == "Sub-Saharan Africa"
replace iso = "SSEAsia"           if iso == "South & South East Asia"
replace iso = "EastAsia"          if iso == "Eastern Asia"

reshape wide KYnweal KYgweal KYpweal, i(year) j(iso) string

order year KYnwealEastAsia KYnwealEurope KYnwealLatinAmerica KYnwealMENA KYnwealNorthAmerica KYnwealRussiaCentralAsia KYnwealSSAfrica KYnwealSSEAsia KYgwealEastAsia KYgwealEurope KYgwealLatinAmerica KYgwealMENA KYgwealNorthAmerica KYgwealRussiaCentralAsia KYgwealSSAfrica KYgwealSSEAsia KYpwealEastAsia KYpwealEurope KYpwealLatinAmerica KYpwealMENA KYpwealNorthAmerica KYpwealRussiaCentralAsia KYpwealSSAfrica KYpwealSSEAsia 


export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.1supp") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.1supp")
putexcel (B3:Y28), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                   F3.2                                    *
*  The rise of private wealth and decline of public wealth in rich countries   *
* ---------------------------------------------------------------------------- *

import excel "$workdatawir/WIR2022TablesFigures-temp.xlsx", firstrow clear sheet("data-F3.2")

ren Year year
renvars Germany Spain France UK Japan USA, prefix("KYgweal")
renvars *private, prefix("KYpweal")
renvars *private, postdrop(7)
renvars *Germany, postsub("Germany" "DE")
renvars *Spain,   postsub("Spain" "ES")
renvars *France,  postsub("France" "FR")
renvars *UK,      postsub("UK" "GB")
renvars *Japan,   postsub("Japan" "JP")
renvars *USA,     postsub("USA" "US")

ds year, not
local allvar `r(varlist)'
di `allvar'

foreach x of local allvar{
	replace `x'= `x'/100
}

destring(year), replace force
keep if year<1995
tempfile pre95KY
save `pre95KY', replace

use if inlist(widcode, "wwealp999i", "wwealg999i") & inrange(year, 1995, 2020) using $wid, clear
keep if inlist(iso, "DE", "ES", "FR", "GB", "US", "JP", "NO")
drop currency p
reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
renvars wwealg999i wwealp999i \ KYgweal KYpweal

keep iso year KYgweal KYpweal 
replace KYgweal = KYgweal 
replace KYpweal = KYpweal 

bys year: egen gwealAVGRICH = mean(KYgweal)
bys year: egen pwealAVGRICH = mean(KYpweal)

reshape wide KYgweal KYpweal, i(year) j(iso) string

append using `pre95KY'
sort year

label var KYgwealDE Germany 
label var KYgwealES Spain 
label var KYgwealFR France
label var KYgwealGB UK 
label var KYgwealJP Japan 
label var KYgwealNO Norway 
label var KYgwealUS USA

label var KYpwealDE "Germany (private)"
label var KYpwealES "Spain (private)" 
label var KYpwealFR "France (private)"
label var KYpwealGB "UK (private)" 
label var KYpwealJP "Japan (private)" 
label var KYpwealNO "Norway (private)" 
label var KYpwealUS "USA (private)"

drop *NO

export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.2") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.2")
putexcel (B3:O53), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F3.3                                     *
*              Private wealth-income ratios in emerging countries              *
* ---------------------------------------------------------------------------- *

import excel "$workdatawir/WIR2022TablesFigures-temp.xlsx", firstrow clear sheet("data-F3.3") cellrange(A1:I17) 
drop if missing(year)

renvars Brazil China India Russia, prefix("KYgweal")
renvars *private, prefix("KYpweal")
renvars *private, postdrop(7)
renvars *Brazil , postsub("Brazil" "BR")
renvars *China  , postsub("China" "CN")
renvars *India  , postsub("India" "IN")
renvars *Russia , postsub("Russia" "RU")

ds year, not
local allvar `r(varlist)'
di `allvar'

foreach x of local allvar{
	replace `x'= `x'/100
}

destring(year), replace force
keep if year<1995
tempfile pre95KY
save `pre95KY', replace

use if inlist(widcode, "wwealp999i", "wwealg999i") & inrange(year, 1995, 2020) using $wid, clear
keep if inlist(iso, "BR", "CN", "IN", "RU")
drop currency p
reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
renvars wwealg999i wwealp999i \ KYgweal KYpweal

keep iso year KYgweal KYpweal 

bys year: egen gwealAVGPOOR = mean(KYgweal)
bys year: egen pwealAVGPOOR = mean(KYpweal)

reshape wide KYgweal KYpweal, i(year) j(iso) string

append using `pre95KY'
sort year

label var KYgwealBR Brazil 
label var KYgwealCN China 
label var KYgwealIN India 
label var KYgwealRU Russia 

label var KYpwealBR "Brazil (private)"
label var KYpwealCN "China (private)"
label var KYpwealIN "India (private)"
label var KYpwealRU "Russia (private)" 


export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.3") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.3")
putexcel (B3:K44), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F3.4                                     *
*                   Decline of public wealth across the world                  *
* ---------------------------------------------------------------------------- *

import excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.2") first clear
drop *AVGRICH

tempfile rich
save `rich', replace

import excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.3") first clear
drop *AVGPOOR

merge 1:1 year using `rich', nogen 
sort year 

foreach c in Brazil China India Russia Germany Spain France UK Japan USA{
	gen `c'public = `c'/(`c'private+`c')
}

keep year *public
drop if year ==. 
renvars *public, postdrop(6)


export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.4") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.4")
putexcel (B3:K53), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                   F3.5                                    *
*                      Public wealth across world regions                      *
* ---------------------------------------------------------------------------- *

use if inrange(year,2010,2020) & ///
      (inlist(iso, "CN", "XF", "QE", "QP", "XL") | inlist(iso, "XN", "XR", "XS", "JP")) ///
	 & inlist(widcode, "mgweal999i", "mnweal999i") using $wid, clear 

reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
gen KYgweal_ = mgweal999i/mnweal999i
keep iso year KY

bys iso: egen KYgweal = mean(KYgweal_)
drop KYgweal_ year
duplicates drop iso, force

replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "China"               	if iso == "CN"
replace iso = "Japan"               	if iso == "JP"

sort KYgweal 
ren iso regionWID

export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.5") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.5")
putexcel (B3:B11), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F3.6                                     *
*                      Net foreign assets across the world                     *
* ---------------------------------------------------------------------------- *

use if inlist(widcode, "mnninc999i", "xlceup999i") & inrange(year,1995,2021) using $wid, clear
reshape wide value, i(iso year) j(widcode) string 
renvars value*, predrop(5)

bys iso (year): replace xlceup999i = xlceup999i[_N]
gen mnninc999PPP = mnninc999i/xlceup999i

keep iso year mnninc999PPP
tempfile natinc
save `natinc', replace

import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code region5 / iso region
keep iso region 
tempfile regions 
save `regions', replace

use "$workdatawir/macro-2021.dta", clear
keep if variable == "nwnxa" 
ren value nwnxa 
keep iso year nwnxa
replace iso = "KS" if iso == "KV"

merge 1:1 iso year using `natinc', keep(master matched) nogen 
replace nwnxa = nwnxa * mnninc999PPP

preserve 
keep if iso == "CN"

gen China = nwnxa/mnninc999PPP
keep year China
tempfile china
save `china', replace

restore 

merge m:1 iso using `regions', keep(master matched) nogen
// drop if iso == "CN"

collapse (sum) nwnxa mnninc999PPP if region!="", by(region year)
replace nwnxa = nwnxa/mnninc999PPP

keep year region nwnxa
replace region = subinstr(region, " ", "", .)
replace region = subinstr(region, "-", "", .)
reshape wide nwnxa, i(year) j(region) string
renvars nwnxa*, predrop(5)

merge 1:1 year using `china', nogen

label var China   "China"
label var Eastern "East Asia"
label var Europe  "Europe"
label var LatinAm "Latin America"
label var MiddleE "MENA"
label var Norther "North America"
label var SouthSo "South & South East Asia"
label var Russiaa "Russia & Central Asia"
label var SubSaha "Sub-Saharan Africa"

drop Eastern

export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.6") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.6")
putexcel (B3:I28), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F3.7                                     *
*         Rise of financial intermediation in rich countries and China         *
* ---------------------------------------------------------------------------- *

use "$workdatawir/liabilities.dta", clear

keep if concept != "Foreign liab."
encode concept, gen(concept_)
drop concept
reshape wide China USA France Germany Japan, i(Year) j(concept_)

label var China1   China
label var China2   China
label var France1  France
label var France2  France
label var Germany1 Germany
label var Germany2 Germany
label var Japan1   Japan
label var Japan2   Japan
label var USA1     USA
label var USA2     USA

export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.7") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.7")
putexcel (B3:K43), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F3.8                                     *
*             Rise of foreign ownership in rich countries and China            *
* ---------------------------------------------------------------------------- *

use "$workdatawir/liabilities.dta", clear

keep if concept == "Foreign liab."
drop concept

export excel using "$export/WIR2022-Chapter3.xlsx", sheet("data-F3.8") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter3.xlsx", modify sheet("data-F3.8")
putexcel (B3:F43), nformat(percent)





