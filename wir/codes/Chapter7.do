// ---------------------------- // 
*		CHAPTER 7
// ---------------------------- //


* get total household wealth 

use if inlist(widcode, "npopul992i", "ahweal992j") & year == 2021 & inlist(iso, "WO-MER", "XN-MER", "XF-MER", "QE-MER", "QP-MER", "XL-MER", "XR-MER", "XS-MER", "QL-MER") & p == "p0p100" using $wid, clear 
 
reshape wide value, i(iso) j(widcode) string 
gen aggwealth = valuenpop*valueahweal

replace iso = "SSAfrica"			if iso == "XF-MER"
replace iso = "Europe"              if iso == "QE-MER"
replace iso = "NorthAmerica"        if iso == "QP-MER"
replace iso = "LatinAmerica"        if iso == "XL-MER"
replace iso = "MENA"                if iso == "XN-MER"
replace iso = "RussiaCentralAsia"   if iso == "XR-MER"
replace iso = "SSEAsia" 			if iso == "XS-MER"
replace iso = "EastAsia"            if iso == "QL-MER"
replace iso = "World"           	if iso == "WO-MER"

ren iso region_name 
keep region_name aggwealth

tempfile aggwealth 
save `aggwealth', replace

* ---------------------------------------------------------------------------- *
* 				   	  	         T7.1 & T7.1bis                			       *
*                    		  Global millionaires  		                       *
* ---------------------------------------------------------------------------- * 

use "$workdatawir/wir2022_tables_ch7.dta", clear


merge m:1 region_name using `aggwealth', nogen

gen share = wealthd*1e9/1.18585/aggwealth 
* USD to EUR MER
sort exchangerate region wealthgroup

preserve 
	keep if exchangerate == "mer" & region == "World" & wealthgroup!=.
	keep wealthgroup nd wealthd avg effrate1 rev1p effrate2 rev2p effrate3 rev3p
	order wealthgroup nd wealthd avg effrate1 rev1p effrate2 rev2p effrate3 rev3p
	export excel "$export/WIR2022-Chapter7.xlsx", sheet("data-T7.1", replace) firstrow(varl)
restore 

preserve
keep if exchangerate=="mer" & region=="World" & wealthgroup!=.
		keep wealthgroup nd wealthd avg share
		order wealthgroup nd wealthd avg share
		export excel "$export/WIR2022-Chapter7.xlsx", sheet("data-T7.1bis", replace) firstrow(varl)
restore 

* ---------------------------------------------------------------------------- *
* 				   	  	               T7.2                			           *
*                    		      Tax scenarios  		                       *
* ---------------------------------------------------------------------------- * 

* Imputed

* ---------------------------------------------------------------------------- *
* 				   	  	           T7.3abcdefgh                			       *
*                    		  Regional millionaires  		                   *
* ---------------------------------------------------------------------------- * 


local regions Europe NorthAmerica EastAsia SSEAsia LatinAmerica SSAfrica MENA RussiaCentralAsia
local i = 0
foreach r of local regions{
	
	local letter = char(97+`i')
	local sheet "data-T7.3`letter'"
	
	preserve 
		keep if exchangerate=="mer" & region=="`r'" & wealthgroup!=.
		keep wealthgroup nd wealthd avg effrate1 rev1p effrate2 rev2p effrate3 rev3p
		order wealthgroup nd wealthd avg effrate1 rev1p effrate2 rev2p effrate3 rev3p
		export excel "$export/WIR2022-Chapter7.xlsx", sheet("`sheet'", replace) firstrow(varl)
	restore
	local i = `i'+1

}

* ---------------------------------------------------------------------------- *
* 				   	  	            T7.supp                  			       *
*                    		  All scenarios revenues  		                   *
* ---------------------------------------------------------------------------- * 


keep if exchangerate=="mer" & wealthgroup == 1
keep region_name rev*
drop *tot

renvars rev1 rev2 rev3, postfix(e0d0)
renvars rev1p rev2p rev3p, postdrop(1)
renvars rev1 rev2 rev3, postfix(e20d5)
renvars rev1pp rev2pp rev3pp, postdrop(2)
renvars rev1 rev2 rev3, postfix(e40d10)

reshape long rev1 rev2 rev3, i(region_name) j(evasiondepreciation e0d0 e20d5 e40d10)

replace rev1 = rev1/100
replace rev2 = rev2/100
replace rev3 = rev3/100

replace region_name = "" if evasiondepreciation != "e0d0" 
replace e = "E = 0%, D = 0%"   if e == "e0d0"
replace e = "E = 20%, D = 5%"  if e == "e20d5"
replace e = "E = 40%, D = 10%" if e == "e40d10"

replace region = "Sub-Saharan Africa" 		if region == "SSAfrica"
replace region = "South & South-East Asia" 	if region == "SSEAsia"
replace region = "Latin America" 			if region == "LatinAmerica"
replace region = "North America & Oceania" 	if region == "NorthAmerica"
replace region = "East Asia" 				if region == "EastAsia"
replace region = "Russia & Central Asia"    if region == "RussiaCentralAsia"

label var rev1 "Scenario 1"
label var rev2 "Scenario 2"
label var rev3 "Scenario 3"

export excel "$export/WIR2022-Chapter7.xlsx", sheet("data-T7.supp", replace) firstrow(varl)
putexcel set "$export/WIR2022-Chapter7.xlsx", modify sheet("data-T7.supp")
putexcel (C3:E29), nformat(percent)
