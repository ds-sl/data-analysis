// ---------------------------- // 
*		COUNTRY APPENDIX   
// ---------------------------- //




* ---------------------------------------------------------------------------- *
* 				   	                  A1F1                                     *
*                       Top 10%/Bottom 50% income shares                       *
* ---------------------------------------------------------------------------- * 


use if widcode == "sptinc992j"  ///
		 & inlist(p, "p0p50", "p90p100") using $wid, clear

keep if inlist(iso, "AR", "AU", "BR", "CA", "CL", "CN") ///
      | inlist(iso, "CO", "DE", "DZ", "EG", "ES", "FR") ///
	  | inlist(iso, "GB", "ID", "IN", "IT", "JP", "MX") ///
	  | inlist(iso, "NZ", "RU", "SE", "TR", "US", "ZA") ///
	  | inlist(iso, "DZ", "KR", "SE", "IL", "MA", "NG") ///
	  | inlist(iso, "PL", "SA")
drop widcode currency 

replace value = round(value, .001)
reshape wide value, i(year iso) j(p) string
renvars value*, pred(5)


reshape wide p0p50 p90p100, i(year) j(iso) string

tempfile wid
save `wid'

// Get Historical
import excel "$datawir/Backup/WIR2022TablesFigures-CountryAppendix-A1F1-backup240921.xlsx", first clear sheet("data-F1")
drop if year == .
drop if year>1979
drop p0p50MO p90p100MO
merge 1:1 year using "`wid'", update replace nogen
gsort year
drop if year<1900
replace p0p50DE = .   if inrange(year, 1901, 1909) | inrange(year, 1911, 1919)
replace p90p100DE = . if inrange(year, 1901, 1909) | inrange(year, 1911, 1919)


export excel "$workdatawir/stata_export/WIR2022-A1F1.xlsx", sheetmod cell(A1) first(var) sheet("data-F1")
putexcel set "$workdatawir/stata_export/WIR2022-A1F1.xlsx", modify sheet("data-F1")
putexcel (B2:Bk123), nformat(percent)



* ---------------------------------------------------------------------------- *
* 				   	                  A1F2                                     *
*                       Top 10%/Bottom 50% wealth shares                       *
* ---------------------------------------------------------------------------- * 


use if widcode == "shweal992j"  ///
		 & inlist(p, "p0p50", "p90p100") using $wid, clear

keep if inlist(iso, "AR", "AU", "BR", "CA", "CL", "CN") ///
      | inlist(iso, "CO", "DE", "DZ", "EG", "ES", "FR") ///
	  | inlist(iso, "GB", "ID", "IN", "IT", "JP", "MX") ///
	  | inlist(iso, "NZ", "RU", "SE", "TR", "US", "ZA") ///
	  | inlist(iso, "DZ", "KR", "SE", "IL", "MA", "NG") ///
	  | inlist(iso, "PL", "SA")
drop widcode currency 


replace value = round(value, .001)
reshape wide value, i(year iso) j(p) string
renvars value*, pred(5)


reshape wide p0p50 p90p100, i(year) j(iso) string

drop if year<1900 | (year<1980 & mod(year,10)!=0)


export excel "$workdatawir/stata_export/WIR2022-A1F2.xlsx", sheetmod cell(A1) first(var) sheet("data-F2")
putexcel set "$workdatawir/stata_export/WIR2022-A1F2.xlsx", modify sheet("data-F2")
putexcel (B2:BK50), nformat(percent)

// import excel "$workdatawir/historical_wealth_shares.xlsx", first clear 

* ---------------------------------------------------------------------------- *
* 				   	                  A1F3                                     *
*                          Female labor income shares                          *
* ---------------------------------------------------------------------------- * 

use if inlist(year, 1991, 2000, 2010, 2019) using $wid, clear
keep if widcode == "spllin992f" & p == "p0p100"
		
keep if inlist(iso, "AR", "AU", "BR", "CA", "CL", "CN") ///
      | inlist(iso, "CO", "DE", "DZ", "EG", "ES", "FR") ///
	  | inlist(iso, "GB", "ID", "IN", "IT", "JP", "MX") ///
	  | inlist(iso, "NZ", "RU", "SE", "TR", "US", "ZA") ///
	  | inlist(iso, "DZ", "KR", "SE", "IL", "MA", "NG") ///
	  | inlist(iso, "PL", "SA")
drop widcode currency p

reshape wide value, i(year) j(iso) string
renvars value*, predrop(5)

label var ZA "South Africa" 
label var DE "Germany" 
label var SA "Saudi Arabia" 
label var AR "Argentina" 
label var AU "Australia" 
label var BR "Brazil" 
label var CA "Canada" 
label var CN "China" 
label var KR "Korea" 
label var US "USA" 
label var FR "France" 
label var IN "India" 
label var ID "Indonesia" 
label var IT "Italy" 
label var JP "Japan" 
label var MX "Mexico" 
label var GB "United Kingdom" 
label var RU "Russian Federation" 
label var TR "Turkey" 
label var ES "Spain"
label var CL "Chile"
label var IL "Israel"
label var MA "Morocco"
label var NG "Nigeria"
label var PL "Poland"
label var SE "Sweden"
label var DZ "Algeria"

tempfile gender
save `gender', replace

export excel "$workdatawir/stata_export/WIR2022-A1F3.xlsx", sheetmod cell(A1) first(varl) sheet("data-F3")
putexcel set "$workdatawir/stata_export/WIR2022-A1F3.xlsx", modify sheet("data-F3")
putexcel (B2:AF5), nformat(percent)
		
		 
* ---------------------------------------------------------------------------- *
* 				   	                  A1T1                                     *
*                             Inequality overview                              *
* ---------------------------------------------------------------------------- * 
	

use "$wid", clear

keep if (year == 2021 | year == 2019) & inlist(widcode,"aptinc992j","ahweal992j","shweal992j","lpfghg999i","sptinc992j","xlceup999i","spllin992f","iquali999i") & inlist(p,"p0p50","p0p100","p50p90","p90p100","p99p100")

keep if inlist(iso, "AR", "AU", "BR", "CA", "CL", "CN") ///
      | inlist(iso, "CO", "DE", "DZ", "EG", "ES", "FR") ///
	  | inlist(iso, "GB", "ID", "IN", "IT", "JP", "MX") ///
	  | inlist(iso, "NZ", "RU", "SE", "TR", "US", "ZA") ///
	  | inlist(iso, "DZ", "KR", "SE", "IL", "MA", "NG") ///
	  | inlist(iso, "PL", "SA")

drop if (year == 2019 & (widcode!="lpfghg999i" & widcode!="spllin992f")) | (year != 2019 & widcode=="lpfghg999i") 

reshape wide value, i(iso year p) j(widcode) string
renvars value*, predrop(5)

bys iso (xlceup): replace xlceup = xlceup[1]

replace ahweal = ahweal/xlceup
replace aptinc = aptinc/xlceup

gen b50 = aptinc if p == "p0p50"
bys iso (b50): replace b50 = b50[1]
gen t10 = aptinc if p == "p90p100"
bys iso (t10): replace t10 = t10[1]
gen t10b50 = t10/b50
drop t10 b50 

foreach v in ahweal992j aptinc992j lpfghg999i shweal992j spllin992f sptinc992j{
	bys iso p (`v'): replace `v' = `v'[1]
}
duplicates drop iso p, force
drop year xlceup999i currency
replace t10b50 = . if p!="p0p100"

renvars ahweal992j aptinc992j lpfghg999i shweal992j spllin992f sptinc992j t10b50 iquali999i, prefix(value)
reshape long value, i(iso p) j(widcode) string
reshape wide value, i(widcode p) j(iso) string
renvars value*, predrop(5)
drop if inlist(widcode,"spllin992f","t10b50","iquali999i","lpfghg999i") & p!="p0p100"

label var widcode "WID code"
label var ZA "South Africa" 
label var DE "Germany" 
label var SA "Saudi Arabia" 
label var AR "Argentina" 
label var AU "Australia" 
label var BR "Brazil" 
label var CA "Canada" 
label var CN "China" 
label var KR "Korea" 
label var US "USA" 
label var FR "France" 
label var IN "India" 
label var ID "Indonesia" 
label var IT "Italy" 
label var JP "Japan" 
label var MX "Mexico" 
label var GB "United Kingdom" 
label var RU "Russian Federation" 
label var TR "Turkey" 
label var ES "Spain"
label var CL "Chile"
label var IL "Israel"
label var MA "Morocco"
label var NG "Nigeria"
label var PL "Poland"
label var SE "Sweden"
label var DZ "Algeria"

export excel "$workdatawir/stata_export/WIR2022-A1T1.xlsx", sheetmod cell(A1) first(varl) sheet("data-T1")
putexcel set "$workdatawir/stata_export/WIR2022-A1T1.xlsx", modify sheet("data-T1")
putexcel (C2:AF28), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  A1T1                                     *
*                              Carbon inequality                               *
* ---------------------------------------------------------------------------- * 



use "$wid", clear

keep if inlist(iso, "AR", "AU", "BR", "CA", "CL", "CN") ///
      | inlist(iso, "CO", "DE", "DZ", "EG", "ES", "FR") ///
	  | inlist(iso, "GB", "ID", "IN", "IT", "JP", "MX") ///
	  | inlist(iso, "NZ", "RU", "SE", "TR", "US", "ZA") ///
	  | inlist(iso, "DZ", "KR", "SE", "IL", "MA", "NG") ///
	  | inlist(iso, "PL", "SA")

keep if year == 2019 & widcode == "lpfghg999i" & inlist(p,"p0p50","p0p100","p50p90","p90p100","p99p100")
keep iso p value
reshape wide value, i(p) j(iso) string
renvars value*, predrop(5)

label var ZA "South Africa" 
label var DE "Germany" 
label var SA "Saudi Arabia" 
label var AR "Argentina" 
label var AU "Australia" 
label var BR "Brazil" 
label var CA "Canada" 
label var CN "China" 
label var KR "Korea" 
label var US "USA" 
label var FR "France" 
label var IN "India" 
label var ID "Indonesia" 
label var IT "Italy" 
label var JP "Japan" 
label var MX "Mexico" 
label var GB "United Kingdom" 
label var RU "Russian Federation" 
label var TR "Turkey" 
label var ES "Spain"
label var CL "Chile"
label var IL "Israel"
label var MA "Morocco"
label var NG "Nigeria"
label var PL "Poland"
label var SE "Sweden"
label var DZ "Algeria"

export excel "$workdatawir/stata_export/WIR2022-A1T2.xlsx", sheetmod cell(A1) first(varl) sheet("data-T2")
putexcel set "$workdatawir/stata_export/WIR2022-A1T2.xlsx", modify sheet("data-T2")
putexcel (B2:AF6), nformat(percent)
