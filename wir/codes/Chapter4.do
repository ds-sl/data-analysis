// ---------------------------- // 
*		CHAPTER 4
// ---------------------------- //



* ---------------------------------------------------------------------------- *
* 				   	                  F4.1                                     *
*                     Regional composition of wealth groups                    *
* ---------------------------------------------------------------------------- * 


use if inlist(widcode,"ahweal992j","thweal992j") & year == 2021 & inlist(iso, "WO", "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") using $wid, clear

gen t10 = value if widcode == "thweal992j" & iso == "WO" & p == "p90p100"
gen t1 =  value if widcode == "thweal992j" & iso == "WO" & p == "p99p100"
gen t50 = value if widcode == "thweal992j" & iso == "WO" & p == "p50p100"

gen atot = value if widcode == "ahweal992j" & iso == "WO" & p == "p0p100"
gen at10 = value if widcode == "ahweal992j" & iso == "WO" & p == "p90p100"
gen at1 =  value if widcode == "ahweal992j" & iso == "WO" & p == "p99p100"
gen am40 = value if widcode == "ahweal992j" & iso == "WO" & p == "p50p90"
gen ab50 = value if widcode == "ahweal992j" & iso == "WO" & p == "p0p50"

foreach i in t10 t1 t50 atot at10 at1 am40 ab50{
	sort `i' 
	replace `i' = `i'[1]
}
 

drop if widcode == "thweal992j"
generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency year widcode n
rename p_min p
rename value a
sort iso p

save "$workdatawir/f4p1_temp.dta", replace


rsource, terminator(END_OF_R) rpath("$Rpath") roptions(--vanilla)

rm(list = ls())

library(pacman)
p_load(magrittr)
p_load(dplyr)
p_load(readr)
p_load(haven)
p_load(tidyr)
p_load(gpinter)
p_load(purrr)
p_load(stringr)
p_load(glue)
p_load(progress)
options(dplyr.summarise.inform = FALSE)

setwd("~/Dropbox/WIR2022/Data/work-data")
data <- read_dta("~/Dropbox/WIR2022/Data/work-data/f4p1_temp.dta")

countries <- unique(data$iso) 
prop = list()
i <- 1
for (iso in countries){
  region <- data[data$iso==iso,]
  dist <- shares_fit(
      bracketavg = region$a,
      p = region$p/1e5,
      fast = TRUE
    )
    
  region$frac_1  = max(1 - fitted_cdf(dist, region$t1[1]),1e-13)
  region$frac_10 = max(1 - fitted_cdf(dist, region$t10[1]),1e-13)
  region$frac_50 = max(1 - fitted_cdf(dist, region$t50[1]),1e-13)
    
  prop[[i]] <- region
  i<-i+1
}
data <- do.call(rbind, prop)
data <- distinct(data[,!names(data) %in% c("a","p")])

write_dta(data, "~/Dropbox/WIR2022/Data/work-data/f4p1_temp2.dta") 

END_OF_R

use if widcode == "npopul992i" & year == 2021 & inlist(iso, "WO", "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") using $wid, clear
ren value npopul992i 
keep iso npopul

merge 1:1 iso using "$workdatawir/f4p1_temp2.dta", nogen
gen ntop1  = npop*frac_1
gen ntop10 = npop*frac_10
gen ntop50 = npop*frac_50
gen nmid40 = ntop50 - ntop10
gen nbot50 = npopul - ntop50

foreach i in ntop1 ntop10 nmid40 nbot50{
	gen `i'_w = `i' if iso == "WO"
	sort `i'_w
	replace `i'_w = `i'_w[1]
	gen `i'_comp = `i'/`i'_w 
}

keep *comp npop a* iso 

gsort -npop

replace atot = atot[1]*npopul[1]
replace at1  = at1[1]*0.01*npopul[1]
replace at10 = at10[1]*0.1*npopul[1]
replace am40 = am40[1]*0.4*npopul[1]
replace ab50 = ab50[1]*0.5*npopul[1]

gen st1  = at1[1]/atot[1]
gen st10 = at10[1]/atot[1]
gen sm40 = am40[1]/atot[1]
gen sb50 = ab50[1]/atot[1]


replace iso = "Sub-Saharan Africa"      if iso == "XF"
replace iso = "Europe"                  if iso == "QE"
replace iso = "North America"        if iso == "QP"
replace iso = "Latin America"           if iso == "XL"
replace iso = "MENA"                    if iso == "XN"
replace iso = "Russia & Central Asia"   if iso == "XR"
replace iso = "South & South East Asia" if iso == "XS"
replace iso = "East Asia"               if iso == "QL"

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.1") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.1")
putexcel (H3:O9), nformat(percent)


* share of each region in global wealth 
use if inlist(widcode,"ahweal992j","npopul992i") & year == 2021 & inlist(iso, "WO", "XN", "XF", "QE", "QP", "XL", "XR", "XS", "QL") & (p == "p0p100" | p == "p90p100") using $wid, clear
// drop if p == "p0p100" & widcode != "npopul992i"
drop if p == "p90p100"
drop year currency p 

reshape wide value, i(iso) j(widcode) string
renvars value*, predrop(5)
replace ah = ah*npop
sort ah
gen share = ah/ah[_N]



* ---------------------------------------------------------------------------- *
* 				   	             T4.1 & T4.1supp                               *
*                         Global distribution of wealth                        *
* ---------------------------------------------------------------------------- *

use if widcode == "ahweal992j" & (year == 2021 | year == 1995) & (iso == "WO" | iso == "WO-MER") using $wid, clear

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency widcode n
rename p_min p
rename value a
sort iso year p

save "$workdatawir/t4p1_temp.dta", replace
 
rsource, terminator(END_OF_R) rpath("$Rpath") roptions(--vanilla)

rm(list = ls())

library(pacman)
p_load(magrittr)
p_load(dplyr)
p_load(readr)
p_load(haven)
p_load(tidyr)
p_load(gpinter)
p_load(purrr)
p_load(stringr)
p_load(glue)
p_load(progress)
options(dplyr.summarise.inform = FALSE)

setwd("~/Dropbox/WIR2022/Data/work-data")
data <- read_dta("~/Dropbox/WIR2022/Data/work-data/t4p1_temp.dta")

gperc <- c(
  0, 50000, 90000, 99000, 99900, 99990, 99999, 99999.9, 99999.99, 99999.999
)

list <- list()
i<- 1
for (iso in c("WO","WO-MER")){
for (y in c(1995,2021)){
  dist <- shares_fit(
    bracketavg = data[data$year==y & data$iso==iso,]$a,
    p = data[data$year==y & data$iso==iso,]$p/1e5,
    fast = TRUE
  )
  tab <- as.data.frame(generate_tabulation(dist, gperc/1e5))
  tab$year <- y
  tab$iso <- iso 
  list[[i]] <- tab
  i <- i+1
}
}
data <- do.call(rbind, list)

write_dta(data, "~/Dropbox/WIR2022/Data/work-data/t4p1_temp2.dta") 

END_OF_R

* get average growth of 10-50% for bottom 50%
use if p>9000 & p<51000 using "$workdatawir/t4p1_temp.dta", clear
bys iso p (year): gen g_ = (a/a[_n-1])^(1/(2021-1995))-1
bys iso: egen g = mean(g_)
keep iso g
gen p = 0
duplicates drop iso, force
tempfile bot50g
save `bot50g', replace


use if widcode == "npopul992i" & (year == 2021 | year == 1995) & (iso == "WO") using $wid, clear
keep year value 
ren value npop992i
tempfile adultpop
save `adultpop', replace

use "$workdatawir/t4p1_temp2.dta", clear

ren fractile p
expand 2 if p == 0, gen(new)
sort iso year p new

gen s = top_share
replace s = bracket_share if (p == 0 & new == 1) | p == .5 

gen a = top_average 
replace a = bracket_average if (p == 0 & new == 1) | p == .5 

gen t = max(threshold,0)
keep iso year p a s t
replace p = -1 if p == 0 & s == 1

bys iso p (year): gen g = (a/a[_n-1])^(1/(2021-1995))-1

merge m:1 year using `adultpop', nogen

bys iso p (year): gen totg = a*npop*(1-p)-a[_n-1]*npop[_n-1]*(1-p)
replace totg = a*npop-a[_n-1]*npop[_n-1] if p == -1
replace totg = a*npop*0.4-a[_n-1]*npop[_n-1]*0.4 if p == 0.5
replace totg = a*npop*0.5-a[_n-1]*npop[_n-1]*0.5 if p == 0

drop if year == 1995
drop year

replace g = . if p == 0
merge 1:1 iso p using `bot50g', nogen update 

bys iso (p): gen sg = totg/totg[1]


preserve 
keep if iso == "WO"
drop iso totg sg
export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-T4.1") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-T4.1")
putexcel (B3:B13), nformat(percent)
putexcel (E3:E13), nformat(percent)
restore 

preserve
keep if iso == "WO-MER"
drop iso totg sg
export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-T4.1supp") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-T4.1supp")
putexcel (B3:B13), nformat(percent)
putexcel (E3:E13), nformat(percent)
restore 

* ---------------------------------------------------------------------------- *
* 				   	                  T4.2                                     *
*             Share of global wealth growth captured since 1995                *
* ---------------------------------------------------------------------------- *

keep p iso sg
replace iso = substr(iso,4,3)
replace iso = "PPP" if iso == ""
reshape wide sg, i(p) j(iso "PPP" "MER") string

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-T4.2") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-T4.2")
putexcel (B3:C13), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F4.2                                     *
*                      Average annual wealth growth rate                       *
* ---------------------------------------------------------------------------- *

use "$workdatawir/t4p1_temp2.dta", clear
keep iso year fractile bracket_average
ren (fractile bracket_average) (p ahweal992j)
keep if p>.999999 & iso == "WO"
replace p = p*1e5

tempfile verytop
save `verytop', replace

use if iso == "WO" & (year == 1980 | year == 1995 | year == 2019 | year == 2021) & inlist(widcode,"ahweal992j","aptinc992j") using $wid, clear

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)
keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency n
rename p_min p
reshape wide value, i(iso year p) j(widcode) string
renvars value*, predrop(5)

append using `verytop'
sort iso year p

reshape wide ah ap, i(iso p) j(year)

gen elephantw2019 = (ahweal992j2019/ahweal992j1995)^(1/(2019-1995))-1
gen elephantw2021 = (ahweal992j2021/ahweal992j1995)^(1/(2021-1995))-1
gen elephanti2021 = (aptinc992j2021/aptinc992j1980)^(1/(2021-1980))-1

drop a*
gen perc = _n
tsset perc
tssmooth ma elephanti2021s = elephanti2021 if p<99999, w(2 1 2) 
tssmooth ma elephantw2021s = elephantw2021 if p<99999, w(2 1 2) 
tssmooth ma elephantw2019s = elephantw2019 if p<99999, w(2 1 2) 
replace elephantw2021s = elephantw2021[10] if _n<=10
replace elephantw2021s = elephantw2021 if p>99999

tssmooth ma elephantw2021ss = elephantw2021s, w(4 1 4) 
replace elephantw2021ss = elephantw2021s if p>99999


replace p=p/1000


label var elephantw2021ss "Wealth growth 1995-2021"
keep p elephantw2021ss
replace elephant = elephant[_N] if _n == _N-2 
drop if p>99.999
replace p = 99.9 if p > 99 & p <= 99.9
replace p = 99.99 if p > 99.9 & p <= 99.99
replace p = 99.999 if p > 99.99 

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.2") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.2")
putexcel (B3:B129), nformat(percent)


cap rm "$workdatawir/t4p1_temp.dta"
cap rm "$workdatawir/t4p1_temp2.dta"


* share of growth captured by bottom 50% and top 1%
use if iso == "WO" & (year == 1995 | year == 2021) & inlist(widcode, "ahweal992j", "npopul992i") & inlist(p, "p0p100", "p50p90", "p90p100", "p0p50", "p99p100") using $wid, clear

reshape wide value, i(year p) j(widcode) string
renvars value*, predrop(5)
bys year: replace npop = npop[1]
replace ah = ah*npop if p == "p0p100"
replace ah = ah*npop*0.5 if p == "p0p50"
replace ah = ah*npop*0.4 if p == "p50p90"
replace ah = ah*npop*0.1 if p == "p90p100"
replace ah = ah*npop*0.01 if p == "p99p100"

drop npop iso currency
reshape wide ahweal992j, i(p) j(year) 

gen g = ahweal992j2021 - ahweal992j1995
gen share = g/g[1]


* ---------------------------------------------------------------------------- *
* 				   	                 F4.3a                                     *
*             Extreme wealth inequality: top 0.001% vs. bottom 50%             *
* ---------------------------------------------------------------------------- *

use if iso == "WO" & inrange(year,1995,2021) & widcode == "shweal992j" & inlist(p, "p0p50", "p99.999p100") using $wid, clear
replace p = "bot50"  if p == "p0p50"
replace p = "top001" if p == "p99.999p100"

reshape wide value, i(year) j(p) string
renvars value*, predrop(5)
keep year bot50 top001

label var bot50  "Bottom 50%"
label var top001 "Top 0.001%"


export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.3a") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.3a")
putexcel (B3:C29), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F4.3b                                    *
*                        The rise of global billionaires                       *
* ---------------------------------------------------------------------------- *

use if inlist(widcode, "xlceux999i", "inyixx999i") & inrange(year,1995,2021) & iso == "US" using $wid, clear 
keep value widcode year
reshape wide value, i(year) j(widcode) string
renvars value*, predrop(5)
replace xlceux999i = xlceux999i[_N]

tempfile prices
save `prices', replace

use if inlist(widcode, "mnninc999i", "mnweal999i", "mhweal999i", "ahweal992j", "npopul992i") & inrange(year,1995,2021) & iso == "WO-MER" & p == "p0p100" using $wid, clear 
keep value widcode year
reshape wide value, i(year) j(widcode) string
renvars value*, predrop(5)
gen mhweal999i = ahweal992j*npopul992i
drop ahweal992j npopul992i

tempfile aggvalues
save `aggvalues', replace

use "$workdatawir/forbes_worth.dta", clear

collapse (sum) worth, by(year)

merge 1:1 year using `prices', nogen
merge 1:1 year using `aggvalues', nogen

replace worth = worth*1e6/xlceux999i/inyixx999i 

gen bn_natinc  = worth/mnninc999i
gen bn_natweal = worth/mnweal999i
gen bn_hhweal  = worth/mhweal999i

keep year bn*
export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.3b") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.3b")
putexcel (B3:D29), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F4.4                                     *
*                    Historical shares in Europe and the US                    *
* ---------------------------------------------------------------------------- *

import excel "$workdatawir/historical_wealth_shares.xlsx", firstrow clear cellrange(A3:I114) 
ren (A France UK USA Scand Netherlands) (year FRtop1 GBtop1 UStop1 SCtop1 NLtop1)
destring *, replace
drop E F G
tempfile top1
save `top1', replace

import excel "$workdatawir/historical_wealth_shares.xlsx", firstrow clear cellrange(M3:P114) 
gen year = _n+1909
ren (M France UK Scand) (USbot50 FRbot50 GBbot50 SCbot50)
destring *, replace

tempfile bot50
save `bot50', replace

use if inlist(iso, "GB", "US", "FR", "NL", "NO", "DK", "SE")  & widcode == "shweal992j" & inlist(p, "p0p50", "p99p100") using $wid, clear

reshape wide value, i(year p) j(iso) string
renvars value*, predrop(5)
drop widcode currency
replace p = "top1" if p == "p99p100"
replace p = "bot50" if p == "p0p50"
gen SC = (NO+SE+DK)/3
drop NO SE DK
reshape wide FR GB NL US SC, i(year) j(p) string 

merge 1:1 year using `top1', update nogen 
merge 1:1 year using `bot50', update nogen

drop if year<1910
gen id = mod(_n-1,10) == 0
gen period = sum(id)

foreach y in FRbot50 GBbot50 NLbot50 USbot50 SCbot50 FRtop1 GBtop1 NLtop1 UStop1 SCtop1{
	bys period: egen `y'_ = mean(`y')
	replace `y'_ = . if id == 0
}
keep year *_
renvars *_, postdrop(1)

egen EUbot50 = rowmean(FRbot50 GBbot50 NLbot50 SCbot50)
egen EUtop1 = rowmean(FRtop1 GBtop1 NLtop1 SCtop1)

keep year EUbot50 EUtop1 USbot50 UStop1 
drop if EUbot50 == . 


export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.4") sheetmod cell(A2) first(var)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.4")
putexcel (B3:E14), nformat(percent)



* ---------------------------------------------------------------------------- *
* 				   	                  F4.5                                     *
*                       Top 1% wealth share in the BRICS                       *
* ---------------------------------------------------------------------------- * 

use if inlist(iso, "BR", "CN", "ZA", "IN", "RU")  & widcode == "shweal992j" & inlist(p, "p99p100") & inrange(year,1995,2021) using $wid, clear
reshape wide value, i(year) j(iso) string
keep year value*

renvars value*, predrop(5)
label var BR Brazil 
label var CN China 
label var ZA "South Africa"
label var IN India
label var RU Russia

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.5") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.5")
putexcel (B3:F29), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                  F4.6                                     *
*                Projection of groups wealth shares, 2000-2100                 *
* ---------------------------------------------------------------------------- *

use if iso == "WO" & inrange(year,1995,2021) & widcode == "ahweal992j" & inlist(p, "p0p50", "p50p90", "p99.9p100", "p90p100", "p0p100") using $wid, clear
replace p = "bot50"  if p == "p0p50"
replace p = "top01" if p == "p99.9p100"
replace p = "mid40"  if p == "p50p90"
replace p = "top10"  if p == "p90p100"
replace p = "total"  if p == "p0p100"

reshape wide value, i(year) j(p) string
renvars value*, predrop(5)
keep year bot50 top01 mid40 top10 total

replace top10 = (top10*0.1-top01*0.001)*100/9.999
ren total total0

gen g50  = (bot50-bot50[_n-1])/bot50[_n-1]
gen g01  = (top01-top01[_n-1])/top01[_n-1]
gen g40  = (mid40-mid40[_n-1])/mid40[_n-1]
gen g10  = (top10-top10[_n-1])/top10[_n-1]

expand 80 if year == 2021
bys year: replace year = year+_n-1
foreach y in g40 g01 g50 g10{
	replace `y' = . if year > 2021
	egen mean`y' = mean(`y')
}  

replace bot50  = bot50*(1+meang50)^(_n-27) if year>2021
replace mid40  = mid40*(1+meang40)^(_n-27) if year>2021
replace top01  = top01*(1+meang01)^(_n-27) if year>2021
replace top10  = top10*(1+meang10)^(_n-27) if year>2021

gen total = bot50*0.5+mid40*0.4+top10*0.099+top01*0.001

gen s50  = bot50*0.5/total
gen s40  = mid40*0.4/total
gen s01  = top01*0.001/total
gen s10  = top10*0.099/total

keep year s*

label var s50 "Bottom 50%"
label var s10 "Top 10%"
label var s01 "Top 0.1%"
label var s40 "Middle 40%"

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.6") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.6")
putexcel (B3:E108), nformat(percent)


* ---------------------------------------------------------------------------- *
* 				   	                 FB4.1                                     *
*                    Wealth inequality vs. Income inequality                   *
* ---------------------------------------------------------------------------- *

import excel using "~/Dropbox/WIR2022-Temp/CountryPages/country-codes-new.xlsx", firstrow clear
renvars code shortname / iso isoname
keep iso isoname 
tempfile isoname 
save `isoname', replace

use if year >1995 & inlist(widcode, "shweal992j", "sptinc992j") & p == "p90p100" using $wid, clear

keep if strpos("ZA US FR RU IN IE IT ES NO EE LV PL PT HU LU DE GB AT LT CN CY CH GR FI SI DK BE HR NL MT SK",iso)!=0

reshape wide value, i(iso year) j(widcode) string
renvars value*, predrop(5)
bys iso: egen shweal = mean(shweal)
bys iso: egen sptinc = mean(sptinc)
duplicates drop iso, force

merge 1:1 iso using `isoname', nogen keep(master matched)
keep isoname shweal sptinc
order isoname sh sp
replace isoname = "UK" if isoname == "United Kingdom"
replace isoname = "Russia" if isoname == "Russian Federation"

label var sh "Top 10% wealth share"
label var sp "Top 10% pretax income share"

export excel using "$export/WIR2022-Chapter4.xlsx", sheet("data-F4.7") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter4.xlsx", modify sheet("data-F4.7")
putexcel (B3:C33), nformat(percent)









