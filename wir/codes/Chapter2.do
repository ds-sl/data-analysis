// ---------------------------- // 
*		CHAPTER 2
// ---------------------------- //


* ALL OTHER GRAPHS ARE PRODUCED IN CHANCEL AND PIKETTY (2021), see the paper and corresponding do-files


* ---------------------------------------------------------------------------- *
* 				   	                  F2.14                                    *
*                   Global income distribution in 1820-2020                    *
* ---------------------------------------------------------------------------- * 



tempfile combined 
save `combined', emptyok


foreach year in 2020 1980 1950 1910 1820 {
preserve

use "$workdatawir/hist-dist-pop-regions.dta", clear
sort iso year p
keep if year == `year'


gen aptinc992jm = aptinc992j/12

range dincm 0 11 200
gen expdincm = exp(dincm)
gen logincm = log(aptinc992jm)


foreach c in QE QL QP XF XL XN XR XS{	
	kdensity logincm if iso == "`c'", gen(dincm`c' incm`c') at(dincm) nograph
	replace incm`c' = incm`c'*npopul992i`c'[1]
}

* monthly income 
keep dincm expdincm incm* 
renvars incm*, predrop(4)
generate year = `year'

ds year dincm expdincm, not

foreach x in `r(varlist)' {
	lowess `x' dincm, bwidth(.125) gen(`x'_sm)
}


replace expdincm = round(expdincm,.1)
replace expdincm = round(expdincm,1) if expdincm>=1
replace expdincm = round(expdincm,5) if expdincm>=20
replace expdincm = round(expdincm,10) if expdincm>=100
replace expdincm = round(expdincm,50) if expdincm>=200
replace expdincm = round(expdincm,100) if expdincm>=1000
replace expdincm = round(expdincm,500) if expdincm>=2000
replace expdincm = round(expdincm,1000) if expdincm>=10000
replace expdincm = round(expdincm,5000) if expdincm>=20000
keep if !missing(dincm)
label var XF_sm "Sub-Saharan Africa" 
label var XL_sm "Latin America" 
label var XN_sm "MENA" 
label var XR_sm "Russia & Central Asia" 
label var QE_sm "Europe" 
label var QP_sm "North America"
label var QL_sm "East Asia"
label var XS_sm "South & South-East Asia"
keep year d expd QP_sm QE_sm QL_sm XR_sm XN_sm XL_sm XS_sm XF_sm
order year d expd QP_sm QE_sm QL_sm XR_sm XN_sm XL_sm XS_sm XF_sm
line QP_sm QE_sm QL_sm XR_sm XN_sm XL_sm XS_sm XF_sm d

	append using "`combined'"
 	save "`combined'", replace

restore
}
use "`combined'", clear
//
export excel using "$export/WIR2022-Chapter2.xlsx", sheet("data-F2.14") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter2.xlsx", modify sheet("data-F2.14")
putexcel (D3:K1002), nformat(percent)

