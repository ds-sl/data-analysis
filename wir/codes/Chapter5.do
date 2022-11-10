// ---------------------------- // 
*		CHAPTER 5
// ---------------------------- //

* ALL OTHER GRAPHS ARE PRODUCED IN NEEF AND ROBILLARD (2021)

	
* ---------------------------------------------------------------------------- *
* 				   	                  F5.5                                     *
*         Female representation among top 10%, top 1% and all earners          *
* ---------------------------------------------------------------------------- * 
	

* Top 10%
import excel using $gender, sheet("Fig5.5._FemaleRepresentation") first clear
ren (top10femrep top1femrep	femrep_allearners) (t10femrep t1femrep femrep100)
keep if inlist(iso, "FR", "US", "ES", "BR")

keep iso year t10femrep
reshape wide t10femrep, i(year) j(iso) string
renvars t10femrep*, pred(9)
label var US USA
label var FR France
label var BR Brazil
label var ES Spain


export excel using "$export/WIR2022-Chapter5.xlsx", sheet("data-F5.5a") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter5.xlsx", modify sheet("data-F5.5a")
putexcel (B3:F45), nformat(percent)

* All earners
import excel using $gender, sheet("Fig5.5._FemaleRepresentation") first clear
ren (top10femrep top1femrep	femrep_allearners) (t10femrep t1femrep femrep100)
keep if inlist(iso, "FR", "US", "ES", "BR")

keep iso year femrep100
reshape wide femrep100, i(year) j(iso) string
renvars femrep100*, pred(9)
label var US USA
label var FR France
label var BR Brazil
label var ES Spain


export excel using "$export/WIR2022-Chapter5.xlsx", sheet("data-F5.5supp") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter5.xlsx", modify sheet("data-F5.5supp")
putexcel (B3:F45), nformat(percent)

* Top 1%
import excel using $gender, sheet("Fig5.5._FemaleRepresentation") first clear
ren (top10femrep top1femrep	femrep_allearners) (t10femrep t1femrep femrep100)
keep if inlist(iso, "FR", "US", "ES", "BR")

keep iso year t1femrep
reshape wide t1femrep, i(year) j(iso) string
renvars t1femrep*, pred(8)
label var US USA
label var FR France
label var BR Brazil
label var ES Spain


export excel using "$export/WIR2022-Chapter5.xlsx", sheet("data-F5.5b") sheetmod cell(A2) first(varl)
putexcel set "$export/WIR2022-Chapter5.xlsx", modify sheet("data-F5.5b")
putexcel (B3:F45), nformat(percent)








