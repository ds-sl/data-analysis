// ---------------------------- // 
*		Main do.file
// ---------------------------- //
clear all

if "`c(os)'"=="MacOSX" | "`c(os)'"=="UNIX" {
    global Rpath "/usr/local/bin/R"
}
else {  // windows, change version number if necerssary
    global Rpath `"c:\r\R-3.5.1\bin\Rterm.exe"') 
}

set graphics off
// Please change the directories to accomodate your code 
global do				"~/Dropbox/WIR2022/Do"
global datawir 			"~/Dropbox/WIR2022/Data"
global wid 				"$datawir/External/wid-data-25102021"
global workdatawir 		"$datawir/work-data"
global export 			"$workdatawir/stata_export"
global gender 			"$workdatawir/wir_fe_labinc_share_210923_clean.xlsx"
global historical_WO 	"$workdatawir/WO_hist.dta"



* Run do-files

do "$do/Chapter1.do"
do "$do/Chapter2.do"
do "$do/Chapter3.do"
do "$do/Chapter4.do"
do "$do/Chapter5.do"
do "$do/Chapter6.do"
do "$do/Chapter7.do"
do "$do/CountryAppendix.do"
