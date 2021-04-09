clear
clear all
clear matrix
set mem 1g
set more off, permanently
set maxvar 15000
set matsize 10000
set emptycells drop, permanently

*Set directory (direct to the folder prior to the TIER subfolders: command_rep, documents_rep, etc.)
*In the example below, the folder prior to the TIER subfolders is called SSRP_McGuirk_Burke_2020_JPE

global dir "*your directory/SSRP_McGuirk_Burke_2020_JPE*" 
global output "$dir/command_rep/analysis/output_rep/tables" 

ssc install sutex
ssc install reghdfe
ssc install geonear
ssc	install tmpdir
ssc	install reg2hdfe

do 	"$dir/adofiles/reg2hdfespatial.ado"

do	"$dir/adofiles/ols_spatial_HAC.ado"


*******************************
*	APPENDIX TABLES		  
*******************************
/*
Table A2	UCDP Factor Conflict, Prices and Luminosity
Table A3	UCDP Factor Conflict w/ YFEs and Added Controls
Table A4	UCDP Factor Conflict: Two-Sided Violence Only
Table A6	UCDP Factor Conflict: One Degree Aggregation 
Table A7	UCDP Factor Conflict: One Degree Aggregation w/ Spatial Spillovers
Table A8	UCDP Factor Conflict w/ Control for Population
Table A9	UCDP Factor Conflict: Conditional Logit
Table A10	UCDP Factor Conflict w/ Trade Weights on Price Indices
Table A11	UCDP Factor Conflict w/ Yield Weights on PPI
Table A12	UCDP Factor Conflict w/out Lags
Table A13	ACLED Output Conflict w/ YFEs and Added Controls
Table A14	ACLED Output Conflict w/ Added Controls
Table A15	ACLED Output Conflict, Riots Only
Table A17	ACLED Output Conflict: One Degree Aggregation
Table A18	ACLED Output Conflict: One Degree Aggregation w/ Spatial Spillovers
Table A19	ACLED Output Conflict with Control for Population
Table A20	ACLED Output Conflict: Conditional Logit
Table A21	ACLED Output Conflict w/ Trade Weights
Table A22	ACLED Output Conflict w/ Yield Weights on PPI
Table A23	ACLED Output Conflict w/out Lags
Table A24	ACLED Output Conflict, PPI and CPI: Urban Riots
Table A25	Comparison of Effects on Output Conflict and Factor Conflict Incidence
Table A26	Afrobarometer: Prices and Poverty
Table A27	Afrobarometer: Output Conflict Validation Tests
Table A28	Afrobarometer Output Conflict: Triple Difference with Non-Commercial Farmer
Table A29	UCDP Factor Conflict: Precolonial Institutions 
Table A30	ACLED Output Conflict: Precolonial Institutions 
*/


*----------------------------------


*******************************
*	TABLE A2	
*******************************

*SEE TABLE 3

*******************************
*	TABLE A3	
*******************************

use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA1
					
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA2

				
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA3

					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA4
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA5
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell ) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
											
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA6
									
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA7
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA8


reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					estadd local wt " "
					
					est sto regA9
					
	esttab regA*  using "$output/table_A3.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					 "mc \hline \\ Mine controls"	 "wc  Weather controls"  "oc  Oil controls"   ///
					"yfe  Year FE" ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 0 1 0 0 1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 


					
*******************************
*	TABLE A4	
*******************************

use "$dir/input_rep/cell_data.dta", clear

	fsum ucdp*
	gen ucdp_12 = (ucdp_type1 == 1 | ucdp_type2 == 1) & !missing(ucdp_10)
	replace ucdp_12 = . if ucdp_10 == .
	gen onset_ucdp_12 = 1 if (ucdp_12 == 1 & l.ucdp_12 == 0)
	replace onset_ucdp_12 = 0 if ucdp_12 == 0
	gen end1_ucdp_12 = 1 if (ucdp_12 == 1 & f.ucdp_12 == 0)
	replace end1_ucdp_12 = 0 if ucdp_12 == 1 & end1_ucdp_12 != 1
	
	cap est drop reg*
	est clear
	
	summ ucdp_12
	loc mucdp = r(mean)	
	summ onset_ucdp_12
	loc monset = r(mean)
	summ end1_ucdp_12
	loc mend1 = r(mean)
	
reghdfe 		ucdp_12 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
											
					estadd local tw "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					
					est sto regE1

					
reghdfe 		onset_ucdp_12 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)					
											
					estadd local tw "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					
					est sto regE2

					
reghdfe 		end1_ucdp_12 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
											
					estadd local tw "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					estadd local wt " "	

					est sto regE3
					
esttab regE*  using "$output/table_A4.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} SE" "pvalp \hspace{15pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///		
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"ctt \hline \\ Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1  1  1 )  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
	
cap drop *ucdp_12


*******************************
*		TABLE A5	
*******************************

*in replication_conleySE.do


*******************************
*		TABLE A6	
*******************************

use "$dir/input_rep/1deg_data.dta", clear

cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
reghdfe 		ucdp_10  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy) 
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					est sto regA1
					
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					est sto regA2

reghdfe 		onset_ucdp_10  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)

					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					est sto regA3
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_ucdp_10  L(0/2).z_PPI,  a(deg1cell cy) cluster (deg1cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
						
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					 
					est sto regA5
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
 					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					estadd local wt " "
					 
					est sto regA6
					
esttab regA*  using "$output/table_A6.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"cyfe \hline \\ Country $\times$ year FE"   /// 
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 

					
*******************************
*		TABLE A7	
*******************************

use "$dir/input_rep/1deg_data.dta", clear

					
cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
reghdfe 		ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mucdp')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
					
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					
					est sto regA1

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mucdp')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					
					est sto regA2
					
					
reghdfe 		onset_ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)
					
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
						
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`monset')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
			
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					 
					est sto regA3
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`monset')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mend1')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					
					est sto regA5
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mend1')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					estadd local wt " "
					
					est sto regA6
					
esttab regA*  using "$output/table_A7.tex", replace se noconstant drop(*)  ///
					 star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					 scalars(  ///
						"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Two-way SE " "pvalp \hspace{15pt} p-value"  ///
						"pw_sumcoef \\  Producer Price Index in neighboring cells" "sepw \hspace{15pt} Two-way SE" "pvalpw \hspace{15pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} Two-way SE " "pvalc \hspace{15pt} p-value"  ///
						"stdp \hline \\ PPI impact (\%)"   ///
						"stdpw  PPI impact in neighboring cells (\%)"   ///
						"stdc  CPI impact (\%)"   ///
									"wt  Wald test: PPI = CPI"  ///		
						"test_cp  \hspace{15pt} p-value"  ///
						"cyfe \hline \\ Country $\times$ year FE" "ctt Country $\times$ time trend"  "cfe Cell FE"   /// 
						 "r2 R squared"  "N Observations")  ///
						mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1  0 1  0)  ///
						prefix(\multicolumn{@span}{c}{) suffix(})  ///
						span erepeat(\cmidrule(lr){@span}))  ///
					 sfmt( 	%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f  %10.1f  %10.1f  ///
							%10.3f  %10.3f   ///
							%~12s %~12s %~12s ///
							%10.3f %10.0f  )  /// 
					 label obslast nomtitles 	



*******************************
********	TABLE A8	*******
*******************************

use "$dir/input_rep/cell_data.dta", clear


cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
reghdfe 		ucdp_10 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
					

					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
											
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"	
					
					est sto regE1

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "No"						
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					
					est sto regE2

reghdfe 		onset_ucdp_10 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
					
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
											
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"	
					
					est sto regE3
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)					
											
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					
					est sto regE4

reghdfe 		end1_ucdp_10 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
					
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
											
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"	
					
					est sto regE5
					
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					lincom ln_ex_pop
					estadd sca pop_coef = r(estimate)
					estadd sca pop_se = r(se)
					test ln_ex_pop = 0
					estadd sca pop_pval = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
											
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"	
					estadd local wt " "	

					est sto regE6
					
		esttab regE*  using "$output/table_A8.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} SE" "pvalp \hspace{15pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///		
					"pop_coef \\ ln Population" "pop_se \hspace{15pt} SE" "pop_pval \hspace{15pt} p-value"  ///		
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"cyfe \hline \\  Country $\times$ Year FE" ///
					"ctt   Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0  1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f ///
					%10.4f %10.3f %10.3f ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					 %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 


*******************************
********	TABLE A9	*******
*******************************

use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear

*	Incidence
	clogit ucdp_10 	z_CPI l_z_CPI l2_z_CPI ///
					z_PPI l_z_PPI l2_z_PPI year , group(cell) cluster(country)
	
	lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
		estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_PPI + l_z_PPI + l2_z_PPI
	estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
	test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
	test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
	estadd sca test_cp = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"
	est sto reg11

*	Onset
	clogit onset_ucdp_10 z_CPI l_z_CPI l2_z_CPI ///
					z_PPI l_z_PPI l2_z_PPI year , group(cell) cluster(country)
	
	lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
		estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_PPI + l_z_PPI + l2_z_PPI
	estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
	test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
	test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
	estadd sca test_cp = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"	
	est sto reg12
	
*	Offset
	clogit end1_ucdp_10 	z_CPI l_z_CPI l2_z_CPI ///
					z_PPI l_z_PPI l2_z_PPI year , group(cell) cluster(country)
	
	lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
		estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_PPI + l_z_PPI + l2_z_PPI
	estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
	test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
	test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
	estadd sca test_cp = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"
	est sto reg13


	esttab reg1* using "$output/table_A9.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} SE" "pvalp \hspace{15pt} p-value"  ///
		"c_sumcoef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///
		"wt \hline \\  Wald test: PPI = CPI"  ///		
		"test_cp  \hspace{15pt} p-value"  ///
		 "tt \hline \\ Time trend" "fe Cell fixed effects"   /// 
		 "r2_p Pseudo-R squared"  "N Observations")  ///
		mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1  1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f  ///
			 %10.3f %10.3f  ///
			 %~12s  %~12s  ///
			%10.3f %10.0f  )  /// 
     label obslast nomtitles 

	 
*******************************
********	TABLE A10	*******
*******************************	 
	 use "$dir/input_rep/cell_data.dta", clear


*	Normalize Price variables
foreach i in b88 {

	loneway `i'_tr_area_price_cell cell
	loc psd`i' = r(sd_w)
	summ `i'_tr_area_price_cell
	loc pmean`i' = r(mean)
	gen z_`i'_PPI = (`i'_tr_area_price_cell-`pmean`i'')/`psd`i''

	loneway `i'_tr_area_food_1pc cell
	loc fpsd`i' = r(sd_w)
	summ `i'_tr_area_food_1pc
	loc fpmean`i' = r(mean)
	gen z_`i'_food_PPI = (`i'_tr_area_food_1pc-`fpmean`i'')/`fpsd`i''
	
	loneway `i'_tr_area_cash_1pc cell
	loc cpsd`i' = r(sd_w)
	summ `i'_tr_area_cash_1pc
	loc cpmean`i' = r(mean)
	gen z_`i'_cash_PPI = (`i'_tr_area_cash_1pc-`cpmean`i'')/`cpsd`i''

	loneway `i'_all_tr_basket_price_country cell
	loc csd`i' = r(sd_w)
	summ `i'_all_tr_basket_price_country
	loc cmean`i' = r(mean)
	gen z_`i'_CPI = (`i'_all_tr_basket_price_country-`cmean`i'')/`csd`i''
	}
	
	cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
	reghdfe 		ucdp_10 L(0/2).zt_CPI L(0/2).zt_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_PPI + l.zt_PPI + l2.zt_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test zt_PPI + l.zt_PPI + l2.zt_PPI = 0
					estadd sca pvalp = r(p)
					
					test zt_CPI + l.zt_CPI + l2.zt_CPI  =  zt_PPI + l.zt_PPI + l2.zt_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA1
					
	reghdfe 		ucdp_10 L(0/2).z_b88_CPI L(0/2).z_b88_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI = 0
					estadd sca pvalp = r(p)
					
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI  =  z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2
					
	reghdfe 		onset_ucdp_10 L(0/2).zt_CPI L(0/2).zt_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_PPI + l.zt_PPI + l2.zt_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test zt_PPI + l.zt_PPI + l2.zt_PPI = 0
					estadd sca pvalp = r(p)
					
					test zt_CPI + l.zt_CPI + l2.zt_CPI  =  zt_PPI + l.zt_PPI + l2.zt_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA3
					
	reghdfe 		onset_ucdp_10 L(0/2).z_b88_CPI L(0/2).z_b88_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI = 0
					estadd sca pvalp = r(p)
					
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI  =  z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA4
					
	reghdfe 		end1_ucdp_10 L(0/2).zt_CPI L(0/2).zt_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_PPI + l.zt_PPI + l2.zt_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test zt_PPI + l.zt_PPI + l2.zt_PPI = 0
					estadd sca pvalp = r(p)
					
					test zt_CPI + l.zt_CPI + l2.zt_CPI  =  zt_PPI + l.zt_PPI + l2.zt_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					
					est sto regA5
					
	reghdfe 		end1_ucdp_10 L(0/2).z_b88_CPI L(0/2).z_b88_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI = 0
					estadd sca pvalp = r(p)
					
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI  =  z_b88_PPI + l.z_b88_PPI + l2.z_b88_PPI 
					estadd sca test_cp = r(p)
					
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					
					est sto regA6
					
					esttab regA*  using "$output/table_A10.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} SE" "pvalp \hspace{15pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///		
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"tw \hline \\ Trade weight"   /// 
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s  ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
					
*******************************
********	TABLE A11	*******
*******************************	 
use "$dir/input_rep/cell_data.dta", clear

	loneway yield_t_PPI cell
	loc psd_y = r(sd_w)
	summ yield_t_PPI
	loc pmean_y = r(mean)
	cap gen z_yield_t_PPI = (yield_t_PPI-`pmean_y')/`psd_y'
	
	loneway yield_PPI cell
	loc psd_ynot = r(sd_w)
	summ yield_PPI
	loc pmean_ynot = r(mean)
	cap gen z_yield_PPI = (yield_PPI-`pmean_ynot')/`psd_ynot'

		
cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)

reghdfe 		ucdp_10  L(0/2).z_yield_PPI ,  a(cell cy) cluster (cell cy)

					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					
					est sto regA1
					
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2

reghdfe 		onset_ucdp_10 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA3
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_ucdp_10 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
				
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA5

reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					 
					est sto regA6
					
		esttab regA*  using "$output/table_A11.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index $\times$ Yield"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Two-way p-value"  ///
					"cyfe \hline \\ Country $\times$ year FE" ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
					
*******************************
********	TABLE A12	*******
*******************************	 
use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)
	
reghdfe 		ucdp_10  z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI  = 0 
					estadd sca pvalp = r(p)
					
										
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					
					est sto regA1
					
reghdfe 		ucdp_10 z_CPI z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI  = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI = z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2
					
reghdfe 		onset_ucdp_10  z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI  = 0 
					estadd sca pvalp = r(p)
					
										
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					
					est sto regA3
					
				
reghdfe 		onset_ucdp_10 z_CPI z_PPI _ct*,  a(cell ) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI  = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI  = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI  =  z_PPI 
					estadd sca test_cp = r(p)	
					
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
				
					est sto regA4
					
reghdfe 		end1_ucdp_10  z_PPI ,  a(cy cell) cluster (cell cy)
				
					lincom z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI = 0 
					estadd sca pvalp = r(p)
										
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					estadd local wt " "
				
					est sto regA5
					
reghdfe 		end1_ucdp_10 z_CPI z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI  = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI  =  z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
				
					est sto regA6
					
esttab regA*  using "$output/table_A12.tex", replace se noconstant drop(*)  ///
						star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
						scalars(  ///
						"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///		
						"stdp \hline \\ PPI impact (\%)"   ///
						"stdc  CPI impact (\%)"   ///
						"wt  Wald test: PPI = CPI"  ///		
						"test_cp  \hspace{15pt} p-value"  ///
						"ctt \hline \\ Country $\times$ year FE" ///
						"ctt  Country $\times$ time trend" ///
						"cfe  Cell FE" ///
						"N Observations")  ///
						mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
						prefix(\multicolumn{@span}{c}{) suffix(})  ///
						span erepeat(\cmidrule(lr){@span}))  ///
						sfmt( 	%10.4f %10.3f %10.3f  ///
						%10.4f %10.3f %10.3f  ///
						%10.1f  %10.1f  ///
						%10.3f %10.3f ///
						%~12s %~12s %~12s %~12s ///
						%10.0f  )  /// 
						label obslast nomtitles 



*******************************
********	TABLE A13	*******
*******************************	 
use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
				
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA1
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
				
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA2

				
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
									
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA3

					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
				
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA4
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
				
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA5
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell ) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA6
					

					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA7
					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
	
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA8


reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)

										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					estadd local wt " "
					
					est sto regA9
					
	esttab regA*  using "$output/table_A13.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					 "mc \hline \\ Mine controls"	 "wc  Weather controls"  "oc  Oil controls"   ///
					"yfe  Year FE" ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 0 1 0 0 1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%~12s %~12s %~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 


	
*******************************
********	TABLE A14	*******
*******************************	

use "$dir/input_rep/cell_data.dta", clear

	summ p_oil
	loc posd = r(sd)
	loc pomean = r(mean)
*	gen z_p_oil = (p_oil-`pomean')/`posd'

	cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA2

				
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines  _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
									
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA3

									
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
								
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA5
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(cell ) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA6

					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
	
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA8


reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp L(0/2).z_p_oil L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)

										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA9
					
	esttab regA*  using "$output/table_A14.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					 "mc \hline \\ Mine controls"	 "wc  Weather controls"  "oc  Oil controls"   ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1  0 1  0 1  0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%~12s %~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
					
	
*******************************
********	TABLE A15	*******
*******************************	
					
use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear
	
	summ acled_fight_6
	loc macled = r(mean)	
	summ onset_acled_fight_6
	loc monset = r(mean)
	summ end1_acled_fight_6
	loc mend1 = r(mean)

reghdfe 		acled_fight_6  L(0/2).z_PPI ,  a(cell cy) cluster (cell cy)

					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					
					est sto regA1
					
reghdfe 		acled_fight_6 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2

reghdfe 		onset_acled_fight_6 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA3
					
reghdfe 		onset_acled_fight_6 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)

					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_acled_fight_6 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
				
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA5

reghdfe 		end1_acled_fight_6 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					 
					est sto regA6
					
		esttab regA*  using "$output/table_A15.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"cyfe \hline \\ Country $\times$ year FE" ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 

					
*******************************
*	TABLE A16 in replication_conleySE.do
*******************************	


*******************************
*******		TABLE A17	*******
*******************************	

use "$dir/input_rep/1deg_data.dta", clear

cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc mac67 = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
reghdfe 		acled_riot_67  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mac67')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					est sto regA1
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mac67')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mac67')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					est sto regA2

reghdfe 		onset_acled_riot_67  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)

					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					est sto regA3
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_acled_riot_67  L(0/2).z_PPI,  a(deg1cell cy) cluster (deg1cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
						
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					 
					est sto regA5
					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
 					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					estadd local wt " "
					 
					est sto regA6
					
esttab regA*  using "$output/table_A17.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"cyfe \hline \\ Country $\times$ year FE"   /// 
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 


*******************************
*******		TABLE A18	*******
*******************************	

use "$dir/input_rep/1deg_data.dta", clear

cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc mac67 = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
reghdfe 		acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mac67')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mac67')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
					
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					
					est sto regA1

reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mac67')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mac67')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mac67')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
					
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					
					est sto regA2
					
					
reghdfe 		onset_acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)
					
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
						
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`monset')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
			
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					 
					est sto regA3
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`monset')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mend1')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local cfe "Yes"
					
					est sto regA5
					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
						lincom z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not
						estadd sca pw_sumcoef = r(estimate)
						estadd sca sepw = r(se)
						estadd sca stdpw = (r(estimate)/`mend1')*100
						test z_mw_prod_not + l.z_mw_prod_not + l2.z_mw_prod_not = 0 
						estadd sca pvalpw = r(p)
						
					test z_CPI + l.z_CPI + l2.z_CPI  =  z_PPI + l.z_PPI + l2.z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local cfe "Yes"
					estadd local wt " "
					
					est sto regA6
					
esttab regA*  using "$output/table_A18.tex", replace se noconstant drop(*)  ///
					 star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					 scalars(  ///
						"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Two-way SE " "pvalp \hspace{15pt} p-value"  ///
						"pw_sumcoef \\  Producer Price Index in neighboring cells" "sepw \hspace{15pt} Two-way SE" "pvalpw \hspace{15pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} Two-way SE " "pvalc \hspace{15pt} p-value"  ///
						"stdp \hline \\ PPI impact (\%)"   ///
						"stdpw  PPI impact in neighboring cells (\%)"   ///
						"stdc  CPI impact (\%)"   ///
									"wt  Wald test: PPI = CPI"  ///		
						"test_cp  \hspace{15pt} p-value"  ///
						"cyfe \hline \\ Country $\times$ year FE" "ctt Country $\times$ time trend"  "cfe Cell FE"   /// 
						 "r2 R squared"  "N Observations")  ///
						mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1  0 1  0)  ///
						prefix(\multicolumn{@span}{c}{) suffix(})  ///
						span erepeat(\cmidrule(lr){@span}))  ///
					 sfmt( 	%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f  %10.1f  %10.1f  ///
							%10.3f  %10.3f   ///
							%~12s %~12s %~12s ///
							%10.3f %10.0f  )  /// 
					 label obslast nomtitles 


*******************************
*******		TABLE A19	*******
*******************************	
use "$dir/input_rep/cell_data.dta", clear

	cap est drop reg*
			est clear
			
			summ acled_riot_67
			loc macled = r(mean)	
			summ onset_acled_riot_67
			loc monset = r(mean)
			summ end1_acled_riot_67
			loc mend1 = r(mean)
			
		reghdfe 		acled_riot_67 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
							
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`macled')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)
													
							estadd local cyfe "Yes"
							estadd local ctt "N/A"
							estadd local cfe "Yes"	
							
							est sto regE1

		reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
							
							lincom z_CPI + l.z_CPI + l2.z_CPI
							estadd sca c_sum_coef = r(estimate)
							estadd sca sec = r(se)
							estadd sca stdc = (r(estimate)/`macled')*100
							test z_CPI + l.z_CPI + l2.z_CPI = 0
							estadd sca pvalc = r(p)
			
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`macled')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)

													
							estadd local cyfe "No"
							estadd local ctt "Yes"
							estadd local cfe "Yes"	
							
							est sto regE2

		reghdfe 		onset_acled_riot_67 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
							
			
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`monset')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)
													
							estadd local cyfe "Yes"
							estadd local ctt "N/A"
							estadd local cfe "Yes"	
							
							est sto regE3
							
		reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
							lincom z_CPI + l.z_CPI + l2.z_CPI
							estadd sca c_sum_coef = r(estimate)
							estadd sca sec = r(se)
							estadd sca stdc = (r(estimate)/`monset')*100
							test z_CPI + l.z_CPI + l2.z_CPI = 0
							estadd sca pvalc = r(p)
			
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`monset')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)
							
						
													
							estadd local cyfe "No"
							estadd local ctt "Yes"
							estadd local cfe "Yes"	
							
							est sto regE4

		reghdfe 		end1_acled_riot_67 L(0/2).z_PPI ln_ex_pop,  a(cy cell) cluster (cell cy)
							
			
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`mend1')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)
													
							estadd local cyfe "Yes"
							estadd local ctt "N/A"
							estadd local cfe "Yes"	
							
							est sto regE5
							
							
		reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct* ln_ex_pop,  a(cell) cluster (cell cy)
							lincom z_CPI + l.z_CPI + l2.z_CPI
							estadd sca c_sum_coef = r(estimate)
							estadd sca sec = r(se)
							estadd sca stdc = (r(estimate)/`mend1')*100
							test z_CPI + l.z_CPI + l2.z_CPI = 0
							estadd sca pvalc = r(p)
			
							lincom z_PPI + l.z_PPI + l2.z_PPI
							estadd sca p_sumcoef = r(estimate)
							estadd sca sep = r(se)
							estadd sca stdp = (r(estimate)/`mend1')*100
							test z_PPI + l.z_PPI + l2.z_PPI = 0 
							estadd sca pvalp = r(p)
							
							lincom ln_ex_pop
							estadd sca pop_coef = r(estimate)
							estadd sca pop_se = r(se)
							test ln_ex_pop = 0
							estadd sca pop_pval = r(p)
			
													
							estadd local cyfe "No"
							estadd local ctt "Yes"
							estadd local cfe "Yes"	
							estadd local wt " "	

							est sto regE6
							
				esttab regE*  using "$output/table_A19.tex", replace se noconstant drop(*)  ///
							star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
							scalars(  ///
							"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} SE" "pvalp \hspace{15pt} p-value"  ///
							"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///		
							"pop_coef \\ ln Population" "pop_se \hspace{15pt} SE" "pop_pval \hspace{15pt} p-value"  ///		
							"stdp \hline \\ PPI impact (\%)"   ///
							"stdc  CPI impact (\%)"   ///
							"cyfe \hline \\  Country $\times$ Year FE" ///
							"ctt   Country $\times$ time trend" ///
							"cfe  Cell FE" ///
							"N Observations")  ///
							mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0  1 0 1 0)  ///
							prefix(\multicolumn{@span}{c}{) suffix(})  ///
							span erepeat(\cmidrule(lr){@span}))  ///
							sfmt( 	%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f ///
							%10.4f %10.3f %10.3f ///
							%10.1f  %10.1f  ///
							%~12s %~12s %~12s ///
							%10.0f  )  /// 
							label obslast nomtitles 


*******************************
*******		TABLE A20	*******
*******************************	
use "$dir/input_rep/cell_data.dta", clear

cap est drop reg*
	est clear
	
*	Incidence
	clogit acled_riot_67 	z_CPI l_z_CPI l2_z_CPI ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI year , group(cell) cluster(country)
	
	lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
		estadd sca sepf = r(se)
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
		estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
		estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
		
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"
	est sto reg11

*	Onset
	clogit onset_acled_riot_67 z_CPI l_z_CPI l2_z_CPI ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI year , group(cell) cluster(country)
	
	lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
		estadd sca sepf = r(se)
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
		estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
		
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"	
	est sto reg12
	
*	Offset
	clogit end1_acled_riot_67 	z_CPI l_z_CPI l2_z_CPI ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI year , group(cell) cluster(country)
	
		lincom z_CPI + l_z_CPI + l2_z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca sec = r(se)
	test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
		estadd sca sepf = r(se)
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
		estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
		
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local tt "Yes"
	estadd local fe "Yes"
	est sto reg13


	esttab reg1* using "$output/table_A20.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} SE" "pvalpf \hspace{15pt} p-value"  ///
		"pc_sumcoef \\ Producer Price Index: Cash crops" "sepc \hspace{15pt} SE" "pvalpc \hspace{15pt} p-value"  ///
		"c_sumcoef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///
		"wt \hline \\  Wald test: PPI food = PPI Cash"  ///		
		"test_fc  \hspace{15pt} p-value"  ///
		 "tt \hline \\ Time trend" "fe Cell fixed effects"   /// 
		 "r2_p Pseudo-R squared"  "N Observations")  ///
		mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1  1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f  ///
			 %10.3f %10.3f  ///
			 %~12s  %~12s  ///
			%10.3f %10.0f  )  /// 
     label obslast nomtitles 


*******************************
*******		TABLE A21	*******
*******************************	
use "$dir/input_rep/cell_data.dta", clear

foreach i in b96 b88 {

	loneway `i'_tr_area_price_cell cell
	loc psd`i' = r(sd_w)
	summ `i'_tr_area_price_cell
	loc pmean`i' = r(mean)
	gen z_`i'_PPI = (`i'_tr_area_price_cell-`pmean`i'')/`psd`i''

	loneway `i'_tr_area_food_1pc cell
	loc fpsd`i' = r(sd_w)
	summ `i'_tr_area_food_1pc
	loc fpmean`i' = r(mean)
	gen z_`i'_food_PPI = (`i'_tr_area_food_1pc-`fpmean`i'')/`fpsd`i''
	
	loneway `i'_tr_area_cash_1pc cell
	loc cpsd`i' = r(sd_w)
	summ `i'_tr_area_cash_1pc
	loc cpmean`i' = r(mean)
	gen z_`i'_cash_PPI = (`i'_tr_area_cash_1pc-`cpmean`i'')/`cpsd`i''

	loneway `i'_all_tr_basket_price_country cell
	loc csd`i' = r(sd_w)
	summ `i'_all_tr_basket_price_country
	loc cmean`i' = r(mean)
	gen z_`i'_CPI = (`i'_all_tr_basket_price_country-`cmean`i'')/`csd`i''
	}
	
cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
	reghdfe 		acled_riot_67 L(0/2).zt_CPI L(0/2).zt_food_PPI L(0/2).zt_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI = 0
					estadd sca pvalpc = r(p)


					
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA1
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)					
	reghdfe 		acled_riot_67 L(0/2).z_b96_CPI L(0/2).z_b96_food_PPI  L(0/2).z_b96_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI = 0
					estadd sca pvalpc = r(p)


		
					
					estadd local tw "BL 1996"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2
					
reghdfe 		acled_riot_67 L(0/2).z_b88_CPI L(0/2).z_b88_food_PPI L(0/2).z_b88_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI = 0
					estadd sca pvalpc = r(p)


	
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA3
					
	reghdfe 		onset_acled_riot_67 L(0/2).zt_CPI L(0/2).zt_food_PPI L(0/2).zt_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`monset')*100
					test zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`monset')*100
					test zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI = 0
					estadd sca pvalpc = r(p)
					
	
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					est sto regA4
					
	reghdfe 		onset_acled_riot_67 L(0/2).z_b96_CPI L(0/2).z_b96_food_PPI  L(0/2).z_b96_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`monset')*100
					test z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`monset')*100
					test z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI = 0
					estadd sca pvalpc = r(p)

	
					estadd local tw "BL 1996"
					estadd local ctt "Yes"
					estadd local cfe "Yes"

					est sto regA5
					
	reghdfe 		onset_acled_riot_67 L(0/2).z_b88_CPI L(0/2).z_b88_food_PPI L(0/2).z_b88_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`monset')*100
					test z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`monset')*100
					test z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI = 0
					estadd sca pvalpc = r(p)

						
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA6
					
	reghdfe 		end1_acled_riot_67 L(0/2).zt_CPI L(0/2).zt_food_PPI L(0/2).zt_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom zt_CPI + l.zt_CPI + l2.zt_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test zt_CPI + l.zt_CPI + l2.zt_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`mend1')*100
					test zt_food_PPI + l.zt_food_PPI + l2.zt_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`mend1')*100
					test zt_cash_PPI + l.zt_cash_PPI + l2.zt_cash_PPI = 0
					estadd sca pvalpc = r(p)
					
						
					estadd local tw "Avg"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					est sto regA7
					
	reghdfe 		end1_acled_riot_67 L(0/2).z_b96_CPI L(0/2).z_b96_food_PPI  L(0/2).z_b96_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_b96_CPI + l.z_b96_CPI + l2.z_b96_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`mend1')*100
					test z_b96_food_PPI + l.z_b96_food_PPI + l2.z_b96_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`mend1')*100
					test z_b96_cash_PPI + l.z_b96_cash_PPI + l2.z_b96_cash_PPI = 0
					estadd sca pvalpc = r(p)


					estadd local tw "BL 1996"
					estadd local ctt "Yes"
					estadd local cfe "Yes"

					est sto regA8
					
	reghdfe 		end1_acled_riot_67 L(0/2).z_b88_CPI L(0/2).z_b88_food_PPI L(0/2).z_b88_cash_PPI _ct*,  a(cell) cluster (cell cy)
	
					lincom z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_b88_CPI + l.z_b88_CPI + l2.z_b88_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`mend1')*100
					test z_b88_food_PPI + l.z_b88_food_PPI + l2.z_b88_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`mend1')*100
					test z_b88_cash_PPI + l.z_b88_cash_PPI + l2.z_b88_cash_PPI = 0
					estadd sca pvalpc = r(p)

					
					estadd local tw "BL 1988"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					
					est sto regA9
					
					esttab regA*  using "$output/table_A21.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} SE" "pvalpf \hspace{15pt} p-value"  ///
					"pc_sumcoef \\ Producer Price Index: Cash crops" "sepc \hspace{15pt} SE" "pvalpc \hspace{15pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///		
					"stdpf \hline \\ PPI impact: food (\%)"   ///
					"stdpc PPI impact: cash (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"tw \hline \\ Trade weight"   /// 
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 0 1 0 0 1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f ///
					%10.4f %10.3f %10.3f ///
					%10.1f  %10.1f  %10.1f  ///
					%~12s %~12s %~12s  ///
					%10.0f  )  /// 
					label obslast nomtitles 
					

*******************************
*******		TABLE A22	*******
*******************************	
use "$dir/input_rep/cell_data.dta", clear		

	loneway yield_t_PPI cell
	loc psd_y = r(sd_w)
	summ yield_t_PPI
	loc pmean_y = r(mean)
	cap gen z_yield_t_PPI = (yield_t_PPI-`pmean_y')/`psd_y'
	
	loneway yield_PPI cell
	loc psd_ynot = r(sd_w)
	summ yield_PPI
	loc pmean_ynot = r(mean)
	cap gen z_yield_PPI = (yield_PPI-`pmean_ynot')/`psd_ynot'

*			2. Output conflict (CYFE + CTT) x 3

	cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)

reghdfe 		acled_riot_67  L(0/2).z_yield_PPI ,  a(cell cy) cluster (cell cy)

					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					
					est sto regA1
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2

reghdfe 		onset_acled_riot_67 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA3
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)

					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					 
					est sto regA4
					
reghdfe 		end1_acled_riot_67 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
				
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					 
					est sto regA5

reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI + l.z_CPI + l2.z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l.z_CPI + l2.z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_yield_PPI + l.z_yield_PPI + l2.z_yield_PPI = 0 
					estadd sca pvalp = r(p)
					
	
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					 
					est sto regA6
					
		esttab regA*  using "$output/table_A22.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index $\times$ Yield"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"cyfe \hline \\ Country $\times$ year FE" ///
					"ctt  Country $\times$ time trend" ///
					"cfe  Cell FE" ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 		

					

*******************************
*******		TABLE A23	*******
*******************************
use "$dir/input_rep/cell_data.dta", clear						
					
cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
reghdfe 		acled_riot_67  z_food_PPI z_cash_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI  = 0 
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)	
					
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					
					est sto regA1
					
reghdfe 		acled_riot_67 z_CPI z_food_PPI z_cash_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI  = 0 
					estadd sca pvalpf = r(p)
						
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)	
									
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
				
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					
					est sto regA2
					
reghdfe 		onset_acled_riot_67  z_food_PPI z_cash_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`monset')*100
					test z_food_PPI  = 0 
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)
					
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
				
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					
					est sto regA3
					
				
reghdfe 		onset_acled_riot_67 z_CPI z_food_PPI z_cash_PPI _ct*,  a(cell ) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI  = 0
					estadd sca pvalc = r(p)
	
					lincom z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`monset')*100
					test z_food_PPI  = 0 
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)
						
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "No"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
				
					est sto regA4
					
reghdfe 		end1_acled_riot_67  z_food_PPI z_cash_PPI ,  a(cy cell) cluster (cell cy)
				
					lincom z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`mend1')*100
					test z_food_PPI = 0 
					estadd sca pvalpf = r(p)
										
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)
					
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
					
					estadd local cyfe "Yes"
					estadd local ctt "N/A"
					estadd local cfe "Yes"
					estadd local wt " "
				
					est sto regA5
					
reghdfe 		end1_acled_riot_67 z_CPI z_food_PPI z_cash_PPI _ct*,  a(cell) cluster (cell cy)
					
					lincom z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI  = 0
					estadd sca pvalc = r(p)
	
					lincom z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`mend1')*100
					test z_food_PPI = 0 
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI  = 0 
					estadd sca pvalpc = r(p)
					
					test z_cash_PPI  =  z_food_PPI 
					estadd sca test_cp = r(p)
										
					estadd local cyfe "no"
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
				
					est sto regA6
					
			esttab regA*  using "$output/table_A23.tex", replace se noconstant drop(*)  ///
						star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
						scalars(  ///
						"pf_sumcoef \\ Producer Price Index: Food crops"  "sepf \hspace{15pt} Two-way SE" "pvalpf \hspace{25pt} p-value"  ///
						"pc_sumcoef \\ Producer Price Index: Cash crops"  "sepc \hspace{15pt} Two-way SE" "pvalpc \hspace{25pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///		
						"stdpf \hline \\ PPI impact: food (\%)"   ///
						"stdpc  PPI impact: cash (\%)"   ///
						"stdc  CPI impact (\%)"   ///
						"wt  Wald test: PPI food = PPI cash"  ///		
						"test_cp  \hspace{15pt} p-value"  ///
						"cyfe \hline \\ Country $\times$ year FE" ///
						"ctt Country $\times$ time trend" ///
						"cfe  Cell FE" ///
						"N Observations")  ///
						mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
						prefix(\multicolumn{@span}{c}{) suffix(})  ///
						span erepeat(\cmidrule(lr){@span}))  ///
						sfmt( 	%10.4f %10.3f %10.3f  ///
						%10.4f %10.3f %10.3f  ///
						%10.4f %10.3f %10.3f  ///
						%10.1f  %10.1f %10.1f  ///
						%10.3f %10.3f ///
						%~12s %~12s %~12s ///
						%10.0f  )  /// 
						label obslast nomtitles 
						
						
*******************************
*******		TABLE A24	*******
*******************************
use "$dir/input_rep/cell_data.dta", clear	


cap est drop reg*
	est clear
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	summ ucdp_10
	loc mucdp = r(mean)
	
	cap gen pf_priceXpop = z_food_PPI*population 
	cap gen pc_priceXpop = z_cash_PPI*population 
	
	foreach var of varlist pctareaurban urbanpopulation {
		qui summ `var', det
		loc p5 = `r(p5)'
		loc p95 = `r(p90)'
		
		cap gen c_priceX`var' = z_CPI*`var'
	
*5.1	Incidence: separated index / with trade weight / consumer
	reghdfe acled_riot_67  L(0/2).z_CPI	L(0/2).z_food_PPI L(0/2).z_cash_PPI  L(0/2).c_priceX`var' _ct_* , a(cell ) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sum_coef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca stdpf = (r(estimate)/`macled')*100
	estadd sca pvalpf =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sepf = r(se)

	lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`macled')*100
	estadd sca pvalpc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sepc = r(se)
	
	lincom c_priceX`var' + l.c_priceX`var' + l2.c_priceX`var'
	estadd sca c_sum_`var' = r(estimate)
	estadd sca pvalc_`var' =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec`var' = r(se)
		
	lincom z_CPI + l.z_CPI + l2.z_CPI + `p5'*c_priceX`var'   + `p5'*l.c_priceX`var'  + `p5'*l2.c_priceX`var' 
	estadd sca c_sum_coef5_`var' = r(estimate)
	estadd sca c_std5_`var' = (r(estimate)/`macled')*100
				
	lincom z_CPI + l.z_CPI + l2.z_CPI +  `p95'*c_priceX`var'   + `p95'*l.c_priceX`var'  + `p95'*l2.c_priceX`var' 
	estadd sca c_sum_coef95_`var' = r(estimate)
	estadd sca c_std95_`var' = (r(estimate)/`macled')*100
	
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local ctt "Yes"
	estadd local cyfe "No"
	estadd local fe "Yes"
	est sto reg51`var'

	
*5.2	Incidence: separated index / with trade weight / CYFE
	reghdfe acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  L(0/2).c_priceX`var'  , a(cell cy) cluster(cell cy)
	
	lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca stdpf = (r(estimate)/`macled')*100
	estadd sca pvalpf =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sepf = r(se)

	lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`macled')*100
	estadd sca pvalpc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sepc = r(se)
	
	lincom c_priceX`var' + l.c_priceX`var' + l2.c_priceX`var'
	estadd sca c_sum_`var' = r(estimate)
	estadd sca pvalc_`var' =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec`var' = r(se)
		
	lincom  `p5'*c_priceX`var'   + `p5'*l.c_priceX`var'  + `p5'*l2.c_priceX`var' 
	estadd sca c_sum_coef5_`var' = r(estimate)
	estadd sca c_std5_`var' = (r(estimate)/`macled')*100
				
	lincom `p95'*c_priceX`var'   + `p95'*l.c_priceX`var'  + `p95'*l2.c_priceX`var' 
	estadd sca c_sum_coef95_`var' = r(estimate)
	estadd sca c_std95_`var' = (r(estimate)/`macled')*100
	
	
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"
	estadd local ctt "N/a"
	estadd local fe "Yes"	
		est sto reg52`var'

	}
	
	esttab reg5* using "$output/table_A24.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\  Producer Price Index: Food crops" "sepf \hspace{15pt} SE " "pvalpf \hspace{15pt} p-value"  ///
		"pc_sumcoef \\  Producer Price Index: Cash crops" "sepc \hspace{15pt} SE" "pvalpc \hspace{15pt} p-value"  ///
		"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///
		"c_sum_pctareaurban \\  Consumer Price Index $\times$ urban area" "secpctareaurban \hspace{15pt} SE" "pvalc_pctareaurban \hspace{15pt} p-value"  ///
		"c_sum_urbanpopulation \\ Consumer Price Index $\times$ urban population" "securbanpopulation \hspace{15pt} SE" "pvalc_urbanpopulation \hspace{15pt} p-value"  ///	
		"c_std5_pctareaurban \hline \\ CPI impact (\%) at urban area = 0 " "c_std95_pctareaurban CPI impact (\%) at urban area 90th pctile"  ///
		"c_std5_urbanpopulation  \\ CPI impact (\%) at urban pop = 0" "c_std95_urbanpopulation CPI impact (\%) at urban pop 90th pctile"  ///
		"ctt \hline \\ Country $\times$ time trend" "cyfe Country $\times$ year fixed effects" "fe Cell FE"   /// 
		 "r2 R squared"  "N Observations")  ///
		mgroups("ACLED Incidence: 1(Conflict $>0$)", pattern(1 0 0 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ {t-k} _{t-k}        )  /// 
     sfmt( 	%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.1f %10.1f %10.1f %10.1f   ///
			%~12s %~12s %~12s  ///
			%10.3f %10.0f  )  /// 
     label obslast nomtitles 
	


*******************************
*******		TABLE A25	*******
*******************************
use "$dir/input_rep/cell_data.dta", clear	

	summ acled_riot_67
	loc macled67 = r(mean)	
	summ acled_fight_2
	loc macled2 = r(mean)	
	summ ucdp_10
	loc mucdp = r(mean)	

*1	ACLED OUTPUT CONFLICT  1997 - 2013 
	reghdfe acled_riot_67  L(0/2).z_CPI	L(0/2).z_PPI _ct_* , a(cell ) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled67')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled67')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)

	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)

	estadd local dv "Output Conflict"	
	estadd local yr "1997-2013"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	est sto reg1
	
*2	ACLED OUTPUT CONFLICT  1997 - 2010
	reghdfe acled_riot_67  L(0/2).z_CPI	L(0/2).z_PPI _ct_* if year <= 2010, a(cell ) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled67')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled67')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)

	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)


	estadd local dv "ACLED Output"	
	estadd local yr "1997-2010"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	est sto reg2
		
*3	ACLED TERRITORY   1997 - 2013 
	reghdfe acled_fight_2 	L(0/2).z_CPI 	L(0/2).z_PPI _ct_* , a(cell) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled2')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled2')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)

	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)
	
	estadd local dv "ACLED Territory"	
	estadd local yr "1997-2013"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	est sto reg3
		
*4	ACLED TERRITORY   1997 - 2010 
	reghdfe acled_fight_2 	L(0/2).z_CPI 	L(0/2).z_PPI _ct_* if year <= 2010, a(cell) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled2')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled2')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)

	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)

	
	estadd local dv "ACLED Territory"	
	estadd local yr "1997-2010"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	est sto reg4
	
*5	UCDP FACTOR   1989 - 2010 
 	reghdfe ucdp_10 	L(0/2).z_CPI 	L(0/2).z_PPI _ct_* , a(cell) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`mucdp')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`mucdp')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)
	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)
	
	estadd local dv "UCDP Factor"	
	estadd local yr "1989-2010"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	estadd local wt1 " "
	est sto reg5
	
 
*6	UCDP FACTOR   1997 - 2013 
	reghdfe ucdp_10 	L(0/2).z_CPI 	L(0/2).z_PPI _ct_* if year >= 1997 & year <= 2010, a(cell) cluster(cell cy)
	
	lincom z_CPI + l.z_CPI + l2.z_CPI
	estadd sca c_sumcoef = r(estimate)
	estadd sca stdc = (r(estimate)/`mucdp')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom z_PPI + l.z_PPI + l2.z_PPI
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`mucdp')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)

	
	test z_CPI + l.z_CPI + l2.z_CPI = z_PPI + l.z_PPI + l2.z_PPI  
	estadd sca test_cp = r(p)

	estadd local dv "UCDP Factor"	
	estadd local yr "1997-2010"
	estadd local ctt "Yes"
	estadd local fe "Yes"
	est sto reg6

 
  esttab reg5 reg6 reg3 reg4 reg1 reg2  using "$output/table_A25.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Two-way SE " "pvalp \hspace{15pt} p-value"  ///
		"c_sumcoef \\ Consumer Price Index" "sec \hspace{15pt} SE" "pvalc \hspace{15pt} p-value"  ///
		"stdp \hline \\ PPI Impact"  ///
		"stdc  CPI impact (\%)"   ///
		"wt1  Wald test: PPI (total) = CPI"  ///		
		"test_cp  \hspace{15pt} p-value"  ///
		 "yr \hline \\ Sample"  "ctt Country $\times$ time trend" "fe Cell fixed effects"   /// 
		    "N Observations")  ///
		mgroups("\shortstack{UCDP\\Factor Conflict}" "\shortstack{ACLED\\Territorial Change}"  "\shortstack{ACLED\\Output Conflict}", pattern(1 0 1 0 1 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ price{t-k} price_{t-k})  /// 
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f  ///
			%10.1f  %10.1f  ///
			%10.3f  %10.3f  ///
			 %~12s %~12s %~12s  ///
			 %10.0f  )  /// 
     label obslast nomtitles 	
	
	

*******************************
*******		TABLE A29	*******
*******************************
use "$dir/input_rep/cell_data.dta", clear	

			foreach var of varlist  lit_92    mnt capdist centr_tribe gr d_gr   dist_lights92 distancetoport bdist1 {
			cap gen z_CPIX`var' = z_CPI*`var'
			cap gen z_PPIX`var' = z_PPI*`var'
			cap gen l_z_CPIX`var' = l.z_CPI*`var'
			cap gen l_z_PPIX`var' = l.z_PPI*`var'
			cap gen l2_z_CPIX`var' = l2.z_CPI*`var'
			cap gen l2_z_PPIX`var' = l2.z_PPI*`var'
			}
		
		global t_newcon92 = 	"zt_CPIXlit_92			l_zt_CPIXlit_92	l2_zt_CPIXlit_92		zt_PPIXlit_92		l_zt_PPIXlit_92			l2_zt_PPIXlit_92	zt_CPIXdist_lights92  	zt_CPIXcapdist	zt_CPIXdistancetoport	zt_CPIXbdist1	 	l_zt_CPIXdist_lights92	l_zt_CPIXcapdist	l_zt_CPIXdistancetoport	l_zt_CPIXbdist1	 	l2_zt_CPIXdist_lights92		l2_zt_CPIXcapdist 		l2_zt_CPIXdistancetoport	l2_zt_CPIXbdist1	 zt_PPIXdist_lights92	zt_PPIXcapdist		zt_PPIXdistancetoport	zt_PPIXbdist1	 		l_zt_PPIXdist_lights92 	l_zt_PPIXcapdist  	l_zt_PPIXdistancetoport	l_zt_PPIXbdist1	 		l2_zt_PPIXdist_lights92 l2_zt_PPIXcapdist 	l2_zt_PPIXdistancetoport	l2_zt_PPIXbdist1	 "
		global newcon92 = 		"z_CPIXlit_92			l_z_CPIXlit_92	l2_z_CPIXlit_92			z_PPIXlit_92		l_z_PPIXlit_92			l2_z_PPIXlit_92		z_CPIXdist_lights92  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights92	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights92		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights92		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights92 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights92 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	"


				cap est drop reg*
				est clear
				
		summ ucdp_10
		loc mucdp = r(mean)	
		reghdfe ucdp_10  L(0/2).z_CPIXd_gr L(0/2).z_PPIXd_gr L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXd_gr + l.z_PPIXd_gr + l2.z_PPIXd_gr 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXd_gr + l.z_PPIXd_gr + l2.z_PPIXd_gr  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local xc "No"
					est sto regH1
					
					
reghdfe ucdp_10  L(0/2).z_CPIXd_gr L(0/2).z_PPIXd_gr L(0/2).z_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXd_gr + l.z_PPIXd_gr + l2.z_PPIXd_gr 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXd_gr + l.z_PPIXd_gr + l2.z_PPIXd_gr  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local xc "Yes"
					est sto regH2

						
	esttab regH1 regH2  using "$output/table_A29.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
		"l_psumcoef \\ Producer Price Index $\times$ Precolonial political centralization" "l_sep \hspace{15pt} Two-way SE" "l_pvalp \hspace{25pt} p-value"  ///
		"l_c_sum_coef \\ Consumer Price Index $\times$ Precolonial political centralization" "l_sec \hspace{15pt} Two-way SE" "l_pvalc \hspace{25pt} p-value"  ///
		"stdp \hline \\ PPI impact (\%)"   ///
		"l_stdp  PPI impact (\%) $\times$ Precolonial political centralization"   ///
		"l_stdc  CPI impact (\%) $\times$ Precolonial political centralization"   ///
		"cy  \hline \\  Country $\times$ year FE" ///
		"fe  Cell FE" ///
		"xc  Extra Controls" ///
		"N Observations")  ///
		mgroups("\shortstack{Incidence: \\ 1(Conflict $>0$)}", pattern(1  0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ price{t-k} price_{t-k})  /// 
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.1f  %10.1f %10.1f  ///
			 %~12s %~12s %~12s  ///
			%10.0f  )  /// 
     label obslast nomtitles 
	 

*******************************
*******		TABLE A30	*******
*******************************
	
use "$dir/input_rep/cell_data.dta", clear	

			foreach var of varlist  lit_92    mnt capdist centr_tribe gr d_gr   dist_lights92 distancetoport bdist1 {
			cap gen z_CPIX`var' = z_CPI*`var'
			cap gen z_PPIX`var' = z_PPI*`var'
			cap gen l_z_CPIX`var' = l.z_CPI*`var'
			cap gen l_z_PPIX`var' = l.z_PPI*`var'
			cap gen l2_z_CPIX`var' = l2.z_CPI*`var'
			cap gen l2_z_PPIX`var' = l2.z_PPI*`var'
			cap gen z_food_PPIX`var' = z_food_PPI*`var'
			cap gen z_cash_PPIX`var' = z_cash_PPI*`var'
			cap gen l_z_food_PPIX`var' = l.z_food_PPI*`var'
			cap gen l_z_cash_PPIX`var' = l.z_cash_PPI*`var'
			cap gen l2_z_food_PPIX`var' = l2.z_food_PPI*`var'
			cap gen l2_z_cash_PPIX`var' = l2.z_cash_PPI*`var'
			}
		
		global newcon92 = 		"z_CPIXlit_92			l_z_CPIXlit_92	l2_z_CPIXlit_92			z_PPIXlit_92		l_z_PPIXlit_92			l2_z_PPIXlit_92		z_CPIXdist_lights92  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights92	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights92		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights92		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights92 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights92 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	"

	
				cap est drop reg*
				est clear
				
		summ acled_riot_67
		loc macled = r(mean)	
		reghdfe acled_riot_67  L(0/2).z_CPIXd_gr L(0/2).z_food_PPIXd_gr L(0/2).z_food_PPI L(0/2).z_cash_PPIXd_gr L(0/2).z_cash_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`macled')*100
					test z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_food_PPIXd_gr + l.z_food_PPIXd_gr + l2.z_food_PPIXd_gr 
					estadd sca lf_psumcoef = r(estimate)
					estadd sca lf_sep = r(se)
					estadd sca lf_stdp = (r(estimate)/`macled')*100
					test z_food_PPIXd_gr + l.z_food_PPIXd_gr + l2.z_food_PPIXd_gr  = 0
					estadd sca lf_pvalp = r(p)
					
					lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPIXd_gr + l.z_cash_PPIXd_gr + l2.z_cash_PPIXd_gr 
					estadd sca lc_psumcoef = r(estimate)
					estadd sca lc_sep = r(se)
					estadd sca lc_stdp = (r(estimate)/`macled')*100
					test z_cash_PPIXd_gr + l.z_cash_PPIXd_gr + l2.z_cash_PPIXd_gr  = 0
					estadd sca lc_pvalp = r(p)
					
					lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI = 0
					estadd sca pvalpc = r(p)
					
					estadd local tw "No"
					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local xc "No"
					estadd local yr "1992"
					est sto regH1
					
					
reghdfe acled_riot_67  L(0/2).z_CPIXd_gr L(0/2).z_food_PPIXd_gr L(0/2).z_food_PPI L(0/2).z_cash_PPIXd_gr L(0/2).z_cash_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`macled')*100
					test z_CPIXd_gr + l.z_CPIXd_gr + l2.z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_food_PPIXd_gr + l.z_food_PPIXd_gr + l2.z_food_PPIXd_gr 
					estadd sca lf_psumcoef = r(estimate)
					estadd sca lf_sep = r(se)
					estadd sca lf_stdp = (r(estimate)/`macled')*100
					test z_food_PPIXd_gr + l.z_food_PPIXd_gr + l2.z_food_PPIXd_gr  = 0
					estadd sca lf_pvalp = r(p)
					
					lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPIXd_gr + l.z_cash_PPIXd_gr + l2.z_cash_PPIXd_gr 
					estadd sca lc_psumcoef = r(estimate)
					estadd sca lc_sep = r(se)
					estadd sca lc_stdp = (r(estimate)/`macled')*100
					test z_cash_PPIXd_gr + l.z_cash_PPIXd_gr + l2.z_cash_PPIXd_gr  = 0
					estadd sca lc_pvalp = r(p)
					
					lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI = 0
					estadd sca pvalpc = r(p)
					
					estadd local tw "No"
					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local xc "Yes"
					est sto regH2

						
	esttab regH1 regH2  using "$output/table_A30.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} Two-way SE" "pvalpf \hspace{25pt} p-value"  ///
		"lf_psumcoef \\ Producer Price Index: Food $\times$ Precolonial political centralization" "lf_sep \hspace{15pt} Two-way SE" "lf_pvalp \hspace{25pt} p-value"  ///
		"pc_sumcoef \\ Producer Price Index: Cash crops" "sepc \hspace{15pt} Two-way SE" "pvalpc \hspace{25pt} p-value"  ///
		"lc_psumcoef \\ Producer Price Index: Cash $\times$ Precolonial political centralization" "lc_sep \hspace{15pt} Two-way SE" "lc_pvalp \hspace{25pt} p-value"  ///
		"l_c_sum_coef \\ Consumer Price Index $\times$ Precolonial political centralization" "l_sec \hspace{15pt} Two-way SE" "l_pvalc \hspace{25pt} p-value"  ///
		"stdpf \hline \\ PPI impact: food (\%)"   ///
		"lf_stdp  PPI impact: food (\%) $\times$ Precolonial political centralization"   ///
		"stdpc PPI impact: cash (\%)"   ///
		"lc_stdp  PPI impact: cash (\%) $\times$ Precolonial political centralization"   ///
		"l_stdc \\  CPI impact (\%) $\times$ Precolonial political centralization"   ///
		"cy \hline \\ Country $\times$ year FE" ///
		"fe  Cell FE" ///
		"xc  Extra Controls" ///
		"N Observations")  ///
		mgroups("\shortstack{Incidence: \\ 1(Conflict $>0$)}", pattern(1  0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ price{t-k} price_{t-k})  /// 
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.1f  %10.1f %10.1f %10.1f  %10.1f  ///
			%~12s %~12s %~12s   ///
			%10.0f  )  /// 
     label obslast nomtitles 





