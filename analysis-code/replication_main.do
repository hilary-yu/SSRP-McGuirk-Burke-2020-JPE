clear
clear all
clear matrix
set mem 1g
set more off, permanently
set maxvar 15000
set matsize 10000
set emptycells drop, permanently

global dir "*your directory*" 
global output "$dir/output_rep/tables" 

use "$dir/input_rep/cell_data.dta", clear

ssc install sutex
ssc install reghdfe
ssc install geonear
ssc	install tmpdir
ssc	install reg2hdfe

do 	"$dir/adofiles/reg2hdfespatial.ado"

do	"$dir/adofiles/ols_spatial_HAC.ado"

***********************************
*******		MAIN TABLES		*******
***********************************

/*
Table 1		Summary Statistics
Table 2		UCDP Factor Conflict, Producer Prices and Consumer Prices
Table 3		UCDP Factor Conflict, Prices and Luminosity
Table 4		ACLED Output Conflict, Combined Producer Prices and Consumer Prices
Table 5		ACLED Output Conflict and Disaggregated Producer Prices
Table 7		Summary of Naive Regression Results
*/


*----------------------------------


***********************************
*******		TABLE 1			*******
***********************************
sutex ucdp_10 onset_ucdp_10 end1_ucdp_10  acled_riot_67 onset_acled_riot_67 end1_acled_riot_67 ///
		      ///
		anycrop	ar_allcrops ex_pop  urbanpopulation pctareaurban distancetocity500k  lit_92 lit_10  if year >= 1989, minmax lab 		
		*Note: stata gives output to copy and paste into file (table_1.tex)

		
***********************************
*******		TABLE 2			*******
***********************************
	est clear

	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)

reghdfe 		ucdp_10  L(0/2).z_PPI ,  a(cell cy) cluster (cell cy)

					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					
					est sto regA1
					
reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "No"

					est sto regA2

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
				
					est sto regA2Y

reghdfe 		onset_ucdp_10 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					 
					est sto regA3
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "No"
					 
					est sto regA4
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
					
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					 
					est sto regA4Y
					
reghdfe 		end1_ucdp_10 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
				
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					 
					est sto regA5

reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
						
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					estadd local yfe "No"
					 
					est sto regA6
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
						
					estadd local cyfe "No"					 
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local wt " "
					estadd local yfe "Yes"
					 
					est sto regA6Y
					
esttab regA*  using "$output/table_2.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Two-way p-value"  ///
					"cyfe \hline \\ Country $\times$ year FE" ///
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
					%~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
				
***********************************
*******		TABLE 3			*******
***********************************

												
			foreach var of varlist  lit_92 lit_00 lit_10  capdist dist_lights00 dist_lights10 dist_lights92 distancetoport bdist1 {
			cap gen z_CPIX`var' = z_CPI*`var'
			cap gen z_PPIX`var' = z_PPI*`var'
			cap gen l_z_CPIX`var' = l.z_CPI*`var'
			cap gen l_z_PPIX`var' = l.z_PPI*`var'
			cap gen l2_z_CPIX`var' = l2.z_CPI*`var'
			cap gen l2_z_PPIX`var' = l2.z_PPI*`var'
		}
		
			global newcon92 = 		"z_CPIXdist_lights92  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights92	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights92		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights92		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights92 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights92 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	"
			global newcon00 = 		"z_CPIXdist_lights00  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights00	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights00		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights00		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights00 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights00 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	 "
			global newcon10 = 		"z_CPIXdist_lights10  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights10	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights10		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights10		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights10 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights10 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	"
					
		cap est drop reg*
		est clear
				
		summ ucdp_10
		loc mucdp = r(mean)	
		reghdfe ucdp_10  L(0/2).z_CPIXlit_92 L(0/2).z_PPIXlit_92 L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXlit_92 + l.z_CPIXlit_92 + l2.z_CPIXlit_92 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_92 + l.z_CPIXlit_92 + l2.z_CPIXlit_92 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_92 + l.z_PPIXlit_92 + l2.z_PPIXlit_92 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_92 + l.z_PPIXlit_92 + l2.z_PPIXlit_92  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "1992"
					est sto regH1
					
					
reghdfe ucdp_10  L(0/2).z_CPIXlit_92 L(0/2).z_PPIXlit_92 L(0/2).z_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)
					
					lincom z_CPIXlit_92 + l.z_CPIXlit_92 + l2.z_CPIXlit_92 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_92 + l.z_CPIXlit_92 + l2.z_CPIXlit_92 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_92 + l.z_PPIXlit_92 + l2.z_PPIXlit_92 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_92 + l.z_PPIXlit_92 + l2.z_PPIXlit_92  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					lincom z_CPIXdist_lights92 + l_z_CPIXdist_lights92 + l2_z_CPIXdist_lights92 
					estadd sca dl_c_sum_coef = r(estimate)
					estadd sca dl_sec = r(se)
					estadd sca dl_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdist_lights92 + l_z_CPIXdist_lights92 + l2_z_CPIXdist_lights92 = 0
					estadd sca dl_pvalc = r(p)
					
					lincom z_PPIXdist_lights92 + l_z_PPIXdist_lights92 + l2_z_PPIXdist_lights92 
					estadd sca dl_psumcoef = r(estimate)
					estadd sca dl_sep = r(se)
					estadd sca dl_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdist_lights92 + l_z_PPIXdist_lights92 + l2_z_PPIXdist_lights92 = 0
					estadd sca dl_pvalp = r(p)
					
					lincom z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport 
					estadd sca dp_c_sum_coef = r(estimate)
					estadd sca dp_sec = r(se)
					estadd sca dp_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport = 0
					estadd sca dp_pvalc = r(p)
					
					lincom z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport 
					estadd sca dp_psumcoef = r(estimate)
					estadd sca dp_sep = r(se)
					estadd sca dp_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport = 0
					estadd sca dp_pvalp = r(p)
					
					lincom z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 
					estadd sca db_c_sum_coef = r(estimate)
					estadd sca db_sec = r(se)
					estadd sca db_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 = 0
					estadd sca db_pvalc = r(p)
					
					lincom z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 
					estadd sca db_psumcoef = r(estimate)
					estadd sca db_sep = r(se)
					estadd sca db_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 = 0
					estadd sca db_pvalp = r(p)
					
					lincom z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist 
					estadd sca dc_c_sum_coef = r(estimate)
					estadd sca dc_sec = r(se)
					estadd sca dc_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist = 0
					estadd sca dc_pvalc = r(p)
					
					lincom z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist 
					estadd sca dc_psumcoef = r(estimate)
					estadd sca dc_sep = r(se)
					estadd sca dc_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist = 0
					estadd sca dc_pvalp = r(p)

					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "1992"
					est sto regH2

reghdfe ucdp_10  L(0/2).z_CPIXlit_00 L(0/2).z_PPIXlit_00 L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)

					lincom z_CPIXlit_00 + l.z_CPIXlit_00 + l2.z_CPIXlit_00 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_00 + l.z_CPIXlit_00 + l2.z_CPIXlit_00 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_00 + l.z_PPIXlit_00 + l2.z_PPIXlit_00 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_00 + l.z_PPIXlit_00 + l2.z_PPIXlit_00  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)

					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "2000"
					est sto regH3
					
reghdfe ucdp_10  L(0/2).z_CPIXlit_00 L(0/2).z_PPIXlit_00 L(0/2).z_PPI ln_ex_pop $newcon00,  a(cell cy) cluster (cell cy)

					lincom z_CPIXlit_00 + l.z_CPIXlit_00 + l2.z_CPIXlit_00 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_00 + l.z_CPIXlit_00 + l2.z_CPIXlit_00 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_00 + l.z_PPIXlit_00 + l2.z_PPIXlit_00 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_00 + l.z_PPIXlit_00 + l2.z_PPIXlit_00  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					lincom z_CPIXdist_lights00 + l_z_CPIXdist_lights00 + l2_z_CPIXdist_lights00 
					estadd sca dl_c_sum_coef = r(estimate)
					estadd sca dl_sec = r(se)
					estadd sca dl_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdist_lights00 + l_z_CPIXdist_lights00 + l2_z_CPIXdist_lights00 = 0
					estadd sca dl_pvalc = r(p)
					
					lincom z_PPIXdist_lights00 + l_z_PPIXdist_lights00 + l2_z_PPIXdist_lights00 
					estadd sca dl_psumcoef = r(estimate)
					estadd sca dl_sep = r(se)
					estadd sca dl_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdist_lights00 + l_z_PPIXdist_lights00 + l2_z_PPIXdist_lights00 = 0
					estadd sca dl_pvalp = r(p)
					
					lincom z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport 
					estadd sca dp_c_sum_coef = r(estimate)
					estadd sca dp_sec = r(se)
					estadd sca dp_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport = 0
					estadd sca dp_pvalc = r(p)
					
					lincom z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport 
					estadd sca dp_psumcoef = r(estimate)
					estadd sca dp_sep = r(se)
					estadd sca dp_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport = 0
					estadd sca dp_pvalp = r(p)
					
					lincom z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 
					estadd sca db_c_sum_coef = r(estimate)
					estadd sca db_sec = r(se)
					estadd sca db_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 = 0
					estadd sca db_pvalc = r(p)
					
					lincom z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 
					estadd sca db_psumcoef = r(estimate)
					estadd sca db_sep = r(se)
					estadd sca db_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 = 0
					estadd sca db_pvalp = r(p)
					
					lincom z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist 
					estadd sca dc_c_sum_coef = r(estimate)
					estadd sca dc_sec = r(se)
					estadd sca dc_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist = 0
					estadd sca dc_pvalc = r(p)
					
					lincom z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist 
					estadd sca dc_psumcoef = r(estimate)
					estadd sca dc_sep = r(se)
					estadd sca dc_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist = 0
					estadd sca dc_pvalp = r(p)

					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "2000"
					est sto regH4
					
	
reghdfe ucdp_10  L(0/2).z_CPIXlit_10 L(0/2).z_PPIXlit_10 L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)

					lincom z_CPIXlit_10 + l.z_CPIXlit_10 + l2.z_CPIXlit_10 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_10 + l.z_CPIXlit_10 + l2.z_CPIXlit_10 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_10 + l.z_PPIXlit_10 + l2.z_PPIXlit_10 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_10 + l.z_PPIXlit_10 + l2.z_PPIXlit_10  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)

					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "2010"
					est sto regH5
					
reghdfe ucdp_10  L(0/2).z_CPIXlit_10 L(0/2).z_PPIXlit_10 L(0/2).z_PPI ln_ex_pop $newcon10,  a(cell cy) cluster (cell cy)

					lincom z_CPIXlit_10 + l.z_CPIXlit_10 + l2.z_CPIXlit_10 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_10 + l.z_CPIXlit_10 + l2.z_CPIXlit_10 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_10 + l.z_PPIXlit_10 + l2.z_PPIXlit_10 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_10 + l.z_PPIXlit_10 + l2.z_PPIXlit_10  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0
					estadd sca pvalp = r(p)
					
					lincom z_CPIXdist_lights10 + l_z_CPIXdist_lights10 + l2_z_CPIXdist_lights10 
					estadd sca dl_c_sum_coef = r(estimate)
					estadd sca dl_sec = r(se)
					estadd sca dl_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdist_lights10 + l_z_CPIXdist_lights10 + l2_z_CPIXdist_lights10 = 0
					estadd sca dl_pvalc = r(p)
					
					lincom z_PPIXdist_lights10 + l_z_PPIXdist_lights10 + l2_z_PPIXdist_lights10 
					estadd sca dl_psumcoef = r(estimate)
					estadd sca dl_sep = r(se)
					estadd sca dl_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdist_lights10 + l_z_PPIXdist_lights10 + l2_z_PPIXdist_lights10 = 0
					estadd sca dl_pvalp = r(p)
					
					lincom z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport 
					estadd sca dp_c_sum_coef = r(estimate)
					estadd sca dp_sec = r(se)
					estadd sca dp_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXdistancetoport + l_z_CPIXdistancetoport + l2_z_CPIXdistancetoport = 0
					estadd sca dp_pvalc = r(p)
					
					lincom z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport 
					estadd sca dp_psumcoef = r(estimate)
					estadd sca dp_sep = r(se)
					estadd sca dp_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXdistancetoport + l_z_PPIXdistancetoport + l2_z_PPIXdistancetoport = 0
					estadd sca dp_pvalp = r(p)
					
					lincom z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 
					estadd sca db_c_sum_coef = r(estimate)
					estadd sca db_sec = r(se)
					estadd sca db_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXbdist1 + l_z_CPIXbdist1 + l2_z_CPIXbdist1 = 0
					estadd sca db_pvalc = r(p)
					
					lincom z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 
					estadd sca db_psumcoef = r(estimate)
					estadd sca db_sep = r(se)
					estadd sca db_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXbdist1 + l_z_PPIXbdist1 + l2_z_PPIXbdist1 = 0
					estadd sca db_pvalp = r(p)
					
					lincom z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist 
					estadd sca dc_c_sum_coef = r(estimate)
					estadd sca dc_sec = r(se)
					estadd sca dc_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXcapdist + l_z_CPIXcapdist + l2_z_CPIXcapdist = 0
					estadd sca dc_pvalc = r(p)
					
					lincom z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist 
					estadd sca dc_psumcoef = r(estimate)
					estadd sca dc_sep = r(se)
					estadd sca dc_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXcapdist + l_z_PPIXcapdist + l2_z_PPIXcapdist = 0
					estadd sca dc_pvalp = r(p)

					estadd local cy "Yes"
					estadd local fe "Yes"
					estadd local yr "2010"
					est sto regH6
					
	esttab regH1 regH2 regH3 regH4 regH5 regH6  using "$output/table_3.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef Producer Price Index" "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
		"l_psumcoef \\ Producer Price Index $\times$ Luminosity" "l_sep \hspace{15pt} Two-way SE" "l_pvalp \hspace{25pt} p-value"  ///
		"dl_psumcoef \\ Producer Price Index $\times$ Dist. to lights" "dl_sep \hspace{15pt} Two-way SE" "dl_pvalp \hspace{25pt} p-value"  ///
		"dp_psumcoef \\ Producer Price Index $\times$ Dist. to port" "dp_sep \hspace{15pt} Two-way SE" "dp_pvalp \hspace{25pt} p-value"  ///
		"db_psumcoef \\ Producer Price Index $\times$ Dist. to border" "db_sep \hspace{15pt} Two-way SE" "db_pvalp \hspace{25pt} p-value"  ///
		"dc_psumcoef \\ Producer Price Index $\times$ Dist. to capital" "dc_sep \hspace{15pt} Two-way SE" "dc_pvalp \hspace{25pt} p-value"  ///	
		"l_c_sum_coef \\ Consumer Price Index $\times$ Luminosity" "l_sec \hspace{15pt} Two-way SE" "l_pvalc \hspace{25pt} p-value"  ///
		"dl_c_sum_coef \\ Consumer Price Index $\times$ Dist. to lights" "dl_sec \hspace{15pt} Two-way SE" "dl_pvalc \hspace{25pt} p-value"  ///
		"dp_c_sum_coef \\ Consumer Price Index $\times$ Dist. to port" "dp_sec \hspace{15pt} Two-way SE" "dp_pvalc \hspace{25pt} p-value"  ///
		"db_c_sum_coef \\ Consumer Price Index $\times$ Dist. to border" "db_sec \hspace{15pt} Two-way SE" "db_pvalc \hspace{25pt} p-value"  ///
		"dc_c_sum_coef \\ Consumer Price Index $\times$ Dist. to capital" "dc_sec \hspace{15pt} Two-way SE" "dc_pvalc \hspace{25pt} p-value"  ///
		"stdp \hline \\ PPI impact (\%)"   ///
		"l_stdp  PPI impact (\%) $\times$ Luminosity"   ///
		"l_stdc  CPI impact (\%) $\times$ Luminosity"   ///
		"yr \hline  Luminosity year"   /// 
		"cy  Country $\times$ year FE" ///
		"fe  Cell FE" ///
		"N Observations")  ///
		mgroups("\shortstack{UCDP Factor Conflict Incidence: 1(Conflict $>0$)}", pattern(1 0 0 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.1f  %10.1f %10.1f  ///
			%~12s %~12s  %~12s ///
			%10.0f  )  /// 
     label obslast nomtitles 			
					
								
												
***********************************
*******		TABLE 4		***********
***********************************	
cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)

reghdfe 		acled_riot_67  L(0/2).z_PPI _ct*,  a(cell cy) cluster (cell cy)
	
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
									
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					
					est sto regA1
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
					estadd local yfe "No"
					
					est sto regA2
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
					estadd local yfe "Yes"
					
					est sto regA2Y

reghdfe 		onset_acled_riot_67 L(0/2).z_PPI _ct*,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					 
					est sto regA3
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
					estadd local yfe "No"
					 
					est sto regA4
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
					estadd local yfe "Yes"
			 
					est sto regA4Y
					
					
					
reghdfe 		end1_acled_riot_67 L(0/2).z_PPI _ct*,  a(cy cell) cluster (cell cy)
						
					lincom z_PPI + l.z_PPI + l2.z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l.z_PPI + l2.z_PPI = 0 
					estadd sca pvalp = r(p)
				
					estadd local cyfe "Yes"					 
					estadd local ctt "N/A"					 
					estadd local cfe "Yes"
					estadd local yfe "No"
					 
					est sto regA5

reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
					
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
					estadd local yfe "No"
					 
					est sto regA6
					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
					
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
					estadd local yfe "Yes"
					 
					est sto regA6Y
					
		esttab regA*  using "$output/table_4.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Two-way SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Two-way SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"cyfe \hline \\ Country $\times$ year FE" ///
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
					%~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
					
***********************************
********	TABLE 5		***********
***********************************	
							
cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
	
reghdfe acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  , a(cell cy) cluster(cell cy)
	
	lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`macled')*100
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`macled')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	est sto reg10
	
	
reghdfe onset_acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  , a(cell cy) cluster(cell cy)
	
	lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`monset')*100
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`monset')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	est sto reg11
	

reghdfe end1_acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  , a(cell cy) cluster(cell cy)
	
	lincom z_food_PPI + l.z_food_PPI + l2.z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`mend1')*100
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`mend1')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l.z_food_PPI + l2.z_food_PPI = z_cash_PPI + l.z_cash_PPI + l2.z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	estadd local wt " "
	est sto reg12
		 
esttab reg1* using "$output/table_5.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} Two-way SE " "pvalpf \hspace{25pt} p-value"  ///
		"pc_sumcoef \\  Producer Price Index: Cash crops" "sepc \hspace{15pt} Two-way SE" "pvalpc \hspace{25pt} p-value"  ///
		"stdpf \hline \\ PPI Impact: Food crops"  ///
		"stdpc PPI Impact: Cash crops"  ///
		"wt Wald test: PPI Food = PPI Cash"  ///		
		"test_fc  \hspace{15pt} Two-way p-value"  ///
		"cyfe  \hline \\ Country $\times$ year FE" "cfe Cell FE"   /// 
		"N Observations")  ///
		mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f /// 
			%10.1f %10.1f  ///
			%10.3f %10.3f   ///
			%~12s %~12s  ///
			%10.0f  )  /// 
     label obslast nomtitles 

	

***********************************
*******		TABLE 7		***********
***********************************	

use "$dir/input_rep/country_data.dta", clear

summ cy_ucdp
	loc mucdp = r(mean)	

summ cy_acled
	loc macled = r(mean)	

	areg cy_ucdp 	L(0/2).cy_area_price_cell _ct*, a(ctry) cluster(ctry)
	lincom cy_area_price_cell + l.cy_area_price_cell + l2.cy_area_price_cell
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`mucdp')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)
	est sto reg01

	areg cy_acled 	L(0/2).cy_area_price_cell _ct*, a(ctry) cluster(ctry)
	lincom cy_area_price_cell + l.cy_area_price_cell + l2.cy_area_price_cell
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)
	est sto reg02

	areg cy_ucdp 	L(0/2).cy_all_basket_price_country _ct*, a(ctry) cluster(ctry)
	lincom cy_all_basket_price_country + l.cy_all_basket_price_country + l2.cy_all_basket_price_country
	estadd sca c_sum_coef = r(estimate)
	estadd sca stdc = (r(estimate)/`mucdp')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	est sto reg03

	areg cy_acled 	L(0/2).cy_all_basket_price_country _ct*, a(ctry) cluster(ctry)
	lincom cy_all_basket_price_country + l.cy_all_basket_price_country + l2.cy_all_basket_price_country
	estadd sca c_sum_coef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	est sto reg04

	areg cy_ucdp 	L(0/2).cy_all_basket_price_country L(0/2).cy_area_price_cell _ct*, a(ctry) cluster(ctry)
	lincom cy_all_basket_price_country + l.cy_all_basket_price_country + l2.cy_all_basket_price_country
	estadd sca c_sum_coef = r(estimate)
	estadd sca stdc = (r(estimate)/`mucdp')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom cy_area_price_cell + l.cy_area_price_cell + l2.cy_area_price_cell
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`mucdp')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)
	est sto reg05

	areg cy_acled 	L(0/2).cy_all_basket_price_country L(0/2).cy_area_price_cell _ct*, a(ctry) cluster(ctry)
	lincom cy_all_basket_price_country + l.cy_all_basket_price_country + l2.cy_all_basket_price_country
	estadd sca c_sum_coef = r(estimate)
	estadd sca stdc = (r(estimate)/`macled')*100
	estadd sca pvalc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sec = r(se)
	
	lincom cy_area_price_cell + l.cy_area_price_cell + l2.cy_area_price_cell
	estadd sca p_sumcoef = r(estimate)
	estadd sca stdp = (r(estimate)/`macled')*100
	estadd sca pvalp =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
	estadd sca sep = r(se)
	est sto reg06


	esttab reg01 reg02 using "$output/table_7.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt}  SE" "pvalp \hspace{15pt} p-value" "stdp \hspace{15pt} Impact (\%)"  ///
		 )  ///
		mgroups("UCDP Conflict" "ACLED Conflict", pattern(1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.1f  %10.1f  ///
			)  /// 
     label obslast nomtitles 
	 
	 esttab reg03 reg04 using "$output/table_7.tex", append se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt}  SE" "pvalc \hspace{15pt} p-value" "stdc \hspace{15pt} Impact (\%)"  ///		
		 )  ///
		mgroups("UCDP Conflict" "ACLED Conflict", pattern(1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.1f  %10.1f  ///
			)  /// 
     label obslast nomtitles 
	 
	 
	 esttab reg05 reg06 using "$output/table_7.tex", append se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt}  SE" "pvalp \hspace{15pt} p-value" "stdp \hspace{15pt} Impact (\%)"  ///
		"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt}  SE" "pvalc \hspace{15pt} p-value" "stdc \hspace{15pt} Impact (\%)"  ///		
		 )  ///
		mgroups("UCDP Conflict" "ACLED Conflict", pattern(1 1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f %10.1f  ///
			%10.4f %10.3f %10.3f  %10.1f  ///
			%10.1f ///
			)  /// 
     label obslast nomtitles 

