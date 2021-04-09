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


ssc install sutex
ssc install reghdfe
ssc install geonear
ssc	install tmpdir
ssc	install reg2hdfe

do 	"$dir/adofiles/reg2hdfespatial.ado"

do	"$dir/adofiles/ols_spatial_HAC.ado"



***********************************
*******		CONLEY SEs		*******
***********************************

/*
Table 2		UCDP Factor Conflict, Producer Prices and Consumer Prices
Table 3		UCDP Factor Conflict, Prices and Luminosity
Table 4		ACLED Output Conflict, Combined Producer Prices and Consumer Prices
Table 5		ACLED Output Conflict and Disaggregated Producer Prices
Table A2	UCDP Factor Conflict, Prices and Luminosity
Table A3	UCDP Factor Conflict w/ YFEs and Added Controls
Table A5	UCDP Factor Conflict: Sensitivity of Standard Errors
Table A6	UCDP Factor Conflict: One Degree Aggregation 
Table A7	UCDP Factor Conflict: One Degree Aggregation w/ Spatial Spillovers
Table A11	UCDP Factor Conflict w/ Yield Weights on PPI
Table A13	ACLED Output Conflict w/ YFEs and Added Controls
Table A16	ACLED Output Conflict: Sensitivity of Standard Errors
Table A17	ACLED Output Conflict: One Degree Aggregation
Table A18	ACLED Output Conflict: One Degree Aggregation w/ Spatial Spillovers
Table A22	ACLED Output Conflict w/ Yield Weights on PPI
Table A29	UCDP Factor Conflict: Precolonial Institutions 
Table A30	ACLED Output Conflict: Precolonial Institutions 
*/


***********************************
***********************************
*	FACTOR CONFLICT CONLEY SEs	  *
***********************************
***********************************


use "$dir/input_rep/cell_data.dta", clear

*	Preparing data
*		Generate residuals
		gen miss_notrade = missing(ucdp_10) | missing(z_PPI) | missing(z_CPI) 
		gen miss_notrade_onset = missing(onset_ucdp_10) | missing(z_PPI) | missing(z_CPI)
		gen miss_notrade_end1 = missing(end1_ucdp_10) | missing(z_PPI) | missing(z_CPI) 
	
*		Outcome vars
		areg ucdp_10 _ct* if miss_notrade == 0, a(cell)
		predict ucdp_resid_not if miss_notrade == 0, resid
		
		areg onset_ucdp_10 _ct* if miss_notrade_onset == 0 , a(cell)
		predict onset_ucdp_resid_not if miss_notrade_onset == 0 , resid
		
		areg end1_ucdp_10 _ct* if miss_notrade_end1 == 0 , a(cell)
		predict end1_ucdp_resid_not if miss_notrade_end1 == 0 , resid
	
*		Incidence
		areg z_PPI _ct* if miss_notrade == 0, a(cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict l2con_resid_not if miss_notrade == 0, resid
	
*		Onset
		areg z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid
	
*		Offset
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid

	foreach var of varlist spi_d2 spi_d3 spi_d4 temp oil_cell oil_country z_p_oil main_lprice_mines main_lprice mines	{
	cap gen l_`var' = l.`var'
	cap gen l2_`var' = l2.`var'
	}
	
********************************
*		TABLE 2 Conley SEs		
********************************	

		summ ucdp_10
		cap gen mucdp = r(mean)	
		summ onset_ucdp_10
		cap gen monset = r(mean)
		summ end1_ucdp_10
		cap gen mend1 = r(mean)
	
		cap est drop reg*
		est clear
						
*	Incidence 	

	reghdfe 		ucdp_10  L(0/2).z_PPI ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1

	reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
	
	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
		estadd sca test_cp = r(p)	

		est sto regA2
		
	reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
		
	reg2hdfespatial ucdp_10 z_PPI l_z_PPI l2_z_PPI z_CPI l_z_CPI l2_z_CPI _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)
		
		test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
		estadd sca test_cp = r(p)	

		est sto regA2Y		
		
*	Onset 	

	reghdfe 		onset_ucdp_10 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)

	reg2hdfespatial onset_ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3

	reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not if e(sample) ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
		estadd sca test_cp = r(p)
		est sto regA4
	
	
	reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)

	reg2hdfespatial onset_ucdp_10 z_PPI l_z_PPI l2_z_PPI z_CPI l_z_CPI l2_z_CPI _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)
		
		test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
		estadd sca test_cp = r(p)	

		est sto regA4Y	
	
*	Ending 
	reghdfe 		end1_ucdp_10 L(0/2).z_PPI ,  a(cy cell) cluster (cell cy)

	reg2hdfespatial end1_ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
								

	reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)
							
	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca test_cp = r(p)	

		estadd local wt " "
	
		est sto regA6
		
	reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
	
	reg2hdfespatial end1_ucdp_10 z_PPI l_z_PPI l2_z_PPI z_CPI l_z_CPI l2_z_CPI _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)
		
		test z_CPI + l_z_CPI + l2_z_CPI = z_PPI + l_z_PPI + l2_z_PPI
		estadd sca test_cp = r(p)	

		est sto regA6Y	
	
esttab regA*  using "$output/table_2con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Conley p-value"  ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 0 1 0  1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.3f %10.3f ///
					%10.0f  )  /// 
					label obslast nomtitles 								

*******************************
*		TABLE 3 Conley SEs		
*******************************
												
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

	reg2hdfespatial ucdp_10 z_CPIXlit_92 l_z_CPIXlit_92 l2_z_CPIXlit_92 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_92 l_z_PPIXlit_92 l2_z_PPIXlit_92 ///
								ln_ex_pop ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
								
					lincom z_CPIXlit_92 + l_z_CPIXlit_92 + l2_z_CPIXlit_92 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_92 + l_z_CPIXlit_92 + l2_z_CPIXlit_92 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_92 + l_z_PPIXlit_92 + l2_z_PPIXlit_92 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_92 + l_z_PPIXlit_92 + l2_z_PPIXlit_92  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH1
	
	reghdfe ucdp_10  L(0/2).z_CPIXlit_92 L(0/2).z_PPIXlit_92 L(0/2).z_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_CPIXlit_92 l_z_CPIXlit_92 l2_z_CPIXlit_92 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_92 l_z_PPIXlit_92 l2_z_PPIXlit_92 ///
								ln_ex_pop $newcon92 ///
								if e(sample),  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
					lincom z_CPIXlit_92 + l_z_CPIXlit_92 + l2_z_CPIXlit_92 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_92 + l_z_CPIXlit_92 + l2_z_CPIXlit_92 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_92 + l_z_PPIXlit_92 + l2_z_PPIXlit_92 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_92 + l_z_PPIXlit_92 + l2_z_PPIXlit_92  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH2	
					

	reghdfe ucdp_10  L(0/2).z_CPIXlit_00 L(0/2).z_PPIXlit_00 L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)

					
	reg2hdfespatial ucdp_10 z_CPIXlit_00 l_z_CPIXlit_00 l2_z_CPIXlit_00 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_00 l_z_PPIXlit_00 l2_z_PPIXlit_00 ///
								ln_ex_pop ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
					lincom z_CPIXlit_00 + l_z_CPIXlit_00 + l2_z_CPIXlit_00 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_00 + l_z_CPIXlit_00 + l2_z_CPIXlit_00 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_00 + l_z_PPIXlit_00 + l2_z_PPIXlit_00 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_00 + l_z_PPIXlit_00 + l2_z_PPIXlit_00  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH3
													
	reghdfe ucdp_10  L(0/2).z_CPIXlit_00 L(0/2).z_PPIXlit_00 L(0/2).z_PPI ln_ex_pop $newcon00,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_CPIXlit_00 l_z_CPIXlit_00 l2_z_CPIXlit_00 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_00 l_z_PPIXlit_00 l2_z_PPIXlit_00 ///
								ln_ex_pop $newcon00 ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
					lincom z_CPIXlit_00 + l_z_CPIXlit_00 + l2_z_CPIXlit_00 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_00 + l_z_CPIXlit_00 + l2_z_CPIXlit_00 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_00 + l_z_PPIXlit_00 + l2_z_PPIXlit_00 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_00 + l_z_PPIXlit_00 + l2_z_PPIXlit_00  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH4	
					
	reghdfe ucdp_10  L(0/2).z_CPIXlit_10 L(0/2).z_PPIXlit_10 L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)

	
	reg2hdfespatial ucdp_10 z_CPIXlit_10 l_z_CPIXlit_10 l2_z_CPIXlit_10 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_10 l_z_PPIXlit_10 l2_z_PPIXlit_10 ///
								ln_ex_pop ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
				
				lincom z_CPIXlit_10 + l_z_CPIXlit_10 + l2_z_CPIXlit_10 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*110
					test z_CPIXlit_10 + l_z_CPIXlit_10 + l2_z_CPIXlit_10 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_10 + l_z_PPIXlit_10 + l2_z_PPIXlit_10 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*110
					test z_PPIXlit_10 + l_z_PPIXlit_10 + l2_z_PPIXlit_10  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH5
													
	reghdfe ucdp_10  L(0/2).z_CPIXlit_10 L(0/2).z_PPIXlit_10 L(0/2).z_PPI ln_ex_pop $newcon10,  a(cell cy) cluster (cell cy)

	
	reg2hdfespatial ucdp_10 z_CPIXlit_10 l_z_CPIXlit_10 l2_z_CPIXlit_10 ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXlit_10 l_z_PPIXlit_10 l2_z_PPIXlit_10 ///
								ln_ex_pop $newcon10 ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
					lincom z_CPIXlit_10 + l_z_CPIXlit_10 + l2_z_CPIXlit_10 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXlit_10 + l_z_CPIXlit_10 + l2_z_CPIXlit_10 = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXlit_10 + l_z_PPIXlit_10 + l2_z_PPIXlit_10 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXlit_10 + l_z_PPIXlit_10 + l2_z_PPIXlit_10  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH6

	esttab regH1 regH2 regH3 regH4 regH5 regH6 using "$output/table_3con.tex", replace se noconstant drop(*)  ///
    star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
		"l_psumcoef \\ Producer Price Index $\times$ Luminosity" "l_sep \hspace{15pt} Conley SE" "l_pvalp \hspace{25pt} p-value"  ///
		"l_c_sum_coef \\ Consumer Price Index $\times$ Luminosity" "l_sec \hspace{15pt} Conley SE" "l_pvalc \hspace{25pt} p-value"  ///
		"N Observations")  ///
		mgroups("\shortstack{Factor Conflict Incidence: 1(Conflict $>0$)}", pattern(1 0 0 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ price{t-k} price_{t-k})  /// 
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.0f  )  /// 
     label obslast nomtitles 
	 
				
***********************************
*		TABLE A3 Conley SEs	
***********************************

cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)

reg2hdfespatial 		ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample) ,  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA1

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)

					
reg2hdfespatial 		ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA2

reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines  _ct*,  a(year cell) cluster (cell cy)
			
reg2hdfespatial 		ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mucdp')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA3


reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)

reg2hdfespatial 		onset_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA4
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)
					
reg2hdfespatial 		onset_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA5
					
reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell ) cluster (cell cy)
					
reg2hdfespatial 		onset_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)	
											
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA6
					

reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
reg2hdfespatial 	end1_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA7
					
reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 	end1_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)	
						
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA8

reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 	end1_ucdp_10  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					test z_CPI + l_z_CPI + l2_z_CPI  =  z_PPI + l_z_PPI + l2_z_PPI 
					estadd sca test_cp = r(p)
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					estadd local wt " "
					
					est sto regA9
					
	esttab regA*  using "$output/table_A3con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
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
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%10.3f %10.3f ///
					%~12s %~12s %~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 			
					
						
*******************************
********	TABLE A5	*******
*******************************

cap est drop reg*
	est clear
	
	foreach i of numlist 100 200 300 400 500 600 700 800 900 1000 {
		
*	Incidence 	

	reg2hdfespatial ucdp_10 z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1
														
	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
		estadd sca test_cp = r(p)	

		est sto regA2

*	Onset 	

	reg2hdfespatial onset_ucdp_10  z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3
							
	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not  ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
		estadd sca test_cp = r(p)
		est sto regA4
	
*	Ending 
	reg2hdfespatial end1_ucdp_10  z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
								
	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca test_cp = r(p)	

		estadd local wt " "
	
		est sto regA6
	
esttab regA*  using "$output/table_A5_`i'.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Conley p-value"  ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.3f %10.3f ///
					%10.0f  )  /// 
					label obslast nomtitles 								
}
						

*******************************
*	TABLE A6 Conley SEs
*******************************

use "$dir/input_rep/1deg_data.dta", clear

	gen miss_notrade = missing(ucdp_10) | missing(z_PPI) | missing(z_CPI) 
	gen miss_notrade_onset = missing(onset_ucdp_10) | missing(z_PPI) | missing(z_CPI)
	gen miss_notrade_end1 = missing(end1_ucdp_10) | missing(z_PPI) | missing(z_CPI) 
	
	areg ucdp_10 _ct* if miss_notrade == 0, a(deg1cell)
	predict ucdp_resid_not if miss_notrade == 0, resid
	
	areg onset_ucdp_10 _ct* if miss_notrade_onset == 0 , a(deg1cell)
	predict onset_ucdp_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_ucdp_10 _ct* if miss_notrade_end1 == 0 , a(deg1cell)
	predict end1_ucdp_resid_not if miss_notrade_end1 == 0 , resid
	
*	Incidence
		areg z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2con_resid_not if miss_notrade == 0, resid
		
*	Onset
		areg z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

*	Offset
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid
				
	cap est drop reg*
	est clear
	cap drop _merge
	
	reghdfe 		ucdp_10  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy) 

	reg2hdfespatial ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)

								est sto regA1

	reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
				
								lincom con_resid_not + lcon_resid_not + l2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test con_resid_not + lcon_resid_not + l2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
								estadd sca pvalp = r(p)
							
								test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
								estadd sca test_cp = r(p)					
								est sto regA2
					
					
	reghdfe 		onset_ucdp_10  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial onset_ucdp_10 z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)	

								est sto regA3
								
	reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not if e(sample) ,   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
	
								lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
								estadd sca pvalp = r(p)
							
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
								estadd sca test_cp = r(p)
								est sto regA4								
								
	reghdfe 		end1_ucdp_10  L(0/2).z_PPI,  a(deg1cell cy) cluster (deg1cell cy)

	
	reg2hdfespatial end1_ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)

								est sto regA5
								
	reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	
	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
							
								lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
								estadd sca pvalc = r(p)
								
								lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
								estadd sca pvalp = r(p)
							
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca test_cp = r(p)	

								estadd local wt " "
								est sto regA6							
												
esttab regA*  using "$output/table_A6con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} p-value"  ///
					"cyfe \hline \\ Country $\times$ year FE"   /// 
					   ///
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
					%10.3f %10.3f ///
					%~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 				
												
	
***********************************
*		TABLE A7 Conley SEs	
***********************************

use "$dir/input_rep/1deg_data.dta", clear	
	
	
gen miss_notrade = missing(ucdp_10) | missing(z_PPI) | missing(z_CPI) | missing(z_mw_prod_not) 
	gen miss_notrade_onset = missing(onset_ucdp_10) | missing(z_PPI) | missing(z_CPI) | missing(z_mw_prod_not) 
	gen miss_notrade_end1 = missing(end1_ucdp_10) | missing(z_PPI) | missing(z_CPI)  | missing(z_mw_prod_not) 
	
	areg ucdp_10 _ct* if miss_notrade == 0, a(deg1cell)
	predict ucdp_resid_not if miss_notrade == 0, resid
	
	areg onset_ucdp_10 _ct* if miss_notrade_onset == 0 , a(deg1cell)
	predict onset_ucdp_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_ucdp_10 _ct* if miss_notrade_end1 == 0 , a(deg1cell)
	predict end1_ucdp_resid_not if miss_notrade_end1 == 0 , resid
	
*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2prod_resid_not if miss_notrade == 0, resid
		
		areg z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict mw_prod_resid_not if miss_notrade == 0, resid

		areg l_z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict lmw_prod_resid_not if miss_notrade == 0, resid

		areg l2_z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict l2mw_prod_resid_not if miss_notrade == 0, resid		
		
		areg z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2con_resid_not if miss_notrade == 0, resid
		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid
		
			areg z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onmw_prod_resid_not if miss_notrade_onset == 0, resid

			areg l_z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onlmw_prod_resid_not if miss_notrade_onset == 0, resid

			areg l2_z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onl2mw_prod_resid_not if miss_notrade_onset == 0, resid		

		areg z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid
		
			areg z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offmw_prod_resid_not if miss_notrade_end1 == 0, resid

			areg l_z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offlmw_prod_resid_not if miss_notrade_end1 == 0, resid

			areg l2_z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offl2mw_prod_resid_not if miss_notrade_end1 == 0, resid		

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid
				
	
	cap est drop reg*
	est clear
	cap drop _merge
	
	reghdfe 		ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	
	reg2hdfespatial ucdp_10  z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)
											
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA1

	reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	
	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not mw_prod_resid_not lmw_prod_resid_not l2mw_prod_resid_not if e(sample) ,   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
				
								lincom con_resid_not + lcon_resid_not + l2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test con_resid_not + lcon_resid_not + l2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom mw_prod_resid_not + lmw_prod_resid_not + l2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test mw_prod_resid_not + lmw_prod_resid_not + l2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)								
							
								test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
								estadd sca test_cp = r(p)					
								est sto regA2
					
					
	reghdfe 		onset_ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial onset_ucdp_10 z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)	
								
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA3
								
	reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not onmw_prod_resid_not  onlmw_prod_resid_not  onl2mw_prod_resid_not if e(sample) ,   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
	
								lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom onmw_prod_resid_not + onlmw_prod_resid_not + onl2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test onmw_prod_resid_not + onlmw_prod_resid_not + onl2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)
							
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
								estadd sca test_cp = r(p)
								est sto regA4								
								
	reghdfe 		end1_ucdp_10  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial end1_ucdp_10  z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)
								
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA5
								
	reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not offmw_prod_resid_not  offlmw_prod_resid_not  offl2mw_prod_resid_not if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
							
								lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
								estadd sca pvalc = r(p)
								
								lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom offmw_prod_resid_not + offlmw_prod_resid_not + offl2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test offmw_prod_resid_not + offlmw_prod_resid_not + offl2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)
							
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca test_cp = r(p)	

								estadd local wt " "
								est sto regA6			
	
	
esttab regA*  using "$output/table_A7con.tex", replace se noconstant drop(*)  ///
					 star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					 scalars(  ///
						"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Conley SE " "pvalp \hspace{15pt} p-value"  ///
						"pw_sumcoef \\  Producer Price Index in neighboring cells" "sepw \hspace{15pt} Conley SE" "pvalpw \hspace{15pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} Conley SE " "pvalc \hspace{15pt} p-value"  ///
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
						substitute(\_ _ price{t-k} price_{t-k})  /// 
					 sfmt( 	%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f  %10.1f  %10.1f  ///
							%10.3f  %10.3f   ///
							%~12s %~12s %~12s ///
							%10.3f %10.0f  )  /// 
					 label obslast nomtitles 		
	
	
***********************************
*		TABLE A11 Conley SEs	
***********************************	
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

	replace z_PPI = 	z_yield_PPI
	replace l_z_PPI = l.z_yield_PPI
	replace l2_z_PPI = l2.z_yield_PPI
	replace l3_z_PPI = l3.z_yield_PPI

	
	cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
	summ onset_ucdp_10
	loc monset = r(mean)
	summ end1_ucdp_10
	loc mend1 = r(mean)

	gen miss_notrade = missing(ucdp_10) | missing(z_PPI) | missing(z_CPI) 
	gen miss_notrade_onset = missing(onset_ucdp_10) | missing(z_PPI) | missing(z_CPI)
	gen miss_notrade_end1 = missing(end1_ucdp_10) | missing(z_PPI) | missing(z_CPI) 
	
	areg ucdp_10 _ct* if miss_notrade == 0, a(cell)
	predict ucdp_resid_not if miss_notrade == 0, resid
	
	areg onset_ucdp_10 _ct* if miss_notrade_onset == 0 , a(cell)
	predict onset_ucdp_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_ucdp_10 _ct* if miss_notrade_end1 == 0 , a(cell)
	predict end1_ucdp_resid_not if miss_notrade_end1 == 0 , resid

*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict l2con_resid_not if miss_notrade == 0, resid
		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

	
*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid
		

		summ ucdp_10
		cap gen mucdp = r(mean)	
		summ onset_ucdp_10
		cap gen monset = r(mean)
		summ end1_ucdp_10
		cap gen mend1 = r(mean)
	
					cap est drop reg*
					est clear
						
*	Incidence 	

	reghdfe 		ucdp_10  L(0/2).z_yield_PPI ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1
														
	reghdfe 		ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not if e(sample) ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
		estadd sca test_cp = r(p)	

		est sto regA2

*	Onset 	

	reghdfe 		onset_ucdp_10 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)

	
	reg2hdfespatial onset_ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3
							
	reghdfe 		onset_ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not if e(sample) ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
		estadd sca test_cp = r(p)
		est sto regA4
	
*	Ending 
	reghdfe 		end1_ucdp_10 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)

	reg2hdfespatial end1_ucdp_10  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
								
	reghdfe 		end1_ucdp_10 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca test_cp = r(p)	

		estadd local wt " "
	
		est sto regA6
	
esttab regA*  using "$output/table_A11con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index $\times$ Yield"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Conley p-value"  ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.3f %10.3f ///
					%10.0f  )  /// 
					label obslast nomtitles 	



***********************************
*		TABLE A29 Conley SEs	
***********************************	

use "$dir/input_rep/cell_data.dta", clear	

			foreach var of varlist  lit_92    mnt capdist centr_tribe gr d_gr   dist_lights92 distancetoport bdist1 {
			cap gen z_CPIX`var' = z_CPI*`var'
			cap gen z_PPIX`var' = z_PPI*`var'
			cap gen l_z_CPIX`var' = l.z_CPI*`var'
			cap gen l_z_PPIX`var' = l.z_PPI*`var'
			cap gen l2_z_CPIX`var' = l2.z_CPI*`var'
			cap gen l2_z_PPIX`var' = l2.z_PPI*`var'
			}
		
		global newcon92 = 		"z_CPIXlit_92			l_z_CPIXlit_92	l2_z_CPIXlit_92			z_PPIXlit_92		l_z_PPIXlit_92			l2_z_PPIXlit_92		z_CPIXdist_lights92  	z_CPIXcapdist	z_CPIXdistancetoport	z_CPIXbdist1	 	l_z_CPIXdist_lights92	l_z_CPIXcapdist		l_z_CPIXdistancetoport	l_z_CPIXbdist1	 	l2_z_CPIXdist_lights92		l2_z_CPIXcapdist 		l2_z_CPIXdistancetoport		l2_z_CPIXbdist1	 	z_PPIXdist_lights92		z_PPIXcapdist		z_PPIXdistancetoport	z_PPIXbdist1	 		l_z_PPIXdist_lights92 	l_z_PPIXcapdist  	l_z_PPIXdistancetoport	l_z_PPIXbdist1	 		l2_z_PPIXdist_lights92 l2_z_PPIXcapdist 	l2_z_PPIXdistancetoport	l2_z_PPIXbdist1	"

	

cap est drop reg*
	est clear
	
	summ ucdp_10
	loc mucdp = r(mean)	
			reghdfe ucdp_10  L(0/2).z_CPIXd_gr L(0/2).z_PPIXd_gr L(0/2).z_PPI ln_ex_pop ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_CPIXd_gr l_z_CPIXd_gr l2_z_CPIXd_gr ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXd_gr l_z_PPIXd_gr l2_z_PPIXd_gr ///
								ln_ex_pop ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
					lincom z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXd_gr + l_z_PPIXd_gr + l2_z_PPIXd_gr 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXd_gr + l_z_PPIXd_gr + l2_z_PPIXd_gr  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH1
													
	reghdfe ucdp_10  L(0/2).z_CPIXd_gr L(0/2).z_PPIXd_gr L(0/2).z_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial ucdp_10 z_CPIXd_gr l_z_CPIXd_gr l2_z_CPIXd_gr ///
								z_PPI l_z_PPI l2_z_PPI ///
								z_PPIXd_gr l_z_PPIXd_gr l2_z_PPIXd_gr ///
								ln_ex_pop $newcon92 ///
							if e(sample)	,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

				lincom z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`mucdp')*100
					test z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_PPIXd_gr + l_z_PPIXd_gr + l2_z_PPIXd_gr 
					estadd sca l_psumcoef = r(estimate)
					estadd sca l_sep = r(se)
					estadd sca l_stdp = (r(estimate)/`mucdp')*100
					test z_PPIXd_gr + l_z_PPIXd_gr + l2_z_PPIXd_gr  = 0
					estadd sca l_pvalp = r(p)
					
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mucdp')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0
					estadd sca pvalp = r(p)
					est sto regH2	
					

	esttab regH1 regH2  using "$output/table_A29con.tex", replace se noconstant drop(*)  ///
    star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
		"l_psumcoef \\ Producer Price Index $\times$ Precolonial political centralization" "l_sep \hspace{15pt} Conley SE" "l_pvalp \hspace{25pt} p-value"  ///
		"l_c_sum_coef \\ Consumer Price Index $\times$ Precolonial political centralization" "l_sec \hspace{15pt} Conley SE" "l_pvalc \hspace{25pt} p-value"  ///
		"N Observations")  ///
		mgroups("\shortstack{Incidence: \\ 1(Conflict $>0$)}", pattern(1 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
		substitute(\_ _ price{t-k} price_{t-k})  /// 
     sfmt( 	%10.4f %10.3f %10.3f  ///
			%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f ///
			%10.0f  )  /// 
     label obslast nomtitles	
	
***********************************
***********************************
*	OUTPUT CONFLICT CONLEY SEs	  *
***********************************
***********************************





***********************************
*		TABLE 4 Conley SEs	
***********************************

use "$dir/input_rep/cell_data.dta", clear

*	Generate residuals 
	
	gen miss_notrade = missing(acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	gen miss_notrade_onset = missing(onset_acled_riot_67) | missing(z_PPI) | missing(z_CPI)
	gen miss_notrade_end1 = missing(end1_acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	
	areg acled_riot_67 _ct* if miss_notrade == 0, a(cell)
	predict acled_67 if miss_notrade == 0, resid
	
	areg onset_acled_riot_67 _ct* if miss_notrade_onset == 0 , a(cell)
	predict onset_acled_67 if miss_notrade_onset == 0 , resid
	
	areg end1_acled_riot_67 _ct* if miss_notrade_end1 == 0 , a(cell)
	predict end1_acled_67 if miss_notrade_end1 == 0 , resid
	

*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict l2con_resid_not if miss_notrade == 0, resid	
		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

	
*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid
		
foreach var of varlist spi_d2 spi_d3 spi_d4 temp oil_cell oil_country z_p_oil main_lprice_mines main_lprice mines	{
	cap gen l_`var' = l.`var'
	cap gen l2_`var' = l2.`var'
	}
	
	
		summ acled_riot_67
		cap gen macled = r(mean)	
		summ onset_acled_riot_67
		cap gen monset = r(mean)
		summ end1_acled_riot_67
		cap gen mend1 = r(mean)
	
					cap est drop reg*
					est clear
						
*	Incidence 	
	reghdfe 		acled_riot_67  L(0/2).z_PPI _ct*,  a(cell cy) cluster (cell cy)

	reg2hdfespatial acled_riot_67 z_PPI l_z_PPI l2_z_PPI  if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1
														
	reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)

	qui ols_spatial_HAC acled_67 con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  if e(sample) ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
		estadd sca test_cp = r(p)	

		est sto regA2

		
	reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)
		
	reg2hdfespatial acled_riot_67 z_PPI l_z_PPI l2_z_PPI z_CPI  l_z_CPI  l2_z_CPI _ct*  if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
				
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

		est sto regA2Y
		
*	Onset 	

	reghdfe 		onset_acled_riot_67 L(0/2).z_PPI _ct*,  a(cy cell) cluster (cell cy)

	reg2hdfespatial onset_acled_riot_67  z_PPI l_z_PPI l2_z_PPI  if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3
				
	
	reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC onset_acled_67 oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not  if e(sample) ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
		estadd sca test_cp = r(p)
		est sto regA4
	

	reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)

	reg2hdfespatial onset_acled_riot_67  z_PPI l_z_PPI l2_z_PPI z_CPI l_z_CPI  l2_z_CPI _ct*  if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	
		
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

		est sto regA4Y
		
*	Ending 
	reghdfe 		end1_acled_riot_67 L(0/2).z_PPI _ct*,  a(cy cell) cluster (cell cy)

	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI  if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
		
	reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC end1_acled_67 offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not  if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca test_cp = r(p)	

		estadd local wt " "
	
		est sto regA6
		
	reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(year cell) cluster (cell cy)

	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI z_CPI l_z_CPI l2_z_CPI _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)
		
		lincom z_CPI + l_z_CPI + l2_z_CPI
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)
		test z_CPI + l_z_CPI + l2_z_CPI = 0
		estadd sca pvalc = r(p)

		est sto regA6Y
	
esttab regA*  using "$output/table_4con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Conley p-value"  ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.3f %10.3f ///
					%10.0f  )  /// 
					label obslast nomtitles 	 

					
***********************************
*		TABLE 5 Conley SEs	
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

reg2hdfespatial acled_riot_67  z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
	
	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`macled')*100
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`macled')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	est sto reg11

reghdfe onset_acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  , a(cell cy) cluster(cell cy)

reg2hdfespatial onset_acled_riot_67  z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`monset')*100
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`monset')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	est sto reg12

reghdfe end1_acled_riot_67 	L(0/2).z_food_PPI L(0/2).z_cash_PPI  , a(cell cy) cluster(cell cy)

reg2hdfespatial end1_acled_riot_67  z_food_PPI l_z_food_PPI l2_z_food_PPI z_cash_PPI l_z_cash_PPI l2_z_cash_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
	lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
	estadd sca pf_sumcoef = r(estimate)
	estadd sca sepf = r(se)
	estadd sca stdpf = (r(estimate)/`mend1')*100
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
	estadd sca pvalpf = r(p)

	lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca pc_sumcoef = r(estimate)
	estadd sca stdpc = (r(estimate)/`mend1')*100
	estadd sca sepc = r(se)
	test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
	estadd sca pvalpc = r(p)
	
	test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
	estadd sca test_fc = r(p)
	
	estadd local cyfe "Yes"					 
	estadd local cfe "Yes"
	est sto reg13

esttab reg1* using "$output/table_5con.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} Conley SE " "pvalpf \hspace{25pt} p-value"  ///
		"pc_sumcoef \\  Producer Price Index: Cash crops" "sepc \hspace{15pt} Conley SE" "pvalpc \hspace{25pt} p-value"  ///
		"stdpf \hline \\ PPI Impact: Food crops"  ///
		"stdpc PPI Impact: Cash crops"  ///
		"wt Wald test: PPI Food = PPI Cash"  ///		
		"test_fc  \hspace{15pt} Conley p-value"  ///
		"N  \hline \\ Observations")  ///
		mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1  1  1)  ///
		prefix(\multicolumn{@span}{c}{) suffix(})  ///
		span erepeat(\cmidrule(lr){@span}))  ///
     sfmt( 	%10.4f %10.3f %10.3f ///
			%10.4f %10.3f %10.3f /// 
			%10.1f %10.1f  ///
			%10.3f %10.3f   ///
			%10.0f  )  /// 
     label obslast nomtitles
	 
	 
	
***********************************
*		TABLE A13 Conley SEs	
***********************************
cap est drop reg*
	est clear
	
	summ acled_riot_67
	loc macled = r(mean)	
	summ onset_acled_riot_67
	loc monset = r(mean)
	summ end1_acled_riot_67
	loc mend1 = r(mean)
	
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)

reg2hdfespatial 		acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA1
					
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 		acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA2

				
reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines  _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 		acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`macled')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`macled')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
					
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA3

					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)

reg2hdfespatial 		onset_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)

					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA4
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 		onset_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)

					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA5
					
reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell ) cluster (cell cy)

reg2hdfespatial 		onset_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`monset')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`monset')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
											
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA6
					

reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(cell year) cluster (cell cy)
					
reg2hdfespatial 		end1_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI   _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
	
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "No"
					estadd local wc "No"
					
					est sto regA7
					
reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 		end1_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
						
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "No"
					estadd local oc "Yes"
					estadd local wc "Yes"
					
					est sto regA8


reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).spi_d2 L(0/2).spi_d3 L(0/2).spi_d4 L(0/2).temp  L(0/2).oil_cell L(0/2).oil_country  L(0/2).main_lprice_mines L(0/2).main_lprice L(0/2).mines _ct*,  a(year cell) cluster (cell cy)

reg2hdfespatial 		end1_acled_riot_67  z_CPI l_z_CPI l2_z_CPI z_PPI l_z_PPI l2_z_PPI spi_d2 l_spi_d2 l2_spi_d2 spi_d3 l_spi_d3 l2_spi_d3 spi_d4 l_spi_d4 l2_spi_d4 temp l_temp l2_temp  oil_cell l_oil_cell l2_oil_cell oil_country l_oil_country l2_oil_country main_lprice_mines l_main_lprice_mines l2_main_lprice_mines main_lprice l_main_lprice l2_main_lprice mines l_mines l2_mines  _ct* if e(sample),  lat(y) lon(x)  timevar(year) panelvar(cell) dist(500) lag(20) bartlett disp star dropvar
					
					lincom z_CPI + l_z_CPI + l2_z_CPI
					estadd sca c_sum_coef = r(estimate)
					estadd sca sec = r(se)
					estadd sca stdc = (r(estimate)/`mend1')*100
					test z_CPI + l_z_CPI + l2_z_CPI = 0
					estadd sca pvalc = r(p)
	
					lincom z_PPI + l_z_PPI + l2_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca sep = r(se)
					estadd sca stdp = (r(estimate)/`mend1')*100
					test z_PPI + l_z_PPI + l2_z_PPI = 0 
					estadd sca pvalp = r(p)
										
					estadd local ctt "Yes"
					estadd local cfe "Yes"
					estadd local yfe "Yes"
					estadd local mc "Yes"
					estadd local oc "Yes"
					estadd local wc "Yes"
					estadd local wt " "
					
					est sto regA9
					
	esttab regA*  using "$output/table_A13con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
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
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.1f  %10.1f  ///
					%~12s %~12s %~12s %~12s %~12s %~12s ///
					%10.0f  )  /// 
					label obslast nomtitles 
					
					
***********************************
*		TABLE A16	
***********************************

	
					cap est drop reg*
					est clear
		foreach i of numlist 100 200 300 400 500 600 700 800 900 1000 {
					
*	Incidence 	

	reg2hdfespatial acled_riot_67 z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1
														
	qui ols_spatial_HAC ucdp_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test con_resid_not + lcon_resid_not + l2con_resid_not  =  prod_resid_not + lprod_resid_not + l2prod_resid_not 
		estadd sca test_cp = r(p)	

		est sto regA2

*	Onset 	

	reg2hdfespatial onset_acled_riot_67  z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3
							
	ols_spatial_HAC onset_ucdp_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not  ,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not  =  onprod_resid_not + onlprod_resid_not + onl2prod_resid_not 
		estadd sca test_cp = r(p)
		est sto regA4
	
*	Ending 
	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(`i') lag(20) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
								
	ols_spatial_HAC end1_ucdp_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not,   lat(y) lon(x) timevar(year) panelvar(cell) dist(`i') lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not  =  offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca test_cp = r(p)	

		estadd local wt " "
	
		est sto regA6
	
esttab regA*  using "$output/table_A16_`i'.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} `i'km Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"wt  Wald test: PPI = CPI"  ///		
					"test_cp  \hspace{15pt} Conley p-value"  ///
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.3f %10.3f ///
					%10.0f  )  /// 
					label obslast nomtitles 								
		}					
	 
***********************************
*		TABLE A17 Conley SEs
***********************************

use "$dir/input_rep/1deg_data.dta", clear

gen miss_notrade = missing(acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	gen miss_notrade_onset = missing(onset_acled_riot_67) | missing(z_PPI) | missing(z_CPI)
	gen miss_notrade_end1 = missing(end1_acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	
	areg acled_riot_67 _ct* if miss_notrade == 0, a(deg1cell)
	predict ac67_resid_not if miss_notrade == 0, resid
	
	areg onset_acled_riot_67 _ct* if miss_notrade_onset == 0 , a(deg1cell)
	predict onset_ac67_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_acled_riot_67 _ct* if miss_notrade_end1 == 0 , a(deg1cell)
	predict end1_ac67_resid_not if miss_notrade_end1 == 0 , resid
	
*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2con_resid_not if miss_notrade == 0, resid
		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid

	cap est drop reg*
	est clear
	cap drop _merge
	
	reghdfe 		acled_riot_67  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial acled_riot_67  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)

								est sto regA1

	reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	qui ols_spatial_HAC ac67_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not if e(sample) ,   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
				
								lincom con_resid_not + lcon_resid_not + l2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test con_resid_not + lcon_resid_not + l2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
								est sto regA2
					
					
	reghdfe 		onset_acled_riot_67  L(0/2).z_PPI ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial onset_acled_riot_67 z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)	

								est sto regA3
								
	reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC onset_ac67_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not if e(sample) ,   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
	
								lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
								estadd sca pvalp = r(p)
			
								est sto regA4								
								
	reghdfe 		end1_acled_riot_67  L(0/2).z_PPI,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)

								est sto regA5
								
	reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC end1_ac67_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
							
								lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
								estadd sca pvalc = r(p)
								
								lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
								estadd sca pvalp = r(p)
							
	

								estadd local wt " "
								est sto regA6							
								
				
esttab regA*  using "$output/table_A17con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"stdp \hline \\ PPI impact (\%)"   ///
					"stdc  CPI impact (\%)"   ///
					"cyfe \hline \\ Country $\times$ year FE"   /// 
					   ///
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


***********************************
*		TABLE A18 Conley SEs
***********************************
use "$dir/input_rep/1deg_data.dta", clear


gen miss_notrade = missing(acled_riot_67) | missing(z_PPI) | missing(z_CPI) | missing(z_mw_prod_not) 
	gen miss_notrade_onset = missing(onset_acled_riot_67) | missing(z_PPI) | missing(z_CPI) | missing(z_mw_prod_not) 
	gen miss_notrade_end1 = missing(end1_acled_riot_67) | missing(z_PPI) | missing(z_CPI)  | missing(z_mw_prod_not) 
	
	areg acled_riot_67 _ct* if miss_notrade == 0, a(deg1cell)
	predict ac67_resid_not if miss_notrade == 0, resid
	
	areg onset_acled_riot_67 _ct* if miss_notrade_onset == 0 , a(deg1cell)
	predict onset_ac67_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_acled_riot_67 _ct* if miss_notrade_end1 == 0 , a(deg1cell)
	predict end1_ac67_resid_not if miss_notrade_end1 == 0 , resid
	
*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2prod_resid_not if miss_notrade == 0, resid
		
		areg z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict mw_prod_resid_not if miss_notrade == 0, resid

		areg l_z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict lmw_prod_resid_not if miss_notrade == 0, resid

		areg l2_z_mw_prod_not _ct* if miss_notrade == 0, a(deg1cell)
		predict l2mw_prod_resid_not if miss_notrade == 0, resid		
		
		areg z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(deg1cell)
		predict l2con_resid_not if miss_notrade == 0, resid
		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid
		
			areg z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onmw_prod_resid_not if miss_notrade_onset == 0, resid

			areg l_z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onlmw_prod_resid_not if miss_notrade_onset == 0, resid

			areg l2_z_mw_prod_not _ct* if miss_notrade_onset == 0, a(deg1cell)
			predict onl2mw_prod_resid_not if miss_notrade_onset == 0, resid		

		areg z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(deg1cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid

*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid
		
			areg z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offmw_prod_resid_not if miss_notrade_end1 == 0, resid

			areg l_z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offlmw_prod_resid_not if miss_notrade_end1 == 0, resid

			areg l2_z_mw_prod_not _ct* if miss_notrade_end1 == 0, a(deg1cell)
			predict offl2mw_prod_resid_not if miss_notrade_end1 == 0, resid		

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(deg1cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid
				
	
	
	cap est drop reg*
	est clear
	cap drop _merge
	
	reghdfe 		acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial acled_riot_67  z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)
											
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA1

	reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	qui ols_spatial_HAC ac67_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not mw_prod_resid_not lmw_prod_resid_not l2mw_prod_resid_not  if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
				
								lincom con_resid_not + lcon_resid_not + l2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test con_resid_not + lcon_resid_not + l2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom mw_prod_resid_not + lmw_prod_resid_not + l2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test mw_prod_resid_not + lmw_prod_resid_not + l2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)								
							
							
								est sto regA2
					
					
	reghdfe 		onset_acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial onset_acled_riot_67 z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)	
								
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA3
								
	reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC onset_ac67_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not onmw_prod_resid_not  onlmw_prod_resid_not  onl2mw_prod_resid_not  if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
	
								lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
								estadd sca pvalc = r(p)

								lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom onmw_prod_resid_not + onlmw_prod_resid_not + onl2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test onmw_prod_resid_not + onlmw_prod_resid_not + onl2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)
	
								est sto regA4								
								
	reghdfe 		end1_acled_riot_67  L(0/2).z_PPI L(0/2).z_mw_prod_not ,  a(deg1cell cy) cluster (deg1cell cy)

	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI z_mw_prod_not l_z_mw_prod_not l2_z_mw_prod_not if e(sample),  lat(deg1y) lon(deg1x)  timevar(cy) panelvar(deg1cell) dist(500) lag(21) bartlett disp star dropvar 

								lincom z_PPI + l_z_PPI + l2_z_PPI
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test z_PPI + l_z_PPI + l2_z_PPI = 0
								estadd sca pvalp = r(p)
								
									lincom z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test z_mw_prod_not + l_z_mw_prod_not + l2_z_mw_prod_not = 0 
									estadd sca pvalpw = r(p)

								est sto regA5
								
	reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_PPI L(0/2).z_mw_prod_not _ct*,  a(deg1cell) cluster (deg1cell cy)

	ols_spatial_HAC end1_ac67_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not offmw_prod_resid_not  offlmw_prod_resid_not  offl2mw_prod_resid_not if e(sample),   lat(deg1y) lon(deg1x) timevar(year) panelvar(deg1cell) dist(500) lag(21) star dropvar disp
							
								lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
								estadd sca c_sum_coef = r(estimate)
								estadd sca sec = r(se)	
								test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
								estadd sca pvalc = r(p)
								
								lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
								estadd sca p_sumcoef = r(estimate)
								estadd sca sep = r(se)
								test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
								estadd sca pvalp = r(p)
								
									lincom offmw_prod_resid_not + offlmw_prod_resid_not + offl2mw_prod_resid_not 
									estadd sca pw_sumcoef = r(estimate)
									estadd sca sepw = r(se)
									test offmw_prod_resid_not + offlmw_prod_resid_not + offl2mw_prod_resid_not = 0 
									estadd sca pvalpw = r(p)


								estadd local wt " "
								est sto regA6			
	
	
esttab regA*  using "$output/table_A18con.tex", replace se noconstant drop(*)  ///
					 star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					 scalars(  ///
						"p_sumcoef \\ Producer Price Index" "sep \hspace{15pt} Conley SE " "pvalp \hspace{15pt} p-value"  ///
						"pw_sumcoef \\  Producer Price Index in neighboring cells" "sepw \hspace{15pt} Conley SE" "pvalpw \hspace{15pt} p-value"  ///
						"c_sum_coef \\ Consumer Price Index" "sec \hspace{15pt} Conley SE " "pvalc \hspace{15pt} p-value"  ///
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
						substitute(\_ _ price{t-k} price_{t-k})  /// 
					 sfmt( 	%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f  %10.1f  %10.1f  ///
							%10.3f  %10.3f   ///
							%~12s %~12s %~12s ///
							%10.3f %10.0f  )  /// 
					 label obslast nomtitles 	



***********************************
*		TABLE A22 Conley SEs
***********************************

use "$dir/input_rep/cell_data.dta", clear

	loneway yield_t_PPI cell
	loc psd_y = r(sd_w)
	summ yield_t_PPI
	loc pmean_y = r(mean)
	gen z_yield_t_PPI = (yield_t_PPI-`pmean_y')/`psd_y'
	
	loneway yield_PPI cell
	loc psd_ynot = r(sd_w)
	summ yield_PPI
	loc pmean_ynot = r(mean)
	gen z_yield_PPI = (yield_PPI-`pmean_ynot')/`psd_ynot'	

	replace z_PPI = 	z_yield_PPI
	replace l_z_PPI = l.z_yield_PPI
	replace l2_z_PPI = l2.z_yield_PPI
	replace l3_z_PPI = l3.z_yield_PPI
	
	
	gen miss_notrade = missing(acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	gen miss_notrade_onset = missing(onset_acled_riot_67) | missing(z_PPI) | missing(z_CPI)
	gen miss_notrade_end1 = missing(end1_acled_riot_67) | missing(z_PPI) | missing(z_CPI) 
	
	areg acled_riot_67 _ct* if miss_notrade == 0, a(cell)
	predict acled_resid_not if miss_notrade == 0, resid
	
	areg onset_acled_riot_67 _ct* if miss_notrade_onset == 0 , a(cell)
	predict onset_acled_resid_not if miss_notrade_onset == 0 , resid
	
	areg end1_acled_riot_67 _ct* if miss_notrade_end1 == 0 , a(cell)
	predict end1_acled_resid_not if miss_notrade_end1 == 0 , resid


*	Incidence: no trade
		areg z_PPI _ct* if miss_notrade == 0, a(cell)
		predict prod_resid_not if miss_notrade == 0, resid

		areg l_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict lprod_resid_not if miss_notrade == 0, resid

		areg l2_z_PPI _ct* if miss_notrade == 0, a(cell)
		predict l2prod_resid_not if miss_notrade == 0, resid

		areg z_CPI _ct* if miss_notrade == 0, a(cell)
		predict con_resid_not if miss_notrade == 0, resid

		areg l_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict lcon_resid_not if miss_notrade == 0, resid

		areg l2_z_CPI _ct* if miss_notrade == 0, a(cell)
		predict l2con_resid_not if miss_notrade == 0, resid

		
*	Onset: no trade
		areg z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onprod_resid_not if miss_notrade_onset == 0, resid

		areg l_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlprod_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2prod_resid_not if miss_notrade_onset == 0, resid

		areg z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict oncon_resid_not if miss_notrade_onset == 0, resid

		areg l_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onlcon_resid_not if miss_notrade_onset == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_onset == 0, a(cell)
		predict onl2con_resid_not if miss_notrade_onset == 0, resid
	
*	Offset: no trade
		areg z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offprod_resid_not if miss_notrade_end1 == 0, resid
		
		areg l_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlprod_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_PPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2prod_resid_not if miss_notrade_end1 == 0, resid

		areg z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offcon_resid_not if miss_notrade_end1 == 0, resid

		areg l_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offlcon_resid_not if miss_notrade_end1 == 0, resid

		areg l2_z_CPI _ct* if miss_notrade_end1 == 0, a(cell)
		predict offl2con_resid_not if miss_notrade_end1 == 0, resid

		summ acled_riot_67
		cap gen macled = r(mean)	
		summ onset_acled_riot_67
		cap gen monset = r(mean)
		summ end1_acled_riot_67
		cap gen mend1 = r(mean)
	
					cap est drop reg*
					est clear
						
*	Incidence 	

	reghdfe 		acled_riot_67  L(0/2).z_yield_PPI ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial acled_riot_67 z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA1
														
	reghdfe 		acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	qui ols_spatial_HAC acled_resid_not con_resid_not lcon_resid_not l2con_resid_not prod_resid_not lprod_resid_not l2prod_resid_not  if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom con_resid_not + lcon_resid_not + l2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test con_resid_not + lcon_resid_not + l2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom prod_resid_not + lprod_resid_not + l2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test prod_resid_not + lprod_resid_not + l2prod_resid_not = 0
		estadd sca pvalp = r(p)
	


		est sto regA2

*	Onset 	

	reghdfe 		onset_acled_riot_67 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)

	reg2hdfespatial onset_acled_riot_67  z_PPI l_z_PPI l2_z_PPI if e(sample) ,  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 

		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)	

		est sto regA3
							
	reghdfe 		onset_acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC onset_acled_resid_not oncon_resid_not onlcon_resid_not onl2con_resid_not onprod_resid_not onlprod_resid_not onl2prod_resid_not  if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom oncon_resid_not + onlcon_resid_not + onl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test oncon_resid_not + onlcon_resid_not + onl2con_resid_not = 0
		estadd sca pvalc = r(p)

		lincom onprod_resid_not + onlprod_resid_not + onl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test onprod_resid_not + onlprod_resid_not + onl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	

		est sto regA4
	
*	Ending 
	reghdfe 		end1_acled_riot_67 L(0/2).z_yield_PPI ,  a(cy cell) cluster (cell cy)

	reg2hdfespatial end1_acled_riot_67  z_PPI l_z_PPI l2_z_PPI if e(sample),  lat(y) lon(x)  timevar(cy) panelvar(cell) dist(500) lag(21) bartlett disp star dropvar 
								
		lincom z_PPI + l_z_PPI + l2_z_PPI
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test z_PPI + l_z_PPI + l2_z_PPI = 0
		estadd sca pvalp = r(p)

		est sto regA5
								
	reghdfe 		end1_acled_riot_67 L(0/2).z_CPI L(0/2).z_yield_PPI _ct*,  a(cell) cluster (cell cy)

	ols_spatial_HAC end1_acled_resid_not offcon_resid_not offlcon_resid_not offl2con_resid_not offprod_resid_not offlprod_resid_not offl2prod_resid_not if e(sample),   lat(y) lon(x) timevar(year) panelvar(cell) dist(500) lag(21) star dropvar disp
	
		lincom offcon_resid_not + offlcon_resid_not + offl2con_resid_not
		estadd sca c_sum_coef = r(estimate)
		estadd sca sec = r(se)	
		test offcon_resid_not + offlcon_resid_not + offl2con_resid_not = 0
		estadd sca pvalc = r(p)
		
		lincom offprod_resid_not + offlprod_resid_not + offl2prod_resid_not
		estadd sca p_sumcoef = r(estimate)
		estadd sca sep = r(se)
		test offprod_resid_not + offlprod_resid_not + offl2prod_resid_not = 0
		estadd sca pvalp = r(p)
	


		estadd local wt " "
	
		est sto regA6
	
esttab regA*  using "$output/table_A22con.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index $\times$ Yield"  "sep \hspace{15pt} Conley SE" "pvalp \hspace{25pt} p-value"  ///
					"c_sum_coef \\ Consumer Price Index"  "sec \hspace{15pt} Conley SE" "pvalc \hspace{25pt} p-value"  ///			
					"N Observations")  ///
					mgroups("\shortstack{Incidence\\1(Conflict $>0$)}" "1(\shortstack{Onset\\Conflict Begins)}" "\shortstack{Offset\\1(Conflict Ends)}", pattern(1 0 1 0 1 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ price{t-k} price_{t-k})  /// 
					sfmt( 	%10.4f %10.3f %10.3f  ///
					%10.4f %10.3f %10.3f  ///
					%10.0f  )  /// 
					label obslast nomtitles 

	

***********************************
*		TABLE A30 Conley SEs
***********************************

	
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

			reg2hdfespatial acled_riot_67 z_CPIXd_gr l_z_CPIXd_gr l2_z_CPIXd_gr ///
								z_food_PPI l_z_food_PPI l2_z_food_PPI ///
								z_food_PPIXd_gr l_z_food_PPIXd_gr l2_z_food_PPIXd_gr ///
								z_cash_PPI l_z_cash_PPI l2_z_cash_PPI ///
								z_cash_PPIXd_gr l_z_cash_PPIXd_gr l2_z_cash_PPIXd_gr ///
								ln_ex_pop ///
								if e(sample) ,  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
				
					lincom z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`macled')*100
					test z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_food_PPIXd_gr + l_z_food_PPIXd_gr + l2_z_food_PPIXd_gr 
					estadd sca lf_psumcoef = r(estimate)
					estadd sca lf_sep = r(se)
					estadd sca lf_stdp = (r(estimate)/`macled')*100
					test z_food_PPIXd_gr + l_z_food_PPIXd_gr + l2_z_food_PPIXd_gr  = 0
					estadd sca lf_pvalp = r(p)
					
					lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPIXd_gr + l_z_cash_PPIXd_gr + l2_z_cash_PPIXd_gr 
					estadd sca lc_psumcoef = r(estimate)
					estadd sca lc_sep = r(se)
					estadd sca lc_stdp = (r(estimate)/`macled')*100
					test z_cash_PPIXd_gr + l_z_cash_PPIXd_gr + l2_z_cash_PPIXd_gr   = 0
					estadd sca lc_pvalp = r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
					estadd sca pvalpc = r(p)
					est sto regH1
													
	reghdfe acled_riot_67  L(0/2).z_CPIXd_gr L(0/2).z_food_PPIXd_gr L(0/2).z_food_PPI L(0/2).z_cash_PPIXd_gr L(0/2).z_cash_PPI ln_ex_pop $newcon92 ,  a(cell cy) cluster (cell cy)

	reg2hdfespatial acled_riot_67 z_CPIXd_gr l_z_CPIXd_gr l2_z_CPIXd_gr ///
								z_food_PPI l_z_food_PPI l2_z_food_PPI ///
								z_food_PPIXd_gr l_z_food_PPIXd_gr l2_z_food_PPIXd_gr ///
								z_cash_PPI l_z_cash_PPI l2_z_cash_PPI ///
								z_cash_PPIXd_gr l_z_cash_PPIXd_gr l2_z_cash_PPIXd_gr ///
								ln_ex_pop $newcon92 ///
								if e(sample),  lat(y) lon(x)  timevar(cy)  panelvar(cell) dist(500) lag(20) bartlett disp star dropvar 
										
					lincom z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr 
					estadd sca l_c_sum_coef = r(estimate)
					estadd sca l_sec = r(se)
					estadd sca l_stdc = (r(estimate)/`macled')*100
					test z_CPIXd_gr + l_z_CPIXd_gr + l2_z_CPIXd_gr = 0
					estadd sca l_pvalc = r(p)
					
					lincom z_food_PPIXd_gr + l_z_food_PPIXd_gr + l2_z_food_PPIXd_gr 
					estadd sca lf_psumcoef = r(estimate)
					estadd sca lf_sep = r(se)
					estadd sca lf_stdp = (r(estimate)/`macled')*100
					test z_food_PPIXd_gr + l_z_food_PPIXd_gr + l2_z_food_PPIXd_gr  = 0
					estadd sca lf_pvalp = r(p)
					
					lincom z_food_PPI + l_z_food_PPI + l2_z_food_PPI
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`macled')*100
					test z_food_PPI + l_z_food_PPI + l2_z_food_PPI = 0
					estadd sca pvalpf = r(p)
					
					lincom z_cash_PPIXd_gr + l_z_cash_PPIXd_gr + l2_z_cash_PPIXd_gr 
					estadd sca lc_psumcoef = r(estimate)
					estadd sca lc_sep = r(se)
					estadd sca lc_stdp = (r(estimate)/`macled')*100
					test z_cash_PPIXd_gr + l_z_cash_PPIXd_gr + l2_z_cash_PPIXd_gr   = 0
					estadd sca lc_pvalp = r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI
					estadd sca pc_sumcoef = r(estimate)
					estadd sca sepc = r(se)
					estadd sca stdpc = (r(estimate)/`macled')*100
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI = 0
					estadd sca pvalpc = r(p)
					est sto regH2	
					

	esttab regH1 regH2  using "$output/table_A30con.tex", replace se noconstant drop(*)  ///
     star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
     scalars(  ///
		"pf_sumcoef \\ Producer Price Index: Food crops" "sepf \hspace{15pt} Conley SE" "pvalpf \hspace{25pt} p-value"  ///
		"lf_psumcoef \\ Producer Price Index: Food $\times$ Precolonial political centralization" "lf_sep \hspace{15pt} Conley SE" "lf_pvalp \hspace{25pt} p-value"  ///
		"pc_sumcoef \\ Producer Price Index: Cash crops" "sepc \hspace{15pt} Conley SE" "pvalpc \hspace{25pt} p-value"  ///
		"lc_psumcoef \\ Producer Price Index: Cash $\times$ Precolonial political centralization" "lc_sep \hspace{15pt} Conley SE" "lc_pvalp \hspace{25pt} p-value"  ///
		"l_c_sum_coef \\ Consumer Price Index $\times$ Precolonial political centralization" "l_sec \hspace{15pt} Conley SE" "l_pvalc \hspace{25pt} p-value"  ///
		"stdp \hline \\ PPI impact (\%)"   ///
		"l_stdp  PPI impact (\%) $\times$ Precolonial political centralization"   ///
		"l_stdc  CPI impact (\%) $\times$ Precolonial political centralization"   ///
		"tw \hline \\ Trade weight"   /// 
		"cy  Country $\times$ year FE" ///
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
			%10.1f  %10.1f %10.1f  %10.1f %10.1f  ///
			%~12s %~12s %~12s %~12s  ///
			%10.0f  )  /// 
     label obslast nomtitles 
	
