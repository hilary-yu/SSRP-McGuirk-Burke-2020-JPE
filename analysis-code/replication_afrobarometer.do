
*Set directory (direct to the folder prior to the TIER subfolders: command_rep, documents_rep, etc.)
*In the example below, the folder prior to the TIER subfolders is called SSRP_McGuirk_Burke_2020_JPE

global dir "*your directory/SSRP_McGuirk_Burke_2020_JPE*" 
global output "$dir/command_rep/analysis/output_rep/tables" 


use "$dir/input_rep/afro_data.dta", clear


***********************************
***		AFROBAROMETER TABLES	***
***********************************

/*
Table 6		Afrobarometer Output Conflict: Triple Difference
Table A26	Afrobarometer: Prices and Poverty
Table A27	Afrobarometer: Output Conflict Validation Tests
Table A28	Afrobarometer Output Conflict: Triple Difference with Non-Commercial Farmers
*/

*----------------------------------

***********************************
*******		TABLE 6		***********
***********************************	

global hardship  hardship_index hardship_income hardship_food
global outcome1  theft_pastyear_01 violence_pastyear_01 protest 
*fear_crime_01 safety only in 2 rounds
global theftviolence  theft_pastyear_01  violence_pastyear_01  

*Macro
global land ar_cocoa ar_coffee ar_maize ar_oilpalm ar_rice ar_sorghum ar_soybean ar_sugarcane ar_tea ar_tobacco ar_wheat


cap est drop reg*
	foreach i of varlist $theftviolence {

	summ `i'
	loc m`i' = r(mean)

		cap gen z_food_PPIXfarmer_comm = z_food_PPI *farmer_comm
		cap gen z_cash_PPIXfarmer_comm = z_cash_PPI *farmer_comm
		
		cap gen l_z_food_PPIXfarmer_comm = l_z_food_PPI *farmer_comm
		cap gen l_z_cash_PPIXfarmer_comm = l_z_cash_PPI *farmer_comm
		
		cap gen l2_z_food_PPIXfarmer_comm = l2_z_food_PPI *farmer_comm
		cap gen l2_z_cash_PPIXfarmer_comm = l2_z_cash_PPI *farmer_comm
		
		cap gen l3_z_food_PPIXfarmer_comm = l3_z_food_PPI *farmer_comm
		cap gen l3_z_cash_PPIXfarmer_comm = l3_z_cash_PPI *farmer_comm
		
		cap gen l4_z_food_PPIXfarmer_comm = l4_z_food_PPI *farmer_comm
		cap gen l4_z_cash_PPIXfarmer_comm = l4_z_cash_PPI *farmer_comm
		
		cap gen z_food_PPIXtrader = z_food_PPI *trader
		cap gen z_cash_PPIXtrader = z_cash_PPI *trader
		
		cap gen l_z_food_PPIXtrader = l_z_food_PPI *trader
		cap gen l_z_cash_PPIXtrader = l_z_cash_PPI *trader
		
		cap gen l2_z_food_PPIXtrader = l2_z_food_PPI *trader
		cap gen l2_z_cash_PPIXtrader = l2_z_cash_PPI *trader
		
		cap gen l3_z_food_PPIXtrader = l3_z_food_PPI *trader
		cap gen l3_z_cash_PPIXtrader = l3_z_cash_PPI *trader
		
		cap gen l4_z_food_PPIXtrader = l4_z_food_PPI *trader
		cap gen l4_z_cash_PPIXtrader = l4_z_cash_PPI *trader
		
		
reghdfe `i' 	    z_CPI l_z_CPI l2_z_CPI l3_z_CPI l4_z_CPI    ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_comm l_z_food_PPIXfarmer_comm l2_z_food_PPIXfarmer_comm l3_z_food_PPIXfarmer_comm l4_z_food_PPIXfarmer_comm  ///
					z_cash_PPIXfarmer_comm l_z_cash_PPIXfarmer_comm l2_z_cash_PPIXfarmer_comm l3_z_cash_PPIXfarmer_comm l4_z_cash_PPIXfarmer_comm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
					i.trader i.farmer_comm age age2 i.education i.male i.urban $land i.round _cp* , a( ctry) cluster(deg1cell)
					
					lincom z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI 
					estadd sca c_sumcoef = r(estimate)
					estadd sca stdc = (r(estimate)/`m`i'')*100
					estadd sca sec = r(se)
					test z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI = 0
					estadd sca pvalc =r(p)
	
					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)

					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "Yes"
					estadd local cpfe "No"
					estadd local yfe "No"
					estadd local afe "Country"
					estadd local con "Yes"
					est sto reg`i'1
					
reghdfe `i' 	    z_CPI l_z_CPI l2_z_CPI l3_z_CPI l4_z_CPI    ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_comm l_z_food_PPIXfarmer_comm l2_z_food_PPIXfarmer_comm l3_z_food_PPIXfarmer_comm l4_z_food_PPIXfarmer_comm  ///
					z_cash_PPIXfarmer_comm l_z_cash_PPIXfarmer_comm l2_z_cash_PPIXfarmer_comm l3_z_cash_PPIXfarmer_comm l4_z_cash_PPIXfarmer_comm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
					i.trader i.farmer_comm age age2 i.education i.male i.urban $land _cp* , a(period ctry) cluster(deg1cell)
				
					lincom z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI 
					estadd sca c_sumcoef = r(estimate)
					estadd sca stdc = (r(estimate)/`m`i'')*100
					estadd sca sec = r(se)
					test z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI = 0
					estadd sca pvalc =r(p)
	
					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)
					
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "Yes"
					estadd local cpfe "No"
					estadd local yfe "Yes"
					estadd local afe "Country"
					estadd local con "Yes"
					est sto reg`i'2


reghdfe `i' 	   	z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_comm l_z_food_PPIXfarmer_comm l2_z_food_PPIXfarmer_comm l3_z_food_PPIXfarmer_comm l4_z_food_PPIXfarmer_comm  ///
					z_cash_PPIXfarmer_comm l_z_cash_PPIXfarmer_comm l2_z_cash_PPIXfarmer_comm l3_z_cash_PPIXfarmer_comm l4_z_cash_PPIXfarmer_comm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
					i.trader i.farmer_comm age age2 i.education i.male i.urban if nval !=1  , a(cp deg1cell) cluster(deg1cell)

					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm  - (z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)
					
					test z_food_PPIXfarmer_comm + l_z_food_PPIXfarmer_comm + l2_z_food_PPIXfarmer_comm + l3_z_food_PPIXfarmer_comm + l4_z_food_PPIXfarmer_comm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_comm + l_z_cash_PPIXfarmer_comm + l2_z_cash_PPIXfarmer_comm + l3_z_cash_PPIXfarmer_comm + l4_z_cash_PPIXfarmer_comm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "N/A"
					estadd local cpfe "Yes"
					estadd local yfe "N/A"
					estadd local afe "Cell"
					estadd local con "Yes"
					est sto reg`i'3

					}
					
					esttab regtheft* regviol*  using "$output/table_6.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"pf_sumcoef  Producer Price Index: Food crops" "sepf \hspace{15pt} SE" "pvalpf  \hspace{15pt} p-value"  ///
					"pf_sumint_farm  Producer Price Index: Food crops $\times$ farmer" "sepff \hspace{15pt} SE" "pvalpf_int_farm  \hspace{15pt} p-value"  ///
					"pf_sumint_trade  Producer Price Index: Food crops $\times$ trader" "sepft \hspace{15pt} SE" "pvalpf_int_trade  \hspace{15pt} p-value"  ///
					"pc_sumcoef Producer Price Index: Cash crops" "sepc \hspace{15pt} SE" "pvalpc  \hspace{15pt} p-value"  ///
					"pc_sumint_farm  Producer Price Index: Cash crops  $\times$ farmer" "sepcf \hspace{15pt} SE" "pvalpc_int_farm  \hspace{15pt} p-value"  ///
					"pc_sumint_trade  Producer Price Index: Cash crops  $\times$ trader" "sepct \hspace{15pt} SE" "pvalpc_int_trade  \hspace{15pt} p-value"  ///
					"c_sumcoef  Consumer Price Index" "sec \hspace{15pt} SE" "pvalc  \hspace{15pt} p-value"  ///
					"te \hline \\  \underline{\emph{Treatment effects}}"  ///
					"treatment_farm \\  (PPI Food $-$ PPI Cash) $\times$ farmer" "treatment_farm_se  \hspace{15pt} SE" "test_farm  \hspace{15pt} p-value" "stdfc_farm \hspace{15pt} Impact on farmers (\%)" ///
					"treatment_trade \\ (PPI Food $-$ PPI Cash) $\times$ trader" "treatment_trade_se  \hspace{15pt} SE" "test_trade  \hspace{15pt} p-value" "stdfc_trade \hspace{15pt} Impact on traders (\%)" ///
					"cpfe \hline \\ Country $\times$ half-year fixed effects" "cpt  Country $\times$ time trend"  "yfe Half-year fixed effects" "con Controls" "afe Area fixed effects"    /// 
					 "N Observations")  ///
					mgroups("Theft" "Violence", pattern(1 0 0 1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ {t-k} _{t-k})  /// 
					sfmt(   ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%~12s ///
							%10.4f %10.3f  %10.3f %10.1f  ///
							%10.4f %10.3f  %10.3f %10.1f ///
							%~12s %~12s %~12s %~12s ///
							 %10.0f  )  /// 
				label obslast nomtitles
				


***********************************
*******		TABLE A26	***********
***********************************	


cap est drop reg*
foreach i of varlist $hardship {
	
	summ `i'
	loc m`i' = r(mean)

		cap drop c_priceX*
		cap drop lc_priceX*
		cap drop l2c_priceX*
		cap drop l3c_priceX*
		cap drop l4c_priceX*
		cap drop p_priceX*
		cap drop lp_priceX*
		cap drop l2p_priceX*
		cap drop l3p_priceX*
		cap drop l4p_priceX*

		cap gen z_PPIXfarmer_any = 	z_PPI*farmer_any
		cap gen l_z_PPIXfarmer_any = 	l_z_PPI*farmer_any
		cap gen l2_z_PPIXfarmer_any = 	l2_z_PPI*farmer_any
		cap gen l3_z_PPIXfarmer_any = 	l3_z_PPI*farmer_any
		cap gen l4_z_PPIXfarmer_any = 	l4_z_PPI*farmer_any
		
reghdfe `i' 	    z_CPI l_z_CPI l2_z_CPI l3_z_CPI l4_z_CPI  ///
					z_PPI l_z_PPI l2_z_PPI l3_z_PPI l4_z_PPI  ///
					age age2 i.education i.male i.urban i.round _cp* $land , a(ctry) cluster(deg1cell)
				
					lincom z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI
					estadd sca c_sumcoef = r(estimate)
					estadd sca stdc = (r(estimate)/`m`i'')*100
					estadd sca sec = r(se)
					test z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI = 0
					estadd sca pvalc = r(p)

					lincom z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI
					estadd sca p_sumcoef = r(estimate)
					estadd sca stdp = (r(estimate)/`m`i'')*100
					estadd sca sep = r(se)
					test z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI = 0
					estadd sca pvalp = r(p)
										
					estadd local ctt "Yes"
					estadd local cyfe "No"
					estadd local round "Yes"
					estadd local afe "Country"
					estadd local con "Yes"
					
					est sto reg`i'1
					

	
 reghdfe `i' 	     ///
					z_PPI l_z_PPI l2_z_PPI l3_z_PPI l4_z_PPI    ///
					z_PPIXfarmer_any l_z_PPIXfarmer_any l2_z_PPIXfarmer_any l3_z_PPIXfarmer_any l4_z_PPIXfarmer_any  ///
					i.farmer_any age age2 i.education i.male i.urban  $land  , a(cp) cluster(deg1cell)
				
					lincom z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca stdp = (r(estimate)/`m`i'')*100
					estadd sca sep = r(se)
					test z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI = 0
					estadd sca pvalp = r(p)
					
					lincom z_PPIXfarmer_any + l_z_PPIXfarmer_any + l2_z_PPIXfarmer_any + l3_z_PPIXfarmer_any + l4_z_PPIXfarmer_any 
					estadd sca p_sumint_farm = r(estimate)
					estadd sca stdp_farm = (r(estimate)/`m`i'')*100
					estadd sca sepf = r(se)
					test z_PPIXfarmer_any + l_z_PPIXfarmer_any + l2_z_PPIXfarmer_any + l3_z_PPIXfarmer_any + l4_z_PPIXfarmer_any = 0
					estadd sca pvalp_int_farm = r(p)
					
					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local round "Yes"
					estadd local afe "Country"
					estadd local con "Yes"
					
					est sto reg`i'3

		
reghdfe `i' 	    	 ///
					z_PPI l_z_PPI l2_z_PPI l3_z_PPI l4_z_PPI    ///
					z_PPIXfarmer_any l_z_PPIXfarmer_any l2_z_PPIXfarmer_any l3_z_PPIXfarmer_any l4_z_PPIXfarmer_any  ///
					i.farmer_any age age2 i.education i.male i.urban, a(cp deg1cell) cluster(deg1cell)
				

					lincom z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI 
					estadd sca p_sumcoef = r(estimate)
					estadd sca stdp = (r(estimate)/`m`i'')*100
					estadd sca sep = r(se)
					test z_PPI + l_z_PPI + l2_z_PPI + l3_z_PPI + l4_z_PPI = 0
					estadd sca pvalp = r(p)
					
					lincom z_PPIXfarmer_any + l_z_PPIXfarmer_any + l2_z_PPIXfarmer_any + l3_z_PPIXfarmer_any + l4_z_PPIXfarmer_any 
					estadd sca p_sumint_farm = r(estimate)
					estadd sca stdp_farm = (r(estimate)/`m`i'')*100
					estadd sca sepf = r(se)
					test z_PPIXfarmer_any + l_z_PPIXfarmer_any + l2_z_PPIXfarmer_any + l3_z_PPIXfarmer_any + l4_z_PPIXfarmer_any = 0
					estadd sca pvalp_int_farm = r(p)

					estadd local ctt "N/A"
					estadd local cyfe "Yes"
					estadd local round "Yes"
					estadd local afe "Cell"
					estadd local con "Yes"
					
					est sto reg`i'4

					}
					
					esttab reghardship_ind* reghardship_inc* reghardship_foo* using "$output/table_A26.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"p_sumcoef \\ Producer Price Index" "sep  \hspace{15pt} SE"  "pvalp  \hspace{15pt} p-value"  ///
					"p_sumint_farm \\ Producer Price Index $\times$ farmer" "sepf  \hspace{15pt} SE" "pvalp_int_farm  \hspace{15pt}  p-value"  ///
					"c_sumcoef \\ Consumer Price Index" "sec  \hspace{15pt} SE"  "pvalc  \hspace{15pt} p-value"  ///
					"stdp \hline \\  PPI impact (\%)" "stdp_farm  PPI impact $\times$ farmer (\%)" "stdc  CPI impact (\%)"    ///
					"ctt \hline \\ Country $\times$ time trend" "cyfe  Country $\times$ period fixed effects" "con Controls" "afe Area FE"  "round Survey round fixed effects"   /// 
					"N Observations")  ///
					mgroups("Poverty: index" "Poverty: income" "Poverty: food", pattern(1 0 0  1 0  0 1 0  0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ {t-k} _{t-k})  /// 
					sfmt(   ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f %10.1f  %10.1f  ///
							%~12s %~12s %~12s  %~12s %~12s  ///
							%10.0f  )  /// 
				label obslast nomtitles
							

***********************************
*******		TABLE A27	***********
***********************************	


foreach i of varlist d1acled_riot_67 d1lacled_riot_67 d1l2acled_riot_67 d1ucdp_10 d1lucdp_10 d1l2ucdp_10 {
	replace `i' = 1 if `i' > 0 & !missing(`i')
	}
	
	
	cap est drop reg*
	foreach i of varlist $outcome1 {
	
	summ `i'
	loc m`i' = r(mean)
	
reg `i'   d1acled_riot_67 d1lacled_riot_67 d1l2acled_riot_67 ,  cluster(deg1cell)
			lincom d1acled_riot_67 + d1lacled_riot_67 + d1l2acled_riot_67 
			estadd sca ac_sumcoef = r(estimate)
			estadd sca pvalac =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
			estadd sca stdac = (r(estimate)/`m`i'')*100
			estadd sca seac = r(se)
					estadd local cy "No"
					estadd local fe "No"
					est sto reg`i'1

reghdfe `i'   d1acled_riot_67 d1lacled_riot_67 d1l2acled_riot_67  i.round, a(ctry) cluster(deg1cell)
			lincom d1acled_riot_67 + d1lacled_riot_67 + d1l2acled_riot_67 
			estadd sca ac_sumcoef = r(estimate)
			estadd sca pvalac =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
			estadd sca stdac = (r(estimate)/`m`i'')*100
			estadd sca seac = r(se)
					estadd local cy "Yes"
					estadd local fe "Yes"
			est sto reg`i'2

reghdfe `i'   d1acled_riot_67 d1lacled_riot_67 d1l2acled_riot_67 d1ucdp_10 d1lucdp_10 d1l2ucdp_10 i.round, a(ctry) cluster(deg1cell)
			lincom d1acled_riot_67 + d1lacled_riot_67 + d1l2acled_riot_67 
			estadd sca ac_sumcoef = r(estimate)
			estadd sca pvalac =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
			estadd sca stdac = (r(estimate)/`m`i'')*100
			estadd sca seac = r(se)
			
			lincom d1ucdp_10 + d1lucdp_10 + d1l2ucdp_10
			estadd sca uc_sumcoef = r(estimate)
			estadd sca pvaluc =2*ttail(e(N_clust),abs(r(estimate)/r(se)))
			estadd sca stduc = (r(estimate)/`m`i'')*100
			estadd sca seuc = r(se)
					estadd local cy "Yes"
					estadd local fe "Yes"
			est sto reg`i'3

}
			esttab regtheft*  regviolence* regpro*  using "$output/table_A27.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"ac_sumcoef \\ ACLED Output Conflict" "seac \hspace{15pt} SE"  "pvalac  \hspace{15pt} p-value"  ///
					"uc_sumcoef \\ UCDP Factor Conflict" "seuc \hspace{15pt} SE"  "pvaluc  \hspace{15pt} p-value"  ///
					"stdac \hline \\  ACLED Output Conflict (\%)"   ///
					"stduc   UCDP Factor Conflict (\%)"   ///
					"fe \hline \\ Country fixed effects" "cy  Survey round fixed effects"   /// 
					"r2 R squared"  "N Observations")  ///
					mgroups("Theft" "Violence" "Protest", pattern(1 0 0 1 0 0 1 0 0 )  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					sfmt(   %10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.1f %10.1f   ///
							%~12s %~12s  ///
							%10.3f %10.0f  )  /// 
				label obslast nomtitles
			

		
***********************************
*******		TABLE A28	***********
***********************************	

	cap est drop reg*
	foreach i of varlist $theftviolence {

	summ `i'
	loc m`i' = r(mean)

		
		cap gen z_food_PPIXfarmer_noncomm = z_food_PPI *farmer_noncomm
		cap gen z_cash_PPIXfarmer_noncomm = z_cash_PPI *farmer_noncomm
		
		cap gen l_z_food_PPIXfarmer_noncomm = l_z_food_PPI *farmer_noncomm
		cap gen l_z_cash_PPIXfarmer_noncomm = l_z_cash_PPI *farmer_noncomm
		
		cap gen l2_z_food_PPIXfarmer_noncomm = l2_z_food_PPI *farmer_noncomm
		cap gen l2_z_cash_PPIXfarmer_noncomm = l2_z_cash_PPI *farmer_noncomm
		
		cap gen l3_z_food_PPIXfarmer_noncomm = l3_z_food_PPI *farmer_noncomm
		cap gen l3_z_cash_PPIXfarmer_noncomm = l3_z_cash_PPI *farmer_noncomm
		
		cap gen l4_z_food_PPIXfarmer_noncomm = l4_z_food_PPI *farmer_noncomm
		cap gen l4_z_cash_PPIXfarmer_noncomm = l4_z_cash_PPI *farmer_noncomm
		
		cap gen z_food_PPIXtrader = z_food_PPI *trader
		cap gen z_cash_PPIXtrader = z_cash_PPI *trader
		
		cap gen l_z_food_PPIXtrader = l_z_food_PPI *trader
		cap gen l_z_cash_PPIXtrader = l_z_cash_PPI *trader
		
		cap gen l2_z_food_PPIXtrader = l2_z_food_PPI *trader
		cap gen l2_z_cash_PPIXtrader = l2_z_cash_PPI *trader
		
		cap gen l3_z_food_PPIXtrader = l3_z_food_PPI *trader
		cap gen l3_z_cash_PPIXtrader = l3_z_cash_PPI *trader
		
		cap gen l4_z_food_PPIXtrader = l4_z_food_PPI *trader
		cap gen l4_z_cash_PPIXtrader = l4_z_cash_PPI *trader
		
		
reghdfe `i' 	    z_CPI l_z_CPI l2_z_CPI l3_z_CPI l4_z_CPI    ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_noncomm l_z_food_PPIXfarmer_noncomm l2_z_food_PPIXfarmer_noncomm l3_z_food_PPIXfarmer_noncomm l4_z_food_PPIXfarmer_noncomm  ///
					z_cash_PPIXfarmer_noncomm l_z_cash_PPIXfarmer_noncomm l2_z_cash_PPIXfarmer_noncomm l3_z_cash_PPIXfarmer_noncomm l4_z_cash_PPIXfarmer_noncomm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
						i.trader i.farmer_comm age age2 i.education i.male i.urban $land i.round _cp* , a( ctry) cluster(deg1cell)
					
					
					
					lincom z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI 
					estadd sca c_sumcoef = r(estimate)
					estadd sca stdc = (r(estimate)/`m`i'')*100
					estadd sca sec = r(se)
					test z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI = 0
					estadd sca pvalc =r(p)
	
					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)

					/*
					test pf_priceXfarmer_noncomm + lpf_priceXfarmer_noncomm + l2pf_priceXfarmer_noncomm + l3pf_priceXfarmer_noncomm + l4pf_priceXfarmer_noncomm = pc_priceXfarmer_noncomm + lpc_priceXfarmer_noncomm + l2pc_priceXfarmer_noncomm + l3pc_priceXfarmer_noncomm + l4pc_priceXfarmer_noncomm
					estadd sca test_farm = r(p)
					test pf_priceXtrader + lpf_priceXtrader + l2pf_priceXtrader + l3pf_priceXtrader + l4pf_priceXtrader =  pc_priceXtrader + lpc_priceXtrader + l2pc_priceXtrader + l3pc_priceXtrader + l4pc_priceXtrader
					estadd sca test_trade = r(p) */
					
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "Yes"
					estadd local cpfe "No"
					estadd local srfe "No"
					estadd local afe "Country"
					estadd local con "Yes"
					est sto reg`i'1


		
reghdfe `i' 	    z_CPI l_z_CPI l2_z_CPI l3_z_CPI l4_z_CPI    ///
					z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_noncomm l_z_food_PPIXfarmer_noncomm l2_z_food_PPIXfarmer_noncomm l3_z_food_PPIXfarmer_noncomm l4_z_food_PPIXfarmer_noncomm  ///
					z_cash_PPIXfarmer_noncomm l_z_cash_PPIXfarmer_noncomm l2_z_cash_PPIXfarmer_noncomm l3_z_cash_PPIXfarmer_noncomm l4_z_cash_PPIXfarmer_noncomm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
						i.trader i.farmer_comm age age2 i.education i.male i.urban $land _cp* , a(period ctry) cluster(deg1cell)	
						
					lincom z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI 
					estadd sca c_sumcoef = r(estimate)
					estadd sca stdc = (r(estimate)/`m`i'')*100
					estadd sca sec = r(se)
					test z_CPI + l_z_CPI + l2_z_CPI + l3_z_CPI + l4_z_CPI = 0
					estadd sca pvalc =r(p)
	
					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)

					/*
					test pf_priceXfarmer_noncomm + lpf_priceXfarmer_noncomm + l2pf_priceXfarmer_noncomm + l3pf_priceXfarmer_noncomm + l4pf_priceXfarmer_noncomm = pc_priceXfarmer_noncomm + lpc_priceXfarmer_noncomm + l2pc_priceXfarmer_noncomm + l3pc_priceXfarmer_noncomm + l4pc_priceXfarmer_noncomm
					estadd sca test_farm = r(p)
					test pf_priceXtrader + lpf_priceXtrader + l2pf_priceXtrader + l3pf_priceXtrader + l4pf_priceXtrader =  pc_priceXtrader + lpc_priceXtrader + l2pc_priceXtrader + l3pc_priceXtrader + l4pc_priceXtrader
					estadd sca test_trade = r(p) */
					
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "Yes"
					estadd local cpfe "No"
					estadd local srfe "Yes"
					estadd local afe "Country"
					estadd local con "Yes"
					est sto reg`i'2
					
					
reghdfe `i' 	   	z_food_PPI l_z_food_PPI l2_z_food_PPI l3_z_food_PPI l4_z_food_PPI   ///
					z_cash_PPI l_z_cash_PPI l2_z_cash_PPI l3_z_cash_PPI l4_z_cash_PPI    ///					
					z_food_PPIXfarmer_noncomm l_z_food_PPIXfarmer_noncomm l2_z_food_PPIXfarmer_noncomm l3_z_food_PPIXfarmer_noncomm l4_z_food_PPIXfarmer_noncomm  ///
					z_cash_PPIXfarmer_noncomm l_z_cash_PPIXfarmer_noncomm l2_z_cash_PPIXfarmer_noncomm l3_z_cash_PPIXfarmer_noncomm l4_z_cash_PPIXfarmer_noncomm  ///
					z_food_PPIXtrader l_z_food_PPIXtrader l2_z_food_PPIXtrader l3_z_food_PPIXtrader l4_z_food_PPIXtrader  ///
					z_cash_PPIXtrader l_z_cash_PPIXtrader l2_z_cash_PPIXtrader l3_z_cash_PPIXtrader l4_z_cash_PPIXtrader  ///
								i.trader i.farmer_comm age age2 i.education i.male i.urban if nval !=1  , a(cp deg1cell) cluster(deg1cell)
	
					lincom z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI 
					estadd sca pf_sumcoef = r(estimate)
					estadd sca sepf = r(se)
					estadd sca stdpf = (r(estimate)/`m`i'')*100
					test z_food_PPI  + l_z_food_PPI + l2_z_food_PPI + l3_z_food_PPI + l4_z_food_PPI  = 0
					estadd sca pvalpf =r(p)
					
					lincom z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI 
					estadd sca pc_sumcoef = r(estimate)
					estadd sca stdpc = (r(estimate)/`m`i'')*100
					estadd sca sepc = r(se)
					test z_cash_PPI + l_z_cash_PPI + l2_z_cash_PPI + l3_z_cash_PPI + l4_z_cash_PPI = 0
					estadd sca pvalpc = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm 
					estadd sca pf_sumint_farm = r(estimate)
					estadd sca stdpf_farm = (r(estimate)/`m`i'')*100
					estadd sca sepff = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = 0
					estadd sca pvalpf_int_farm = r(p)

					lincom z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm 
					estadd sca pc_sumint_farm = r(estimate)
					estadd sca stdpc_farm = (r(estimate)/`m`i'')*100
					estadd sca sepcf = r(se)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = 0
					estadd sca pvalpc_int_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader 
					estadd sca pf_sumint_trade = r(estimate)
					estadd sca stdpf_trade = (r(estimate)/`m`i'')*100
					estadd sca sepft = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader = 0
					estadd sca pvalpf_int_trade = r(p)

					lincom z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader 
					estadd sca pc_sumint_trade = r(estimate)
					estadd sca stdpc_trade = (r(estimate)/`m`i'')*100
					estadd sca sepct = r(se)
					test z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader = 0
					estadd sca pvalpc_int_trade = r(p)

					lincom z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm )
					estadd sca treatment_farm = r(estimate)
					estadd sca stdfc_farm = (r(estimate)/`m`i'')*100
					estadd sca treatment_farm_se = r(se)
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm  - (z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm ) = 0
					estadd sca test_farm = r(p)

					lincom z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader)
					estadd sca treatment_trade = r(estimate)
					estadd sca stdfc_trade = (r(estimate)/`m`i'')*100
					estadd sca treatment_trade_se = r(se)
					test z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader  - (z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader) = 0
					estadd sca test_trade = r(p)

					/*
					test pf_priceXfarmer_noncomm + lpf_priceXfarmer_noncomm + l2pf_priceXfarmer_noncomm + l3pf_priceXfarmer_noncomm + l4pf_priceXfarmer_noncomm = pc_priceXfarmer_noncomm + lpc_priceXfarmer_noncomm + l2pc_priceXfarmer_noncomm + l3pc_priceXfarmer_noncomm + l4pc_priceXfarmer_noncomm
					estadd sca test_farm = r(p)
					test pf_priceXtrader + lpf_priceXtrader + l2pf_priceXtrader + l3pf_priceXtrader + l4pf_priceXtrader =  pc_priceXtrader + lpc_priceXtrader + l2pc_priceXtrader + l3pc_priceXtrader + l4pc_priceXtrader
					estadd sca test_trade = r(p) */
					
					test z_food_PPIXfarmer_noncomm + l_z_food_PPIXfarmer_noncomm + l2_z_food_PPIXfarmer_noncomm + l3_z_food_PPIXfarmer_noncomm + l4_z_food_PPIXfarmer_noncomm = z_food_PPIXtrader + l_z_food_PPIXtrader + l2_z_food_PPIXtrader + l3_z_food_PPIXtrader + l4_z_food_PPIXtrader
					estadd sca test_food = r(p)
					test z_cash_PPIXfarmer_noncomm + l_z_cash_PPIXfarmer_noncomm + l2_z_cash_PPIXfarmer_noncomm + l3_z_cash_PPIXfarmer_noncomm + l4_z_cash_PPIXfarmer_noncomm = z_cash_PPIXtrader + l_z_cash_PPIXtrader + l2_z_cash_PPIXtrader + l3_z_cash_PPIXtrader + l4_z_cash_PPIXtrader
					estadd sca test_cash = r(p)
					
					estadd local cpt "N/A"
					estadd local cpfe "Yes"
					estadd local srfe "Yes"
					estadd local afe "Cell"
					estadd local con "Yes"
					est sto reg`i'3

					}
					
					esttab regtheft* regviol*  using "$output/table_A28.tex", replace se noconstant drop(*)  ///
					star(* 0.100 ** 0.050 *** 0.010) b(%10.4f %10.4f)  nonotes   ///
					scalars(  ///
					"pf_sumcoef  Producer Price Index: Food crops" "sepf \hspace{15pt} SE" "pvalpf  \hspace{15pt} p-value"  ///
					"pf_sumint_farm  Producer Price Index: Food crops $\times$ farmer" "sepff \hspace{15pt} SE" "pvalpf_int_farm  \hspace{15pt} p-value"  ///
					"pf_sumint_trade  Producer Price Index: Food crops $\times$ trader" "sepft \hspace{15pt} SE" "pvalpf_int_trade  \hspace{15pt} p-value"  ///
					"pc_sumcoef Producer Price Index: Cash crops" "sepc \hspace{15pt} SE" "pvalpc  \hspace{15pt} p-value"  ///
					"pc_sumint_farm  Producer Price Index: Cash crops  $\times$ farmer" "sepcf \hspace{15pt} SE" "pvalpc_int_farm  \hspace{15pt} p-value"  ///
					"pc_sumint_trade  Producer Price Index: Cash crops  $\times$ trader" "sepct \hspace{15pt} SE" "pvalpc_int_trade  \hspace{15pt} p-value"  ///
					"c_sumcoef  Consumer Price Index" "sec \hspace{15pt} SE" "pvalc  \hspace{15pt} p-value"  ///
/*					"stdpf \hline \\ PPI impact: Food (\%)" "stdpf_farm \hspace{15pt} $\times$ farmer" "stdpf_trade \hspace{15pt} $\times$ trader"   ///
					"stdpc PPI impact: Cash (\%)" "stdpc_farm \hspace{15pt} $\times$ farmer" "stdpc_trade \hspace{15pt} $\times$ trader"   ///
					"stdc CPI impact (\%)"  */ /// 
					"te \hline \\  \underline{\emph{Treatment effects}}"  ///
					"treatment_farm \\  (PPI Food $-$ PPI Cash) $\times$ farmer" "treatment_farm_se  \hspace{15pt} SE" "test_farm  \hspace{15pt} p-value" "stdfc_farm \hspace{15pt} Impact on farmers (\%)" ///
					"treatment_trade \\ (PPI Food $-$ PPI Cash) $\times$ trader" "treatment_trade_se  \hspace{15pt} SE" "test_trade  \hspace{15pt} p-value" "stdfc_trade \hspace{15pt} Impact on traders (\%)" ///
					"cpt \hline \\ Country $\times$ time trend" "srfe Half-year fixed effects" "cpfe Country $\times$ half-year fixed effects"  "con Controls" "afe Area fixed effects"    /// 
					 "N Observations")  ///
					mgroups("Theft" "Violence", pattern(1 0 0 1 0 0)  ///
					prefix(\multicolumn{@span}{c}{) suffix(})  ///
					span erepeat(\cmidrule(lr){@span}))  ///
					substitute(\_ _ {t-k} _{t-k})  /// 
					sfmt(   ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
							%10.4f %10.3f %10.3f  ///
/*							%10.1f %10.1f %10.1f  ///
							%10.1f %10.1f %10.1f  ///
							%10.1f */ ///
							%~12s ///
							%10.4f %10.3f  %10.3f %10.1f  ///
							%10.4f %10.3f  %10.3f %10.1f ///
							%~12s %~12s %~12s %~12s ///
							 %10.0f  )  /// 
				label obslast nomtitles
					
				
