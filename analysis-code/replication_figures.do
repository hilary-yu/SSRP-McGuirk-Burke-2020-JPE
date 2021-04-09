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
global output "$dir/command_rep/analysis/output_rep" 
global plots "$dir/command_rep/analysis/output_rep/plots" 


ssc install sutex
ssc install reghdfe
ssc install geonear
ssc	install tmpdir
ssc	install reg2hdfe

do 	"$dir/command_rep/analysis/adofiles/reg2hdfespatial.ado"

do 	"$dir/command_rep/analysis/adofiles/grc1leg.ado"

do	"$dir/command_rep/analysis/adofiles/ols_spatial_HAC.ado"


***********************************
***		SELECTED FIGURES		***
***********************************



***********************************
*******		FIGURE 1	***********
***********************************	

use "$dir/input_rep/cell_data.dta", clear


* Descriptive evidence
cap gen farm = ar_allcrops > 0 & !missing(ar_allcrops)
cap label var foodpriceindex				"FAO Food Price Index"
cap label define farm 0 "Cell with no cropland" 1 "Cells with cropland"
cap label values farm farm 

*	DRAFT
	set scheme lean1
	twoway 		 (lpolyci  ucdp_10 foodpriceindex if farm == 1  , ciplot(rline) bwidth(20) level(95))  (lpolyci   ucdp_10 foodpriceindex if farm == 0 ,  ciplot(rline) bwidth(20) level(95)), legend(position(3) cols(1) order ( 2 3 ) label( 1 "" ) label( 2 "Producer cells" ) label( 3 "Consumer cells")  region(ls(none))) ytit(Pr(Factor Conflict = 1)) xtit(FAO Global Food Price Index)
	graph save "$plots/factor_faoprice", replace
*	graph export "$plots/factor_faoprice.png", as(png) replace  
*	graph export "$plots/factor_faoprice.eps", as(eps) replace  

	twoway 		 (lpolyci  acled_riot_67 foodpriceindex if farm == 1  , ciplot(rline) bwidth(20) level(95))  (lpolyci   acled_riot_67 foodpriceindex if farm == 0 ,  ciplot(rline) bwidth(20) level(95)), legend(position(3) cols(1) order ( 2 3 ) label( 1 "" ) label( 2 "Producer cells" ) label( 3 "Consumer cells")  region(ls(none))) ytit(Pr(Output Conflict = 1)) xtit(FAO Global Food Price Index) 
	graph save "$plots/output_faoprice", replace
*	graph export "$plots/output_faoprice.png", as(png) replace  
*	graph export "$plots/output_faoprice.eps", as(eps) replace  

	graph combine "$plots/factor_faoprice" "$plots/output_faoprice", col(1) iscale(0.8)  xsize(4)  ysize(5)
	graph save "$plots/fig_1", replace
*	graph export "$plots/descriptive_fao.png", as(png) replace
*	graph export "$plots/descriptive_fao.pdf", as(pdf) replace
	graph export "$plots/fig_1.eps", as(eps) replace
	
	erase "$plots/factor_faoprice.gph"
	erase "$plots/output_faoprice.gph"
	
	
***********************************
*******		FIGURE 2	***********
***********************************	
use "$dir/input_rep/cell_data.dta", clear

bys year: egen total_ucdp = total(ucdp_10), missing
bys year: egen total_acled67 = total(acled_riot_67), missing
bys year: egen total_acled123 = total(acled_battle_123), missing
bys year: egen total_acled2 = total(acled_fight_2), missing

collapse (mean) p_*  ar_* foodpriceindex meatpriceindex dairypriceindex sugarpriceindex oilspriceindex total_acled123 total_acled2  total_acled67 total_ucdp cerealspriceindex _all_tr_area_price_cell _all_tr_basket_price_country _all_tr_area_cash_1pc _all_tr_area_food_1pc, by(year)

label var total_ucdp					"UCDP Factor Conflict"
label var total_acled67					"ACLED Output Conflict"
label var total_acled123				"Factor conflict ACLED"
label var total_acled2				"ACLED Territorial Change"
label var foodpriceindex				"FAO Food Price"
label var cerealspriceindex				"FAO Cereals Price"
label var meatpriceindex				"FAO Meat Price"
label var dairypriceindex				"FAO Dairy Price"
label var sugarpriceindex				"FAO Sugar Price"
label var oilspriceindex				"FAO Oils Price "
label var p_tobacco				"Tobacco"
label var p_coffee				"Coffee"
label var p_wheat				"Wheat"
label var p_rice				"Rice"
label var p_maize				"Maize"
label var p_cocoa				"Cocoa"
label var year "Year"

	cap graph drop g*
	set scheme lean1
	
	gen share_factor = total_ucdp/10229
	gen share_output = total_acled67/10229
	
	label var share_output "ACLED Output Conflict"
	label var share_factor "UCDP Factor Conflict"
		
	twoway  (tsline share_output share_factor, yaxis(1)) 		if year >= 1989 & year <= 2013, name(gs1, replace) ytit(Share of Cells in Conflict) legend(position(6) cols(1)region(ls(none)) )
	twoway (tsline p_maize) (tsline p_wheat p_rice , yaxis(1)) 	if year >= 1989 & year <= 2013 , name(g5, replace) ytitl(Price Index) legend(position(6) cols(2) region(ls(none)) )
	twoway (tsline p_coffee p_cocoa) (tsline p_tobacco  , yaxis(1)) if year >= 1989 & year <= 2013 , name(g6, replace) ytitl(Price Index) legend(position(6) cols(2) region(ls(none)) )
	graph combine gs1 g5 g6, col(1) iscale(1)   xcomm  xsize(4) ysize(10)
		graph save "$plots/fig_2", replace
	graph export "$plots/fig_2.eps", as(eps) replace
	
	


***********************************
*******		FIGURE 4-6	***********
***********************************	

*See R code files in cope_rep folder

***********************************
*******		FIGURE 7	***********
***********************************	
/*

IFPRI aggregates projected demand side scenarios caused by population 
and economic growth, as well as projected supply side scenarios caused 
by climate change. 

SOURCE: http://www.ifpri.org/publication/food-security-farming-and-climate-change-2050
PAGE 23

Normalizing all prices to 2010 = 100. The mean change from 2010 to 2050 is
	Maize 	100.7
	Rice 	54.8
 	Wheat 	54.2
	
With full climate change mitigation, the 2050 price is: 
	Maize 	2050 price * 100/132.2
	Rice 	2050 price * 100/119.8
 	Wheat 	2050 price * 100/123.1

*-----------------------------------

Producer price index for 2010: 5.088741

Producer price index shares
	Maize 	.0073392 
	Rice 	.0022041 
	Wheat 	.002997 
	
Change: area * change * 2010 price
	(.0073392 * 100.7/100 * 210.8461) + (.0022041 * 54.8/100 * 255.558) + (.002997 * 54.2/100 * 196.1982)
	= 2.185648

2050 index is 7.274389
	Percentage change is 2.185648/5.088741 = 0.42950663
	
2050 components are
	Maize: (.0073392 * 100.7/100 * 210.8461) + (210.8461 * .0073392) = 3.1057155
	Rice: (.0022041 * 54.8/100 * 255.558) + (255.558 * .0022041) = 0.8719503
	Wheat: (.002997 * 54.2/100 * 196.1982) + (196.1982 * .002997) = 0.90670526
	Residual: 7.274389 - 3.1057155 - 0.8719503 - 0.90670526 = 2.3900179

2050 index with perfect climate change mitigation is: 
	2.3900179 + (3.1057155 * 100/132.2) + (0.8719503 * 100/119.8) + (0.90670526 * 100/123.1) = 6.2036714
	Percentage change is 1.1149304/5.088741 = 0.21909749

*-----------------------------------
	
Consumer price index for 2010: 117.9135
	
Consumer price index shares
	Maize 	.1231617 
	Rice 	.0678087 
	Wheat 	.1617384  

Change:
	(.1231617 * 100.7/100 * 210.8461) + (.0678087 * 54.8/100 * 255.558) + (.1617384 * 54.2/100 * 196.1982)
	= 52.845432
	
2050 index is 170.75893
	Percentage change is 52.845432/117.9135 = .44817118
	
2050 components are
	Maize: (.1231617 * 100.7/100 * 210.8461) + (210.8461 * .1231617) = 52.118105
	Rice: (.0678087 * 54.8/100 * 255.558) + (255.558 * .0678087) = 26.825378
	Wheat: (.1617384 * 54.2/100 * 196.1982) + (196.1982 * .1617384) = 48.931951
	Residual: 170.75893 - 52.118105 - 26.825378 - 48.931951 = 42.883496	
	
2050 index with perfect climate change mitigation is: 
	42.883496 + (52.118105 * 100/132.2) + (26.825378 * 100/119.8) + (48.931951 * 100/123.1) = 144.44873
	Percentage change is 26.53523/117.9135 = 0.2250398

*-----------------------------------

*FACTOR CONFLICT
	
*	PRODUCER EFFECT
	loneway area_price_cell cell // within cell sd is  4.037585
	Incidence: (2.185648/4.037585) * 17.2 = 9.3107998 (of dep var mean)
	Incidence effect in terms of percentage points: 0.093107998 * .027031 = 0.0025168
	So from 0.0215075 in 2010 to 0.240243 in 2050. Or a 0.11701964 percentage change. 
*		With climate change mitigation
		(1.1149304/4.037585) * 17.2 = 4.7495725 (of dep var mean)
		Incidence effect in terms of percentage points: .047495725 * .027031 = .00128386
		So from .0215075 in 2010 to .02279136 in 2050. Or a 0.0596936 percentage change. 

*	CONSUMER EFFECT
	loneway _all_basket_price_country cell // within cell sd is 33.43792
	Incidence (52.845432/33.43792) * 8.6 = 13.591477 (of dep var mean)
	Incidence effect in terms of percentage points: .13591477 * .027031 = .00367391
	So from .0215075 in 2010 to .02518141 Or a 0.17081995 percentage change. 
*		With climate change mitigation
		(26.53523/33.43792) * 8.6 = 6.8246762 (of dep var mean)
		Incidence effect in terms of percentage points: .068246762 * .027031 = 0.00184478
		So from .0215075 in 2010 to .02335228 in 2050. Or a 0.0857738 percentage change. 

*-----------------------------

*OUTPUT CONFLICT

*	PRODUCER EFFECT
	loneway area_price_cell cell // within cell sd is 4.037585
	Incidence: (2.185648/4.037585) * 18.9 = 10.231053 (of dep var mean)
	Incidence effect in terms of percentage points: .10231053 * .0503873 = .00515515
	So from .0490762 in 2010 to .05423135 in 2010 = 0.10504379 percentage change
*		With climate change mitigation
		(1.1149304/4.037585) * 18.9 = 5.219007 (of dep var mean)
		Incidence effect in terms of percentage points: .05219007 * .0503873 = .00262972
		So from .0490762 in 2010 to .05170592 in 2010 = 0.05358443 percentage change

*	CONSUMER EFFECT
	loneway _all_basket_price_country cell // within cell sd is 33.43792
	Incidence (52.845432/33.43792) * 14.4 = 22.757822 (of dep var mean)
	Incidence effect in terms of percentage points: .22757822 * .0503873 = 0.01146705
	So from .0490762 in 2010 to .06054325 in 2010 => 0.23365807 percentage change
*		With climate change mitigation
		(26.53523/33.43792) * 14.4 = 11.427365 (of dep var mean)
		Incidence effect in terms of percentage points: .11427365  * .0503873 = .00575791
		So from .0490762 in 2010 to .05483411 in 2010 => .11732591 percentage change

*-----------------------------

*WEIGHTED AVERAGE

*	FACTOR CONFLICT 
	-11.7(0.63) + 17.1 = 9.729	//	with climate change
	-6(0.63) + 8.6 = 4.82		//	with mitigation
	
*	OUTPUT CONFLICT 
	+10.5(0.63) + 23.4 = 30.015	//	with climate change
	+5.4(0.63) + 11.7 = 15.102	//	with mitigation
	

*-----------------------------
*/
insheet using  "$dir/input_rep/projections.csv",  comma clear

*GRAPHS
tsset year

label var foveralleffectwithclimatechange "Overall effect with climate change"
label var foveralleffectwithoutclimatechan "Overall effect with perfect mitigation"
label var ooveralleffectwithclimatechange "Overall effect with climate change"
label var ooveralleffectwithoutclimatecha "Overall effect with perfect mitigation"

set scheme lean1
cap graph drop g*
tsline fo* , yline(100) ysize(1)  ytitle("UCDP Factor Conflict (2010 = 100)")  legend(off) name(g1)
tsline oo* , yline(100) ysize(1)  ytitle("ACLED Output Conflict (2010 = 100)") legend(off) name(g2)
grc1leg g1 g2,  ycomm xcomm ysize(1) legendfrom(g1) name(g3, replace)
graph display g3, xsize(6) ysize(2.5)
graph use "$plots/fig_7.gph"
graph export "$plots/fig_7.eps", as(eps) replace 


	
***********************************
*******		FIGURE A2	***********
***********************************	
use "$dir/input_rep/cell_data.dta", clear

areg ucdp_10 	_ct_*, a(cell) cluster(cy)
predict ucdp_resid, resid

qui areg ucdp_10 z_PPI	_ct_*, a(cell) cluster(cy)
predict ucdpp_resid, resid

qui areg ucdp_10 z_CPI	_ct_*, a(cell) cluster(cy)
predict ucdpc_resid, resid

areg acled_riot_67 	_ct_*, a(cell) cluster(cy)
predict acled67_resid, resid
label var acled67_resid "output"

areg acled_riot_67 z_PPI	_ct_*, a(cell) cluster(cy)
predict acled67p_resid, resid

areg acled_riot_67 z_CPI 	_ct_*, a(cell) cluster(cy)
predict acled67c_resid, resid

areg z_CPI	z_PPI _ct_*, a(cell) cluster(cy)
predict con_resid, resid
label var con_resid "consumer"

areg z_PPI  z_CPI _ct_*, a(cell) cluster(cy)
predict prod_resid, resid
label var prod_resid "producer"


set scheme lean1
twoway 		 (qfitci ucdpc_resid prod_resid, range(-2 2) ciplot(rline) level(95)) (qfitci  ucdpp_resid con_resid, range(-2 2) ciplot(rline) level(95) )   , legend(position(3) cols(1) order ( 2 3 )  label( 2 "Producer Price Index" ) label( 3 "Consumer Price Index")   region(ls(none))) ytit(Pr(UCDP Factor Conflict = 1) Residuals) xtit(Price Index (Standard Deviations) Residuals) 

reghdfe ucdp_10 z_PPI z_CPI _ct*, a(cell)

	graph save "$plots/factor_regs.gph", replace

twoway 		(qfitci acled67c_resid prod_resid, range(-2 2) ciplot(rline) level(95)) (qfitci  acled67p_resid con_resid, range(-2 2) ciplot(rline) level(95))   , legend(position(3) cols(1) order ( 2 3 )  label( 2 "Producer Price Index" ) label( 3 "Consumer Price Index")   region(ls(none))) ytit(Pr(ACLED Output Conflict = 1) Residuals) xtit(Price Index (Standard Deviations) Residuals)

	graph save "$plots/output_regs.gph", replace
	
	graph combine "$plots/factor_regs" "$plots/output_regs", col(1) iscale(0.6)  xsize(10)  ysize(10.5)
	graph save "$plots/fig_A2.gph", replace
	graph export "$plots/fig_A2.eps", as(eps) replace  

	erase "$plots/factor_regs.gph"
	erase "$plots/output_regs.gph"



