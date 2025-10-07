********************************************************************************
*Title:			Lab in the field
*Survey:		Experiment		
*Purpose:		Data cleaning
*Author:		Jeremy DO NASCIMENTO MIGUEL - email: jeremy.dnmiguel@gmail.com
*Date: 			April 2022
**********
********************************************************************************


use "$raw\data_anonymized.dta", clear
*Renaming variables
rename (m0_q01 m0_q02 m0_q03 m0_q04 m0_q05 m0_q06 m0_q08 ) (covidfever covidcough covidbreath covidsmell covidclose covidworried covidpositive)


rename (ma_a1 ma_a2 enum ma_a4 ma_a5 ma_a6 ma_a7 trad ma_a9 ) (day month enumerator region zone woreda market trader_name obsID)

rename (mb_b1 mb_b3 mb_b4 mb_b5 mb_b6 mb_b7 mb_b8 mb_b9a mb_b9b mb_b9c mb_b10 mb_b11 mb_b12 ///
		mb_b13 mb_b14 mb_b15 mb_b16 mb_b17 mb_b18 mb_b19 mb_b20 mb_b21) ///
		(agreement age gender educ_formal educ_years trader_years traderwheat_years decision_price decision_quantity decision_location ///
		residence association trucks grain_storage max_storage worker_full worker_part loan loanbirr family_trade traders_knoww traders_knowo)

rename (mc_e1 mc_e2 mc_e3 mc_e4 mc_e5 mc_e6 mc_e7 mc_e8 mc_e9) ///
		(check_red_price check_blue_price check_nbrchips check_separate_blue check_cheaper check_know check_cup check_understand check_explain)
		
rename (task2 me_c1 me_c2 me_c3 me_c4 me_c5 me_c6 me_c7a me_c7b me_c8a me_c8a_oth me_c8b me_c8b_oth me_c9a me_c9b me_c9c me_c9d me_c9e me_c9f me_c9f_oth me_c10a me_c10b ///
		me_c10c me_c10d me_c10e me_c10f me_c10f_oth  me_c11a me_c11b me_c11c me_c13a me_c13b me_c13c me_c13c_oth me_c14a me_c14b me_c15a me_c15b me_c15b_oth  ///
		me_c16a me_c16b me_c16c me_c16d me_c16e me_c16f me_c16f_oth me_c17a me_c17b me_c17c me_c17d me_c17e me_c18 me_c19 me_c19d_ext me_c20 me_c21 me_c22 me_c22a me_c23a me_c23b ///
		me_c23c me_c23d me_c23e me_c23f me_c23f_oth me_c24 me_c25a me_c25b me_c25b_ext me_c25c me_c26 me_c27 me_c28a me_c28b me_c28c me_c28d me_c28e me_c28f me_c28f_oth) ///	
		(order_large nbr_market nbr_trader rank share_wheat_value share_wheat_volume volume_bought volume_sold price_avg cost_first cost_firstoth cost_second cost_secondoth bean_shf bean_assembler bean_commercial ///
		bean_trader bean_ownharvest  bean_buyerother bean_buyerothersp bean_market bean_store bean_farmgate bean_othermarket bean_outworeda bean_locationother bean_locationothersp volume_min volume_avg volume_max premium ///
		premium_pay premium_not premium_nototh premium_trader premium_paytrader quality_buy quality_how quality_howoth color impurity grainsize moisture extraction qual_other qual_othersp ///
		quality_day quality_transac quality_know quality_unknow quality_trust agreement_supplier agreement_topic agreement_price share_buy_agreement share_buy_permanent  nbr_sell_market ///
		separate bean_miller bean_broker bean_wholesale bean_retailers bean_consumer bean_soldoth bean_soldothsp agreement_buyer agreement_bpayment agreement_bprice agreement_bvalue agreement_bmeasure soldmiller ///
		quality_miller impurity_mill moisture_mill extraction_mill color_mill grainsize_mill qual_oth_mill qual_othsp_mill)

rename (mf_d1a mf_d1b mf_d1c mf_d1d mf_d1e mf_d2a mf_d2b mf_d2c mf_d2d mf_d2e mf_d3 mf_d4 mf_d5 mf_d6 mf_d7 mf_d8 mf_d9 mf_d10 mf_d11 mf_d12 mf_d13 mf_d14 ///
 mf_d15 mf_d16 mf_d17 mf_d18 mf_d19 mf_d20 mf_d21) ///
		(tp1a tp1b tp1c tp1d tp1e tp2a tp2b tp2c tp2d tp2e loc1 loc2 loc3 loc4 loc5 loc6 loc7 loc8 loc9 loc10 ///
		entrepreneurship1 entrepreneurship2 entrepreneurship3 entrepreneurship4 entrepreneurship5 entrepreneurship6 entrepreneurship7 risk_av loss_av)
		
		
*Generate Treatment and Control Woreda from the RCT
gen treatment=1 if woreda==133 | woreda==132 | woreda==122 | woreda==221 | woreda==213 | woreda==231 | woreda==232 | woreda==411
recode treatment (.=0)		
		
		
		
		
/*
Small GAME results are stored in two different variables according to the small/large market order 
Replace to have every results in a single variable 		
*/

***Game 1 
*Color
replace f2=f21 if order_large==2
replace f3_1c=f31_1c  if order_large==2 
replace f3_2c=f31_2c  if order_large==2
replace f3_3c=f31_3c  if order_large==2
replace f3_4c=f31_4c  if order_large==2
replace f3_5c=f31_5c  if order_large==2
replace f3_6c=f31_6c  if order_large==2
replace f3_7c=f31_7c  if order_large==2
replace f3_8c=f31_8c if order_large==2
replace f3_9c=f31_9c if order_large==2
replace f3_10c=f31_10c if order_large==2
replace f3_11c=f31_11c if order_large==2
replace f3_12c=f31_12c if order_large==2
replace f3_13c=f31_13c if order_large==2
replace f3_14c=f31_14c if order_large==2
replace f3_15c=f31_15c  if order_large==2
replace f3_16c=f31_16c if order_large==2

*Decisions
replace f3_1d=f31_1d  if order_large==2 
replace f3_2d=f31_2d  if order_large==2
replace f3_3d=f31_3d  if order_large==2
replace f3_4d=f31_4d  if order_large==2
replace f3_5d=f31_5d  if order_large==2
replace f3_6d=f31_6d  if order_large==2
replace f3_7d=f31_7d  if order_large==2
replace f3_8d=f31_8d if order_large==2
replace f3_9d=f31_9d if order_large==2
replace f3_10d=f31_10d if order_large==2
replace f3_11d=f31_11d if order_large==2
replace f3_12d=f31_12d if order_large==2
replace f3_13d=f31_13d if order_large==2
replace f3_14d=f31_14d if order_large==2
replace f3_15d=f31_15d  if order_large==2
replace f3_16d=f31_16d if order_large==2

*Game results
replace f3b=f31b  if order_large==2 
replace f3r=f31r  if order_large==2
replace f3ba=f31ba if order_large==2 
replace f3ra=f31ra  if order_large==2
replace f3t=f31t  if order_large==2
replace f3rq=f31rq  if order_large==2
replace f3e1=f31e1  if order_large==2
replace f3e2=f31e2 if order_large==2

***Game 2 
*Color
replace f4=f41 if order_large==2
replace f5_1c=f51_1c  if order_large==2 
replace f5_2c=f51_2c  if order_large==2
replace f5_3c=f51_3c  if order_large==2
replace f5_4c=f51_4c  if order_large==2
replace f5_5c=f51_5c  if order_large==2
replace f5_6c=f51_6c  if order_large==2
replace f5_7c=f51_7c  if order_large==2
replace f5_8c=f51_8c if order_large==2
replace f5_9c=f51_9c if order_large==2
replace f5_10c=f51_10c if order_large==2
replace f5_11c=f51_11c if order_large==2
replace f5_12c=f51_12c if order_large==2
replace f5_13c=f51_13c if order_large==2
replace f5_14c=f51_14c if order_large==2
replace f5_15c=f51_15c  if order_large==2
replace f5_16c=f51_16c if order_large==2

*Decisions
replace f5_1d=f51_1d  if order_large==2 
replace f5_2d=f51_2d  if order_large==2
replace f5_3d=f51_3d  if order_large==2
replace f5_4d=f51_4d  if order_large==2
replace f5_5d=f51_5d  if order_large==2
replace f5_6d=f51_6d  if order_large==2
replace f5_7d=f51_7d  if order_large==2
replace f5_8d=f51_8d if order_large==2
replace f5_9d=f51_9d if order_large==2
replace f5_10d=f51_10d if order_large==2
replace f5_11d=f51_11d if order_large==2
replace f5_12d=f51_12d if order_large==2
replace f5_13d=f51_13d if order_large==2
replace f5_14d=f51_14d if order_large==2
replace f5_15d=f51_15d  if order_large==2
replace f5_16d=f51_16d if order_large==2

*Game results
replace f5b=f51b  if order_large==2 
replace f5r=f51r  if order_large==2
replace f5ba=f51ba if order_large==2 
replace f5ra=f51ra  if order_large==2
replace f5t=f51t  if order_large==2
replace f5rq=f51rq  if order_large==2
replace f5e1=f51e1  if order_large==2
replace f5e2=f51e2 if order_large==2		
		
**Game 3
*Color
replace f6=f61 if order_large==2
replace f7_1c=f71_1c  if order_large==2 
replace f7_2c=f71_2c  if order_large==2
replace f7_3c=f71_3c  if order_large==2
replace f7_4c=f71_4c  if order_large==2
replace f7_5c=f71_5c  if order_large==2
replace f7_6c=f71_6c  if order_large==2
replace f7_7c=f71_7c  if order_large==2
replace f7_8c=f71_8c if order_large==2
replace f7_9c=f71_9c if order_large==2
replace f7_10c=f71_10c if order_large==2
replace f7_11c=f71_11c if order_large==2
replace f7_12c=f71_12c if order_large==2
replace f7_13c=f71_13c if order_large==2
replace f7_14c=f71_14c if order_large==2
replace f7_15c=f71_15c  if order_large==2
replace f7_16c=f71_16c if order_large==2

*Decisions
replace f7_1d=f71_1d  if order_large==2 
replace f7_2d=f71_2d  if order_large==2
replace f7_3d=f71_3d  if order_large==2
replace f7_4d=f71_4d  if order_large==2
replace f7_5d=f71_5d  if order_large==2
replace f7_6d=f71_6d  if order_large==2
replace f7_7d=f71_7d  if order_large==2
replace f7_8d=f71_8d if order_large==2
replace f7_9d=f71_9d if order_large==2
replace f7_10d=f71_10d if order_large==2
replace f7_11d=f71_11d if order_large==2
replace f7_12d=f71_12d if order_large==2
replace f7_13d=f71_13d if order_large==2
replace f7_14d=f71_14d if order_large==2
replace f7_15d=f71_15d  if order_large==2
replace f7_16d=f71_16d if order_large==2

*Game results
replace f7b=f71b  if order_large==2 
replace f7r=f71r  if order_large==2
replace f7ba=f71ba if order_large==2 
replace f7ra=f71ra  if order_large==2
replace f7t=f71t  if order_large==2
replace f7rq=f71rq  if order_large==2
replace f7e1=f71e1  if order_large==2
replace f7e2=f71e2 if order_large==2

**Game 4
*Color
replace f8=f81 if order_large==2
replace f9_1c=f91_1c  if order_large==2 
replace f9_2c=f91_2c  if order_large==2
replace f9_3c=f91_3c  if order_large==2
replace f9_4c=f91_4c  if order_large==2
replace f9_5c=f91_5c  if order_large==2
replace f9_6c=f91_6c  if order_large==2
replace f9_7c=f91_7c  if order_large==2
replace f9_8c=f91_8c if order_large==2
replace f9_9c=f91_9c if order_large==2
replace f9_10c=f91_10c if order_large==2
replace f9_11c=f91_11c if order_large==2
replace f9_12c=f91_12c if order_large==2
replace f9_13c=f91_13c if order_large==2
replace f9_14c=f91_14c if order_large==2
replace f9_15c=f91_15c  if order_large==2
replace f9_16c=f91_16c if order_large==2

*Decisions
replace f9_1d=f91_1d  if order_large==2 
replace f9_2d=f91_2d  if order_large==2
replace f9_3d=f91_3d  if order_large==2
replace f9_4d=f91_4d  if order_large==2
replace f9_5d=f91_5d  if order_large==2
replace f9_6d=f91_6d  if order_large==2
replace f9_7d=f91_7d  if order_large==2
replace f9_8d=f91_8d if order_large==2
replace f9_9d=f91_9d if order_large==2
replace f9_10d=f91_10d if order_large==2
replace f9_11d=f91_11d if order_large==2
replace f9_12d=f91_12d if order_large==2
replace f9_13d=f91_13d if order_large==2
replace f9_14d=f91_14d if order_large==2
replace f9_15d=f91_15d  if order_large==2
replace f9_16d=f91_16d if order_large==2

*Game results
replace f9b=f91b  if order_large==2 
replace f9r=f91r  if order_large==2
replace f9ba=f91ba if order_large==2 
replace f9ra=f91ra  if order_large==2
replace f9t=f91t  if order_large==2
replace f9rq=f91rq  if order_large==2
replace f9e1=f91e1  if order_large==2
replace f9e2=f91e2 if order_large==2



		
*********************************************
		
forval i=1/10{
gen round`i'=`i'
}

sort key
gen id=_n

reshape long round, i( id) j(game)

*Extract the round order
gen order1= regexs(1) if regexm( section_order , "(^[0-9]+) ")
gen order2= regexs(2) if regexm( section_order , "([0-9]+) ([0-9]+) ")
gen order3= regexs(3) if regexm( section_order , "([0-9]+) ([0-9]+) ([0-9]+) ")
gen order4= regexs(4) if regexm( section_order , "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)$")

destring order*, replace

*Keep only non test game
gen round_temp=round if round>2
replace round_temp=round_temp-2
replace round_temp=round_temp-4 if round_temp>4

* Define order regardless market size order
gen order=.
forval i=1/4{
replace order=`i' if order`i'==round_temp
}
*Replace order given the market size order
replace order=order+4 if order_large==1 & round==7
replace order=order+4 if order_large==1 & round==8
replace order=order+4 if order_large==1 & round==9
replace order=order+4 if order_large==1 & round==10
replace order=order+4 if order_large==2 & round==3
replace order=order+4 if order_large==2 & round==4
replace order=order+4 if order_large==2 & round==5
replace order=order+4 if order_large==2 & round==6

*Drop temporary variable
drop round_temp  order1 order2 order3 order4

lab var order "Round Order"

*Generate variables which capture the decision and the color at each draw
forval i=1/30{
gen draw`i'=.
gen color`i'=.
}

** TRAINING GAME PART
forval i=1/16{
replace draw`i'=e11_`i'd if round==1
replace color`i'=e11_`i'c if round==1
}

forval i=1/30{
replace draw`i'=e23_`i'd if round==2
replace color`i'=e23_`i'c if round==2
}

** SMALL GAME PART
*Decision and chip's color at the nth draw for the first game
forval i=1/16{
replace draw`i'=f3_`i'd if round==3
replace color`i'=f3_`i'c if round==3
}

* Decision at the nth draw for the second game
forval i=1/16{
replace draw`i'=f5_`i'd if round==4
replace color`i'=f5_`i'c if round==4
}

* Decision at the nth draw for the third game
forval i=1/16{
replace draw`i'=f7_`i'd if round==5
replace color`i'=f7_`i'c if round==5
}

* Decision at the nth draw for the fourth game
forval i=1/16{
replace draw`i'=f9_`i'd if round==6
replace color`i'=f9_`i'c if round==6
}


** LARGE GAME PART
* Decision at the nth draw for the first game

forval i=1/30{
replace draw`i'=f12_`i'd if round==7
replace color`i'=f12_`i'c if round==7
}

* Decision at the nth draw for the second game
forval i=1/30{
replace draw`i'=f14_`i'd if round==8
replace color`i'=f14_`i'c if round==8
}

* Decision at the nth draw for the third game
forval i=1/30{
replace draw`i'=f16_`i'd if round==9
replace color`i'=f16_`i'c if round==9
}

* Decision at the nth draw for the fourth game
forval i=1/30{
replace draw`i'=f18_`i'd if round==10
replace color`i'=f18_`i'c if round==10
}

************************************************
*Generate a single variable to measure whether the trader bought a cup
gen cup=.
replace cup=e9 if round==1
replace cup=e21 if round==2
replace cup=f2 if round==3
replace cup=f4 if round==4
replace cup=f6 if round==5
replace cup=f8 if round==6
replace cup=f11 if round==7
replace cup=f13 if round==8
replace cup=f15 if round==9
replace cup=f17 if round==10
recode cup(2=0)


*Number of blue and red chips drawn and accepted 
foreach x in blue red{
gen `x'_drawn=.
gen `x'_accepted=.
}

*Number of blue chips drawn in each game
replace blue_drawn=e11b if round==1
replace blue_drawn=e23b if round==2
replace blue_drawn=f3b if round==3
replace blue_drawn=f5b if round==4
replace blue_drawn=f7b if round==5
replace blue_drawn=f9b if round==6
replace blue_drawn=f12b if round==7
replace blue_drawn=f14b if round==8
replace blue_drawn=f16b if round==9
replace blue_drawn=f18b if round==10

*Number of blue chips accepted in each game
replace blue_accepted=e11ba if round==1
replace blue_accepted=e23ba if round==2
replace blue_accepted=f3ba if round==3
replace blue_accepted=f5ba if round==4
replace blue_accepted=f7ba if round==5
replace blue_accepted=f9ba if round==6
replace blue_accepted=f12ba if round==7
replace blue_accepted=f14ba if round==8
replace blue_accepted=f16ba if round==9
replace blue_accepted=f18ba if round==10


*Number of red chips drawn in each game
replace red_drawn=e11r if round==1
replace red_drawn=e23r if round==2
replace red_drawn=f3r if round==3
replace red_drawn=f5r if round==4
replace red_drawn=f7r if round==5
replace red_drawn=f9r if round==6
replace red_drawn=f12r if round==7
replace red_drawn=f14r if round==8
replace red_drawn=f16r if round==9
replace red_drawn=f18r if round==10

*Number of red chips accepted in each game
replace red_accepted=e11ra if round==1
replace red_accepted=e23ra if round==2
replace red_accepted=f3ra if round==3
replace red_accepted=f5ra if round==4
replace red_accepted=f7ra if round==5
replace red_accepted=f9ra if round==6
replace red_accepted=f12ra if round==7
replace red_accepted=f14ra if round==8
replace red_accepted=f16ra if round==9
replace red_accepted=f18ra if round==10

*Number of high-quality red chips accepted
gen red_hq=.
replace red_hq=e11rq if round==1
replace red_hq=e23rq if round==2
replace red_hq=f3rq if round==3
replace red_hq=f5rq if round==4
replace red_hq=f7rq if round==5
replace red_hq=f9rq if round==6
replace red_hq=f12rq if round==7
replace red_hq=f14rq if round==8
replace red_hq=f16rq if round==9
replace red_hq=f18rq if round==10


** Earnings per game
gen earnings=(red_accepted-red_hq)*20+red_hq*40+blue_accepted*20 if cup==0
replace earnings=(red_accepted-red_hq)*20+red_hq*40+blue_accepted*60-330 if cup==1


*Drop variables we already have or automatically generated by the tablet

forval i=1(1)9{
drop f`i'*
}

drop calc* sens* closing* devicephonenum duration e1* e2* e9 end* /// 
in* me_c10a_me_c10f me_c16anote me_c23a_me_c23f me_c9a_me_c9f module_0notes ///
next* num_sections random* scaled* section* simid start* subscriberid ///

*Label
lab var order_large "Market size order: 1=Small Market first"



*recode binary variables from No=2 to No=0

label define dummy 0 "No" 1 "Yes"
label define gender 0 "Female" 1 "Male"

global dummy agreement covidfever covidcough covidbreath covidsmell covidclose covidworried covidpositive gender educ_formal ///
 association loan premium premium_trader quality_buy color impurity grainsize moisture extraction qual_other quality_day /// 
 quality_transac quality_know quality_unknow  agreement_supplier separate agreement_buyer agreement_bpayment ///
 agreement_bprice agreement_bmeasure soldmiller quality_miller impurity_mill moisture_mill extraction_mill color_mill grainsize_mill qual_oth_mill ///

 foreach x in $dummy{
 recode `x' (2=0)
 label values `x' dummy
}

 label values gender gender

****************************
*GAME PART
****************************
*Recode Draw and Color
label define color 0 "Red" 1 "Blue"
label define accept 0 "Reject" 1 "Buy"

forval i=1/30{
recode color`i' (2=0)
label values color`i' color

recode draw`i' (2=0)
label values draw`i' accept

}


*Generate Game Test
gen gametest=1 if round<3
recode gametest (.=0)

label define gametest 0 "No Test" 1 "Test"
label values gametest gametest

*Generate Market Size
gen marketsize=1 if round>6 | round==2
recode marketsize (.=0)

label define marketsize 0 "Small" 1 "Large"
label values marketsize marketsize
label var marketsize "1=Large"


*Generate Market Setting
gen marketset=0 if round==1 | round==3 | round==7
replace  marketset=1 if  round==4 | round==8
replace  marketset=2 if round==2 | round==5 | round==9
replace  marketset=3 if  round==6 | round==10

label define marketset 0 "Game 1" 1 "Game 2" 2 "Game 3" 3 "Game 4", replace
label values marketset marketset

*Number of cups bought 
bys id: egen cup_sum=sum(cup) if gametest==0
bys id: egen cup_sumtest=sum(cup) if gametest==1
bys id: egen cup_sums=sum(cup) if gametest==0 & marketsize==0 
bys id: egen cup_suml=sum(cup) if gametest==0 & marketsize==1


gen red_lq=red_accepted-red_hq
egen accepted=rowtotal(draw*)
egen total_drawn= anycount(draw*), value(0 1)

replace blue_drawn=blue_accepted if blue_drawn<blue_accepted

label variable red_drawn "Number of red chips drawn"
label variable red_accepted "Number of red chips accepted"
label variable red_hq "Number of high quality red chips bought"
label variable red_lq "Number of low quality red chips bought"
label variable blue_accepted "Number of blue chips accepted"
label variable accepted "Number of chips accepted"
label variable total_drawn "Number of chips drawn"
label variable cup_sumtest "Number of cups bought: test" 
label variable cup_sum "Number of cups bought: game"
label variable cup_sums "Number of cups bought: small game"
label variable cup_suml "Number of cups bought: large game"



***********************************************************
*Error in full time workers => 100 should be 10
recode worker_full (100=10)

*Error in family_trade 50,000=5
recode family_trade (50000=5)

*Recode price average
recode price_avg (-99=.)
recode price_avg (200=2000)
recode price_avg (32=3200)

*Typo in number of wheat markets the trader actively sell wheat
recode nbr_sell_market (30=3)
recode nbr_sell_market (50=5)

*Age Error
replace age=68 if trader_name=="Tsehay alemayehu"

*Formatting
recode quality_trust (99=-99)
lab def quality_trust -1 "never trust"
lab val quality_trust quality_trust
recode quality_trust (-99=-1)


*Error
replace agreement_price=150 if agreement_price==250

*Typo
recode traders_knoww(1000=100)
recode traders_knowo (400=40)
recode nbr_trader (500=50)
recode family_trade (800000=8)

replace loan=0 if loanbirr==0
recode loanbirr (0=.)
recode price_avg (0=.)
recode premium_pay (5=50)
recode nbr_sell_market (85=8)


*Translation
replace bean_buyerothersp="Ethiopian Seed Entreprise" if bean_buyerothersp=="Mirtizer  dirigit"

*Free text 
replace qual_othersp="Insect" if qual_othersp=="Insect attacked grains"
replace qual_othersp="Insect" if qual_othersp=="Insect attacked grains"
replace qual_othersp="disease" if qual_othersp=="DESEASED"
replace qual_othersp="disease" if qual_othersp=="If Deseased like wag"
replace qual_othersp="variety" if qual_othersp=="Variety HIDASSE AND DENDEA ARE MORE PREFERED ONES"

replace bean_soldothsp="Quality Seed Ent" if bean_soldothsp=="Quality seed interprize"



*******************************
	
*BEHAVIORAL VARIABLE CODING
	
*LOCUS OF CONTROL (Following Abay et al. 2017 and Lajaaj and Macours, 2021)
*Positive: loc3 loc5 loc7 loc8
*Reversed: loc1 loc2  loc4 loc6 loc9 loc10

gen loc_intern=loc3+loc5+loc7+loc8
vreverse loc_intern, gen(loc_intern1)
drop loc_intern
rename loc_intern1 loc_intern

gen loc_extern=loc1+loc2+loc4+loc6+loc9+loc10

gen loc=loc_extern+loc_intern
zscore loc
label variable z_loc "trader's locus of control: increasing with internality"
drop loc loc_intern loc_extern


	
*ATTITUDE TOWARDS CHANGE (Lajaaj and Macours, 2021) entrepreneurship1 entrepreneurship2 entrepreneurship3 entrepreneurship4 entrepreneurship5
vreverse entrepreneurship5, gen(entrepreneurship5_rev)	
swindex  entrepreneurship1 entrepreneurship2 entrepreneurship3 entrepreneurship4 entrepreneurship5, g(attitude) flip(entrepreneurship5) displayw	
lab variable attitude "Attitude towards change: increasing with favorable"


*ASPIRATION (La Ferrara 2019  Presidential Address: Aspirations, Social Norms, and Development )
*Banerjee A. , La Ferrara E. , Orozco V. (2019 ). "The Entertaining Way to Behavioral Change: Fighting HIV with MTV ." NBER Working Paper No. 26096  The individual questions are then aggregated into indexes following Kling et al. (2007), i.e., we construct equally weighted averages of the z-scores of the variables that enter each index. 
*entrepreneurship6 entrepreneurship7
swindex entrepreneurship6 entrepreneurship7, gen(aspirations) flip(entrepreneurship6 entrepreneurship7)
lab variable aspiration "Aspiration: increasing with higher aspirations"


*RISK AVERSION (Bernard et al. 2015)
gen CPRA=5.1 if risk_av==1
replace CPRA=2.94 if risk_av==2
replace CPRA=1.34 if risk_av==3
replace CPRA=0.91 if risk_av==4
replace CPRA=0.39 if risk_av==5
replace CPRA=0 if risk_av==6

rename risk_av risk


gen risk_av=1 if CPRA>0.9
recode risk_av (.=0)

label define risk 1 "Severe" 2 "Intermediate" 3 "Moderate" 4 "Slight-to-neutral" 5 "Neutral-to-preferred" 6 "Prefered"
label values CPRA risk
label values risk risk
label var risk "CPRA categorical form"
label var CPRA "Constant Partial Risk Aversion"
label var risk_av "=1 if Risk Averse"


*LOSS AVERSION (COLE ET AL. 2013) Delta Expected gains/Delta Risk    with risk=sd
gen loss_aversion=1 if loss_av==1
replace loss_aversion=0.54 if loss_av==2
replace loss_aversion=0.49 if loss_av==3
replace loss_aversion=0.45 if loss_av==4
replace loss_aversion=0.42 if loss_av==5

drop loss_av
gen loss_av=1 if loss_aversion==1
recode loss_av (.=0)

label variable loss_aversion "Loss Aversion"
label variable loss_av "=1 if Loss Averse" 



*Variable definition
label define marketsize 0 "Small Market" 1 "Large Market", replace
	
	
*Variables for expectaion 
gen cup_exp=1 if inlist(game, 5,8,9)
recode cup_exp (.=0)
lab var cup_exp "Expected fixed cost decisions"
lab define cup_exp 0"Not Pay" 1"Pay", replace
lab values cup_exp cup_exp

*Generate variables for alignement with fixed cost
gen cup_gap=1 if cup_exp==cup
recode cup_gap (.=0)
lab define cup_gap 0"Unoptimal" 1"Optimal", replace
lab values cup_gap cup_gap
lab var cup_gap "Alignement between expected and realized fixed cost"


* Gen variable for choice at first round
gen first=1 if cup==0 & inlist(round,3)
bys id: egen first_m=max(first)
replace first_m=0 if first_m==.
lab var first_m "First round choice"


save "$clean/data_lfe.dta", replace


******
*IDENTIFYING PRESENT BIASED INDIVIDUAL
*:CAUTION: THIS TAKES A LONG TIME TO RUN (approximately 45 min on HP ELITEBOOK i7)

sort key
gen id=_n
keep id c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

forval i = 1(1)10{
gen a`i'=c`i'
drop c`i'
rename a`i' c`i'
}


*** EXPAND DATASET TO 1 OBSERVATION PER BUDGETxINDIVIDUAL ***
reshape long c, i(id) j(budget_number) string
drop budget_number
bys id: gen budget_number=_n

merge m:1 budget_number using "$raw\instrument_details.dta"


*** CREATE CHOICESET VARIABLES ***
gen t0 = .
replace t0 = 1 if sooner_date == 0
replace t0 = 0 if sooner_date != 0 & sooner_date != .

gen k = .
replace k = delay_weeks*7

gen pratio = endowment_later/endowment_soon

*** DEFINE VALUES FOR EACH OPTION ***
forvalues i = 1(1)2 {
	gen soon_`i' = 250/pratio - (`i'-1)*(250/pratio)/(2)
	
}
gen soon_3 = 0

forvalues i = 1(1)3 {
	gen late_`i' = (`i'-1)*125
}

*** ASSOCIATE CHOICE OPTIONS WITH VALUES ***
gen sooner_choice = .
forvalues i = 1(1)3 {
	replace sooner_choice = soon_`i' if c == `i'
}

*** USE NON-LINEAR LEAST SQUARES TO ESTIMATE AGGREGATE PARAMETERS OF BETA-DELTA, CRRA, TIME-SEPARABLE UTILITY ***
nl (sooner_choice = endowment_later*(((({b}^t0)*({d}^k)*pratio )^(1/({a}-1)))/(1 + pratio*(({b}^t0)*({d}^k)*pratio)^(1/({a}-1))))), ///
	initial(a 0.90 b 1 d 0.999) vce(cluster id)
gen alpha = _b[/a]
gen beta = _b[/b]
gen delta = _b[/d]

*** TRANSFORM DISCOUNT FACTOR TO ANNUAL DISCOUNT RATE ***
gen rate = delta^(-365) - 1

*** USE NON-LINEAR LEAST SQUARES TO ESTIMATE INDIVIDUAL PARAMETERS OF BETA-DELTA, CRRA, TIME-SEPARABLE UTILITY (MAY TAKE SOME TIME) ***
gen alpha_ind = .
gen beta_ind = .
gen delta_ind = .
sum id
forvalues i = 1(1)`r(max)' {
	cap nl (sooner_choice = endowment_later*(((({b}^t0)*({d}^k)*pratio )^(1/({a}-1)))/(1 + pratio*(({b}^t0)*({d}^k)*pratio)^(1/({a}-1))))) ///
	if id == `i', initial(a 0.90 b 1 d 0.999) 
	cap replace alpha_ind = _b[/a] if id == `i'
	cap replace beta_ind = _b[/b] if id == `i'
	cap replace delta_ind = _b[/d] if id == `i'
}

*** TRANSFORM DISCOUNT FACTOR TO ANNUAL DISCOUNT RATE ***
gen rate_ind = delta_ind^(-365) - 1


*IDENTIFY PRESENT BIASED INDIVIDUAL
gen present_biased=1 if beta_ind<1
recode present_biased (.=0)
keep if budget_number==1
keep id present_biased alpha_ind beta_ind delta_ind rate_ind

merge 1:m id using "$clean/data_lfe.dta"
drop _merge

lab var present_biased "=1 if Present Biased"
order present_biased, last
drop tp*

save "$clean\data_lfe.dta", replace

*********************************************************





