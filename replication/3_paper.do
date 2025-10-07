********************************************************************************
*Title:			Buyers' Response to Third Party Quality Certification: Theory and Evidence from Ethiopian Wheat Traders
*Survey:		Experiment		
*Purpose:		Analysis available in the paper
*Author:		Jeremy DO NASCIMENTO MIGUEL - email: jeremy.dnmiguel@gmail.com
*Date: 			August 2023
**********
********************************************************************************
*Basic Summary Statistics Table

use "$clean/data_lfe.dta", clear
	

**# TABLE 1
local sumstats=1
local demographic age gender educ_formal traderwheat_years 
local trader association trucks grain_storage family_trade traders_knoww nbr_sell_market volume_bought price_avg 
local quality premium premium_pay quality_buy color impurity grainsize moisture extraction separate
local behavioral risk_av loss_av present_biased

label var age "Age"
label var gender "Male (0/1)"
label var educ_formal "Formal Education (0/1)"
label var traderwheat_years "Wheat Trading Exp. (years)"
label var association "Traders Association (0/1)"
label var trucks "Number of Trucks"
label var grain_storage "Number of Grain Storage"
label var family_trade "Family Members in Agr Trade"
label var traders_knoww "Traders Known in the Woreda "
label var volume_bought "Volume Bought last season (Qt)"
label var volume_min "Min. volume bought on market day (Qt)"
label var volume_avg "Avg. volume bought on market day (Qt)"
label var volume_max "Max. volume bought on market day (Qt)"


label var price_avg "Average Selling Price (Birr/Qt)"
label var nbr_sell_market "Number of Selling Markets"
label var premium "Pay a Premium (0/1)"
label var premium_pay "Price Premium (Birr/Qt)"
label var quality_buy "Assess Quality (0/1)"
label var color "Color (0/1)"
label var impurity "Impurity (0/1)"
label var grainsize "Grain Size (0/1)"
label var moisture "Moisture (0/1)"
label var extraction "Extraction Rate (0/1)"
label var separate "Separate Quality (0/1)"
label var risk_av "Risk Aversion (0/1)"
label var loss_av "Loss Aversion (0/1)"
*label var present_biased "Present Biased (0/1)"


preserve
bys id: gen n=_n
keep if n==1
	if `sumstats'==1 {
	
	estimates clear

		matrix A = (.,.,.)
		matrix list A
		matrix rownames A = ""
		matrix colnames A = "Mean" "SD" "N" 
		matrix list A
		
		matrix B = (.,.,.)
		matrix rownames B = "\textbf{Panel A. Trader characteritistics}"
		matrix colnames B = "Mean" "SD" "N"
		matrix A = A\B

		foreach y in `demographic' {
			sum `y' 
			local mean :  display %-09.3f `r(mean)'
			local varlab : var lab `y'
			local sd = string(r(sd), "%09.2f")
			local n = string(r(N), "%9.0f")
			matrix B = (`mean',`sd',`n')
			matrix rownames B = "`varlab'"
			matrix colnames B = "Mean" "SD" "N"
			matrix A = A\B
			matrix list A
		}
		
		matrix B = (.,.,.)
		matrix rownames B = "\textbf{Panel B. Trading characteristics}"
		matrix colnames B = "Mean" "SD" "N"
		matrix A = A\B
		
		foreach y in `trader' {
			sum `y' 
			local mean = string(r(mean), "%09.2f")
			local varlab : var lab `y'
			local sd = string(r(sd), "%09.2f")
			local n = string(r(N), "%9.0f")	
			matrix B = (`mean',`sd',`n')
			matrix rownames B = "`varlab'"
			matrix colnames B = "Mean" "SD" "N"
			matrix A = A\B
			matrix list A			
		}
		
		matrix B = (.,.,.)
		matrix rownames B = "\textbf{Panel C. Quality Practices}"
		matrix colnames B = "Mean" "SD" "N"
		matrix A = A\B
		
		foreach y in `quality' {
			sum `y' 
			local mean = string(r(mean), "%9.2f")
			local varlab : var lab `y'
			local sd = string(r(sd), "%9.2f")
			local n = string(r(N), "%9.0f")	
			matrix B = (`mean',`sd',`n')
			matrix rownames B = "`varlab'"
			matrix colnames B = "Mean" "SD" "N"
			matrix A = A\B
			matrix list A			
		}
	
		matrix B = (.,.,.)
		matrix rownames B = "\textbf{Panel D. Behavioral Attributes}"
		matrix colnames B = "Mean" "SD" "N"
		matrix A = A\B
		
		foreach y in `behavioral' {
			sum `y' 
			local mean = string(r(mean), "%9.2f")
			local varlab : var lab `y'
			local sd = string(r(sd), "%9.2f")
			local n = string(r(N), "%9.0f")	
			matrix B = (`mean',`sd',`n')
			matrix rownames B = "`varlab'"
			matrix colnames B = "Mean" "SD" "N"
			matrix A = A\B
			matrix list A			
		}
		
	
	matdelrc A , r(1)
	matdelrc A , r(1)

	# delimit ;				
	esttab matrix(A) using "$table/table_1.tex",
		replace br se label obslast  b(%09.2f) se(%09.2f) width(0.8\hsize)
		booktabs nonumber star(* 0.10 ** 0.05 *** 0.01) 
		nomtitles nolegend nonotes;
	# delimit cr	
	
		
	}
restore	
	


	
	

*generate parameter value


gen pi=1 if marketset==0
replace pi=2 if marketset==1
replace pi=3 if marketset==2 | marketset==3


gen theta=1 if marketset==0 | marketset==1 | marketset==2
replace theta=2 if marketset==3
	



eststo clear
label define marketset 0 "Game 1" 1 "Medium \pi" 2 "High \pi" 3 "High \pi $\times$ High \theta", replace
label values marketset marketset

label define pi 2 " Medium \pi" 3 "High \pi"
label values pi pi 

label define theta 2 "High \theta"
label values theta theta
label var theta "High \theta"

label define marketsize 1 "Large market", replace
label values marketsize marketsize


	
	
*FIGURE: 
preserve 
clear

set obs  1000000
gen a =	220
gen b =	20
gen PC = 280
gen pc = 220
gen pu = 200
gen F =	330
gen Z =	12
gen T =	30
gen zt=Z/T

// Generate a sequence of numbers from 0 to 10,000
gen seq = _n 

// Generate pi by dividing the sequence by 10,000
gen pi = seq / 1000000

// Drop the intermediate sequence variable
gen theta=(PC - pc - a + pu - F/(T * pi))*(1/b) if pi<zt
replace theta=(PC - pc - a + pu - (F / Z))*(1/b) if pi>zt
replace theta=1 + (Z*(PC-a-b)-F)/(b*(1-pi)*T)-(pc-pu)/b if pi>1-zt


replace theta=0 if pi>0.25 & pi<0.27


**#FIGURE 1 
twoway (mspline theta pi  if (theta>=0 & theta<=1) & (pi>0.253 ),  $plotregion $graphregion $scheme $ylabel ytitle("{&theta}",  orient(horizontal)) xtitle("{&pi}") ylabel(0(0.1)1) xlabel(0 0.1 0.2 0.3 0.4 "Z/T" 0.5 0.6 "1-Z/T" 0.7 0.8 0.9)      xline(0.6 0.4) ///
text(0.7 0.2 "No fixed cost and mix quality" 0.2 0.6 "Fixed cost and separate quality" -0.02 0.26 "A" 0.65 0.407 "B" 0.65 0.594 "C"  1 0.75 "D" 0.03 0.409 "E" 0.03 0.609 "F" 1 0.609 "G"))
 graph export "$graph/figure1.png", replace width(1200) 

clear

set obs  10000


gen a =	220
gen b =	20
gen PC = 280
gen pc = 220
gen pu = 200
gen F =	330
gen Z =	12
gen Tsmall=	16
gen Tlarge= 30

// Generate a sequence of numbers from 0 to 10,000
gen seq = _n 

// Generate pi by dividing the sequence by 10,000
gen pi = seq / 10000

// Drop the intermediate sequence variable

gen theta=(PC-pc-a+pu-F/(pi*Tlarge))/b if pi<=1-Z/Tsmall

replace theta=1+(pi*Tsmall*(PC-a-b)-F)/(b*(Tsmall-Z))-(pc-pu)/b if pi>=1-Z/Tsmall & pi<=Z/Tsmall

replace theta=1+(Z*(PC-a-b)-F)/(b*(1-pi)*Tsmall)-(pc-pu)/b if pi>=Z/Tsmall &pi<1

replace theta=. if theta<=0 | theta>=1



**#FIGURE 2:
twoway mspline theta pi if theta>=0 & theta<=1 , $plotregion $graphregion $scheme $ylabel ytitle("{&theta}",  orient(horizontal)) xtitle("{&pi}") ylabel(0(0.1)1) xlabel(0 0.1 0.2 0.25"1-Z/T" 0.3 0.4  0.5 0.6 0.7 0.75 "Z/T"  0.8 0.9)   xline(0.25 0.75) ///
   text(0.5 0.4 "No fixed cost and mix quality" 0.1 0.7 "Fixed cost and separate quality", size(small))
 graph export "$graph/figure2.png", replace width(1200) 

restore


	
**# Figure 4

eststo a3: reghdfe cup i.marketset##i.marketsize   if gametest==0, vce(robust) ab(id order)
matrix res1=r(table)


*Beta small smarket: 1st row = coeff ; 2nd=pval
mat res_sm = J(2,4,.)
mat res_sm[1,1] = res1[1,1]
mat res_sm[1,2] = res1[1,2]
mat res_sm[1,3] = res1[1,3]
mat res_sm[1,4] = res1[1,4]
mat res_sm[2,1] = 1
mat res_sm[2,2] = res1[4,3]
mat res_sm[2,3] = res1[4,4]
mat res_sm[2,4] = res1[4,4]

*Beta large market
mat res_lm = J(2,4,.)

lincom 0.marketset+1.marketsize+0.marketset#1.marketsize
mat res_lm[1,1] = r(estimate)
mat res_lm[2,1] = r(p)


lincom 1.marketset+1.marketsize+1.marketset#1.marketsize
mat res_lm[1,2] = r(estimate)
mat res_lm[2,2] = r(p)


lincom 2.marketset+1.marketsize+2.marketset#1.marketsize
mat res_lm[1,3] = r(estimate)
mat res_lm[2,3] = r(p)

lincom 3.marketset+1.marketsize+3.marketset#1.marketsize
mat res_lm[1,4] = r(estimate)
mat res_lm[2,4] = r(p)

*Confidence interval: small market
mat ci_sm = J(2,4,.)
**Lower 
mat ci_sm[1,1] = res1[5,1]
mat ci_sm[1,2] = res1[5,2]
mat ci_sm[1,3] = res1[5,3]
mat ci_sm[1,4] = res1[5,4]
		
** Upper
mat ci_sm[2,1] = res1[6,1]
mat ci_sm[2,2] = res1[6,2]
mat ci_sm[2,3] = res1[6,3]
mat ci_sm[2,4] = res1[6,4]


*Confidence interval: large market
mat ci_lm = J(2,4,.)

lincom 0.marketset+1.marketsize+0.marketset#1.marketsize
mat ci_lm[1,1] = r(lb)
mat ci_lm[2,1] = r(ub)

lincom 1.marketset+1.marketsize+1.marketset#1.marketsize
mat ci_lm[1,2] = r(lb)
mat ci_lm[2,2] = r(ub)

lincom 2.marketset+1.marketsize+2.marketset#1.marketsize
mat ci_lm[1,3] = r(lb)
mat ci_lm[2,3] = r(ub)

lincom 3.marketset+1.marketsize+3.marketset#1.marketsize
mat ci_lm[1,4] = r(lb)
mat ci_lm[2,4] = r(ub)	


coefplot (matrix(res_sm), aux(2) ci(ci_sm) label(Small Market)) ///
(matrix(res_lm) , aux(2) ci( ci_lm) label(Large Market)), ///
  vert $graphregion $ylabel $plotregion $scheme levels(95) ylabel(-0.05(0.05)0.5) yline(0) yline(-.089, lpattern(solid)) ///
 ytitle("Point estimate") legend(pos(6) row(1)) grid(between) /// 
 coeflabels(c1 ="T1 ({&pi} 0.1 - {&theta}: 0.3)" c2 ="T2 ({&pi} 0.3 - {&theta}: 0.3)  " c3="T3 ({&pi} 0.5 - {&theta}: 0.3)" c4="T4 ({&pi} 0.5 - {&theta}: 0.8)" ) ///
  text(0.5 1.1 "T1 small market mean = `ymean'" ///
  0.1 0.85 "{&beta}1" 0.1 1.15 "{&delta}1" ///
  0.35 1.85 "{&beta}2" 0.35 2.15 "{&beta}2+{&delta}2" ///
  0.5 2.85 "{&beta}3" 0.5 3.15 "{&beta}3+{&delta}3" ///
  0.52 3.85 "{&beta}4" 0.52 4.15 "{&beta}4+{&delta}4" ///
    , placement(c) size(small)) ///
mlabpos(2) mlabgap(*1) mlabel(cond(@aux1<.01, "***", ///
            cond(@aux1<.05, "**",   ///
            cond(@aux1<.1, "*",    ///
            cond(@aux1<.1, "+", ""))))) 
graph export "$graph/figure4.png", replace width(1600)


tempfile cup 
save `cup', replace
			
****************			
** Round decisions analysis 
label define cup 0"No fixed cost"  1"Fixed cost"
lab val cup cup
*To Analyze the decision at each round: need to reshape to have one variable that capture decision at each draw. A row is now equal to a draw.
g id1=_n
drop color color_mill
reshape long color@ draw@, i(id1) j(pick)
*drop id1
drop pi theta 

*Generate parameters
gen pi=0.1 if marketset==0
replace pi=0.3 if marketset==1
replace pi=0.5 if marketset==2 | marketset==3


gen T=30 if marketsize==1 
replace T=16 if marketsize==0

*Truck capacity
gen Z=12

**Expected Threshold Farmer
	*After paying the fixed cost
gen N_exp=(T-Z)/(1-pi)
gen N_real=(total_drawn-Z)/(1-pi) if cup==1
replace N_real=1 if N_real==0

	*Without paying the fixed cost
gen M_exp=(T-Z)/pi
gen M_real=(total_drawn-Z)/pi if cup==0
replace M_real=1 if M_real==0




*Generate a dummy = 1 after meeting the N farmer
gen turning_exp=0 if pick<N_exp & cup==1
replace turning_exp=0 if pick<M_exp & cup==0
recode turning_exp (.=1)

label define turning_exp 0"Before farmer {it:N}"  1"After farmer {it:N}"
lab val  turning_exp turning_exp

*Average acceptance 

 egen avg_accept=mean(draw) if draw!=., by(id1 turning_exp color )
 
tempfile round 
save `round', replace


**# FIGURE 5: Propotion of traders following the optimal decision

preserve
keep if gametest==0
collapse avg_accept   , by(id1 color cup game turning_exp)
drop if avg_accept==.

bys game: gen optimal_cup=1 if avg_accept==1 & cup==1 & turning_exp==0 & color==1
bys game: gen optimal_nocup=1 if avg_accept==1 & cup==0 & turning_exp==0 & color==0
bys game: egen nbr_optimal_c=total(optimal_cup)
bys game: egen nbr_optimal_nc=total(optimal_nocup)
label define game 3 "Low {&pi} - Low{&theta}" 4 "Medium {&pi} - Low {&theta}" 5 "High {&pi} - Low {&theta}" 6 "High {&pi} - High {&theta}" ///
 7 "Low {&pi} - Low{&theta}" 8 "Medium {&pi} - Low {&theta}" 9 "High {&pi} - Low {&theta}" 10 "High {&pi} - High {&theta}", replace

label val game game


gen optimal_c=nbr_optimal_c/178
gen optimal_nc=nbr_optimal_nc/178

graph bar optimal_c optimal_nc if game<7,   over(game)  legend(order(1 "Fixed cost" 2 "No fixed cost") col(2)  pos(6)) bar(1, fcolor(blue)) bar(2, fcolor(red)) ///
 ytitle("% of traders following the optimal strategy") $scheme $plotregion $graphregion lintensity(*75) bargap(5)  ylabel(0(.1)1) $blabel
graph export "$graph/figure5_a.png", replace width(1200) 
 

graph bar optimal_c optimal_nc if game>6,  over(game)  legend(order(1 "Fixed cost" 2 "No fixed cost") col(2)  pos(6)) bar(1, fcolor(blue)) bar(2, fcolor(red)) ///
 ytitle("% of traders following the optimal strategy") $scheme $plotregion $graphregion lintensity(*75) bargap(5)  ylabel(0(.1)1) $blabel
graph export "$graph/figure5_b.png", replace width(1200) 
  
restore 



preserve
keep if gametest==0

gen reject=1 if draw==0
replace reject=0 if draw==1 

sort id1 pick
bys id1: egen rank_r=rank(pick) if reject==1

bys id1 (pick): gen n_left=sum(rank_r) if marketsize==0
bys id1 (pick): gen n_lefta=sum(rank_r) if marketsize==1


recode n_left (0=4) (1=3) (3=2) (6=1) (10=0) 
recode n_lefta (0=18) (1=17) (3=16) (6=15) (10=14) (15=13) (21=12) (28=11) (36=10) (45=9) (55=8) (66=7) (78=6) (91=5) (105=4) (120=3) (136=2) (153=1) (171=0) 
replace n_left=n_lefta if marketsize==1

collapse draw   , by(color n_left cup game )
**# FIGURE 6
collapse draw   , by(color n_left cup game )

*Game 1 and 2 are game test

twoway (  connected  draw n_left if game==3 &  cup==0 & color==1, xaxis(1 2) xlabel("" , axis(2)) xtitle("", axis(2))  /// 
 lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(  connected draw  n_left if game==3 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(  connected draw n_left  if game==3 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o))  /// 
(  connected draw  n_left if game==3 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) mcolor(red%70) msymbol(o) ///
xlab(0 "4" 1 "3" 2 "2" 3 "1" 4 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
 graph export "$graph/figure6_a.png", replace width(1200) 


twoway (connected  draw n_left if game==4 &  cup==0 & color==1,  xaxis(1 2)   xlabel("" , axis(2))  xtitle("", axis(2)) ///
 lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==4 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==4 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==4 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "4" 1 "3" 2 "2" 3 "1" 4 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure6_b.png", replace width(1200) 

twoway (connected  draw n_left if game==5 &  cup==0 & color==1,  xaxis(1 2)   xlabel("" , axis(2))  xtitle("", axis(2)) /// 
lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==5 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==5 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==5 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "4" 1 "3" 2 "2" 3 "1" 4 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure6_c.png", replace width(1200) 

twoway (connected  draw n_left if game==6 &  cup==0 & color==1,  xaxis(1 2)  xlabel("" , axis(2))  xtitle("", axis(2))  /// 
 lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==6 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==6 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==6 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "4" 1 "3" 2 "2" 3 "1" 4 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure6_d.png", replace width(1200) 


**# FIGURE C7

twoway (connected  draw n_left if game==7 &  cup==0 & color==1,  xaxis(1 2) xlabel("" , axis(2))   xtitle("", axis(2))  /// 
 lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==7 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==7 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==7 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "18" 1 "17" 2 "16" 3 "15" 4 "14" 5 "13" 6 "12" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" 13 "5" 14 "4" 15 "3" 16 "2" 17 "1" 18 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure_c7_a.png", replace width(1200)
 
twoway (connected  draw n_left if game==8 &  cup==0 & color==1,  xaxis(1 2) xlabel("" , axis(2))  xtitle("", axis(2))  /// 
  lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==8 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==8 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==8 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "18" 1 "17" 2 "16" 3 "15" 4 "14" 5 "13" 6 "12" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" 13 "5" 14 "4" 15 "3" 16 "2" 17 "1" 18 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure_c7_b.png", replace width(1200) 

twoway (connected  draw n_left if game==9 &  cup==0 & color==1,  xaxis(1 2) xlabel("" , axis(2)) lc(blue%70) xtitle("", axis(2)) lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==9 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==9 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==9 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "18" 1 "17" 2 "16" 3 "15" 4 "14" 5 "13" 6 "12" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" 13 "5" 14 "4" 15 "3" 16 "2" 17 "1" 18 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure_c7_c.png", replace width(1200) 

twoway (connected  draw n_left if game==10 &  cup==0 & color==1, xaxis(1 2) xlabel("" , axis(2)) xtitle("", axis(2)) lc(blue%70)  lp(dash) mcolor(blue%70) msymbol(S)) /// 
(connected  draw n_left if game==10 &  cup==0 & color==0, lc(red%70)  lp(dash) mcolor(red%70) msymbol(S))  /// 
(connected  draw n_left if game==10 &  cup==1 & color==1, lc(blue%70)  lp(solid) mcolor(blue%70) msymbol(o)) /// 
(connected  draw n_left if game==10 &  cup==1 & color==0,  lc(red%70)  lp(solid) mcolor(red%70) msymbol(o) ///
xlab(0 "18" 1 "17" 2 "16" 3 "15" 4 "14" 5 "13" 6 "12" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" 13 "5" 14 "4" 15 "3" 16 "2" 17 "1" 18 "0") xtitle("Number of rejections left") ylab(0(0.2)1) ytitle("% accepeted") $graphregion $plotregion $scheme legend(order(1 "No fixed cost - Certified output" 2 "No fixed cost - Uncertified output" 3 "Fixed cost - Certified output" ///
 4 "Fixed cost - Uncertified output") col(2) pos(6) ))
graph export "$graph/figure_c7_d.png", replace width(1200) 

restore 



**#FIGURE 7 

*Optimal decisions following theoretical predictions: coded such as optimal decision =0 and unoptimal=1
gen optimal=0 if draw==1 & color==1 & cup==1 
replace optimal=0 if draw==0 & color==0 & cup==1 & pick<N_exp
replace optimal=0 if draw==1 & color==0 & cup==1 & pick>=N_exp

replace optimal=0 if draw==1 & color==0 & cup==0 
replace optimal=0 if draw==0 & color==1 & cup==0 & pick<M_exp
replace optimal=0 if draw==1 & color==1 & cup==0 & pick>=M_exp

replace optimal=1 if draw==0 & color==1 & cup==1  & optimal==.
replace optimal=1 if draw==1 & color==0 & cup==1  & optimal==.
replace optimal=1 if draw==0 & color==0 & cup==1 & pick>=N_exp & optimal==.

replace optimal=1 if draw==1 & color==1 & cup==0  & optimal==.
replace optimal=1 if draw==0 & color==0 & cup==0  & optimal==.
replace optimal=1 if draw==0 & color==1 & cup==0 & pick>=M_exp & optimal==.


gen draw_opt=1 if cup==1 &  color==1
replace draw_opt=1 if cup==0 &  color==0
replace draw_opt=1 if cup==0 &  color==1
replace draw_opt=1 if cup==1 &  color==0
recode draw_opt (.=0)

replace draw_opt=1 if  (cup==1 & pick>N_exp) & pick!=.
replace draw_opt=1 if  (cup==0 & pick>M_exp) &  pick!=.


bys id1 : gen draw_opt_lag=optimal[_n-1]





*Small Market
matrix drop _all

eststo clear
forvalues i=2(1)5{
cap quietly eststo a`i': reghdfe optimal draw_opt_lag if cup==0 & marketsize==0 & pick==`i',  ab(id order  i.marketset) vce(rob)
matrix res`i'=r(table)

cap quietly eststo b`i': reghdfe optimal draw_opt_lag if cup==1 & marketsize==0 & pick==`i',  ab(id order  i.marketset) vce(rob)
matrix res_c`i'=r(table)
}


*Beta no cup: 1st row = coeff ; 2nd=pval
mat resu = J(2,5,.)
mat resu[1,1] = .
mat resu[1,2] = .

*Confidence interval
mat ci = J(2,5,.)
**Lower 
mat ci[1,1] = .
*Upper
mat ci[1,2] = .


forvalues i=2(1)5{
*Beta
mat resu[1,`i'] = res`i'[1,1]
*Pval
mat resu[2,`i'] = res`i'[4,1]
**Lower CI
mat ci[1,`i'] = res`i'[5,1]
*Upper
mat ci[2,`i'] = res`i'[6,1]
}

*****
*Beta cup: 1st row = coeff ; 2nd=pval
mat resu_c = J(2,5,.)
mat resu_c[1,1] = .
mat resu_c[1,2] = .

*Confidence interval
mat ci_c = J(2,5,.)
**Lower 
mat ci_c[1,1] = .
*Upper
mat ci_c[1,2] = .


forvalues i=2(1)5{
*Beta
mat resu_c[1,`i'] = res_c`i'[1,1]
*Pval
mat resu_c[2,`i'] = res_c`i'[4,1]
**Lower CI
mat ci_c[1,`i'] = res_c`i'[5,1]
*Upper
mat ci_c[2,`i'] = res_c`i'[6,1]
}



coefplot (matrix(resu), offset(-.25) aux(2) ci(ci) label(No Fixed Cost)) ///
(matrix(resu_c), aux(2) ci(ci_c) label(Fixed Cost)), ///
 vert $graphregion $ylabel $plotregion $scheme levels(95)  ylabel(-0.4(0.2)0.6)  ///
 ytitle("Point estimate") legend(order(2 "No Fixed Cost" 4 "Fixed cost") row(1) pos(6) region(style(none))) ///
  lwidth(*.8) xtitle("Draw") msymbol(s) ciopts(recast(rcap)) yline(0, lcolor(gs8))   xlabel(2(1)5)
 graph export "$graph/figure_7a.png", width(1200) replace

 
	** Large market
	matrix drop _all
	eststo clear
	forvalues i=2(1)19{
	cap quietly eststo a`i': reghdfe optimal draw_opt_lag if cup==0 & marketsize==1 & pick==`i',  ab(id order  i.marketset) vce(rob)
	matrix res`i'=r(table)

	cap quietly eststo b`i': reghdfe optimal draw_opt_lag if cup==1 & marketsize==1 & pick==`i',  ab(id order  i.marketset) vce(rob)
	matrix res_c`i'=r(table)
	}


	*Beta no cup: 1st row = coeff ; 2nd=pval
	mat resu = J(2,19,.)
	mat resu[1,1] = .
	mat resu[1,2] = .

	*Confidence interval
	mat ci = J(2,19,.)
	**Lower 
	mat ci[1,1] = .
	*Upper
	mat ci[1,2] = .


	forvalues i=2(1)19{
	*Beta
	mat resu[1,`i'] = res`i'[1,1]
	*Pval
	mat resu[2,`i'] = res`i'[4,1]
	**Lower CI
	mat ci[1,`i'] = res`i'[5,1]
	*Upper
	mat ci[2,`i'] = res`i'[6,1]
	}

	*****
	*Beta cup: 1st row = coeff ; 2nd=pval
	mat resu_c = J(2,19,.)
	mat resu_c[1,1] = .
	mat resu_c[1,2] = .

	*Confidence interval
	mat ci_c = J(2,19,.)
	**Lower 
	mat ci_c[1,1] = .
	*Upper
	mat ci_c[1,2] = .


	forvalues i=2(1)19{
	*Beta
	mat resu_c[1,`i'] = res_c`i'[1,1]
	*Pval
	mat resu_c[2,`i'] = res_c`i'[4,1]
	**Lower CI
	mat ci_c[1,`i'] = res_c`i'[5,1]
	*Upper
	mat ci_c[2,`i'] = res_c`i'[6,1]
	}


	

	coefplot (matrix(resu), offset(-.25) aux(2) ci(ci) label(No Fixed Cost)) ///
	(matrix(resu_c),  aux(2) ci(ci_c) label(Fixed Cost)), ///
	 vert $graphregion $ylabel $plotregion $scheme levels(95)  ylabel(-.6(0.2).8)  ///
	 ytitle("Point estimate") legend(order(2 "No Fixed Cost" 4 "Fixed cost") row(1) pos(6) region(style(none))) ///
	  lwidth(*.8) xtitle("Draw") msymbol(s) ciopts(recast(rcap)) yline(0, lcolor(gs8))   xlabel(2(1)19)
	 graph export "$graph/figure_7b.png", width(1200) replace

 









**#FIGURE 8: SEE R CODE 
	
	
	
	
	
******************************************
****************Appendix 
******************************************	
use `cup', clear



**#Figure C1: Traders' profits
twoway histogram earnings if  gametest==0, discrete width(20)  bfcolor(blue%60) blcolor(blue%60) $scheme  $graphregion $ylabel xtitle("Earnings (in Birr)") 
graph export "$graph/figure_c1.png", replace width(1200) 






**#Figure C2: Correlation between the order and decision to buy a cup
graph bar (mean) cup, over(order)  ytitle("% of traders paying a fixed cost") ylabel(0(0.2)1) b1title("Round") $graphregion $plotregion $ylabel $scheme $blabel
graph export "$graph/figure_c2.png", replace width(1200) 

pwcorr cup order, sig
pwcorr order total_drawn, sig


**# FIGURE C3
*Total Number of cups bought
hist cup_sum, discrete fraction xtitle("# of fixed cost investments per trader") ytitle("% of traders") width(1) ylabel(0(0.05)0.3) xlab(0(1)8) $plotregion $graphregion $ylabel $scheme
graph export "$graph/figure_c3.png", replace width(1200) 




**#FIGURE C4: correlation between optimal purchasing decision and number of times rules were explained
est clear

eststo: reghdfe cup_gap i.check_explain age gender residence traderwheat_years educ_years , ab(order round) vce(rob)
coefplot , keep(*check_explain)  vert $graphregion $ylabel $plotregion $scheme levels(95) ylabel(-0.5(0.05)0.2) yline(0) yline(-.5, lpattern(solid))  ytitle("Point estimate") legend(pos(6) row(1)) grid(between)  coeflabels(2.check_explain ="2 times" 3.check_explain= "3 times" 4.check_explain ="4 times") xtitle("Number of times rules were explain") legend(off) ///
mlabpos(2) mlabgap(*1) mlabel(cond(@pval<.01, "***", ///
            cond(@pval<.05, "**",   ///
            cond(@pval<.1, "*",    ///
            cond(@pval<.1, "+", ""))))) 
graph export "$graph/figure_c4.png", replace width(1600)


use `round', clear 

**# ROBUSTNESS: FIGURE C.5

sum cup if marketset==0 & gametest==0 & marketsize==0 & first_m==1
	local ymean = string(r(mean), "%9.2f") 
	local  ymean = `ymean'
	
eststo a4: reghdfe cup i.marketset##i.marketsize   if gametest==0 & first_m==1, vce(robust) ab(id order)
matrix res2=r(table)


*Beta small smarket: 1st row = coeff ; 2nd=pval
mat res_sm = J(2,4,.)
mat res_sm[1,1] = res2[1,1]
mat res_sm[1,2] = res2[1,2]
mat res_sm[1,3] = res2[1,3]
mat res_sm[1,4] = res2[1,4]
mat res_sm[2,1] = 1
mat res_sm[2,2] = res2[4,3]
mat res_sm[2,3] = res2[4,4]
mat res_sm[2,4] = res2[4,4]

*Beta large market
mat res_lm = J(2,4,.)

lincom 0.marketset+1.marketsize+0.marketset#1.marketsize
mat res_lm[1,1] = r(estimate)
mat res_lm[2,1] = r(p)


lincom 1.marketset+1.marketsize+1.marketset#1.marketsize
mat res_lm[1,2] = r(estimate)
mat res_lm[2,2] = r(p)


lincom 2.marketset+1.marketsize+2.marketset#1.marketsize
mat res_lm[1,3] = r(estimate)
mat res_lm[2,3] = r(p)

lincom 3.marketset+1.marketsize+3.marketset#1.marketsize
mat res_lm[1,4] = r(estimate)
mat res_lm[2,4] = r(p)

*Confidence interval: small market
mat ci_sm = J(2,4,.)
**Lower 
mat ci_sm[1,1] = res2[5,1]
mat ci_sm[1,2] = res2[5,2]
mat ci_sm[1,3] = res2[5,3]
mat ci_sm[1,4] = res2[5,4]
		
** Upper
mat ci_sm[2,1] = res2[6,1]
mat ci_sm[2,2] = res2[6,2]
mat ci_sm[2,3] = res2[6,3]
mat ci_sm[2,4] = res2[6,4]


*Confidence interval: large market
mat ci_lm = J(2,4,.)

lincom 0.marketset+1.marketsize+0.marketset#1.marketsize
mat ci_lm[1,1] = r(lb)
mat ci_lm[2,1] = r(ub)

lincom 1.marketset+1.marketsize+1.marketset#1.marketsize
mat ci_lm[1,2] = r(lb)
mat ci_lm[2,2] = r(ub)

lincom 2.marketset+1.marketsize+2.marketset#1.marketsize
mat ci_lm[1,3] = r(lb)
mat ci_lm[2,3] = r(ub)

lincom 3.marketset+1.marketsize+3.marketset#1.marketsize
mat ci_lm[1,4] = r(lb)
mat ci_lm[2,4] = r(ub)	


coefplot (matrix(res_sm), aux(2) ci(ci_sm) label(Small Market)) ///
(matrix(res_lm) , aux(2) ci( ci_lm) label(Large Market)), ///
  vert $graphregion $ylabel $plotregion $scheme levels(95) ylabel(-0.05(0.05)0.8) yline(0) yline(-.05, lpattern(solid)) ///
 ytitle("Point estimate") legend(pos(6) row(1)) grid(between) /// 
 coeflabels(c1 ="T1 ({&pi} 0.1 - {&theta}: 0.3)" c2 ="T2 ({&pi} 0.3 - {&theta}: 0.3)  " c3="T3 ({&pi} 0.5 - {&theta}: 0.3)" c4="T4 ({&pi} 0.5 - {&theta}: 0.8)" ) ///
  text(0.7 1.1 "T1 small market mean = `ymean'" ///
  0.2 0.85 "{&beta}1" 0.2 1.15 "{&delta}1" ///
  0.55 1.85 "{&beta}2" 0.55 2.15 "{&beta}2+{&delta}2" ///
  0.75 2.85 "{&beta}3" 0.75 3.15 "{&beta}3+{&delta}3" ///
  0.8 3.85 "{&beta}4" 0.8 4.15 "{&beta}4+{&delta}4" ///
    , placement(c) size(small)) ///
mlabpos(2) mlabgap(*1) mlabel(cond(@aux1<.01, "***", ///
            cond(@aux1<.05, "**",   ///
            cond(@aux1<.1, "*",    ///
            cond(@aux1<.1, "+", ""))))) 
graph export "$graph/figure_c5.png", replace width(1600)






*** Export data for machine learning 

*First: calculate the mean that switch too early and does not optimal follow fixed cost decision: 30% 
bys id1: egen rank_op=rank(pick) if optimal==0
preserve 
gen switch_e=1 if (rank_op==1 & n_left==4 & marketsize==0) | (rank_op==1 & n_left==18 & marketsize==1)
recode switch_e (.=0)
collapse (max) switch_e cup_gap, by(id game)
gen both_unop=1 if switch_e==1 & cup_gap==1
recode both_unop (.=0)

collapse (mean) both_unop, by(id)


sum both_unop
restore

*Export machine learning data
	preserve
	keep if rank_op==1 
	merge m:m id using "$clean/data_ML.dta", keepusing(present_biased)
	bys marketsize: sum n_left  // average first error at 3rd and 17th round

	zscore n_left if game<7
rename z_n_left z_nleft
	zscore n_left if game>6
replace z_n_left=z_nleft if game<7
drop n_left
rename z_n_left n_left
	keep id game region zone market age gender educ_formal traderwheat_years association trucks grain_storage worker_full traders_knoww traders_knowo volume_bought volume_sold volume_avg quality_buy extraction  soldmiller quality_miller treatment cup_sum z_loc entrepreneurship5_rev attitude aspirations  risk_av loss_av present_biased cup cup_gap gametest separate n_left
	keep if   gametest==0
	drop gametest 
	save "$clean\data_ML_switch.dta", replace
	restore



restore 
 


**#TABLE C1
use `cup', clear 
eststo clear
label define marketset 0 "T1" 1 "T2" 2 "T3" 3 "T4", replace
label values marketset marketset


sum cup if marketset==0 & gametest==0 & marketsize==0
	local ymean = string(r(mean), "%9.2f") 
	global  ymean1 = `ymean'

eststo a2: reghdfe cup i.marketset  if gametest==0 & marketsize==0, vce(robust) ab(id order)
	test 3.marketset=2.marketset
	local pv = string(r(p), "%9.2f")	
	global pv = `pv'
	estadd local pv = $pv
		
	
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"	


	test 1.marketset=2.marketset
	local p = string(r(p), "%9.2f")	
	global p = `p'
	estadd local p = $p

	
sum cup if marketset==0 & gametest==0 & marketsize==1
	local ymean = string(r(mean), "%9.2f") 
	global  ymean2 = `ymean'
		
	
eststo a3: reghdfe cup i.marketset  if gametest==0 & marketsize==1, vce(robust) ab(id order)
	test 3.marketset=2.marketset
	local pv = string(r(p), "%9.2f")	
	global pv = `pv'
	estadd local pv = $pv
		
	
	estadd local ymean = $ymean2	
	estadd local trader "Yes"
	estadd local order "Yes"	
	
	test 1.marketset=2.marketset
	local p = string(r(p), "%9.2f")	
	global p = `p'
	estadd local p = $p

	
eststo a4: reghdfe cup i.marketset##i.marketsize   if gametest==0, vce(robust) ab(id order)

	test 3.marketset=2.marketset
	local pv = string(r(p), "%9.2f")	
	global pv = `pv'
	estadd local pv = $pv
		
	
	estadd local ymean = $ymean2	
	estadd local trader "Yes"
	estadd local order "Yes"	
	
	test 1.marketset=2.marketset
	local p = string(r(p), "%9.2f")	
	global p = `p'
	estadd local p = $p

test 2.marketset+2.marketset#1.marketsize=3.marketset+3.marketset#1.marketsize
	local pva = string(r(p), "%9.2f")	
	global pva = `pva'
	estadd local pva = $pva
			
	
esttab  a2 a3 a4 using "$table/table_c1.tex", ///
		 star(* 0.10 ** 0.05 *** 0.01) compress replace label  nomtitle ///
		drop ( 0.marketset   _cons 0.marketsize 0.marketset#0.marketsize 0.marketset#1.marketsize 1.marketset#0.marketsize 2.marketset#0.marketsize 3.marketset#0.marketsize) ///
		booktabs b(%20.2f) se(%20.2f) eqlabels(none)  ///
		stats (coef p pv pva trader order N ymean  , fmt(%3.2f %3.2f %3.2f %3.2f- %~#s %~#s 0 %3.2f ) /// 
		label( "T2=T3" "\textit{p}-value" ///
		" T3=T4 \textit{p}-value" ///
		" T3 + T3 $\times$ Large Market = T4 + T4 $\times$ Large Market \textit{p}-value" ///
		"Trader FE" "Order FE" " \textit{N}" ///
		" Outcome mean, T1" ///
		)) nogap nonotes ///
		mgroups( "Small Market" "Large Market" "Full Sample" , pattern( 1  1 ) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		nogap  	nonotes	
		
		


**# Merge with simulation data 
*# Game riskness based on simulation
merge m:1 game using "$clean/montecarlo_earning.dta"
*Not matched = test game


		


**#TABLE D1 
lab var earning_cup "$ \mathbb{E}[\text{Earnings} | F=1]$"
lab var sd_earning_c "Earnings SD | F=1"
lab var earning_nc "$ \mathbb{E}[\text{Earnings} | F=0]$"
lab var sd_earning_nc "Earnings SD | F=0"
lab var overlap "Overlap in earnings"
lab var earning_dif "$ \mathbb{E}[\text{E}_{1} - \text{E}_{0} ]$ " 
lab var sd_earning_diff "SD $[\text{E}_{1} - \text{E}_{0} ]$"
preserve
bys game: gen n=_n
keep if n==1
***** Setup ******
		global gain earning_cup  sd_earning_c earning_nc sd_earning_nc earning_dif sd_earning_diff overlap
			
		cap file close balance
		file open balance using "$table/eg_cv_game.tex", write replace

		file write balance "\begin{tabular}{l cc cc cc cc}" _n
		file write balance "\hline" _n
		file write balance " & \multicolumn{4}{c}{Small Market} & \multicolumn{4}{c}{Large Market} \\ \cmidrule(lr){2-5}\cmidrule(lr){6-9} " _n
		file write balance " &  Low $\pi$  & Medium $\pi$ & High $\pi$ & High $\pi$ &  Low $\pi$  & Medium $\pi$ & High $\pi$ & High $\pi$  \\ " _n
		file write balance " &  Low $\theta$   & Low $\theta$     &  Low $\theta$ & High $\theta$	&  Low $\theta$   & Low $\theta$     &  Low $\theta$ & High $\theta$ \\" _n
		file write balance " &  (1)     & (2)       & (3)                             & (4) & (5) & (6) & (7) & (8) \\ " _n
		file write balance "\hline" _n
		
		foreach X of varlist $gain {
		
			local variable_label : variable label `X'
			
					
	***** Game 1 *****
				sum `X' if game==3
				local tempmean_1 : display %-8.2f `r(mean)'

				
	***** Game 2 *****
				sum `X' if game==4
				local tempmean_2 : display %-8.2f `r(mean)'

				
	***** Game 3 *****
				sum `X' if game==5
				local tempmean_3 : display %-8.2f `r(mean)'


	***** Game 4 *****
				sum `X' if game==6
				local tempmean_4 : display %-8.2f `r(mean)'
					
	***** Game 5 *****
				sum `X' if game==7
				local tempmean_5 : display %-8.2f `r(mean)'
				
	***** Game 6 *****
				sum `X' if game==8
				local tempmean_6 : display %-8.2f `r(mean)'
				
	***** Game 7 *****
				sum `X' if game==9
				local tempmean_7 : display %-8.2f `r(mean)'

	***** Game 8 *****
				sum `X' if game==10
				local tempmean_8 : display %-8.2f `r(mean)'			
				
				file write balance "`variable_label' &  `tempmean_1' & `tempmean_2' & `tempmean_3' & `tempmean_4' & `tempmean_5' & `tempmean_6' & `tempmean_7' & `tempmean_8'  \\ " _n
				
				}
			file write balance "\hline" _n
			file write balance "\end{tabular}" _n
			file close balance
restore	
	

	
	
**# FIGURE C6	
eststo clear
sum cup if gametest==0
	local ymean: display %3.2f r(mean)
	global  ymean1= `ymean'
	
eststo : reghdfe cup earning_dif, vce(robust) ab(id order)
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"	
predict y_0, xb
scalar r2_0 =  e(r2_a) 
gen  r2_0= r2_0

eststo : reghdfe cup earning_dif sd_earning_diff, vce(robust) ab(id order)
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"	
predict y_1, xb
scalar r2_1 =  e(r2_a) 
gen  r2_1= r2_1

eststo : reghdfe cup earning_dif sd_earning_diff overlap, vce(robust) ab(id order)
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"	
predict y_2, xb
scalar r2_2 =  e(r2_a) 
gen  r2_2= r2_2
	
eststo : reghdfe cup earning_dif sd_earning_diff earning_tan, vce(robust) ab(id order)
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"
predict y_3, xb
scalar r2_3 =  e(r2_a) 
gen  r2_3= r2_3

eststo : reghdfe cup earning_dif sd_earning_diff earning_tan overlap, vce(robust) ab(id order)
	estadd local ymean = $ymean1	
	estadd local trader "Yes"
	estadd local order "Yes"
predict y_4, xb
scalar r2_4 =  e(r2_a) 
gen  r2_4= r2_4
	


lab var r2_0 "E1-E0"
lab var r2_1 "+ SD"
lab var r2_4 "+ salience"
lab var r2_2 "+ overlap"
statplot  r2_0 r2_1 r2_2 r2_4, $scheme $graphregion $plotregion $ylabel $blabel recast(bar) ytitle("Adjusted R-squared ") ylabel(0(0.2)1)
graph export "$graph/figure_c6.png", replace width(1200) 
			


**
end