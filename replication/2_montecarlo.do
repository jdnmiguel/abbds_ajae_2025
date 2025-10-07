********************************************************************************
*Title:			Buyers' Response to Third Party Quality Certification: Theory and Evidence from Ethiopian Wheat Traders
*Purpose:		Monte Carlo simulation
*Author:		Jeremy DO NASCIMENTO MIGUEL - email: jeremy.dnmiguel@gmail.com
*Date: 			September 2023
**********
********************************************************************************

**# GAME 1 no cup
clear

set seed 21092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	16
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 4
scalar r_lq = 26
scalar r_hq = 6
scalar pi=0.1

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game1nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<17

forval j = 1/16{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (16-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'





**# Game 1 Cup
clear

set seed 21092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	16
scalar zt=Z/T
scalar n = 36

// Generate number of blue and red chips
scalar blue = 4
scalar r_lq = 26
scalar r_hq = 6
scalar pi=0.1

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game1cup", replace
	forval i=1/`simulations'{
drop random order  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
replace pick=1 if order<=16 & chips==0
replace pick=0 if chips!=0
egen tot_accepted=sum(pick)

	forval j = 1/12{

			if order <=N & tot_accepted<12 { 
			
			replace pick=1 if order==4+`j' & tot_accepted<12
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
		}
		else {
			replace pick=1 if order==4+`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'

use "$clean\game1cup", clear





**#Game 2 no cup
	clear

	set seed 21092023

	// Define the parameters
	local simulations 1000										


	*Set the values of  the parameters 
	scalar a =	220
	scalar b =	20
	scalar PC = 280
	scalar pc = 220
	scalar pu = 200
	scalar F =	330
	scalar Z =	12
	scalar T =	16
	scalar zt=Z/T
	scalar n = 36



// Generate number of blue and red chips
scalar blue = 11
scalar r_lq = 20
scalar r_hq = 5
scalar pi=0.3


*Set the number of observation equals to the total number of chips
	set obs 36


	*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
	gen chips=0 if _n<=blue
	replace chips=1 if _n<=r_lq+blue & chips==.
	replace chips=2 if chips==.


	*Farmer N for optimal decision
	*If fixed cost
	gen N=(T-Z)/(1-pi)

	*If not fixed fixed cost
	gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game2nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<17

forval j = 1/16{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (16-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j'  & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'




**# Game 2 Cup

clear

set seed 21092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	16
scalar zt=Z/T
scalar n = 36

// Generate number of blue and red chips
scalar blue = 11
scalar r_lq = 20
scalar r_hq = 5
scalar pi=0.3

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game2cup", replace
	forval i=1/`simulations'{
drop random order  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
replace pick=1 if order<=16 & chips==0
replace pick=0 if chips!=0
egen tot_accepted=sum(pick)

	forval j = 1/12{

			if order <=N & tot_accepted<12 { 
			
			replace pick=1 if order==4+`j' &  tot_accepted<12
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
		}
		else {
			replace pick=1 if order==4+`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'



**# GAME 3 cup
clear

set seed 21092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	16
scalar zt=Z/T
scalar n = 36

// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 14
scalar r_hq = 4
scalar pi=0.5

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game3cup", replace
	forval i=1/`simulations'{
drop random order  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
replace pick=1 if order<=12 & chips==0
replace pick=0 if chips!=0
egen tot_accepted=sum(pick)
keep if order<17
	forval j = 1/12{

			if order <=N & tot_accepted<12 { 
			
			replace pick=1 if order==4+`j' & tot_accepted<12
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
		}
		else {
			replace pick=1 if order==4+`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'


	
**#Game 3 no cup
	clear

	set seed 21092023

	// Define the parameters
	local simulations 1000										


	*Set the values of  the parameters 
	scalar a =	220
	scalar b =	20
	scalar PC = 280
	scalar pc = 220
	scalar pu = 200
	scalar F =	330
	scalar Z =	12
	scalar T =	16
	scalar zt=Z/T
	scalar n = 36



// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 14
scalar r_hq = 4
scalar pi=0.5


*Set the number of observation equals to the total number of chips
	set obs 36


	*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
	gen chips=0 if _n<=blue
	replace chips=1 if _n<=r_lq+blue & chips==.
	replace chips=2 if chips==.


	*Farmer N for optimal decision
	*If fixed cost
	gen N=(T-Z)/(1-pi)

	*If not fixed fixed cost
	gen M=(T-Z)/pi


	*Profit
	*gen profit = Z*(a+b*theta-pu)
	gen profit = . 
	gen random=.
	gen order=.
	gen order_blue=. 
	gen pick=.
	gen d_rlq=.
	gen d_rhq=.
	gen d_blue=.
	gen i=_n
	gen tot_accepted = . 
	tempname sim 




postfile `sim' mean  tot rlq rhq blue using "$clean\game3nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<17

	forval j = 1/12{

			if order <=M & tot_accepted<12 { 
			
			replace pick=1 if order==4+`j' & tot_accepted<12
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
		}
		else {
			replace pick=1 if order==4+`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'


**# GAME 4 no cup

clear

	set seed 21092023

	// Define the parameters
	local simulations 1000										


	*Set the values of  the parameters 
	scalar a =	220
	scalar b =	20
	scalar PC = 280
	scalar pc = 220
	scalar pu = 200
	scalar F =	330
	scalar Z =	12
	scalar T =	16
	scalar zt=Z/T
	scalar n = 36



// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 4
scalar r_hq = 14
scalar pi=0.5

*Set the number of observation equals to the total number of chips
	set obs 36


	*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
	gen chips=0 if _n<=blue
	replace chips=1 if _n<=r_lq+blue & chips==.
	replace chips=2 if chips==.


	*Farmer N for optimal decision
	*If fixed cost
	gen N=(T-Z)/(1-pi)

	*If not fixed fixed cost
	gen M=(T-Z)/pi


	*Profit
	*gen profit = Z*(a+b*theta-pu)
	gen profit = . 
	gen random=.
	gen order=.
	gen order_blue=. 
	gen pick=.
	gen d_rlq=.
	gen d_rhq=.
	gen d_blue=.
	gen i=_n
	gen tot_accepted = . 
	tempname sim 

postfile `sim' mean  tot rlq rhq blue using "$clean\game4nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<17

forval j = 1/16{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (16-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j'  & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}
postclose `sim'


**#GAME 4 cup
clear

set seed 21092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	16
scalar zt=Z/T
scalar n = 36

// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 4
scalar r_hq = 14
scalar pi=0.5

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=.
gen order_blue=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game4cup", replace
	forval i=1/`simulations'{
drop random order  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
replace pick=1 if order<=16 & chips==0
replace pick=0 if chips!=0
egen tot_accepted=sum(pick)
keep if order<17
	forval j = 1/12{

			if order <=N & tot_accepted<12 { 
			
			replace pick=1 if order==4+`j' & tot_accepted<12
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
		}
		else {
			replace pick=1 if order==4+`j' & tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'

**#Large MARKET

**#GAME 5 no cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 4
scalar r_lq = 26
scalar r_hq = 6
scalar pi=0.1
*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game5nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'






**#GAME 5 cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36



// Generate number of blue and red chips
scalar blue = 4
scalar r_lq = 26
scalar r_hq = 6
scalar pi=0.1

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game5cup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=30 & chips==0
replace pick=0 if order>12 & chips!=0
egen tot_accepted=sum(pick)

forval j = 1/30{

	if order <=N & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips==0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'





**# GAME 6 cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36



// Generate number of blue and red chips
scalar blue = 12
scalar r_lq = 20
scalar r_hq = 4
scalar pi=0.3

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game6cup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=30 & chips==0
replace pick=0 if order>12 & chips!=0
egen tot_accepted=sum(pick)

forval j = 1/30{

	if order <=N & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips==0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'






**#GAME 6 no cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 11
scalar r_lq = 20
scalar r_hq = 5
scalar pi=0.3
*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game6nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'


**# GAME 7 CUP
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 14
scalar r_hq = 4
scalar pi=0.5

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game7cup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips==0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=N & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips==0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'





**#GAME 7 no cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 14
scalar r_hq = 4
scalar pi=0.5
*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 




postfile `sim' mean  tot rlq rhq blue using "$clean\game7nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'







**# GAME 8 no cup
clear

set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 4
scalar r_hq = 14
scalar pi=0.5
*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 



postfile `sim' mean  tot rlq rhq blue using "$clean\game8nocup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips>0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=M & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips>0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*b

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'




**#GAME 8  cup
clear
set seed 19092023

// Define the parameters
local simulations 1000


*Set the values of  the parameters 
scalar a =	220
scalar b =	20
scalar PC = 280
scalar pc = 220
scalar pu = 200
scalar F =	330
scalar Z =	12
scalar T =	30
scalar zt=Z/T
scalar n = 36


// Generate number of blue and red chips
scalar blue = 18
scalar r_lq = 4
scalar r_hq = 14
scalar pi=0.5

*Set the number of observation equals to the total number of chips
set obs 36


*Assign color to chips : 0=blue; 1=red low quality ; 2=red high quality
gen chips=0 if _n<=blue
replace chips=1 if _n<=r_lq+blue & chips==.
replace chips=2 if chips==.


*Farmer N for optimal decision
*If fixed cost
gen N=(T-Z)/(1-pi)

*If not fixed fixed cost
gen M=(T-Z)/pi


*Profit
*gen profit = Z*(a+b*theta-pu)
gen profit = . 
gen random=.
gen order=. 
gen pick=.
gen d_rlq=.
gen d_rhq=.
gen d_blue=.
gen i=_n
gen tot_accepted = . 
tempname sim 


postfile `sim' mean  tot rlq rhq blue using "$clean\game8cup", replace
	forval i=1/`simulations'{
drop random order*  tot_accepted*   d_rlq d_rhq d_blue
*generate a random number
quietly {
gen random=rnormal()
egen order=rank(random)
egen order_blue=rank(random) if chips!=0
replace pick=1 if order<=12 & chips==0
replace pick=0 if order>12
egen tot_accepted=sum(pick)
keep if order<30

forval j = 1/30{

	if order <=N & tot_accepted<12 { 

	

		replace pick=1 if order==`j' & chips==0 & tot_accepted<12

			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
			replace pick=1 if  order==`j' & (30-order<12-tot_accepted)  // cases where only blue chips left
			drop tot_accepted`j'
			egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			
		
		}
		else {
			replace pick=1 if order==`j' &  tot_accepted<12
	egen tot_accepted`j'=sum(pick)
			replace tot_accepted=tot_accepted`j' if tot_accepted<12
			}

		}

egen d_rlq =sum(pick) if chips==1
sort d_rlq
replace d_rlq=d_rlq[_n-1] if d_rlq==.
 
egen d_rhq =sum(pick) if chips==2
sort d_rhq
replace d_rhq=d_rhq[_n-1] if d_rhq==.
 
egen d_blue =sum(pick) if chips==0
sort d_blue
 replace d_blue=d_blue[_n-1] if d_blue==.


replace profit = (d_rlq*a+d_rhq*(a+b))-pu*(d_rlq+d_rhq)+d_blue*PC-d_blue*pc-F

sum profit 
scalar mean = r(mean)
sum tot_accepted 
scalar tot = r(mean)
sum d_rlq
scalar rlq = r(mean)
sum d_rhq
scalar rhq = r(mean)
sum d_blue
scalar blue = r(mean)
post `sim' (mean) (tot) (rlq) (rhq) (blue)
}
	}

postclose `sim'



**#Append all the game 
use "$clean\game1cup", clear
gen game=1
gen cup=1
forvalues i=2/8 {
append using "$clean\game`i'cup"
replace game=`i' if game==.
replace cup=1 if cup==.
}

forvalues i=1/8 {
append using "$clean\game`i'nocup"
replace game=`i' if game==.
replace cup=0 if cup==.
}
bys game cup: sum 

label define game 1 "Low {&theta} - Low {&pi}" 2 "Low {&theta} - Medium {&pi}" 3 "Low {&theta} - High {&pi}" 4 "High {&theta} - High {&pi}" ///
5 "Low {&theta} - Low {&pi}" 6 "Low {&theta} - Medium {&pi}" 7 "Low {&theta} - High {&pi}" 8 " High {&theta} - High {&pi}", replace
label values game game

*Plot the earning distribution
forvalues i=1/8{
twoway histogram mean if game==`i' & cup==1, discrete xlabel(0(50)500)   width(15) bfcolor(red%30) blcolor(red%30)  percent gap(2)  /// 
|| histogram mean if game==`i' & cup==0, discrete  width(15)  bfcolor(blue%30) blcolor(blue%30)   percent gap(2) ///
$scheme  $graphregion ylabel(0(10)100, nogrid ang(hor) labsize(small)) xtitle("Earnings (in birr)") legend(order(1 "Fixed Cost" 2 "No Fixed Cost") col(2) pos(6))
graph export "$graph/figure3_`i'.png", replace width(1200) 
}


*Generate variation coefficient
bys game cup: egen earning_mean=mean(mean)
bys game cup: egen earning_sd=sd(mean)
bys game cup: gen earning_cv=earning_sd/earning_mean
replace earning_cv=0 if earning_cv==.

bys game cup: egen rlq_mean=mean(rlq)
bys game cup: egen rhq_mean=mean(rhq)
bys game cup: egen blue_mean=mean(blue)

bys game cup: egen rlq_sd=sd(rlq)
bys game cup: egen rhq_sd=sd(rhq)
bys game cup: egen blue_sd=sd(blue)

bys game cup: gen rlq_cv=rlq_sd/rlq_mean
bys game cup: gen rhq_cv=rhq_sd/rhq_mean
bys game cup: gen blue_cv=blue_sd/blue_mean
replace blue_cv=0 if blue_cv==.
replace rlq_cv=0 if rlq_cv==.
replace rhq_cv=0 if rhq_cv==.



*Generate min and max earning per game
bys game cup: egen earning_min=min(mean)
bys game cup: egen earning_max=max(mean)


*generate earnings by cup
bys game: gen earning_cup=earning_mean if cup==1
bys game: gen earning_nc=earning_mean if cup==0
bys game: gen earning_cv_cup=earning_cv if cup==1
bys game: gen earning_cv_nc=earning_cv if cup==0
bys game: gen earning_sd_cup=earning_sd if cup==1
bys game: gen earning_sd_nc=earning_sd if cup==0



bys game: gen earning_min_cup=earning_min if cup==1
bys game: gen earning_min_nc=earning_min if cup==0
bys game: gen earning_max_cup=earning_max if cup==1
bys game: gen earning_max_nc=earning_max if cup==0


bys game: gen rlq_cv_cup=rlq_cv if cup==1
bys game: gen rhq_cv_cup=rhq_cv if cup==1
bys game: gen blue_cv_cup=blue_cv if cup==1

bys game: gen rlq_cv_nc=rlq_cv if cup==0
bys game: gen rhq_cv_nc=rhq_cv if cup==0
bys game: gen blue_cv_nc=blue_cv if cup==0


foreach var of varlist earning_cup  earning_nc earning_cv_cup earning_cv_nc earning_sd_cup earning_sd_nc earning_min_cup  earning_max_cup earning_min_nc earning_max_nc rlq_cv_cup rhq_cv_cup blue_cv_cup rlq_cv_nc rhq_cv_nc blue_cv_nc{
	bys game (`var'): replace `var'=`var'[_n-1] if missing(`var')
}



gen earning_dif=earning_cup-earning_nc


*Compute overlap between earnings distribution 
    gen overlap=.
    replace overlap =1 if (mean < earning_max_cup & mean > earning_min_nc & earning_dif<0)	
	replace overlap =1 if (mean < earning_max_nc & mean > earning_min_cup & earning_dif>0)
    recode overlap (.=0)
	
	
*replace game number with those in main dataset
replace game=game+2




 
collapse earning_cv earning_cv_cup earning_cv_nc earning_mean earning_cup earning_nc earning_sd_nc  earning_sd_cup earning_dif earning_min_cup  earning_max_cup earning_min_nc earning_max_nc overlap rlq_mean rhq_mean blue_mean rlq_sd rhq_sd blue_sd rlq_cv rhq_cv blue_cv rlq_cv_cup rhq_cv_cup blue_cv_cup rlq_cv_nc rhq_cv_nc blue_cv_nc, by(game)
egen game_risk=rank(earning_dif)


save "$clean\montecarlo_earning.dta", replace
