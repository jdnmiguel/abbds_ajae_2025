********************************************************************************
*Title:			Buyers' Response to Third Party Quality Certification: Theory and Evidence from Ethiopian Wheat Traders		
*Purpose:		Path setting 
*Author:		Jeremy DO NASCIMENTO MIGUEL - email: jeremy.dnmiguel@gmail.com
*Date: 			April 2022
********************************************************************************
// Set your file paths, These are relative to where the project file is saved. 
// Change path line 15 and 16

*Package to install: uncomment if you need to install 

*ssc install schemepack 
*ssc install reghdfe
*ssc install coefplot

clear
set more off

*Global path
cd "C:\Users\jdnmiguel\Dropbox\ongoing\Wheat Experiment 2021\replication"

global raw "Data\raw"
global clean "Data\clean"
global graph "Results\graphic"
global table "Results\table"


// Close any open log files
capture log close

// Clear Memory
clear all

// Drop everything in mata
matrix drop _all


*Graph option
	global plotregion plotregion(margin(b=0 t=2) color(white) fcolor(white) lcolor(white) icolor(white) ifcolor(white) ilcolor(white)) 
	global graphregion graphregion(margin(l=0 r=4 b=-2 t=-1) color(white) fcolor(white) lcolor(white) icolor(white) ifcolor(white) ilcolor(white))
	global blabel blabel(bar, format(%4.3f)) 
	global ylabel 	ylabel(, nogrid ang(hor) labsize(small))
	global scheme scheme(white_tableau)
