 /*this class  will discuss setting up longitudinal data analysis.  Often times, this will be a study of people over time*/
 
 /*We're going to start with a data set that is in long form.  Long form means a person (or whatever entity is under study) gets a line for each time period they are observed*/
 
 clear
 eststo clear
 
 webuse nlswork
tab ln_wage
 /*first step just going to the data editor and looking at the data*/  
 
 /*Next my first step is going to be transforming the data into wide format.  This could also be considered a person level data set.  
 Under the wide set up, each person (or whatever entity is being studied) gets one line and all the time data is contained on separate variables.  So how do we do this?*/
 
 /*identify what variables change over time, these variables need to be transformed so that they take on a separate value for each time period in which they are observed*/

 local timevarying age msp nev_mar grade collgrad not_smsa c_city south ind_code occ_code union wks_ue ttl_exp tenure hours wks_work ln_wage 
 /*Also track which variables don't change over time.  We don't need these now, but it's good to track them*/
 local nottimevarying birth_yr race
 
 /*reshape now will let us reshape the data, we specify wide format indicate we are going from multiple observations per person to one.  Next we need to specify three additional options
 
 1. before the comma some variable list, we could write out the variables by hand but in this case, I am going to use the local variable list time varying i just created.  If we leave
 a timevarying variable out of the list, stata will generally give you an error message indicating that a variable changed over time that isn't included in the reshape specification.
 
 2.  The i variable.  I remember this as i stands for id.  The unique identifier for the entities we want to reshape on, in this case, idcode.
 
 3. a j variable, the times observed.  All timevarying variables will get this variable values as a suffix added to their names.  We'll see this once reshape is done*/
 
 reshape wide `timevarying', i(idcode) j(year)
 
 
 /*What happens with reshape wide, many more variables, many fewer rows. Now why do I like to start in wide format.  
 There's a lot I want to know about these people before we go to a longitudinal analysis, plus it's good to know how many people you have and start descriptive tables using the person level.
 To illustrate.  Let's look at the economic well being of these folks*/
 
 egen highest_grade=rowmax(grade*)
 label variable highest_grade "Highest Grade Attained"
 egen ever_union=rowmax(union*)
 label variable ever_union "Ever in Union"
 egen highest_wage=rowmax(ln_wage*)
 label variable highest_wage "Highest Wage Earned"
 egen ever_south=rowmax(south*)
 label variable ever_south "Ever Lived in the South*/
 
 /*We can also use egen commands to check the panel structure and determine if there is missing data*/
 
 egen south_nonmissing=rownonmiss(south*)
  egen south_missing=rowmiss(south*)
 
 tab south_missing
 /*my favorite package for exporting relevant descriptive statistics is eststo.  Let's install it and run a quick example of descriptives*/
 ssc install estout 
 
 estpost tabstat highest_grade ever_union highest_wage, by(race) c(s) s("mean" "min" "max")
 esttab using descriptives.csv, replace label cells("mean min max")
 
 /*Now let's say we want to think about predictors of union membership*/
 
 eststo: logit ever_union highest_grade i.race ever_south
 esttab using logisticregression.csv, replace label eform /*eform option gives odds ratios, we can do more with eststo, but it's not the focus of this class*/
 
 /*now let's say you inherit a data set in wide format and want to move it into long format, what do you do?*/
 
 /*First think about what variables you are actually going to need.  A kitchen sink approach creates more work*/    /* Once you know what variables you need, move to making sure all variables have a common suffix that can be
 used for the j variable*/
 
 /* here is a toy example of something you might inherit from a raw data set */
 
 local raw a1 bt e2 n5 q22 w1 o0 i2 p0 q10 q11 x12 c3 v4 r8
 foreach x in `raw'{
 gen `x'=0
 }
 
 local cleaned  dummy68 dummy69 dummy70 dummy71 dummy72 dummy73 dummy75 dummy77 dummy78 dummy80 dummy82 dummy83 dummy85 dummy87 dummy88
forvalues i=1/15{
local a : word `i' of `raw'
local b : word `i' of `cleaned'
gen `b'=`a'
}
 
 
 drop `raw'
 
 /*Now let's see how to reshape back into long format*/
 
reshape long `timevarying' dummy, i(idcode) j(year)
/*Now let's talk about the concept of balance.  In the context of panel data, balance refers to all observations being observed for the same amount of time
Note: because we created that dummy variable that is present in all years, the data will appear balanced.  We can check this in many ways.  Here are two:*/

sort idcode
by idcode: gen observed=_N
tab observed
/*if this is the same for all observations, the data is balanced*/
drop observed

/*another way to do this is with the xtset command*/
xtset
/*However, the data was originally designed to be only present if a person had wages during a time period, so let's restructure the data so that we only keep cases when they have non-missing wages*/
drop if ln_w==.
/*now the data is unbalanced and of the same number of observations as the data set we started with*/

sort idcode
by idcode: gen observed=_N
tab observed
tab idcode

 xtset
 
 /*Now we're ready to run various models, here is one example of a fixed effect model*/
 xtreg ln_w age c.age#c.age ttl_exp c.ttl_exp#c.ttl_exp tenure c.tenure#c.tenure not_smsa ever_south, fe 
/*we can also run a random effects model with this specification*/
 xtreg ln_w grade age c.age#c.age ttl_exp c.ttl_exp#c.ttl_exp tenure c.tenure#c.tenure not_smsa ever_south, re

/*interpretation of these models isn't the focus of this short class, but I simply want to illustrate this specification works for running these type of change models*/

/* I want to wrap up with a quick lesson in why we need to be cautious using data in long format, this kind of data cleaning actually is needed and comes up a lot in survival analysis*/

/*Let's say for whatever reason, we don't want to include people's data if they were living in the south at the time*/
preserve
drop if south==1
tab id
restore
/*However, how would we drop people if the person lived in the south?*/
/*Option 1 use the data we created in wide format*/
/*Alternatively, we can use egen to create a person level variable for ever having lived in south*/
egen ever_south2=max(south), by(idcode)
/*Tab reveals these variables are the same*/
tab ever_south ever_south2
drop if ever_south==1
