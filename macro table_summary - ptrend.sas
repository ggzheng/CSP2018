/**********************************************************************************
***********************************************************************************
*** Author: Yuanchao Zheng, M.S., Stanford University
*** Purpose: Create Summary tables with trend pvalues
*** OS: Windows 10 Pro 64-bit
*** Software: SAS 9.4

*** Note: 
	There are two SAS macros: Getvars and Table_summary. Getvars computes
	summary statistics for both categorical and continuous variables.
	Table_summary calls Getvars and outputs a summary table.

*** Copyright (C) <2018> <Yuanchao Zheng>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************
**********************************************************************************/

/************************************************************************************************************
*** Marco 1: Getvars;
************************************************************************************************************/

%macro Getvars(dat=&yourdata.,datout=&output_data.);

proc datasets; delete &datout.;run; quit;

*Get summary statistics for categorical variables;
***********************************************************;

%if &varlist_cat.^= %then %do;

*count number of categorical variables;
%let nvarlist_cat=%sysfunc(countw(&varlist_cat.)); 

*for each categorical variable, compute counts and percents;
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&varlist_cat.,&i.);

proc freq data=&dat. noprint; tables &&var&i../missing out=dat_cat_&i.; run;

data dat_cat_&i._2(drop=&&var&i.. COUNT PERCENT);
 set dat_cat_&i.;
 length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

 output_suborder=5;
 xx_variable="%trim(%upcase(&&var&i..))";
 xx_var_label=upcase(VLABEL(&&var&i..));
 xx_description=strip(vvalue(&&var&i..));
 xx_type="count and percent";
 xx_var=strip(put(COUNT,16.))||" ("||strip(put(PERCENT, 16.&decimal_max.))||"%"||")";
 xx_variable_original="%trim(%upcase(&&var&i..))";

 label xx_variable="Variable" xx_var_label="Variable label" xx_description="Description" 
       xx_type="Type of statistic" xx_var="Statistic";
 format percent 8.1;
run;
proc append base=&datout. data=dat_cat_&i._2; run;
%end;

proc datasets library=work; delete dat_cat:; run;
%end;

*Get summary statistics for continuous variables;
***********************************************************;

*part 1: mean and std values; 

%if &varlist_cont.^= %then %do;

%let nvarlist_cont=%sysfunc(countw(&varlist_cont.)); 

%do j=1 %to &nvarlist_cont.;
%let var&j.=%scan(&varlist_cont.,&j.);

proc means data=&dat. noprint; var &&var&j..;
output out=dat_cont1_&j.(drop=_TYPE_ _FREQ_) mean=mean std=std;
run;

data dat_cont1_&j._2(drop=mean std);
 set dat_cont1_&j.;
 length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

 output_suborder=1;
 xx_variable="%trim(%upcase(&&var&j..))";
 xx_var_label=strip(upcase(VLABEL(mean)));
 xx_description="Continuous";
 xx_type="mean and std";
 xx_var=strip(put(mean,16.&decimal_max.))||" ("||strip(put(std, 
16.&decimal_max.))||")";
 xx_variable_original="%trim(%upcase(&&var&j..))";
 if xx_var_label="MEAN" then xx_var_label=xx_variable;

 label xx_variable="Variable" xx_var_label="Variable label" xx_description="Description" 
       xx_type="Type of statistic" xx_var="Statistic";
run;
proc append base=&datout. data=dat_cont1_&j._2; run;

*part 2: median and IQR, min max;

proc means data=&dat. noprint; var &&var&j..;
 output out=dat_cont2_&j.(drop=_TYPE_ _FREQ_) p50=p50 p25=p25 p75=p75 min=min max=max;
run;

data dat_cont2_&j._2(drop=p50 p25 p75);
 set dat_cont2_&j.(drop=min max);
 length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

 output_suborder=2;
 xx_variable="%trim(%upcase(&&var&j..))";
 xx_var_label=strip(upcase(VLABEL(p50)));
 xx_description="Continuous";
 xx_type="median and IQR";
 xx_var=strip(put(p50,16.&decimal_max.))||" ("||strip(put(p25, 
16.&decimal_max.))||", "||strip(put(p75, 16.&decimal_max.))||")";
 xx_variable_original="%trim(%upcase(&&var&j..))";
 if xx_var_label="P50" then xx_var_label=xx_variable;

 label xx_variable="Variable" xx_var_label="Variable label" xx_description="Description" 
       xx_type="Type of statistic" xx_var="Statistic";
run;

data dat_cont2_&j._3(drop=min max);
 set dat_cont2_&j.(drop=p50 p25 p75);
 length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

 output_suborder=3;
 xx_variable="%trim(%upcase(&&var&j..))";
 xx_var_label=strip(upcase(VLABEL(min)));
 xx_description="Continuous";
 xx_type="min and max";
 xx_var="("||strip(put(min, 16.&decimal_max.))||", "||strip(put(max, 16.&decimal_max.))||")";
 xx_variable_original="%trim(%upcase(&&var&j..))";
 if xx_var_label="MIN" then xx_var_label=xx_variable;

 label xx_variable="Variable" xx_var_label="Variable label" xx_description="Description" 
       xx_type="Type of statistic" xx_var="Statistic";
run;

proc append base=&datout. data=dat_cont2_&j._2; run;
proc append base=&datout. data=dat_cont2_&j._3; run;

*part 3: check missingness;
proc format;
 value $mchar
    " "="Missing" 
    other="Not Missing"
 ;
 value mnum  
 . ="Missing" 
 other="Not Missing"
 ;
run;

data dat_cont_check_&j.; 
 set &dat.(keep=&&var&j..); 
 format _CHAR_ $mchar. _NUMERIC_ mnum.;
run;

proc freq data=dat_cont_check_&j. noprint;
  tables &&var&j../missing out=dat_cont_check_&j._2;
run;

data dat_cont_check_&j._3(drop=&&var&j.. COUNT PERCENT);
 set dat_cont_check_&j._2;   
 length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;
 
 if vvalue(&&var&j..)="Missing";

 output_suborder=4;
 xx_variable="%trim(%upcase(&&var&j..))";
 xx_var_label=upcase(VLABEL(&&var&j..));
 xx_description="Continuous: missing";
 xx_type="count and percent";
 xx_var=strip(put(COUNT,16.))||" ("||strip(put(PERCENT, 16.&decimal_max.))||"%"||")";
 xx_variable_original="%trim(%upcase(&&var&j..))";

 label xx_variable="Variable" xx_var_label="Variable label" xx_description="Description" 
       xx_type="Type of statistic" xx_var="Statistic";
 format percent 8.1;
run;

proc append base=&datout. data=dat_cont_check_&j._3; run;
%end;

proc datasets library=work; delete dat_cont:; run;
%end;

%mend;


/************************************************************************************************************
*** Marco 2: ptrendtest;

*** Cochran-Armitage linear trend test for continuous variables using Proc Multtest;
*** Cochran-Armitage trend test for categorical variables using Proc Freq with trend;
************************************************************************************************************/
%macro ptrendtest;

%if &varlist_cont.^= %then %do;

*determine which contrast to use for the trend test;
*Count the total number of categories in the group-by variable;
proc sql; select count(distinct &group_by.) into: groupby_ncat from yourdata2; quit;
%let groupby_ncat = &groupby_ncat.; 

%let contrast_test=;
%if %sysfunc(mod(%eval(&groupby_ncat.),2))=1 %then %do;
    %do kkk= -%eval((&groupby_ncat.-1)/2) %to %eval((&groupby_ncat.-1)/2) %by 1;
    %let contrast_test= &contrast_test. &kkk.;
	%end;
%end;
%else %if %sysfunc(mod(%eval(&groupby_ncat.),2))=0 %then %do;
    %do kkk= -%eval(&groupby_ncat.-1) %to %eval(&groupby_ncat.-1) %by 2;
    %let contrast_test= &contrast_test. &kkk.;
	%end;
%end;

%let contrast_test = &contrast_test.; 

*count number of categorical variables;
%let nvarlist_cont=%sysfunc(countw(&varlist_cont.)); 

*for each continuous variable, get all single non-missing formatted value;
%do pj=1 %to &nvarlist_cont.;
%let var&pj.=%scan(&varlist_cont.,&pj.);
proc glm data=yourdata2 outstat=xx_tempptrend_cont plots=none;
 class &group_by.;
 model &&var&pj.=&group_by.;
  contrast 'linear' &group_by. &contrast_test.;
run;

data xx_tempptrend_cont&pj.(keep=xx_variable_original xx_description ptrend);
 set xx_tempptrend_cont(where=(_TYPE_="CONTRAST"));
 length xx_variable_original xx_description ptrend $ 100;
 xx_variable_original=upcase(_NAME_);
 xx_description="Continuous";
 if VTYPE(PROB)="N" then ptrend=strip(put(PROB,12.4));
 else ptrend=PROB;
 label ptrend="Trend P-value";
run;
%end;
%end;

%if &nvarlist_cont.=1 %then %do;
data tempptrend_cont; set xx_tempptrend_cont1; run; 
%end;
%else %if &nvarlist_cont.>1 %then %do;
data tempptrend_cont; set xx_tempptrend_cont1-xx_tempptrend_cont&nvarlist_cont.; run; 
%end;

%if &varlist_cat.^= %then %do;

*count number of categorical variables;
%let nvarlist_cat=%sysfunc(countw(&varlist_cat.)); 

*for each categorical variable, get all single non-missing formatted value;
%do pj=1 %to &nvarlist_cat.;
%let var&pj.=%scan(&varlist_cat.,&pj.);

proc freq data=yourdata2(where=(not missing(&&var&pj..)));
 tables &&var&pj./out=xx_ptrend_temp1;
run;
data xx_ptrend_temp1;
 set xx_ptrend_temp1(keep=&&var&pj.);
 xx_variable_original="%trim(%upcase(&&var&pj..))";
 rename &&var&pj.=xx_description;
run;
 
*Count the total number of categories;
proc sql; select count(*) into: ncat from xx_ptrend_temp1; quit;
%let ncat = &ncat.; 

*Create macro variable with variable values (take formatted first if available);
*Only taking non-missing values;
proc sql; select xx_description into :cattemp1-:cattemp&ncat. from xx_ptrend_temp1; quit;

data xx_ptrend_temp2;
 set yourdata2(where=(not missing(&&var&pj..)));
 %do pi= 1 %to &ncat.;
 cattemp_&pi.=(strip(lowcase(vvalue(&&var&pj..)))="%trim(%lowcase(&&cattemp&pi..))");
 %end;
run;

ods output TrendTest=xx_ptrend_temp3(where=(label1="Two-sided Pr > |Z|"));
proc freq data=xx_ptrend_temp2; 
 %do pi= 1 %to &ncat.;
 tables &group_by.*cattemp_&pi. / trend norow nocol nopercent scores=table;
 %end;
run;

data tempptrend_cat&pj.(keep=xx_variable_original xx_description ptrend);
 set xx_ptrend_temp3;
 length xx_variable_original xx_description ptrend $ 100;
 val=strip(scan(Table,-1,"_"));
 xx_description=symget("cattemp"||val);
 xx_variable_original=upcase("&&var&pj..");
 ptrend=cValue1;
 label ptrend="Trend P-value";
run;
%end;

%if &nvarlist_cat.=1 %then %do;
data tempptrend_cat; set tempptrend_cat1; run; 
%end;
%else %if &nvarlist_cat.>1 %then %do;
data tempptrend_cat; set tempptrend_cat1-tempptrend_cat&nvarlist_cat.; run; 
%end;
%end;

*put ptrend data together;
%if &varlist_cont.^= and  &varlist_cat.^= %then %do;
 data xx_ptrend_final; set tempptrend_cont tempptrend_cat; run; %end;
%else %if &varlist_cont.^= and &varlist_cat.= %then %do;
 data xx_ptrend_final; set tempptrend_cont; run; %end;
%else %if &varlist_cont.= and &varlist_cat.^= %then %do;
 data xx_ptrend_final; set tempptrend_cat; run; %end;

data xx_ptrend_final; set xx_ptrend_final; if ptrend="0.0000" then ptrend="<.0001";
proc sort data=xx_ptrend_final; by xx_variable_original xx_description; run;
%mend;


/************************************************************************************************************
*** Marco 3: Table_summary;
************************************************************************************************************/

%macro Table_summary(byvar=&group_by.);

/***************************SAS settings***************************/

options missing="" PAGENO=1 label nofmterr formdlim=" ";

*Set SAS formats if there is any external one;
%if &formatsfolder.^= %then %do; libname library &formatsfolder.;%end;

*Set where data to be summarized is stored. Default is SAS work library.;
%if &yourfolder.^= %then %do; 
    libname datain &yourfolder.;
	data &yourdata.; set datain.&yourdata.; run;
%end;
%else %do; data &yourdata.; set work.&yourdata.; run; %end;

*If none of variable is specified as categorical or continuous.;
%if &varlist_cat.= and &varlist_cont.= %then %do; 
    %put ERROR: no variable is specified.; %return;
%end;

*If no variable output order is specifed, then use the default order.;
%if &output_order.= %then %do; 
    %let output_order=%sysfunc(catx(%str( ),&varlist_cont., &varlist_cat.));
%end;

/**********************If group_by option is NOT used*******************/

%if &byvar.= %then %do;

*Count the total number of rows;
proc sql; select count(*) into: n_population from &yourdata.; quit;
%let n_population = &n_population.; 

*Call the macro %getvars to get all statistics;
%getvars(dat=&yourdata., datout=&output_data.)

data &output_data.;
 set &output_data.(rename=(xx_var=xx_var_all));
 label xx_var_all="All (n=%trim(&n_population.))";
run;

*Create a dataset that contains variable output orders.;
proc datasets; delete table_orders; run; 
%let noutput_order=%sysfunc(countw(&output_order.)); 

data table_orders;
  length xx_variable_original $ 100 output_order 8;
  xx_variable_original=" "; output_order=0; 
  output;
run;

%do tableorder=1 %to &noutput_order.;
%let tableorder_var&tableorder.=%scan(&output_order.,&tableorder.);
data table_orders;
 set table_orders end=eof;
  length xx_variable_original $ 100 output_order 8; output;
  if eof then do;
	  xx_variable_original="%trim(%upcase(&&tableorder_var&tableorder..))";
	  output_order=&tableorder.; 
      output;
  end;
run;
%end;

data table_orders; set table_orders(where=(output_order^=0)); run;

*Add variable output orders to dataset with all statistics;
*Sort data by specified orders;
proc sort data=&output_data.; by xx_variable_original output_suborder;
proc sort data=table_orders nodupkey; by xx_variable_original; run;
data &output_data.;
 merge &output_data.(in=a) table_orders;
 by xx_variable_original; 
 if a;
run;
proc datasets; delete table_orders; run; 

proc sort data=&output_data.; by output_order output_suborder; run;
data &output_data.; 
 set &output_data.(drop=output_order output_suborder xx_variable_original); 
 if indexc(xx_var_all, '0123456789')=0 then xx_var_all="";
 if missing(xx_description) then xx_description="Missing";
 if xx_variable=xx_var_label then xx_var_label=" ";
run;

*Output the final summary table;
ods listing close; 
options missing="" orientation=landscape number date;
ods rtf bodytitle style=statistical file="&output_data..rtf";
footnote; 
title "Descriptive Table";

proc print data=&output_data. label; 
 var xx_variable xx_description xx_type xx_var_all;
run;

ods rtf close; 
title; footnote;

%end;

/**********************If group_by option is used*******************/

%else %do;

*Create a character variable to represenst original group-by variable;
*Formatted values will be used if the original variable contains formats.;
data yourdata2;
 set &yourdata.;
 length &byvar._temp $ 50;

 *Create a character variable to represenst original group-by variable;
 &byvar._temp=strip(vvalue(&byvar.));
 
 *By default, unspeficied missing values in group-by variable will be deleted;
 %if &group_by_missing.=0 %then %do;
   if missing(&byvar._temp) then delete;
 %end;
 %else %if &group_by_missing.=1 %then %do;
   if missing(&byvar._temp) then &byvar._temp="Unspecified Missing";
 %end;
run;

%let byvar_old=%trim(&byvar.);
%let byvar=&byvar._temp;

*compute the number of different subpopulations in group-by variable;
proc freq data=yourdata2(keep=&byvar.) noprint; 
 tables &byvar./out=temp_byvar; 
run;
proc sql; select count(*) into: n_byvar from temp_byvar; quit;
%let n_byvar = &n_byvar.; 

*for each subpopulation, compute the number of observations;
data _NULL_;
 length byvar_formatted obs $ 100;
 set temp_byvar;
 byvar_formatted=vvalue(&byvar.);
 obs="value"||strip(_N_); 
 obs2="counts"||strip(_N_);
 call symput(obs,trim(byvar_formatted)); call symput(obs2,trim(COUNT));
run;

*for each subpopulation, compute all the statistics;
proc datasets; delete table_suborders; run; 

%do m=1 %to &n_byvar.;

data temp_data_sub&m.;
 set yourdata2; 
 if strip(upcase(vvalue(&byvar.)))=strip(upcase("&&value&m.."));
run;

%getvars(dat=temp_data_sub&m., datout=temp_Cohort_demo_sub&m.)

data temp_Cohort_demo_sub&m.;
 set temp_Cohort_demo_sub&m.(rename=(xx_var=xx_var&m.));
 if indexc(xx_var&m., '0123456789')=0 then xx_var&m.="";
 label xx_var&m.="%qupcase(&&value&m..) (n=%trim(&&counts&m..))";
run;

proc sort data=temp_Cohort_demo_sub&m.; 
 by xx_variable_original xx_variable xx_var_label xx_description xx_type; 
run;

proc append base=table_suborders  
            data=temp_Cohort_demo_sub&m.(keep=xx_variable_original xx_variable 
                                              xx_var_label xx_description xx_type output_suborder); 
run;
%end;

*compute the number of total observations, and statistics for the whole population;
proc sql;select count(&byvar.) into: n_population from yourdata2; quit;
%let n_population = &n_population.;  

%getvars(dat=yourdata2, datout=temp_cohort_demo_all)

data temp_cohort_demo_all(drop=output_suborder rename=(xx_var=xx_var_all));
 set temp_cohort_demo_all;
  if indexc(xx_var, '0123456789')=0 then xx_var="";
  label xx_var="ALL (n=%trim(&n_population.))";
run;

*Create a dataset that contains variable output orders.;
proc datasets; delete table_orders; run; 
%let noutput_order=%sysfunc(countw(&output_order.)); 
data table_orders;
  length xx_variable_original $ 100 output_order 8;
  xx_variable_original=" "; output_order=0; output;
run;

%do tableorder=1 %to &noutput_order.;
%let tableorder_var&tableorder.=%scan(&output_order.,&tableorder.);
data table_orders;
 set table_orders end=eof;
  length xx_variable_original $ 100 output_order 8;
  output;
  if eof then do;
	  xx_variable_original="%trim(%upcase(&&tableorder_var&tableorder..))";
	  output_order=&tableorder.;output;
  end;
run;
%end;
data table_orders; set table_orders(where=(output_order^=0)); run;

proc sort data=table_orders nodupkey; by xx_variable_original; run;
proc sort data=table_suborders nodup; by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;

data orders;
 merge table_orders(in=a) table_suborders;
 by xx_variable_original;
 if a;
run;

*Add variable output orders to dataset with all statistics;
*Sort data by specified orders;
proc sort data=orders nodup; by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;
proc sort data=temp_cohort_demo_all; by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;

data &output_data.;

 %if &n_byvar.>1 %then %do;
 merge orders(in=a) temp_cohort_demo_all temp_Cohort_demo_sub1 - temp_Cohort_demo_sub&n_byvar.;
 %end;
 %else %if &n_byvar.=1 %then %do;
 merge orders(in=a) temp_cohort_demo_all temp_Cohort_demo_sub1;
 %end;

 by xx_variable_original xx_variable xx_var_label xx_description xx_type;
 if a;
run;

proc sort data=&output_data.; by output_order output_suborder; run;
data &output_data.; 
 set &output_data.; 
 if missing(xx_description) then xx_description="Missing";
 if xx_variable=xx_var_label then xx_var_label=" ";
run;

*Count the total number of categories;
proc sql; select count(distinct &group_by.) into: groupby_ncat 
 from yourdata2 where &group_by. is not missing; 
quit;

*determine when ptrend test is needed for nomissing categories;
%if &group_by.^= and &group_by_missing.=0 and &trendtest.=1 and &groupby_ncat.>=3 %then %do;
%ptrendtest; 
proc sort data=&output_data.; by xx_variable_original xx_description; run;
proc sort data=xx_ptrend_final; by xx_variable_original xx_description; run;
data &output_data.;
 merge &output_data.(in=a) xx_ptrend_final;
 by xx_variable_original xx_description; 
 if a;
 if xx_description="Continuous" and xx_type^="mean and std" then ptrend=" ";
run;
proc sort data=&output_data.; by output_order output_suborder; run;
%end;

data &output_data.; 
  set &output_data.;
  drop xx_variable_original output_order output_suborder; 
run;

*Check whether there is any missing value in the group-by variable.;
proc sql; 
 select count(*) into: nmiss1 from &yourdata.; 
 select count(*) into: nmiss2 from yourdata2; 
quit;
%let nmiss=%eval(&nmiss1.-&nmiss2.);
%let nmiss=%trim(&nmiss.);

*Delete unnecessary datasets;
proc datasets library=work; 
 delete orders table_orders table_suborders yourdata2 temp: tempptrend: xx_ptrend: xx_tempptrend:;
run;

*Output the final summary table;
ods listing close;
options missing="" orientation=landscape number date;
ods rtf bodytitle style=statistical file="&output_data..rtf";

title "Descriptive Table by Variable &byvar_old.";
%if &group_by_missing.=0 %then %do;
   footnote "NOTE: there are &nmiss. observations deleted due to missing values in the variable &byvar_old.";
%end;

proc print data=&output_data.(drop=xx_var_label) label; run;
/**/
/* %if &n_byvar.>1 %then %do;*/
/* var xx_variable xx_description xx_type xx_var_all xx_var1-xx_var&n_byvar. ptrend;*/
/* %end;*/
/* %else %if &n_byvar.=1 %then %do;*/
/* var xx_variable xx_description xx_type xx_var_all xx_var1;*/
/* %end;*/
/**/
/*run;*/

ods rtf close;
title;footnote;
%end;
%mend;

