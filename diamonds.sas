libname sasdata "G:\Classwork\BIA-652\SAS\SAS_DATA"; run;
proc copy in=sasdata out=work;
   select traindia1;
run;
proc surveyselect data=sasdata.rawdiam out=traindia method=srs samprate=.5;
run;
proc copy in = sasdata out=work;
select rawdiam; run;
proc contents data=traindia;
run;
proc means data = traindia;
run;
ods graphics on;
proc corr data=traindia1 plots=matrix(histogram);
run;
ods graphics off;



/* EDA */



/*
data traindia; set traindia; if cut = Fair then do; cut_Fair=1; cut_Good=0; cut_Ideal=0; cut_Premium=0; end;
if cut = Good  then do; cut_Fair=0; cut_Good=1; cut_Ideal=0; cut_Premium=0; end;
if cut = Ideal then do; cut_Fair=0; cut_Good=0; cut_Ideal=1; cut_Premium=0; end;
if cut = Premium then do; cut_Fair=0; cut_Good=0; cut_Ideal=0; cut_Premium=1; end;
run; */
data traindia; set traindia; /* creating dummy variables for cut*/
if cut = 'Fair' then cut_Fair=1;else cut_Fair=0;
if cut = 'Good'  then  cut_Good=1;else cut_Good=0;
if cut = 'Ideal' then  cut_Ideal=1;else cut_Ideal=0;
if cut = 'Premium' then cut_Premium=1;else cut_Premium=0;
run;

/*if condition =  then do; con_1=0; con_2=0; con_3=0;con_4=0;con_5=1; end; */
data traindia; set traindia; /* creating dummy variables for color*/
if color = 'D' then  color_D=1; else color_D=0; 
if color = 'E' then  color_E=1; else color_E=0; 
if color = 'F' then  color_F=1; else color_F=0; 
if color = 'G' then  color_G=1; else color_G=0; 
if color = 'H' then  color_H=1; else color_H=0; 
if color = 'I' then  color_I=1; else color_I=0; 
run;
data traindia; set traindia; /* creating dummy variables for clarity*/
if clarity = 'I1'   then  clarity_I1=1; else clarity_I1=0; 
if clarity = 'IF'   then  clarity_IF=1; else clarity_IF=0;  
if clarity = 'SI1'  then  clarity_SI1=1; else clarity_SI1=0; 
if clarity = 'SI2'  then  clarity_SI2=1; else clarity_SI2=0; 
if clarity = 'VS1'  then  clarity_VS1=1; else clarity_VS1=0; 
if clarity = 'VS2'  then  clarity_VS2=1; else clarity_VS2=0; 
if clarity = 'VVS1' then  clarity_VVS1=1; else clarity_VVS1=0; 
run;

DATA traindia2; /* dropping the variables */
set traindia (drop = VAR1 clarity cut color  );
run;
proc contents data=traindia2;
run;

Proc print data= traindia1;
title'test';
run;

proc reg data=traindia1  outest=traindia1_out ;  /* forward selection*/
     model price = carat 
                        /dwProb;
      OUTPUT OUT=reg_outdia  PREDICTED=   RESIDUAL=  L95M=  U95M=  L95= U95=
       rstudent=C_rstudent h=lev cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
quit;



proc reg data= traindia1  outest= traindia1_out;
     model      price =  carat /   dwProb   ;
      OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=   L95M=  U95M=  L95= U95=
       rstudent= h= cookd=  dffits=
     STDP=  STDR=  STUDENT=    ;  
     
  quit;
proc Standard data=traindia1 MEAN=0 STD=1 OUT=traindata1;
VAR carat depth table x y z ;
run;

  proc reg data=outdata_z plots(maxpoints=none) ;
model price = carat depth table x y z cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 / dwProb  ;
OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=leverage cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
run;
data outdata1;

set outdata;
if abs(r) >2 then delete;
run;

proc copy in = sasdata out=work;
select outdata_z; run;
DATA outdata_z; /* dropping the variables */
set outdata_z (drop = Studentized Residual without Current Obs  );
run;

proc reg data=outdata_z plots(maxpoints=none) ;
model price = carat depth table x y z cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 / dwProb VIF selection = stepwise slentry=.05 slstay= .05  ;
OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=leverage cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
run;

proc reg data=outdata_z plots(maxpoints=none) ;
model price = carat depth table x y z cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 / dwProb VIF selection = MAXR  ;
OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=leverage cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
run;

/* remove carat, x, y, z due to multicollinearity as the VIF is very high, removed clarity_VVS1 as it is insignificant*/
proc reg data=outdata_z plots(maxpoints=none) ;
model price = depth table carat cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
 ;
OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=leverage cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
run;

proc princomp data=outdata_z out=outdata_pca ;
VAR price  carat depth table x y z cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1;
run;

proc reg data=outdata_pca plots(maxpoints=none) ;
model price = Prin1- Prin13 ;
OUTPUT OUT=outdsn  PREDICTED=   RESIDUAL=Res   L95M=C_l95m  U95M=C_u95m  L95=C_l95 U95=C_u95
       rstudent=C_rstudent h=leverage cookd=Cookd  dffits=dffit
     STDP=C_spredicted  STDR=C_s_residual  STUDENT=C_student     ;  
run;


proc sgscatter data=traindia;

  title "Scatterplot Matrix for Diamonds";
  matrix x y z table price depth cut color clarity carat;
run;
proc univariate data= outdata_z;
var price;
run;

data price_cat;

set outdata_z;
if price <= 926  then category= 'cheaper';
if price >=927 and price <=2262  then category= 'affordable';
if price >=2263 and price <=4900  then category= 'expensive';
if price >=4900 and price <=18823  then category= 'mostexpensive';

run;

proc fastclus data = price_cat out = outdata_cluster maxclusters= 4;
var carat depth category table x y z cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1;
RUN;
proc copy in = sasdata out=work;
select price_cat; run;

proc cluster data = price_cat method= ward print= 15 ccc pseudo plots (maxpoints=26000);
var carat depth table cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1;
copy category;
run;

proc tree noprint ncl=4 out=out;
copy carat depth table cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 category;
run;

proc fastclus data = price_cat
   maxclusters=4   out=KC_out; 
var  carat depth table cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 ;
id  category ;
run;
proc fastclus data=price_cat out=clust maxclusters=4;
      var carat depth table cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1; 
id category;
   run;

   proc candisc out=can;
      var carat depth table cut_Fair cut_Ideal cut_Premium cut_Good color_D color_E color_F color_G color_H color_I clarity_I1 clarity_IF clarity_SI1 clarity_SI2 clarity_VS1 clarity_VS2
clarity_VVS1 ;
      class cluster;
   run;

   proc plot;
      plot Can  * cluster;
   run;


proc plot data = can;
run;
