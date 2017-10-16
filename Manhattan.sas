* macro that can be used later to generate symbols for plots with two alternating colors;
%macro twocolors(c1,c2);
%do j=1 %to 21 %by 2;
symbol&j v=dot &chr_var=&c1;
symbol%eval(&j+1) v=dot &chr_var=&c2;
%end;
%mend;

%macro manhattan(dsdin,pos_var,chr_var,P_var,logP);
/**fake data;*/
/*data manhattan ;*/
/*Fake_position=1; */
/*do &chr_var=1 to 22;*/
/*do _n_=1 to ( 1e6 - &chr_var * 10000 ) - 1 by 1000 ;*/
/*   Fake_position + _n_ / 1e6 ;*/
/*   logp = -log( ranuni(2)) ;*/
/*   output ;*/
/*end;*/
/*end;*/
/*run;*/

*real data;
proc sort data=&dsdin;
by &chr_var &pos_var;
run;
data manhattan ;
set &dsdin;
Fake_position=1; 
Fake_position + _n_ / 1e3 ;
run;


data manhattan;
set manhattan;
%if (&logP=1) %then %do;
logp=-log10(&P_var);
%end;
%else %do;
logp=&P_var;
%end;
run;

proc sort data=manhattan;
by &chr_var Fake_position;
run;

 
*find maximum value for the x-axis, store in a macro variable;
proc sql noprint;
select 1.005*ceil(max(Fake_position)) into :maxbp 
from manhattan;
quit;
 
* 
find mean of BP within each chromosome (C)
used later to position x-axis labels
;
proc summary data=manhattan nway;
class &chr_var;
var Fake_position;
output out=mbp mean=;
run;
 
* 
annotate data set used to add x-axis labels
"manually" add the frame around the graph
possibly add a horizontal reference line
;
data anno ;
retain position '8' xsys ysys '2' y 0 function 'label' text 'xx';
do until (last1);
   set mbp (keep = Fake_position &chr_var) end=last1;;
   x = round(Fake_position) ;
   text = cat(&chr_var);
   output;
end;
 
* top of frame;
xsys = '1'; ysys = '1'; function = 'move'; x = 0; y=100; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
* bottom of frame;
xsys = '1'; ysys = '1'; function = 'move'; x = 0; y=0; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
 
* horizontal reference line (if needed for 5x10-08);
xsys = '1'; ysys = '2'; function = 'move'; x = 0; y=7.3; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
run;
 
* reset all then set some graphics options;
goptions reset=all ftext='calibri' htext=2 gunit=pct 
         dev=gif xpixels=1800 ypixels=800 gsfname=gout;
 
* make some room around the plot (white space);
title1 ls=2;
title2 a=90 ls=2;
title3 a=-90 ls=2;
footnote1 ls=2;
 
* let SAS choose the colors;
symbol1 v=dot r=22;
 
* two alternating colors;
* gray-scale;
*%twocolors(gray33,graycc);
* blue and blue-green;
*%twocolors(cx2C7FB8,cx7FCDBB);
 
* suppress drawing of any x-axis feature;
axis1 value=none major=none minor=none label=none style=0;
* rotate y-axis label;
axis2 label=(angle=90 "-Log10(p)");
 
* destination for the plot;
filename gout './manhattan1.gif';
 
* use PROC GPLOT to create the plot;
proc gplot data=manhattan ;
plot logp*Fake_position=&chr_var / haxis = axis1
                 vaxis = axis2
                 href  = &maxbp
                 annotate = anno
                 nolegend
		         noframe
;
run;
%mend;

/*

%Import_Space_Separated_File(abs_filename=E:\Yale_GWAS\ALL_GWGO_HCE_EA_combined_meta.txt,
                             firstobs=1,
							 getnames=yes,
                             outdsd=Assoc);


%manhattan(dsdin=sasuser.Assoc,
           pos_var=Pos,
           chr_var=Chr,
           P_var=P,
           logP=1);
*/




/*
<placed after first data step>
* add some fake info to the data set (SNP name and a p-value);
data manhattan;
set manhattan;
snp_name = cats('rs',_n_);
if ranuni(0) lt .0005 then p_value = 10e-6;
else p_value = 0.1;
run;
 
<modified data step to create the annotate data set>
* 
annotate data set used to add x-axis labels
"manually" add the frame around the graph
possibly add a horizontal reference line
add labels to selected points;
;
data anno ;
length color $8 text $25;
retain position '8' xsys ysys '2' y 0 function 'label' when 'a';
do until (last1);
   set mbp (keep = bp c) end=last1;;
   x = round(bp) ;
   text = cat(c);
   output;
end;
 
* top of frame;
xsys = '1'; ysys = '1'; function = 'move'; x = 0; y=100; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
* bottom of frame;
xsys = '1'; ysys = '1'; function = 'move'; x = 0; y=0; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
 
* horizontal reference line (if needed);
xsys = '1'; ysys = '2'; function = 'move'; x = 0; y=4; output;
xsys = '2'; function = 'draw'; x = &maxbp ; output;
 
* this portion adds labels for points with p_value le 10e-6;
function = 'label';
hsys = '3';
size = 1.5;
position = '5';
cbox = 'white';
color = 'blue';
do until (last2);
   set manhattan end=last2;
   where p_value le 10e-6;
   x = bp;
   y = logp;
   text = snp_name;
   output;
end;
 
run;
*/
