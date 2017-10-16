%macro QQplot(dsdin,P_var);
proc sql noprint;
select count(*) into: tot
from &dsdin;

proc sort data=&dsdin;
by &P_var;
run;

data dataout;
set &dsdin;
retain x_;
gap=1/&tot;
ini=gap/2;
if _n_=1 then x_=ini;
else x_=x_+gap;
x=-log10(x_);
y=-log10(&P_var);
run;

proc sql noprint;
select ceil(max(y)) into: max_y
from dataout;
/*make sure to put reset here*/
/*otherwise symbol2 will not work*/
goptions reset=global hsize=15cm vsize=15cm;
symbol1 interpol=none value=dot color=maroon;
symbol2 interpol=rl value=none color=black;
axis1 label=(f='arial/bo' h=1.9 'Expected -log10(P)' justify=c)
      order=(0 to &max_y by 1) value=(f='arial' h=1.9);
axis2 label=(a=90 f='arial/bo' h=1.9 'Observed -log10(P)')
      order=(0 to &max_y by 1) value=(f='arial' h=1.9);
proc gplot data=dataout;
plot y*x x*x/overlay 
           haxis=axis1
           vaxis=axis2
           noframe;
run;
%mend;
/*

%QQplot(dsdin=sasuser.assoc,P_var=P);

*/
