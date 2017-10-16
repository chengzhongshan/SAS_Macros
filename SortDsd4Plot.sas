%macro SortDsd4Plot(dsd,ByFmtVar,OutFmtVar,OutDsd);
proc rank data=&dsd out=_dsd_;
var &ByFmtVar;
ranks rank_&ByFmtVar;
run;
proc sort data=_dsd_;
by rank_&ByFmtVar;
run;
data _dsd_;
set _dsd_;
rank_&ByFmtVar=_n_;
run;
proc sql;
create table &OutDsd as
select a.*,put(b.rank_&ByFmtVar,8.) as &OutFmtVar
from &dsd as a
natural join 
_dsd_ as b
order by rank_&ByFmtVar;
proc datasets lib=work nolist;
delete _dsd:;
run;
%mend;


/*
data tmp;
set sashelp.cars(obs=10);
run;

options mprint mlogic symbolgen;
%SortDsd4Plot(dsd=tmp,
              ByFmtVar=Horsepower,
              OutFmtVar=x,
              OutDsd=New);
*/
