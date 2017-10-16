
%macro ImportAllMacros(MacroDir,fileRgx);
%put Macro Dir is &MacroDir;
%put Your system is &sysscp;

%if %sysfunc(prxmatch(/WIN/,&sysscp)) %then %do;
 filename M pipe "dir &MacroDir";
 data tmp;
 length filename $2000.;
 infile M lrecl=32767;
 input;
 filename=_infile_;
 filename=prxchange('s/.*\s+([\S]+\.sas)/$1/',-1,filename);
 filename="&MacroDir\"||filename;
 if prxmatch('/\.sas/',filename);
 run; 
%end;
%else %do;
 filename M pipe "ls &MacroDir";
 data tmp;
 length filename $2000.;
 infile M;
 input filename $;
 filename="&MacroDir/"||filename;
 run;
%end;

data tmp;
set tmp;
*where filename contains '.sas';
if prxmatch("/&fileRgx/i",filename) and prxmatch("/\.sas/oi",filename) and 
   not prxmatch("/\.sas\.bak/i",filename) and not prxmatch("/ImportAllMacros.sas/",filename);
run;

/*proc print;run;*/
/*options mprint mlogic symbolgen;*/

data _null_;
set tmp;
call execute('%put Now try to import the macro: '|| left(strip(filename)));
call execute('%include "' ||left(strip(filename))||'";');
call execute('%put The import is OK for the macro: '|| left(strip(filename)));
run;

title 'Loaded Macros into SAS';
proc print data=tmp;
run;


%mend;

/*options mprint mlogic symbolgen;*/

/*

%ImportAllMacros(MacroDir=E:\360yunpan\SASCodesLibrary\SAS-Useful-Codes\Macros,fileRgx=Import);

*/
