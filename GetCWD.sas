%macro GetCWD;
/*Get current working directory;*/
%global cwd;
%let cwd=%qsubstr(
%sysget(sas_execfilepath),
1,
%length(%sysget(sas_execfilepath))-%length(%sysget(sas_execfilename))-1
);
%put %sysget(sas_execfilepath);
%put &cwd;
%mend;
/*Demo
%GetCWD;
%put &cwd;
*/
