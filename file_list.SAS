/******************************************************************************
** PROGRAM:  ISDIR.SAS
**
** DESCRIPTION: DETERMINES IF THE SPECIFIED PATH EXISTS OR NOT.
**              RETURNS: 0 IF THE PATH DOES NOT EXIST OR COULD NOT BE OPENED.
**                       1 IF THE PATH EXISTS AND CAN BE OPENED.
**
** PARAMETERS: iPath: THE FULL PATH TO EXAMINE.  NOTE THAT / AND \ ARE TREATED
**                    THE SAME SO &SASDIR/COMMON/MACROS IS THE SAME AS
**                    &SASDIR\COMMON\MACROS.
**Please see the demo at the end of the macro
******************************************************************************/

%macro isDir(iPath=,iQuiet=1);
  %local result dname;

  %let result = 0;

  %if %sysfunc(filename(dname,&iPath)) eq 0 %then %do;
    %if %sysfunc(dopen(&dname)) %then %do;
      %let result = 1;
    %end;
    %else %if not &iQuiet %then %do;
      %put ERROR: ISDIR: %sysfunc(sysmsg());
    %end;
  %end;
  %else %if not &iQuiet %then %do;
    %put ERROR: ISDIR: %sysfunc(sysmsg());
  %end;

  &result

%mend;

*%put %isDir(iPath=&sasdir/common/macros);
*%put %isDir(iPath=&sasdir/kxjfdkebnefe);
*%put %isDir(iPath=&sasdir/kxjfdkebnefe, iQuiet=0);
*%put %isDir(iPath=L:\Macros);

/******************************************************************************
** PROGRAM:  FILE_LIST.SAS
**
** DESCRIPTION: RETURNS THE LIST OF FILES IN A DIRECTORY SEPERATED BY THE
**              SPECIFIED DELIMITER. RETURNS AN EMPTY STRING IF THE THE 
**              DIRECTORY CAN'T BE READ OR DOES NOT EXIST.
**
** PARAMETERS: iPath      : THE FULL PATH TO EXAMINE.  NOTE THAT / AND \ ARE 
**                          TREATED THE SAME SO &SASDIR/COMMON/MACROS IS THE 
**                          SAME AS &SASDIR\COMMON\MACROS. WORKS WITH BOTH UNIX 
**                          AND WINDOWS.
**             iFilter    : SPECIFY A BASIC FILTER TO THE FILENAMES, NO REGULAR 
**                          EXPRESSIONS OR WILDCARDS.
**             iFiles_only: 0=RETURN FILES AND FOLDERS
**                          1=RETURN FILES ONLY.
**             iDelimiter : SPECIFY THE DELIMITER TO SEPERATE THE RESULTS BY.
******************************************************************************/
/*
** TODO: DOESNT CATER FOR MACRO CHARS IN FILENAMES. FIX SOMETIME.
** TODO: IMPROVE THE FILTER. JUST A SIMPLE IF STATEMENT AT THE MOMENT.
*/
%macro file_list(iPath=, iFilter=, iFiles_only=0, iDelimiter=|);
  %local result did dname cnt num_members filename;

  %let result=;

  %if %sysfunc(filename(dname,&iPath)) eq 0 %then %do;

    %let did = %sysfunc(dopen(&dname));
    %let num_members = %sysfunc(dnum(&did));

    %do cnt=1 %to &num_members;
      %let filename = %sysfunc(dread(&did,&cnt));
      %if "&filename" ne "" %then %do;
        %if &iFiles_only %then %do;
          %if not %isDir(iPath=&iPath/&filename) %then %do;
            %if "&iFilter" ne "" %then %do;
              %if %index(%lowcase(&filename),%lowcase(&iFilter)) %then %do;
                %let result = &result%str(&iDelimiter)&filename;
              %end;
            %end;
            %else %do;
              %let result = &result%str(&iDelimiter)&filename;
            %end;
          %end;
        %end;
        %else %do;
          %if "&iFilter" ne "" %then %do;
            %if %index(%lowcase(&filename),%lowcase(&iFilter)) %then %do;
              %let result = &result%str(&iDelimiter)&filename;
            %end;
          %end;
          %else %do;
            %let result = &result%str(&iDelimiter)&filename;
          %end;
        %end;
      %end;
      %else %do;
        %put ERROR: (CMN_MAC.FILE_LIST) FILE CANNOT BE READ.;
        %put %sysfunc(sysmsg());
      %end;
    %end;

  %end;
  %else %do;
    %put ERROR: (CMN_MAC.FILE_LIST) PATH DOES NOT EXIST OR CANNOT BE OPENED.;
    %put %sysfunc(sysmsg());
  %end;

  /*
  ** RETURN THE RESULT.  TRIM THE LEADING DELIMITER OFF THE FRONT OF THE RESULTS.
  */
  %if "&result" ne "" %then %do;
    %substr(&result,2)
  %end;

%mend; 



**Demo of file_list

** EXAMPLES - HAVENT TESTED THE LAST TWO YET BUT THEY SHOULD WORK IF SYNTAX IS CORRECT
*;

*%put %file_list(iPath=L:\Macros);
*%put %file_list(iPath=L:\Macros, iFilter=a);
*%put %file_list(iPath=L:\Macros,iFiles_only=1);
*
*
*data x;
*  file_list = "%file_list(iPath=L:\Macros)";
*  n=countc(file_list,"|");
*  	do i=1 to n+1;
*  	files=scan(file_list,i,"|");
*  	output;
*    end;
*drop file_list n i;
*run;
/*proc print data=x;run;*/


