%macro towide(longdata,widedata,id,suffix,suffixlo,suffixhi,vars,types=,lengths=,numprint=,quiet=,sorted=);
  /* 
  longdata - name of input data file that is in long form
  widedata - name of output data file that will be in wide form
  id       - variable that uniquely identifies the wide records
  suffix   - variable name for the suffix of the wide variables
  suffixlo - the lowest value of the suffix variable
  suffixhi - the highest value of the suffix variable
  vars     - a list of the base names to transpose

  optional parameters
    types    - (default, all numeric) 
               A list of the types of the long variables separated by spaces.
               N for numeric and C for character.  
    lengths  - (default 8 for each var)
               a list of the lengths of the long variables separated by spaces.
    numprint - (default 10) 
               Number of wide records to print for verification
    quiet    - suppress PROC PRINT and PROC MEANS for verification
               NO (default) - display proc print and proc means
               YES - suppress display of proc print and proc means 
    sorted   - whether the input (long) data file is already sorted by id
               NO (default) - assumes longdata is not sorted and needs to
                              be sorted by id
               YES - assumes longdata is already sorted and does not sort
                     it again ;
  */

  %if &numprint =  %then %let numprint = 10 ;
  %if &quiet    =  %then %let quiet    = NO ;
  %let quiet = %upcase(&quiet) ;

  * count up how many variables in vars, store in VARNUM;
  %let varnum=%eval(1);
  %let varname = %scan(&vars,&varnum," ");
  %do %until (&varname = );
    %let varnum=%eval(&varnum + 1);
    %let varname = %scan(&vars,&varnum," ");
  %end;
  %let nvars = %eval(&varnum - 1);

  * get the last variable from the id, sore in LASTID ;
  %let idnum=%eval(1);
  %let idname = %scan(&id,&idnum," ");
  %do %until (&idname =  );
    %let idnum=%eval(&idnum + 1);
    %let idname = %scan(&id,&idnum," ");
  %end;
  %let nids = %eval(&idnum - 1);
  %let lastid = %scan(&id,&nids);

  %put ------------------------------------------------------------------ ;
  %put - Reshape Long to Wide: Actions to taken summarized below          ; 
  %put ------------------------------------------------------------------ ;
  %put - Input  (long) data file is                        : &longdata    ;
  %put - Output (wide) data file is                        : &widedata    ;
  %put - Variable that uniquely identifies wide records is : &id          ;  
  %put - Variable name with suffix for wide variable is    : &suffix      ;
  %put - and can range from : &suffixlo to &suffixhi ;
  %put - Long variables to wide variables... ;

  %if &sorted EQ YES %then %let infile = &longdata;
  %if &sorted NE YES %then 
  %do;
    %let infile = _123456_ ;
    proc sort data=&longdata out=_123456_ ;
      by &id ;
    run;
  %end;

  * start the data step.  Make widedata from longdata by id;
  data &widedata ;
    set &infile ;
    by &id ; 

    * make ARRAY, RETAIN and DROP for each variable in VARS ;
    * *********************************************;
    %do varnum = 1 %to &nvars ;
      %let varname = %scan(&vars   ,&varnum," ");
      %let vartype = %scan(&types  ,&varnum," ");
      %let varlen  = %scan(&lengths,&varnum," ");
      %if &vartype =   %then %let vartype = N;
      %if &varlen  =  %then %let varlen  = 8;
      %if &vartype = C %then %let dollar = $; %else %let dollar = ;

      array _array&varnum(&suffixlo:&suffixhi) 
            &dollar &varlen &varname&suffixlo - &varname&suffixhi ;
      retain &varname&suffixlo - &varname&suffixhi ;
      drop   &varname ;

      %put -  &varname  -> &varname&suffixlo ... &varname&suffixhi -> of type &vartype and length &varlen ;
    %end;
    * *********************************************;

    * if first record of group, then initialize all to . or " " ;
    if first.&lastid then
    do;
      do _i_ = &suffixlo to &suffixhi;

        * *********************************************;
        %do varnum = 1 %to &nvars ;
          %let varname = %scan(&vars,&varnum," ");
          %let vartype = %scan(&types  ,&varnum," ");
          %if &vartype =   %then %let vartype = N;
          %if &vartype = C %then %let dot = " "; %else %let dot = .;

          _array&varnum(_i_) = &dot ;
    
        %end;
        * *********************************************;
      end;
    end;

    * store wide variable into narrow variable ;
    * *********************************************;
    %do varnum = 1 %to &nvars ;
      %let varname = %scan(&vars,&varnum," ");

      _array&varnum(&suffix) = &varname ;

    %end;
    * *********************************************;

    * if last variable of group, output the record ;
    if last.&lastid then output ;

    * drop _i_ and suffix variable ;
    drop _i_ &suffix ;

  run;


  %if &quiet NE YES %then
  %do;

    title1 "1A. Verify reshaping of long to wide using PROC PRINT of &numprint wide records.";
    title2 "    Compare this print of the long data below (&longdata) with the "        ; 
    title3 "    PROC PRINT of the wide data (&widedata) in step 1B below."   ;
    title5 "PROC PRINT of LONG data file &longdata";
    proc print data=&longdata(obs= %eval(&numprint*(&suffixhi-&suffixlo+1))  );
      var &id &suffix &vars ;
    run;


    title1 "1B. Verify reshaping of long to wide using PROC PRINT of &numprint wide records";
    title2 "    Compare this print of the wide data (&widedata) below with the "        ; 
    title3 "    PROC PRINT of the long data (&longdata) in Step 1A above"   ;
    title5 "PROC PRINT of WIDE data file &widedata";
    proc print data=&widedata(obs=&numprint);
      var &id 
      %do varnum = 1 %to &nvars ;
        %let varname = %scan(&vars,&varnum," ");
        &varname&suffixlo - &varname&suffixhi
      %end;
      ;
    run;

    title1 "2A. Verify reshaping of long to wide using PROC MEANS";
    title2 "    Compare this PROC MEANS for the long data (&longdata) with the "        ; 
    title3 "    PROC MEANS of the wide data (&widedata) in step 2B below"   ;
    title5 "PROC MEANS of LONG data file &longdata";
    proc means data=&longdata;
      class  &suffix;
      var 
      %do varnum = 1 %to &nvars ;
        %let varname = %scan(&vars,&varnum," ");
        %let vartype = %scan(&types  ,&varnum," ");
        %if &vartype NE C %then &varname ;
      %end;
      ;
    run;

    title1 "2B. Verify reshaping of long to wide using PROC MEANS";
    title2 "    Compare this PROC MEANS for the wide data (&widedata) below" ;
    title3 "    with the PROC MEANS of the long data (&longdata) in step 2A above"    ;
    title5 "PROC MEANS of WIDE data file &widedata";
    proc means data=&widedata;
      var 
      %do j = &suffixlo %to &suffixhi ;
        %do varnum = 1 %to &nvars ;
          %let varname = %scan(&vars,&varnum," ");
          %let vartype = %scan(&types  ,&varnum," ");
          %if &vartype NE C %then &varname&j ;
        %end;
      %end;
      ;
    run;
  %end;

%mend towide;
