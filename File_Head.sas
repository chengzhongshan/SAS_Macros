%macro File_Head(filename,n);
data _temp_;
  length Line $500.;
   infile &filename dsd obs=&n;
   line=_infile_;
   *SNP=prxchange("s/.*(rs\d+).*/$1/",-1,_infile_);
   input line;
run;
proc print data=_temp_;run;
%mend;
/*usage demo:
 *for spaces separated file, the dsd in the infile statment is needed;
%File_Head(filename="I:\SASGWASDatabase\Important_Analysis_Codes\Step 1 ensembl SQL data\variation_feature.txt",n=10);
*/
