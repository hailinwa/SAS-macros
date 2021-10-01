%macro mcompare(base=, comp=, id=, compress=%str('|'), objectOID=, tracking=true, maxprint=100);
%********************************************************************************************************************************;
%*                                                                                                                              *;
%*  MACRO: MCOMPARE                                                                                                             *;
%*                                                                                                                              *;
%*  AUTHOR: Max Ma                                                                                                              *;
%*  DATE: 2015-02-23  updated 2016-12-14 to compare number of variables for datasets                                            *;
%*  MODIFIED: Weijie Yang on 2020-09-23                                                                                         *;
%*  REASON OF MODIFIED: Assign 100% if both primary and qc datasets are 0 records                                               *;                                                          *;
%*  PARAMETERS: base=primary dataset                                                                                            *;
%*              comp=secondary validation dataset                                                                               *;
%*              compress=ignore some character for comparison                                                                   *;
%*                                                                                                                              *;
%*  DESCRIPTION:                                                                                                                *;
%*    This macro is used to compare primary and secondary datasets                                                              *;
%*                                                                                                                              *;
%*  EXAMPLE:                                                                                                                    *;
%*    **validation dataset**                                                                                                    *;
%*  data final(keep=variable value);                                                                                            *;
%*    retain skip row variable value;*keep same order as primary                                                                *;
%*    set final;                                                                                                                *;
%*  run;                                                                                                                        *;
%*                                                                                                                              *;
%*  **primary dataset for Table 1.1**                                                                                           *;
%*  data primary;                                                                                                               *;
%*    set val.t0101;                                                                                                            *;
%*    if _BREAK_='';                                                                                                            *;
%*  run;                                                                                                                        *;
%*                                                                                                                              *;
%*%mcompare(base=primary, comp=final, compress=%str('|'));                                                                      *;
%********************************************************************************************************************************;
 %let pct=0;
 %let max_len=0;
 %let nbasev=0;
 %let ncompv=0;

  %if not %sysfunc(exist(&base)) %then %do;  /*no primary dataset*/
     %let pct=??;
     %goto exit2;
  %end;

 /*****conver dataset first*****/
 data base;
   set &base;
 run;

 data comp;
   set &comp;
 run;

/*=====sub macro to convert all numberic variables to character variables========*/
 %macro convert(ds=);
/*=====================convert all variable to characher and keep the same order==================*/
 proc contents data=&ds out=metads noprint;
 run;

 proc sort data=metads;
   by memname varnum;
 run;

 data metads(keep=memname name type length format sascode sasallvar);
**   length SASCODE $32767. oname $50. SASALLVAR $32767.;
     length SASCODE $5000. oname $50. SASALLVAR $5000.;
     set metads end=eof;
     by memname varnum;

    retain SASALLVAR;
    oname=strip(name); /*original variable name*/
    if first.memname then do;
        SASALLVAR='_x_all=' || 'strip(' || '_x_' || strip(oname) || ')';
        SASCODE="data " || strip(memname) || ";";
        output;
        SASCODE="    length _x_all $5000.;";
        output;
        SASCODE="    set " || strip(memname) || ";";
        output;
    end;else SASALLVAR=strip(SASALLVAR) || ' || strip(' || '_x_' || strip(oname) || ')';

   /*=======convert start==========*/
    if type=1 then do;
       SASCODE="    length _x_" || strip(oname) || " $" || strip(length) || ".;";
       output;
       SASCODE="    _x_" || strip(oname) || " = strip(put(" || strip(oname) || ",?? best.));";
       output;
     end;

    if type=2 then do;
       SASCODE="    length _x_" || strip(oname) || " $" || strip(length) || ".;";
       output;
       SASCODE="    _x_" || strip(oname) || " = strip(" || strip(oname) || ");";
       output;
     end;

   /*=======convert end==========*/
    if last.memname then do;
      SASCODE="    " || strip(SASALLVAR) || ';';
      output;
      SASCODE="    _x_all=strip(compress(_x_all, &compress));" ;
      output;
      SASCODE="    _x_all=compress(_x_all);" ;
      output;
      SASCODE="    _x_all=strip(compress(_x_all, , 'kw'));" ;
      output;
      SASCODE="    if _x_all^='';" ;   /***keep no blank lines only**/
      output;
      SASCODE="run;";
      output;
    end;
 run;

 data _null_;
  set metads;
  call execute(SASCODE);
 run;
 %mend convert;

 %let _type=%upcase(%scan(&objectOID, 1, ' '));

 %if &_type = TABLE or &_type = LISTING or &_type = FIGURE %then %do;
   %convert(ds=base);
   %convert(ds=comp);

  /*calculate percentage of matchs*/
   proc sql noprint;
      select strip(put(count(*), best.)) into:nbase from base;
      select strip(put(count(*), best.)) into:ncomp from comp;
    quit;

   %let nmax=&nbase;
   %if %eval(&nbase) < %eval(&ncomp) %then %let nmax = &ncomp;

   %if &nbase=0 and &ncomp=0 %then %do;
      data outcomp;
       _OBS_=.;
       _TYPE_='';
       _x_all="Both datasets &base and &comp are empty.";
       output;
     run;
  %let pct=0;
  %goto exit1;
 %end;
 %else %if &nbase=0 and &ncomp=0 %then %do;
      data outcomp;
       _OBS_=.;
       _TYPE_='';
       _x_all="Dataset &base and &comp is empty.";
       output;
     run;
   %let pct=100;
   %goto exit1;
 %end;
 %else %if &nbase=0 or &ncomp=0 %then %do;
      data outcomp;
       _OBS_=.;
       _TYPE_='';
       _x_all="Dataset &base or &comp is empty.";
       output;
     run;
   %let pct=0;
   %goto exit1;
 %end;

   data _comb_;
     set base comp;
   run;

   proc sql noprint;
     select max(length(_x_all)) into: max_len from _comb_;
   quit;

    PROC COMPARE BASE=base(keep=_x_all) COMPARE=comp(keep=_x_all) OUT=outcomp OUTNOEQUAL OUTBASE OUTCOMP OUTDIF NOPRINT criteria=0.0001;
    RUN;
 %end;%else %do;
  /*calculate percentage of matchs*/
    proc sql noprint;
      select strip(put(count(*), best.)) into:nbase from &base;
      select strip(put(count(*), best.)) into:ncomp from &comp;
      select strip(put(count(*), best.)) into:nbasev separated by '' from dictionary.columns where upcase(libname)=upcase(scan("&base",1,'.')) and upcase(memname)=upcase(scan("&base",2,'.'));
      select strip(put(count(*), best.)) into:ncompv separated by '' from dictionary.columns where upcase(libname)=upcase(scan("&comp",1,'.')) and upcase(memname)=upcase(scan("&comp",2,'.'));
    quit;

   %let nmax=&nbase;
   %if %eval(&nbase) < %eval(&ncomp) %then %let nmax = &ncomp;

   %if &id^=  %then %do;
      PROC SORT DATA=&base out=_base;
         by &id;
      RUN;

      PROC SORT DATA=&comp out=_comp;
         by &id;
      RUN;

      PROC COMPARE BASE=_base COMPARE=_comp OUT=outcomp OUTNOEQUAL OUTBASE OUTCOMP OUTDIF LISTALL criteria=0.0001;
         id &id;
      RUN;
   %end;

   %if &id=  %then %do;
      PROC COMPARE BASE=&base COMPARE=&comp OUT=outcomp OUTNOEQUAL OUTBASE OUTCOMP OUTDIF LISTALL criteria=0.0001;
      RUN;
   %end;
 %end;

 proc sql noprint;
   create table _outcomp as select distinct _obs_ from outcomp;
   select strip(put(count(_obs_), best.)) into:ndif from _outcomp;
 quit;

 %if &ndif=0 %then %do;
    data outcomp;
     _OBS_=.;
     _TYPE_='';
     _x_all="All values compared are exactly equal.";
     output;
   run;

    %let pct=100;
    %if &pct=100 and (%eval(&nbasev) ^= %eval(&ncompv)) %then %let pct=<100;

    %goto exit1;
  %end;

  %let pct=%eval(100-100*&ndif/&nmax);
  %if &pct=100 and &ndif>0 %then %let pct=<100;

  %put "====&nbase==== &ncomp====&nmax====&ndif======&pct=";

  %if not (&_type = TABLE or &_type = LISTING or &_type = FIGURE)  %then %do;
     title1 "&objectOID BASE=&base (nobs = %sysfunc(compress(&nbase)) nvars = %sysfunc(compress(&nbasev)))      COMP=&comp (nobs = %sysfunc(compress(&ncomp)) nvars = %sysfunc(compress(&ncompv)))";
     title2 "&pct% values are matched";

     data outcomp;
       set outcomp end=eof;
       if eof;
       _obs_=.;
       _type_='';
       _x_all="Check outputs from the COMPARE procedure.";
     run;

     %*goto exit2;
  %end;

  data outcomp;
    set outcomp end=eof;
    if _type_='DIF' and &max_len>0 then _x_all=substr(_x_all, 1, &max_len);
    if _n_ <= &maxprint;
  run;

%exit1:

 title1 "&objectOID BASE=&base (nobs = %sysfunc(compress(&nbase)))      COMP=&comp (nobs = %sysfunc(compress(&ncomp)))";
 title2 "&pct% values are matched";
 title3 "Comparison Results - Non-matching records";

 options nocenter nobyline;
 proc report data=outcomp nowd headline headskip spacing=1 missing formchar(2)='_' split='|' ls=200;
   col("&mline" _OBS_ _TYPE_ _x_all);
   define _OBS_ / order order=internal width=10 "RECORD #" left spacing=0;
   define _TYPE_ / order order=internal width=10 "SOURCE";
   define _x_all / display '' width=100 "COMPARED TEXT STRING" left flow;
   break after _OBS_/skip;
 run;

%exit2:

 /*call macro mtracking to tracking*/
 %if &tracking=true %then %do;
     %mtracking(objectOID=&objectOID, type=secondary, match=&pct%);
 %end;

%mend mcompare;
