/* Macro library for appendix tables
 * Maintainer: Zhenhuan Hu <zhu@mcw.edu>
 * Last change: Nov 04, 2016
 *
 * Functions and their uses:
 *
 * %compidx(indata =, setdate =, strata =, outdata =); Create table for completeness index (CIF).
 * %logranktest(indata =, varlst =, outdata =, title = nil); Logrank pvalue for both OS and DFS.
 * %contvar(indata =, varlst =, strata =, outdata =);
 *
 * Reference:
 * Clark TG, Altman DG, De Stavola BL. Quantification of the completeness of follow-up. Lancet. 2002 Apr 13;359(9314):1309-10.
 */

/* SECTION: completeness index of follow-up */

%macro compidx(indata =,		  /* Input sas dataset */
	      event = dead,		  /* Event of interest */
	      competerisk = cpnil,	  /* Competing risk */
	      setdate =,		  /* Cutoff date of the study, eg: mdy(12, 31, 2012) */
	      startdate = datetx,	  /* Start date of follow-up */
	      lastdate = survdt,	  /* Last date of follow-up */
	      strata = nil,		  /* Main comparing group */
	      timelist = 12 24 36 48,	  /* Time to evaluate follow-up */
	      outdata = compidx,	  /* Output file name */
	      rtftitle = "Completeness of follow-up",
	      showoverall = 1,		  /* Show overall follow-up */
              setdaterow = 0,
              style = default,
              target = rtf
	      );

  %if &strata = nil %then %let showoverall = 0;
  %if &target = rtf and &style = default %then %let style = kenyrtf;
  %else %if &target = xls and &style = default %then %let style = kenyxls;

  %* Count the # of obs by strata;
  data _cnt_init_;
    set &indata;
    %if &strata eq nil %then %do; nil = 1; %end;
    %else %do; where not missing(&strata); %end;
    keep &strata;
  run;

  proc sql noprint;
    select count(distinct &strata)
    into :colcnt trimmed from _cnt_init_;
    select count(&strata), mean(&strata) as value
    into :sttcnt1 - :sttcnt&colcnt, :sttvalue1 - :sttvalue&colcnt from _cnt_init_
    group by &strata order by value;
  quit;

  %* Init;
  proc format;
    value overall 1 = "N (%)";

  data _cpidx_init_;
    set &indata (keep = crid &event &startdate &lastdate 
    %if &strata ne nil %then %do; &strata %end;
    %if &competerisk ne cpnil %then %do; &competerisk %end;);
    %if &strata eq nil %then %do; nil = 1; format nil overall.; %end;
    %else %do; where not missing(&strata); %end;
    %if &competerisk eq cpnil %then %do; cpnil = 0; %end;

    setdate = &setdate;
    call symput("sdate", put(setdate, mmddyy8.));
    call symput("sttfmt", vformat(&strata));

    %* User defined set date;
    onstudy = (&lastdate - &startdate) / 30.4;
    limit = (setdate - &startdate) / 30.4;
    if onstudy < 0 then do;
      put "ERROR: CRID = " crid ", EVENT = &event, STARTDATE = " &startdate mmddyy10. ", LASTDATE = " &lastdate mmddyy10.;
      delete;
    end;
    else do;
      if onstudy > limit then do;
        %* If the last contact date is beyond set date, use set date for
        both observation time limit and potential time limit;
        obstm = limit; ptltm = limit;
      end;
      else if &event = 1 or &competerisk = 1 then do;
        %* Otherwise, if the patient is dead, use the last contact date for
        both observation time limit and potential time limit;
        obstm = onstudy; ptltm = onstudy;
      end;
      else do;
        %* Otherwise, if the patient is still alive, use the last contact dat for 
        observation time limit and set date for potential time limit;
        obstm = onstudy; ptltm = limit;
      end;

      %* Default set date: 1-4 years from tx;
      %let ntimepoints = %sysfunc(countw(&timelist, ' '));
      %do i = 1 %to &ntimepoints;
        %let time&i = %sysfunc(scan(&timelist, &i, ' '));
        if limit > &&time&i then do;
          if onstudy > &&time&i then do;
            obstm&i = &&time&i; ptltm&i = &&time&i;
          end;
          else if &event = 1 or &competerisk = 1 then do;
            obstm&i = onstudy; ptltm&i = onstudy;
          end;
          else do;
            obstm&i = onstudy; ptltm&i = &&time&i;
          end;
        end;
        %* If default set date > user defined set date, use the user defined date;
        else do;
          obstm&i = obstm;
          ptltm&i = ptltm;
        end;
      %end;
    end;
  run;

  %* Sum of the observed / potential follow-up times (overall);
  %if &showoverall %then %do;
    proc univariate data = _cpidx_init_ noprint;
      var obstm ptltm %do i = 1 %to &ntimepoints; obstm&i ptltm&i %end;;
      output out = _sum_overall_ sum = obsum ptlsm %do i = 1 %to &ntimepoints; obsum&i ptlsm&i %end;;
    run;

    data _sum_overall_;
      set _sum_overall_;
      cpidx = round((obsum / ptlsm) * 100);
      drop obsum ptlsm;
      %do i = 1 %to &ntimepoints;
	cpidx&i = round((obsum&i / ptlsm&i) * 100);
	drop obsum&i ptlsm&i;
      %end;
      overall = "overall";
    run;

    proc transpose data = _sum_overall_ out = _t_sum_overall_;
      var cpidx %do i = 1 %to &ntimepoints; cpidx&i %end;;
      id overall;
    proc sort data = _t_sum_overall_;
      by _name_;
    run;
  %end;

  %* Sum of the observed / potential follow-up time by strata;
  proc sort data = _cpidx_init_; by &strata;
  proc univariate data = _cpidx_init_ noprint; by &strata;
    var obstm ptltm %do i = 1 %to &ntimepoints; obstm&i ptltm&i %end;;
    output out = _sum_strata_ sum = obsum ptlsm %do i = 1 %to &ntimepoints; obsum&i ptlsm&i %end;;
  run;

  data _sum_strata_;
    set _sum_strata_;
    cpidx = round((obsum / ptlsm) * 100);
    drop obsum ptlsm;
    %do i = 1 %to &ntimepoints;
      cpidx&i = round((obsum&i / ptlsm&i) * 100);
      drop obsum&i ptlsm&i;
    %end;
  run;

  proc transpose data = _sum_strata_ out = _t_sum_strata_;
    var cpidx %do i = 1 %to &ntimepoints; cpidx&i %end;;
    id &strata;
    idlabel &strata;
  proc sort data = _t_sum_strata_;
    by _name_;
  run;

  data _cpidx_ (drop = _name_);
    %if &showoverall %then %do;
      merge _t_sum_strata_ _t_sum_overall_; by _name_;
    %end;
    %else %do;
      set _t_sum_strata_; by _name_;
    %end;

    %if &setdaterow %then %do;
      if _name_ = "cpidx" then varstr = "Set date: &sdate";
    %end;
    %else %do;
      if _name_ = "cpidx" then delete;
    %end;
    %do i = 1 %to &ntimepoints;
      %let time&i = %sysevalf(%sysfunc(scan(&timelist, &i, ' ')) / 12);
      else if _name_ = "cpidx&i" then varstr = "&&time&i-year";
    %end;
  run;

  proc contents data = _cpidx_(drop = varstr %if &showoverall %then %do; overall %end;) 
    out = _vars_(keep = varnum name label) noprint;
  proc sort data = _vars_; by varnum; 
  data _vars_;
    set _vars_;
    call symput(cat("sttname", strip(_n_)), name);
    label = prxchange("s/^-?\d+\.?\s+(.*)/\u$1/", 1, strip(label));
    if label ne "" then call symput(cats("sttlab", _n_), label);
    else call symput(cats("sttlab", _n_), name);
  run;

  %let msize = 35; %* Left margin size;
  %let csize = 15; %* Column size;
  %let lsize = %eval(2 + &msize + (&colcnt + &showoverall) * (&csize + 2));
  %if &lsize > 256 %then %let lsize = max;
  %else %if &lsize < 64 %then %let lsize = 64;

  options ls = &lsize nodate nocenter;
  %if &target = rtf %then %do;
    ods rtf file = "&outdata..rtf" style = &style bodytitle;
  %end;
  %else %if &target = xls %then %do;
    ods tagsets.excelxp file = "&outdata..xls"  style = &style
    options(frozen_rowheaders = "1" frozen_headers = "1");
  %end;
  ods escapechar = '\';
  title justify = left &rtftitle;
  proc report data = _cpidx_ nowd split = '*';
    column varstr %do i = 1 %to &colcnt; &&sttname&i %end; %if &showoverall %then %do; overall %end;;
    %if &setdaterow %then %do;
      define varstr / "Time" order = data style = [just = left] width = &msize;
    %end;
    %else %do;
      define varstr / "Time (set date: &sdate)" order = data style = [just = left] width = &msize;
    %end;
    %if &strata eq nil %then %do;
      define &sttname1 / "Overall (N = &sttcnt1), %" order = data style = [just = right] width = &csize;
    %end;
    %else %do i = 1 %to &colcnt;
      %let sttlab&i = %sysfunc(strip(&&sttlab&i));
      %let sttcnt&i = %sysfunc(strip(&&sttcnt&i));
      define &&sttname&i / "&&sttlab&i (N = &&sttcnt&i), %" order = data style = [just = right] width = &csize;
    %end;
    %if &showoverall %then %do;
      define overall / "Overall, %" order = data style = [just = right] width = &csize;
    %end;
  run;
  %if &target = rtf %then %do; ods rtf close; %end;
  %else %if &target = xls %then %do; ods tagsets.excelxp close; %end;
%mend compidx;

/* SECTION: logrank test */

%macro logranktest(indata =, varlst =, outdata =, title = nil, style = default, target = rtf);
  %if &target = rtf and &style = default %then %let style = kenyrtf;
  %else %if &target = xls and &style = default %then %let style = kenyxls;
  
  data logrankcache;
    set &indata;
    keep dead dfs intxsurv intxrel &varlst;
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&varlst, &i));
    data logrankcache;
      set logrankcache;
      if &firstobs then do;
	call symput("format&i", vformat(&var));
	call symput("label&i", vlabel(&var));
      end;
    run;

    %* Log-rank test;
    ods listing close;
    proc lifetest data = logrankcache reduceout;
      where not missing(&var);
      time intxsurv * dead(0);
      strata &var;
      ods output HomTests = pvalue_dead(keep = test probchisq);
    run;
    
    proc lifetest data = logrankcache reduceout;
      where not missing(&var);
      time intxrel * dfs(0);
      strata &var;
      ods output HomTests = pvalue_dfs(keep = test probchisq);
    run;
    ods listing;

    %* Reorganize;
    data pvalue;
      set pvalue_dead (in = lpos) pvalue_dfs (in = lpdfs);
      where test = "Log-Rank";
      length outcome $ 10 varstr $ 25 pvalue $ 6;
      if lpos then outcome = "dead";
      else if lpdfs then outcome = "dfs";

      varstr = strip("&&label&i");
      if probchisq >= 0.01 then pvalue = strip(put(probchisq, 4.2));
      else if probchisq >= 0.001 then pvalue = strip(put(probchisq, 5.3));
      else if probchisq >= 0 then pvalue = "<0.001";
    run;

    proc transpose data = pvalue out = tpvalue(drop = _name_);
      id outcome;
      var pvalue;
    run;
    
    /* Frequency */
    proc freq data = logrankcache noprint;
      where not missing(&var);
      table &var / out = freqtable;

    proc sort data = freqtable; by &var;

    data freqtable;
      length var $ 50 freq $ 20;
      set freqtable;
      if missing(percent) then percent = 0;
  
      if percent = 0 then freq = "0";
      else if (0 < percent < 1) then freq = cat(count, " (<1)");
      else if (1 <= round(percent) < 100) then freq = cat(count, " (", round(percent), ")");
      else if round(percent) = 100 then freq = cat(count);

      freq = strip(freq);

      prx = prxchange("s/^-?\d+\.?\s+(.*)/\u$1/", 1, strip(put(&var, &&format&i)));
      var = cat("\R'\tab' ", prx);
      drop &var prx count percent;
    run;

    /* Heading */
    data header;
      set tpvalue;
      length var $ 50;
      var = prxchange("s/^(.*)/\u$1/", 1, strip("&&label&i"));
    run;

    data logranktab&i;
      set header freqtable;
    run;
  %end;

  %* Combine;
  data logranktab;
    set %do i = 1 %to &nvarlst; logranktab&i %end;;
    rename var = varlst;
  run;

  %* Report;
  %let msize = 40;
  %let csize = 20;
  %let lsize = %eval(2 + &msize + 3 * (&csize + 2));
  options ls = &lsize nodate nonumber;
  %if &target = rtf %then %do;
    ods rtf file = "&outdata..rtf" style = &style bodytitle;
  %end;
  %else %if &target = xls %then %do;
    ods tagsets.excelxp file = "&outdata..xls"  style = &style
    options(frozen_rowheaders = "1" frozen_headers = "1");
  %end;
  ods escapechar = '\';
  %if &title = nil %then %do; title "Log-rank test"; %end;
  %else %do; title &title; %end;
  proc report data = logranktab nowd split = '*';
    column varlst freq ("Log-rank p-value" dead dfs);
    define varlst / "Variable" order = data style = [just = left] width = &msize;
    define freq / "N (%)" order = data style = [just = right] width = &csize;
    define dead / "Overall survival" order = data style = [just = right] width = &csize;
    define dfs / "Disease free survival" order = data style = [just = right] width = &csize;
  run;
  %if &target = rtf %then %do; ods rtf close; %end;
  %else %if &target = xls %then %do; ods tagsets.excelxp close; %end;
%mend;

/* SECTION: quartiles for continuous variables */

%macro contvar(indata =	,	  /* Input data set */
	      varlst = ,          /* List of continuous variables */
	      strata = ,	  /* Main effect */
	      outdata =	contvar,  /* Output file name */
	      title = nil,	  /* Title */
	      maxdec = 2,	  /* Number of shown decimal places, default = 2 */
              style = default,
              target = rtf
	      );
  
  %let nvarlst = %sysfunc(countw(&varlst)); * Total number of variables;
  %if &target = rtf and &style = default %then %let style = kenyrtf;
  %else %if &target = xls and &style = default %then %let style = kenyxls;

  data _init_;
    set &indata;
    call symput("label0", vlabel(&strata)); * Main effect label;
    call symput("format0", vformat(&strata)); * Main effect format;
    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&varlst, &i));
      call symput("label&i", vlabel(&var));
    %end;
    keep &varlst &strata;
  run;

  %do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&varlst, &i));

    proc means data = _init_ noprint;
      class &strata;
      var &var;
      output out = _quartiles_ n = n min = min p25 = p25 
      p50 = p50 p75 = p75 max = max;
    run;

    data _quartiles_;
      set _quartiles_;
      where _type_ = 1;

      length maineffect $ 50;
      maineffect = prxchange("s/\d+\.?\s+(.*)/\\R\'\\tab\' \u$1/", 1, strip(put(&strata, &format0)));

      n_s = put(n, 8.0);
      min_s = put(min, 8.&maxdec);
      p25_s = put(p25, 8.&maxdec);
      p50_s = put(p50, 8.&maxdec);
      p75_s = put(p75, 8.&maxdec);
      max_s = put(max, 8.&maxdec);

      drop &strata n min p25 p50 p75 max _type_ _freq_;
    run;

    data _header_;
      length maineffect $ 50;
      maineffect = prxchange("s/^(.*)/\u$1/", 1, strip("&&label&i"));
    run;

    data _tab_&i;
      set _header_ _quartiles_;
    run;
  %end;

  data _final_;
    set %do i = 1 %to &nvarlst; _tab_&i %end;;
  run;

  * Output;
  %let msize = 40; %* Left margin size;
  %let csize = 10; %* Column size;
  %let lsize = %eval(2 + &msize + 10 * (&csize + 2));
  options ls = &lsize nodate nonumber;
  %if &target = rtf %then %do;
    ods rtf file = "&outdata..rtf" style = &style bodytitle;
  %end;
  %else %if &target = xls %then %do;
    ods tagsets.excelxp file = "&outdata..xls"  style = &style
    options(frozen_rowheaders = "1" frozen_headers = "1");
  %end;
  ods escapechar = '\';

  %if &title = nil %then %let title = Distribution of continuous variables;
  title &title;

  proc report data = _final_ nowd split = '*';
    column maineffect n_s min_s p25_s p50_s p75_s max_s;
    define maineffect / "Variable" order = data style = [just = left] width = &msize;
    define n_s / "N" order = data style = [just = right] width = &csize;
    define min_s / "Min" order = data style = [just = right] width = &csize;
    define p25_s / "25%" order = data style = [just = right] width = &csize;
    define p50_s / "50%" order = data style = [just = right] width = &csize;
    define p75_s / "75%" order = data style = [just = right] width = &csize;
    define max_s / "Max" order = data style = [just = right] width = &csize;
  run;

  %if &target = rtf %then %do; ods rtf close; %end;
  %else %if &target = xls %then %do; ods tagsets.excelxp close; %end;
%mend;
