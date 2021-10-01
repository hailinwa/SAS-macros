/* Macro library for creating selection exclusion table;
/* Maintainer: Zhenhuan Hu;
/* Last change: 2019-10-24;
/*
/* Fucntions:
/* -------------------------------------------- */
/* %seltable(indata, varlst, outfile)
/*
/* Instructions:
/* -------------------------------------------- */
/* Make sure that all variables (including strata) are correctly
/* formatted and labelled for the best output (although the macro
/* will tolerate unformatted variables).
/* -------------------------------------------- */

%macro seltable(
  indata,             /* Input data set */
  varlst,             /* Selection index list */
  outfile = seltable, /* Output RTF file name, default: seltable */
  id = crid,
  regex = 's/^-?\d+([.-]\d+)?\s+//',
  style = kenyrtf
  );
    
  %let adj = 0; %* Hosei;
  %let marginsize = 90; %* Initialize the size of left margin;
  %let columnsize = 10; %* Define the size of each column; 
  %let cond = %quote(&id > 0); %* Overall default criteria;

  %let nvarlst = %sysfunc(countw(&varlst, ' '));
  %let dvarlst = %sysfunc(prxchange(%str(s/^(.*) \| (.*)$/$1 $2/), 1, %nrbquote(&varlst)));
  %let dvarlst = %sysfunc(prxchange(%str(s/#/ /), -1, %nrbquote(&dvarlst)));

  data _seltable_cache_;
    set &indata(keep = &id &dvarlst);
  run;

  %let dsid = %sysfunc(open(_seltable_cache_));
  %let varnum0 = %sysfunc(varnum(&dsid, &id));
  %let varlbl0 = %qsysfunc(varlabel(&dsid, &varnum0));
  %let rc = %sysfunc(close(&dsid));

  proc sql noprint;
    create table _count_0 as
    select cat(count(*)) as count from _seltable_cache_ where &cond;
  quit;

  data _exlttl_0;
    length varstr $ 1500;
    set _count_0(keep = count);
    varstr = "&varlbl0";
    if strip(varstr) = '' then varstr = 'Total N of patients'; 
  run;

  %do i = 1 %to &nvarlst;
    %let var&i = %qsysfunc(scan(&varlst, &i, ' '));
    %if &&var&i ne %str(|) %then %do;
      %if %sysfunc(prxmatch(%str(/#/), &&var&i)) %then %do;
	%let excl_var&i = %sysfunc(prxchange(%str(s/.+#(.+)/$1/), 1, &&var&i));
	%let var&i = %sysfunc(prxchange(%str(s/(.+)#.+/$1/), 1, &&var&i));
      %end;
      %else %let excl_var&i = ;

      %let dsid = %sysfunc(open(_seltable_cache_));
      %let varnum&i = %sysfunc(varnum(&dsid, &&var&i));
      %let varlbl&i = %qsysfunc(varlabel(&dsid, &&varnum&i));
      %let rc = %sysfunc(close(&dsid));
      %if &&varlbl&i = %then %let varlbl&i = %upcase(&&var&i);

      proc sql noprint;
        create table _count_&i as
        select cat(count(*)) as count from _seltable_cache_ where &cond and &&var&i;

        create table _excl_count_&i as
        select cat(count(*)) as exclude from _seltable_cache_ where &cond and not &&var&i;
      quit;

      %if &&excl_var&i ~= %then %do;
	proc freq data = _seltable_cache_ noprint;
	  where &cond and not &&var&i;
	  table &&excl_var&i / missing out = _excl_freq_&i;
	run;

	data _excl_freq_&i;
	  set _excl_freq_&i;
	  length excl_tag_pre excl_tag $ 100;
          if missing(&&excl_var&i) then excl_tag_pre = 'Missing';
          else excl_tag_pre = prxchange(&regex, 1, vvalue(&&excl_var&i));
	  if prxmatch('/^[A-Z][A-Z0-9_]+/', excl_tag_pre) then excl_tag = catx(' = ', excl_tag_pre, count);
	  else excl_tag = catx(' = ', prxchange('s/(.*)/\l$1/', 1, excl_tag_pre), count);
	run;

        proc sql noprint;
          create table _excl_freq_sorted&i as
          select * from _excl_freq_&i order by missing(&&excl_var&i), &&excl_var&i;
        quit;
        data _excl_lbl_&i;
          length varstr excl_taglst $ 1500;
          do until(eof);
            set _excl_freq_sorted&i end = eof;
            excl_taglst = catx(', ', excl_taglst, excl_tag);
          end;
          varstr = cat(strip("&&varlbl&i"), " (\R'{\i excluding ", strip(excl_taglst), "}')");
          keep varstr;
        run;
      %end;
      %else %do;
        data _excl_lbl_&i;
          length varstr $ 1500;
          varstr = strip("&&varlbl&i");
        run;
      %end;

      data _exlttl_&i;
	merge _excl_lbl_&i _count_&i(keep = count) _excl_count_&i(keep = exclude);
      run;
      %let cond = &cond and &&var&i;
      %let adj = 0;
    %end;
    %else %if &&var&i eq %str(|) %then %do;
      data _exlttl_&i;
	length varstr $ 1500;
	varstr = "\R'\ul{'EXCLUSION:\R'}'";
      run;
      %let adj = 1;
    %end;
  %end;

  data _seltable_;
    set _exlttl_0-_exlttl_&nvarlst;
    keep varstr count exclude;
  run;

  %let save_options = %sysfunc(getoption(number)) %sysfunc(getoption(date)) %sysfunc(getoption(linesize, keyword));
  options nonumber nodate linesize = 120;
  ods rtf file = "&outfile..rtf" style = &style bodytitle;
  ods escapechar = '\';  
  title "The below selection criteria were applied";
  proc report data = _seltable_ nowd split = '*' style(report) = [outputwidth = 100%];
    column varstr exclude count;
    define varstr / "Selection criteria" order = data style = [just = left] width = &marginsize;
    define exclude / "# excluded" order = data style = [just = right] width = &columnsize;
    define count / "N" order = data style = [just = right] width = &columnsize;
  run;
  ods rtf close;
  options &save_options;
  title;
%mend;
