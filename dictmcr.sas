/* MACRO: SEARCHFMT
/* -------------------------------------------- */
/* DESCRIPTION: Search format in given libraries. The list of formats is supplied by
/* the FMTLIST =  parameter. The found formats are stored in a dataset specified by OUT =
/* parameter. If any format not found, a warning message will be displayed in the log.
/* -------------------------------------------- */
/* MAINTAINER: Zhiwei Wang, Zhenhuan Hu <zhu@mcw.edu>
/* LAST UPDATE: 2018-06-04
/* -------------------------------------------- */

%macro searchfmt(fmtlist = , liblist = , ignorelist = mmddyy, out = _fmtcat,
  keep = fmtname start end label, debug = 1);
  %* FMTLIST: List of formats to be looked for in the libraries;
  %* LIBLIST: List of libraries to be searched;
  %* OUT: Output data set;
  %* KEEP: Variables to be kept in the output data set;

  %let fmtlist = %upcase(&fmtlist);
  %let ignorelist = %upcase(&ignorelist);

  %* Initialize output dataset;
  data &out; run;

  %* Search for format by library;
  %do %until (&fmtlist = or &liblist = );
    %let curlib = %scan(&liblist, 1);
    proc format library = &curlib cntlout = _temp;
      select &fmtlist;
    run;

    %* Check if any formats found;
    %local found;
    %if %sysfunc(exist(_temp)) %then %do;
      %let did = %sysfunc(open(_temp));
      %let found = %sysfunc(attrn(&did, any));
      %let close = %sysfunc(close(&did));
    %end;
    %else %let found = -999;
    %if &debug = 1 %then %put site0101 found = &found;
    %if &found > 0 %then %do;
      data &out;
	length label $ 250;
	set &out _temp;
      run;

      %* Get the list of formats found in the current library;
      proc sql noprint;
	select distinct fmtname
	into :foundlist separated by ' '
	from _temp;
      run;

      %* Remove formats found from the list;
      %let fmtlist = %compressword(&fmtlist, &foundlist);
      %if &debug = 1 %then %do;
   	%put site1114 curlib = &curlib fmtlist = &fmtlist foundlist = &foundlist;
      %end;
    %end;
    %* Remove searched library;
    %let liblist = %compressword(&liblist, &curlib);
  %end;
  %* Remove formats in the ignore list;
  %let fmtlist = %compressword(&fmtlist, &ignorelist);
  %* Check remaining fmts;
  %if &fmtlist ne %then %put WARNING: format [&fmtlist] not found;
  %put liblist = &liblist curlib = &curlib fmtlist = &fmtlist foundlist = &foundlist;

  %* Exclude empty observation created by data set initialization
  %* and keep variables needed;
  data &out;
    set &out;
    where not missing(fmtname);
    %* To avoid the empty record;
    keep &keep;
  run;
%mend searchfmt;

/* MACRO: DICT
/* DESCRIPTION: Generate a rtf file of all formats associated with the data set.
/* AUTHOR: Zhiwei Wang, Zhenhuan Hu
/* LAST UPDATE: 2018-06-04
/* -------------------------------------------- */

%macro dict(indata = , optfile = , keep = , sort = 0, meddra = 0, style = kenyrtf, optfmt = rtf);
  %if &sort = 1 %then %let sortby = name;
  %else %let sortby = varnum;

  data _init;
    %if &keep ~= %then retain &keep;;
    set &indata %if &keep ~= %then (keep = &keep);;
  run;

  %* 1. Get contents;
  proc contents data = _init
    out = _contout(keep = name label format type varnum)
    noprint;
  run;

  %* 2. Get formats;
  %* Extract format into a string;
  proc sql noprint;
    select distinct format
    into :fmtlist separated by ' '
    from _contout;
  quit;
  %put fmtlist = &fmtlist;

  * Get library list;
  %let liblist = %sysfunc(getoption(fmtsearch));
  %let liblist = %sysfunc(compress(&liblist, '(,)'));
  %put liblist = &liblist;

  * Get format;
  %searchfmt(fmtlist = &fmtlist, liblist = &liblist, out = _fmtdat);

  * Merge data set contents and formats;
  proc sql;
    create table _dict as
    select a.varnum, a.format
    , case
      when a.type = 1 and b.found = 1 then 'Categorical'
      when a.type = 1 then 'Continuous'
      when a.type = 2 then 'Character'
      end as vartype
    , lowcase(a.name) as varname
    , case
      when missing(a.label) then 'No label'
      else prxchange('s/(.*)/\u$1/', 1, strip(a.label))
      end as varlabel
    , input(strip(b.start), 8.) as str_value
    , input(strip(b.end), 8.) as end_value
    , case
      when input(strip(b.start), 8.) = input(strip(b.end), 8.) then strip(b.start)
      else cat(strip(b.start), '-', strip(b.end))
      end as fmtvalue
    , prxchange('s/^-?\d+\.?\d*\s+(.*)/\u$1/', 1, strip(b.label)) as fmtlabel
    from _contout a left join (select *, 1 as found from _fmtdat) b
    on strip(a.format) = strip(b.fmtname)
    order by &sortby, str_value;
  quit;

  %if &meddra %then %do;
    data _dict;
      set _dict;
      length label_meddra_pt label_meddra_llt $ 250;
      if prxmatch('/^[^|]+\|/', varlabel) then label_meddra_pt = strip(prxchange('s/^[^|]+\|([^|]+).*/$1/', 1, varlabel)); else label_meddra_pt = '-';
      if prxmatch('/^[^|]+\|[^|]+\|/', varlabel) then label_meddra_llt = strip(prxchange('s/^[^|]+\|[^|]+\|([^|]+).*/$1/', 1, varlabel)); else label_meddra_llt = '-';
      varlabel = strip(prxchange('s/^([^|]+).*/$1/', 1, varlabel));
      length fmtlabel_meddra_pt fmtlabel_meddra_llt $ 250;
      if prxmatch('/^[^|]+\|/', fmtlabel) then fmtlabel_meddra_pt = strip(prxchange('s/^[^|]+\|([^|]+).*/$1/', 1, fmtlabel)); else fmtlabel_meddra_pt = '-';
      if prxmatch('/^[^|]+\|[^|]+\|/', fmtlabel) then fmtlabel_meddra_llt = strip(prxchange('s/^[^|]+\|[^|]+\|([^|]+).*/$1/', 1, fmtlabel)); else fmtlabel_meddra_llt = '-';
      fmtlabel = strip(prxchange('s/^([^|]+).*/$1/', 1, fmtlabel));
    run;
  %end;
  
  * Output report;
  %if &optfmt = rtf %then ods rtf file = "&optfile..rtf" style = &style bodytitle;
  %else %if &optfmt = xls %then ods tagsets.excelxp file = "&optfile..xls" style = &style options(frozen_rowheaders = "1" frozen_headers = "1"  autofilter = 'all');;
  title "Data dictionary for data set: &indata.";
  proc report data = _dict style(report) = [frame = void rules = group];
    column varnum varname varlabel vartype fmtvalue fmtlabel
    %if &meddra %then label_meddra_pt label_meddra_llt fmtlabel_meddra_pt fmtlabel_meddra_llt;;
    define varnum / '#' order style = [just = right] width = 5;
    define varname / 'Variable' order style = [just = left] style(column) = [font = fonts('StrongFont')] width = 20;
    define varlabel / 'Description' order width = 50;
    define vartype / 'Type' order width = 15;
    define fmtvalue / 'Value' display style = [just = right] width = 5;
    define fmtlabel / 'Label' display width = 50;
    %if &meddra %then %do;
      define label_meddra_pt / 'Description - MedDRA PT' order width = 35;
      define label_meddra_llt / 'Description - MedDRA LLT' order width = 35;
      define fmtlabel_meddra_pt / 'Label - MedDRA PT' display width = 35;
      define fmtlabel_meddra_llt / 'Label - MedDRA LLT' display width = 35;
    %end;
  run;
  %if &optfmt = rtf %then ods rtf close;
  %else %if &optfmt = xls %then ods tagsets.excelxp close;;
  title;
%mend dict;
