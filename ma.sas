/* Macro for multi-variate Cox regression
/* Maintainer: Zhen-Huan Hu <zhu@mcw.edu>
/* Last update: 2018-08-23
/* -------------------------------------------- */

%let matbcnt = 0;

%macro mvasection(title);
  %let matbcnt = %eval(&matbcnt + 1);
  data _ma_block_&matbcnt;
    length rwtext $ 100;
    rwtext = cat(&title);
    rwtype = 9;
  run;
%mend;

%macro mb(
  indata = ,
  event = dead,
  intv = intxsurv,
  covlst = ,
  tdvlst = ,
  selection = stepwise,
  slentry = 0.05,
  slstay = 0.05,
  detailed = 0
  );

  %if %sysfunc(prxmatch(%str(/\|\|/), &covlst)) %then %do;
    %let catlst = %sysfunc(prxchange(%str(s/^(.*) \|\| (.*)$/$1/), 1, &covlst));
    %let catlst = %sysfunc(prxchange(%str(s/(\w+)\|(\w+)/$1 $2/), -1, &catlst));
    %let covlst = %sysfunc(prxchange(%str(s/^(.*) \|\| (.*)$/$1 $2/), 1, &covlst));
  %end;
  %else %do;
    %let catlst = &covlst;
    %let catlst = %sysfunc(prxchange(%str(s/(\w+)\|(\w+)/$1 $2/), -1, &catlst));
  %end;

  data _mb_init_;
    set &indata;
    where not missing(&event) and not missing(&intv);
  run;

  %if &tdvlst ~= %then %do;
    %let ntdvlst = %sysfunc(countw(&tdvlst));
    %do i = 1 %to &ntdvlst;
      %let tdv = %scan(&tdvlst, &i, %str( ));
      proc sql noprint;
        select count(distinct &tdv)
        into :tdvcatcnt&i trimmed from _mb_init_;
        select mean(&tdv) as value
        into :tdv&i.value1 - :tdv&i.value&&tdvcatcnt&i from _mb_init_
        group by &tdv order by value;
        select min(&tdv)
        into :tdv&i.ref trimmed from _mb_init_;
      quit;
    %end;
  %end;

  ods listing select %if &detailed %then ALL; %else ModelBuildingSummary;;
  proc phreg data = _mb_init_;
    class &catlst;
    model &intv * &event(0) = &covlst
    %if &tdvlst ~= %then %do;
      %let ntdvlst = %sysfunc(countw(&tdvlst));
      %do i = 1 %to &ntdvlst;
        %let tdv = %scan(&tdvlst, &i, %str( ));
        %do j = 2 %to &&tdvcatcnt&i;
          t&j._&tdv
        %end;
      %end;
    %end;
    / risklimits selection = &selection slentry = &slentry slstay = &slstay;
    %if &tdvlst ~= %then %do;
      %let ntdvlst = %sysfunc(countw(&tdvlst));
      %do i = 1 %to &ntdvlst;
        %let tdv = %scan(&tdvlst, &i, %str( ));
        %do j = 2 %to &&tdvcatcnt&i;
          t&j._&tdv = log(&intv) * (&tdv = &&tdv&i.value&j);
        %end;
      %end;
    %end;
  run;
  quit;
  ods listing select ALL;
%mend;

%macro ma(
  indata = ,
  event = dead,
  intv = intxsurv,
  strata = ,
  covlst = ,
  tdvlst = ,
  outlst = ,
  contrast = 1,
  ctrslst = ,
  ) / minoperator;

  %let dsid = %sysfunc(open(&indata, i));
  
  %if %sysfunc(prxmatch(%str(/\|/), &covlst)) %then %do;
    %let catlst = %sysfunc(prxchange(s/^(.*)\|(.*)$/$1/, 1, &covlst));
    %let covlst = %sysfunc(prxchange(s/^(.*)\|(.*)$/$1 $2/, 1, &covlst));
  %end;
  %else %do;
    %let catlst = &covlst;
  %end;

  %let ncatlst = %sysfunc(countw(&catlst, %str( )));
  %let nvarlst = %sysfunc(countw(&covlst, %str( )));

  %do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&covlst, &i));
    %let vnum = %sysfunc(varnum(&dsid, &var));
    %let covlbl&i = %qsysfunc(varlabel(&dsid, &vnum));
    %let covfmt&i = %qsysfunc(varfmt(&dsid, &vnum));
    %if &&covlbl&i = %then %let covlbl&i = %upcase(&var);
    %if &&covfmt&i = %then %let covfmt&i = best8.;
  %end;

  %let ntdvlst = %sysfunc(countw(&tdvlst, %str( )));
  %if &ntdvlst >= 1 %then %do;
    %do i = 1 %to &ntdvlst;
      %let tdvterm = %sysfunc(scan(&tdvlst, &i));
      %let tdv = %sysfunc(prxchange(s/(\w+)@([0-9.]+)/$1/, 1, &tdvterm));
      %let tdt = %sysfunc(prxchange(s/(\w+)@([0-9.]+)/$2/, 1, &tdvterm));

      %let tdvnum = %sysfunc(varnum(&dsid, &tdv));
      %let tdvlbl = %qsysfunc(varlabel(&dsid, &tdvnum));
      %if &tdvlbl = %then %let tdvlbl = %upcase(&tdv);

      %let tdv_eidx = %eval(&nvarlst + 1);
      %let tdv_lidx = %eval(&nvarlst + 2);
      %let covlbl&tdv_eidx = &tdvlbl: < &tdt;
      %let covlbl&tdv_lidx = &tdvlbl: >= &tdt;

      %let covlst = &covlst &tdv.e &tdv.l;
      %let nvarlst = %eval(&nvarlst + 2);
    %end;
  %end;

  %if &outlst = %then %let outlst = &covlst; %else %let contrast = 0;
  %if &ctrslst = %then %let ctrslst = &catlst; %else %let contrast = 1;

  %let rc = %sysfunc(close(&dsid));

  data _ma_init_;
    set &indata;
    where not missing(&event) and not missing(&intv);
    %do i = 1 %to &ncatlst;
      %let vcat = %sysfunc(scan(&catlst, &i));
      if missing(&vcat) then &vcat = 99;
    %end;
  run;

  proc sql noprint;
    %do i = 1 %to &ncatlst;
      %let vcat&i = %sysfunc(scan(&catlst, &i));
      select count(distinct &&vcat&i) into :vcatngrp&i trimmed from _ma_init_;
    %end;
  quit;

  data _unformat_;
    set _ma_init_;
    format _all_;
  run;

  ods listing close;
  ods output ModelANOVA = _type3_ ParameterEstimates = _hazards_
  %if &ncatlst >= 1 %then %do; ContrastEstimate = _contrast_ %end;;
  proc phreg data = _unformat_;
    %if &strata ~= %then %do;
      strata &strata;
    %end;
    class &catlst / ref = first;
    model &intv * &event(0) = &covlst / risklimits;
    %if &ntdvlst >= 1 %then %do;
      %do i = 1 %to &ntdvlst;
        %let tdvterm = %sysfunc(scan(&tdvlst, &i));
        %let tdv = %sysfunc(prxchange(s/(\w+)@([0-9.]+)/$1/, 1, &tdvterm));
        %let tdt = %sysfunc(prxchange(s/(\w+)@([0-9.]+)/$2/, 1, &tdvterm));
        &tdv.e = 0; &tdv.l = 0;
        if (&intv < &tdt) then &tdv.e = &tdv;
        else &tdv.l = &tdv;
      %end;
    %end;
    %if &ncatlst >= 1 %then %do;
      %do i = 1 %to &ncatlst;
        %let vcat = %sysfunc(scan(&catlst, &i));
        %do j = 2 %to &&vcatngrp&i;
  	%do k = %eval(&j + 1) %to &&vcatngrp&i;
  	  %let carray = ;
  	  %do l = 2 %to &&vcatngrp&i;
  	    %if &l = &j %then %let carray = &carray -1;
  	    %else %if &l = &k %then %let carray = &carray 1;
  	    %else %let carray = &carray 0;
  	  %end;
  	  contrast "&vcat &k &j" &vcat &carray / estimate = exp;
  	%end;
        %end;
      %end;
    %end;
  quit;
  ods listing;

  data _type3_;
    set _type3_;
    length pvalue $ 25;
    if 0 <= probchisq < 0.001 then pvalue = "< 0.001";
    else if 0.001 <= probchisq < 0.01 then pvalue = put(round(probchisq, 0.001), 5.3);
    else if 0.01 <= probchisq then pvalue = put(round(probchisq, 0.01), 4.2);
    rename effect = parameter;
    keep effect pvalue;
  run;

  data _hazards_;
    set _hazards_;
    length hazard pvalue $ 25;
    value = input(classval0, 2.0);
    hazard = cat(put(round(hazardratio, 0.01), 4.2), " (", put(round(hrlowercl, 0.01), 4.2), "-", put(round(hruppercl, 0.01), 4.2), ")");
    if 0 <= probchisq < 0.001 then pvalue = "< 0.001";
    else if 0.001 <= probchisq < 0.01 then pvalue = put(round(probchisq, 0.001), 5.3);
    else if 0.01 <= probchisq then pvalue = put(round(probchisq, 0.01), 4.2);
    keep parameter value hazard pvalue;
  run;

  %do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&covlst, &i));
    %if &var in &catlst %then %do;
      %let vcat = &var;
      proc sql noprint;
        create table _catmin_&i as
        select min(&vcat) as value, "&vcat" as parameter, "Reference" as hazard from _unformat_;
      quit;
      data _hazards_;
        set _hazards_ _catmin_&i;
      run;

      proc sql noprint;
        create table _cat_count_&i as
        select &vcat as value, cat(count(&vcat)) as count, "&vcat" as parameter from _unformat_ group by &vcat;
      quit;
      proc sort data = _hazards_; by parameter value;
      proc sort data = _cat_count_&i; by parameter value;
      data _hazards_;
        merge _hazards_ _cat_count_&i;
        by parameter value;
      run;
    %end;
    %else %do;
      proc sql noprint;
        create table _cat_count_&i as
        select cat(count(*)) as count, "&var" as parameter from _unformat_;
      quit;
      proc sort data = _hazards_; by parameter;
      proc sort data = _cat_count_&i; by parameter;
      data _hazards_;
        merge _hazards_ _cat_count_&i;
        by parameter;
      run;
    %end;
  %end;

  %do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&covlst, &i));
    %if &var in &outlst %then %do;
      data _header_&i;
        set _type3_;
        where parameter = "&var";
        length rwtext $ 100;
        rwtext = strip("&&covlbl&i");
        rwtype = 0;
      run;

      %if &var in &catlst %then %do;
        data _hazards_&i;
          set _hazards_;
          where parameter = "&var";
          length rwtext $ 100;
          rwtext = prxchange('s/^-?\d+\.?\d*\s+(.*)/\u$1/', 1, strip(put(value, &&covfmt&i)));
          rwtype = 1;
        run;
        data _section_&i;
          set _header_&i _hazards_&i;
        run;
      %end;
      %else %do;
        data _hazards_&i;
          set _hazards_;
          where parameter = "&var";
        run;
        proc sort data = _header_&i; by parameter;
        proc sort data = _hazards_&i; by parameter;
        data _section_&i;
          merge _header_&i _hazards_&i;
          by parameter;
        run;
      %end;
    %end;
    %else %do;
      data _section_&i;
        stop;
        set _hazards_;
      run;
    %end;
  %end;

  %if &contrast = 1 and &ncatlst >= 1 %then %do;
    data _contrast_;
      set _contrast_;
      length hazard pvalue $ 25;
      hazard = cat(put(round(estimate, 0.01), 4.2), " (", put(round(lowerlimit, 0.01), 4.2), "-", put(round(upperlimit, 0.01), 4.2), ")");
      if 0 <= probchisq < 0.001 then pvalue = "< 0.001";
      else if 0.001 <= probchisq < 0.01 then pvalue = round(probchisq, 0.001);
      else if 0.01 <= probchisq then pvalue = put(round(probchisq, 0.01), 4.2);
      parameter = scan(contrast, 1);
      grp1 = input(scan(contrast, 2), 2.0);
      grp2 = input(scan(contrast, 3), 2.0);
      keep parameter hazard pvalue grp1 grp2;
    run;

    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&covlst, &i));
      %if &var in &ctrslst %then %do;
        %let vcat = &var;
        data _contrast_ref_&i;
          set _hazards_&i;
          grp = _n_;
          keep parameter grp value;
        run;
        proc sql noprint;
          create table _contrast_&i as
          select a.parameter, a.hazard, a.pvalue,
          b.value as value1, c.value as value2 from _contrast_ a
          left join _contrast_ref_&i b on a.parameter = b.parameter and a.grp1 = b.grp
          left join _contrast_ref_&i c on a.parameter = b.parameter and a.grp2 = c.grp
          where a.parameter = "&vcat";
        quit;

        data _contrast_&i;
          set _contrast_&i;
          length rwtext $ 100;
          rwtext = cat(strip("&&covlbl&i"), ": ",
          strip(prxchange('s/^-?\d+\.?\d*\s+(.*)/\u$1/', 1, strip(put(value1, &&covfmt&i)))), " vs. ",
          strip(prxchange('s/^-?\d+\.?\d*\s+(.*)/\u$1/', 1, strip(put(value2, &&covfmt&i)))));
          rwtype = 1;
        run;
      %end;
      %else %do;
        data _contrast_&i;
          stop;
          set _contrast_;
        run;
      %end;
    %end;

    data _contrast_header_;
      length rwtext $ 100;
      rwtext = "Contrast";
      rwtype = 0;
    run;
  %end;

  %let matbcnt = %eval(&matbcnt + 1);
  data _ma_block_&matbcnt;
    set _section_1-_section_&nvarlst
    %if &contrast = 1 and &ncatlst >= 1 %then %do;
      _contrast_header_ _contrast_1-_contrast_&nvarlst
    %end;;
  run;
%mend ma;

%macro exportma(
  outdata = ,   /* Output SAS dataset */
  rtftitle = ,  /* Title of the RTF file */
  style = kenyrtf
  );

  data _ma_block_;
    set _ma_block_1-_ma_block_&matbcnt;
    if rwtype = 1 then rwtext = '   ' || strip(rwtext);
    else if rwtype = 2 then rwtext = '      ' || strip(rwtext);
  run;

  %let msize = 45; %* Size of the left margin;
  %let nsize = 10; %* Size of the N column;
  %let csize = 20; %* Size of the HR columns;
  %let psize = 10; %* Size of the p-value column;

  %let linesize = %eval(2 + &msize + 3 * (&csize + 2));
  %if &linesize > 256 %then %let linesize = max;
  %else %if &linesize < 64 %then %let linesize = 64;
  %let save_options = %sysfunc(getoption(number)) %sysfunc(getoption(date)) %sysfunc(getoption(linesize, keyword));

  options ls = &linesize nodate nonumber;
  ods rtf file = "&outdata..rtf" style = &style bodytitle;
  ods escapechar = '\';

  title justify = left &rtftitle;
  proc report data = _ma_block_ nowd split = '*' style(report) = [outputwidth = 100%];
    column rwtype rwtext count hazard pvalue;

    define rwtype / display noprint;
    define rwtext / "Covariates" order = data style = [just = left] width = &msize;
    define count  / "N" order = data style = [just = right] width = &nsize;
    define hazard / "HR (95% CI)" order = data style = [just = right] width = &csize;
    define pvalue / "P Value" order = data style = [just = right] width = &psize;

    compute rwtext;
      if rwtype = 0 then call define(_col_, 'style/merge', 'style = []');
      else if rwtype = 1 then call define(_col_, 'style/merge', 'style = [leftmargin = 24pt]');
      else if rwtype = 2 then call define(_col_, 'style/merge', 'style = [leftmargin = 48pt]');
      else if rwtype = 9 then do;
        call define(_col_, 'style/merge', 'style = [font_weight = bold just = center]');
        call define(_row_, 'style/merge', 'style = [borderbottomstyle = solid borderbottomwidth = 1pt]');
      end;
    endcomp;
  run;

  ods rtf close;
  options &save_options;
  title;
  %let matbcnt = 0;
%mend exportma;

