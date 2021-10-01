/* One report macro to rule them all!
/* Author: Zhen-Huan (Kenny) Hu <zhu@mcw.edu>
/* Last change: 2020-12-08
/*
/* Fucntions and their basic uses:
/* -------------------------------------------- */
/* %section(title);
/*  - Define a section title.
/* -------------------------------------------- */
/* %subsection(title);
/*  - Define a sub-section title.
/* -------------------------------------------- */
/* %header(indata, strata, total = 0);
/*  - Create header with counts.
/* -------------------------------------------- */
/* %cnt(indata, strata, total = 0, label = 'Number of patients');
/*  - Count the total number of observations.
/*  - The optional LABEL can be used to specify the label of the entry.
/* -------------------------------------------- */
/* %nlevel(indata, var, strata, total = 0, label = 'Number of levels');
/*  - Count the total number of distinct levels of VAR.
/*  - The optional LABEL can be used to specify the label of the entry.
/* -------------------------------------------- */
/* %cntcenter(indata, center, strata, total = 0);
/*  - Count the total number of centers.
/*  - The CENTER is used to specify the center ID variable.
/* -------------------------------------------- */
/* %univarkey(indata, varlst, strata, total = 0, statkey = sum);
/*  - Display the specified statistic of the variables in the VARLST.
/*  - The optional STATKEY can be used to choose the type of statistic.
/*    The default statistic is SUM.
/* -------------------------------------------- */
/* %univar(indata, varlst, strata, total = 0,
/*    outstat = n mean_std median_min_max nmiss);
/*  - List univariate statistics of variables in the VARLST.
/*  - The OUTSTAT parameter can be used to specify the list of
/*    statistics to be presented. Its possible values include the following
/*    keywords: N, NMISS, MEAN, STD, MIN, P5, P25, MEDIAN, P75, P95, MAX,
/*    N_NMISS, MEAN_STD, MIN_MAX, P5_P95, P25_P75, MEDIAN_MIN_MAX, MEDIAN_P25_P75.
/* -------------------------------------------- */
/* %median(indata, varlst, strata, test = 0, total = 0);
/*  - Medians and ranges of continuous variables in the VARLST.
/*  - When TEST = 1, the macro performs Wilcoxon rank sum test.
/* -------------------------------------------- */
/* %mediansubgrp(indata, varlst, subgrp, strata, test = 0, total = 0,
/*    usesublabel = 0, missingtag = 'Missing');
/*  - Medians and ranges of continuous variables by SUBGRP.
/*  - When TEST = 1, the macro performs Wilcoxon rank sum test.
/*  - When USESUBLABEL = 1, the macro uses the label of SUBGRP for the entry.
/*  - The optional parameter MISSINGTAG can be use to specify the missing
/*    value for SUBGRP.
/* -------------------------------------------- */
/* %freq(indata, varlst, strata, test = 0, total = 0,
/*    colpct = 1, nopct = 0, missing = 1, missingtag = 'Missing');
/*  - Frequencies of categorical variables in the VARLST.
/*  - When TEST = 1, the macro performs Pearson chi-square test;
/*    and when TEST = 2, the macro performs Fisher exact test for 2 by 2 tables
/*    and Fisher exact test via Monte-Carlo simulation for tables larger than 2 by 2;
/*  - When COLPCT = 1, the macro shows column percentages, otherwise the macro
/*    shows row percentages.
/*  - When NOPCT = 1, the macro displays the frequencies without percentages.
/*  - When MISSING = 1, the macro includes missing category in computations
/*    of percentages and statistics, otherwise the macro does not take into
/*    account the missing category in computations.
/*  - The optional parameter MISSINGTAG can be use to specify the missing
/*    value for SUBGRP.
/* -------------------------------------------- */
/* %freqr(indata, varlst, strata, test = 0, total = 0);
/*  - Row frequencies of categorical variables in the VARLST.
/* -------------------------------------------- */
/* %freqsubgrp(indata, varlst, subgrp, strata, test = 0, total = 0,
/*    colpct = 1, subfreq = 0, pctbysub = 0, nopct = 0, usesublabel = 0,
/*    foldsub = , foldemptysub = 0, missing = 1, missingtag = 'Missing');
/*  - Frequencies of categorical variables by SUBGRP.
/*  - When TEST = 1, the macro performs Pearson chi-square test;
/*    and when TEST = 2, the macro performs Fisher exact test;
/*  - When COLPCT = 1, the macro shows column percentages, otherwise the macro
/*    shows row percentages.
/*  - When SUBFREQ = 1, the macro shows the SUBGRP percentages.
/*  - When PCTBYSUB = 1, the macro calculates percentages per each SUBGRP.
/*  - When NOPCT = 1, the macro displays the frequencies without percentages.
/*  - When USESUBLABEL = 1, the macro uses the label of SUBGRP for the entry.
/*    of percentages and statistics, otherwise the macro does not take into
/*    account the missing category in computations.
/*  - The optional FOLDSUB parameter allows to specify SUBGRP categories to be folded.
/*  - When FOLDEMPTYSUB = 1, the macro will fold empty SUBGRP categories.
/*  - When MISSING = 1, the macro includes missing category in computations
/*    of percentages and statistics, otherwise the macro does not take into
/*    account the missing category in computations.
/*  - The optional parameter MISSINGTAG can be use to specify the missing
/*    value for SUBGRP.
/* -------------------------------------------- */
/* %medfreq(indata, varlst, strata, total = 0);
/*  - Medians and frequencies for a pair of variables.
/*  - Suppose A is a continuous variable and B is its corresponding categorical
/*    variable, use A|B to define the variable pair in VARLST.
/* -------------------------------------------- */
/* %prop(indata, varlst, strata, total = 0);
/*  - Proportion of variables in the VARLST (yes vs. total) with 95% CIs.
/* -------------------------------------------- */
/* %proplst(indata, varlst, strata, label = '', total = 0);
/*  - List of ratios of variables in the VARLST (yes vs. total).
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %proplstsubgrp(indata, varlst, subgrp, strata, label = '', total = 0);
/*  - List of ratios of variables in the VARLST (yes vs. total) by SUBGRP
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %catalst(indata, varlst, strata, label = , total = )
/*  - Frequencies of CATA variable lists (yes vs. no).
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %catalstsubgrp(indata, varlst, subgrp, strata, label = , total = )
/*  - Frequencies of CATA variable lists (yes vs. no) by SUBMGRP.
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %lifetestbyqt(indata, strata, event = , censorcode = 0, failcode = , time = , label = , total = 0);
/*  - Median, Q1, Q3 estimates of survival/cumulative incidence function.
/*  - The EVENT parameter defines the event indicator variable.
/*  - The CENSORCODE parameter defines the values for censoring.
/*  - The FAILCODE parameter when set indicates outcomes with competing events.
/*    It defines the values for events of interest.
/*    Any value other than those listed in CENSORCODE and FAILCODE is treated as a competing event.
/*  - The TIME parameter defines the time-to-event variable.
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %lifetestbytime(indata, strata, event = , censorcode = 0, failcode = , time = , timelist = , label = , total = 0);
/*  - Survival/cumulative incidence estimates at given points in time.
/*  - The EVENT parameter defines the event indicator variable.
/*  - The CENSORCODE parameter defines the values for censoring.
/*  - The FAILCODE parameter when set indicates outcomes with competing events.
/*    It defines the values for events of interest.
/*    Any value other than those listed in CENSORCODE and FAILCODE is treated as a competing event.
/*  - The TIME parameter defines the time-to-event variable.
/*  - The TIMELIST parameter specifies the points in time to have the estimates.
/*  - The LABEL parameter defines the label of the entry header.
/* -------------------------------------------- */
/* %mfs(indata, strata, total = )
/*  - Median follow-ups of survivors based on reversed Kaplan-Meier estimates.
/*  - When SHOWQ1Q3 = 1, the macro also outputs Q1 and Q3 follow-ups.
/* -------------------------------------------- */
/* %export(outfile, title, style = , outfmt = , delay = )
/*  - Output table results to an RTF or Excel file.
/*  - The OUTFILE defines the name of the output file.
/*  - The TITLE defines the title of the table.
/*  - The optional STYLE parameter can be used to change the output styles.
/*  - The OUTFMT parameter can be used to select output file format between RTF
/*    and Excel. By default, the macro output its results to an RTF file.
/*  - When DELAY = 1, the output will be kept in a temperary document stack
/*    instead of generating the output file. %EXPORT_TOGETHER can be called
/*    later to release the documents from the stack and generate a single output
/*    file. This makes it possible to output several separate tables into a
/*    single output file. The value of OUTFILE will be used to as the name of
/*    the temperary document data set that is needed by %EXPORT_TOGETHER.
/* -------------------------------------------- */
/* %export_listing(indata, varlst, outfile, title, style = , outfmt = , delay = );
/*  - Output raw listing results to an RTF or Excel file.
/*  - The INDATA defines the input data set.
/*  - The VARLST specifies the list of variables in the output.
/*  - The OUTFILE defines the name of the output file.
/*  - The TITLE defines the title of the table.
/*  - The OUTFMT parameter can be used to select output file format between RTF
/*    and Excel. By default, the macro output its results to an RTF file.
/*  - When DELAY = 1, the output will be kept in a temperary document stack
/*    instead of generating the output file. %EXPORT_TOGETHER can be called
/*    later to release the documents from the stack and generate a single output
/*    file. This makes it possible to output several separate tables into a
/*    single output file. The value of OUTFILE will be used to as the name of
/*    the temperary document data set that is needed by %EXPORT_TOGETHER.
/* -------------------------------------------- */
/* %export_together(outfile, style = , outfmt = )
/*  - Output tables in the document stack to an RTF or Excel file.
/*  - The OUTFILE defines the name of the output file.
/*  - The optional STYLE parameter can be used to change the output styles.
/*  - The OUTFMT parameter can be used to select output file format between RTF
/*    and Excel. By default, the macro output its results to an RTF file.
/*
/* Notes:
/* -------------------------------------------- */
/* 1. Missing category of STRATA is ignored by the macro.
/* 2. To output nice-looking formatted tables, make sure that all variables
/*    are properly formatted and labelled. When encountering unformatted or
/*    unlabelled variables, the macro will instead output their variable names
/*    and raw values respectively.
/* -------------------------------------------- */

%let rpt_sid = 0;
%let rpt_tid = 0;
%let rpt_header = 0;
%let rpt_sttvar = ;
%let rpt_sttlbl = ;
%let rpt_fid = 1;
%let rpt_output_delayed_files = ;

%let rpt_preset_test = ;
%let rpt_preset_total = ;

/* Internal macro functions */
/* -------------------------------------------- */
%macro rpttest(testdata, var, strata, by = , type = , testopt = );
  %if %sysfunc(exist(rpt_outtest)) %then %do; %* Clean outdated output;
    proc datasets lib = work nolist;
      delete rpt_outtest;
    quit;
  %end;
  %if &by ~= %then %do; %* By group;
    proc sort data = &testdata;
      by &by;
    run;
  %end;
  /* The parameter TYPE can have the following values:
  /* -------------------------------------------- */
  /*  - CHISQ:    Pearson chi-square test
  /*  - EXACT:    Fisher exact test
  /*  - EXACTMC:  Fisher exact test via Monte Carlo estimation
  /*  - RANKS:    Kruskal-Wallis test
  /*  - TTEST:    Satterthwaite t test
  /*  - ANOVA:    One-way analysis of variance
  /* -------------------------------------------- */
  %if &type = chisq %then %do;
    proc freq data = &testdata noprint;
      %if &by ~= %then by &by;;
      table &var * &strata / chisq &testopt;
      output out = rpt_outtest pchi;
    run;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      length plabel $ 25;
      plabel = 'Pearson chi-square test';
      rename p_pchi = pvalue;
    run;
  %end;
  %else %if &type = exact %then %do;
    proc freq data = &testdata noprint;
      %if &by ~= %then by &by;;
      table &var * &strata / fisher &testopt;
      output out = rpt_outtest fisher;
    run;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      length plabel $ 25;
      plabel = 'Fisher exact test';
      rename xp2_fish = pvalue;
    run;
  %end;
  %else %if &type = exactmc %then %do;
    ods exclude all;
    proc freq data = &testdata;
      %if &by ~= %then by &by;;
      table &var * &strata / &testopt;
      exact fisher / mc;
      ods output FishersExactMC = rpt_outtest;
    run;
    ods exclude none;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      where strip(name1) = 'MCP_FISH';
      length plabel $ 25;
      plabel = 'Fisher exact test via Monte Carlo estimation';
      rename nvalue1 = pvalue;
    run;
  %end;
  %else %if &type = ranks %then %do;
    proc npar1way data = &testdata wilcoxon noprint;
      %if &by ~= %then by &by;;
      class &strata;
      var &var;
      output out = rpt_outtest wilcoxon;
    run;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      length plabel $ 25;
      plabel = 'Kruskal-Wallis test';
      rename p_kw = pvalue;
    run;
  %end;
  %else %if &type = ttest %then %do;
    ods exclude all;
    proc ttest data = &testdata;
      %if &by ~= %then by &by;;
      class &strata;
      var &var;
      ods output TTests = rpt_outtest;
    run;
    ods exclude none;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      where strip(method) = 'Satterthwaite';
      length plabel $ 25;
      plabel = 'Satterthwaite t-test';
    run;
  %end;
  %else %if &type = anova %then %do;
    proc anova data = &testdata outstat = rpt_outtest noprint;
      %if &by ~= %then by &by;;
      class &strata;
      model &var = &strata;
    run;
    data rpt_outtest(keep = &by pvalue plabel);
      set rpt_outtest;
      length plabel $ 25;
      plabel = 'ANOVA test';
    run;
  %end;
%mend;

%macro rptttetest(testdata, event, time, strata, censorcode = 0, failcode = , by = , type = );
  %if %sysfunc(exist(rpt_outttetest)) %then %do; %* Clean outdated output;
    proc datasets lib = work nolist;
      delete rpt_outttetest;
    quit;
  %end;
  %if &by ~= %then %do; %* By group;
    proc sort data = &testdata;
      by &by;
    run;
  %end;
  /* The parameter TYPE can have the following values:
  /* -------------------------------------------- */
  /*  - LOGRANK:  Logrank test
  /*  - GRAY:     Gray test
  /* -------------------------------------------- */
  %else %if &type = logrank %then %do;
    ods exclude all;
    proc lifetest data = &testdata;
      %if &by ~= %then by &by;;
      strata &strata / test = logrank;
      time &time * &event(&censorcode);
      ods output HomTests = rpt_outttetest;
    run;
    ods exclude none;
    data rpt_outttetest(keep = &by pvalue plabel);
      set rpt_outttetest;
      length plabel $ 25;
      plabel = 'Log-rank test';
      rename probchisq = pvalue;
    run;
  %end;
  %else %if &type = gray %then %do;
    %if &failcode = %then %do;
      %put ERROR: Event code is needed;
      %abort;
    %end;
    ods exclude all;
    proc lifetest data = &testdata;
      %if &by ~= %then by &by;;
      strata &strata;
      time &time * &event(&censorcode) / failcode = &failcode;
      ods output GrayTest = rpt_outttetest;
    run;
    ods exclude none;
    data rpt_outttetest(keep = &by pvalue plabel);
      set rpt_outttetest;
      length plabel $ 25;
      plabel = 'Gray test';
      rename probchisq = pvalue;
    run;
  %end;
%mend;

%macro rptmultiztest(testdata = , est = , stderr = , by = );
  %if %sysfunc(exist(rpt_outmultiztest)) %then %do; %* Clean outdated output;
    proc datasets lib = work nolist;
      delete rpt_outmultiztest;
    quit;
  %end;
  proc iml;
    use &testdata where(&stderr >= 0);
    read all var {&by};
    close &testdata;
    byvalues = unique(&by);
    do t = 1 to ncol(byvalues);
      bythisvalue = byvalues[t];
      use &testdata where(&stderr >= 0 & &by = bythisvalue);
      read all var {&est &stderr};
      close &testdata;
      nsample = nrow(&est); %* Number of samples;
      if nsample <= 1 then do; %* Single sample;
        out = j(1, 4);
        out[1] = bythisvalue; out[2] = .; out[3] = 0; out[4] = .;
      end;
      else do;
        p1 = &est[1: (nsample - 1)];
        p2 = &est[2: nsample];
        x = p1 - p2;
        var = &stderr ## 2;
        sigma = j(nsample - 1, nsample - 1, 0); %* Init var matrix;
        do i = 1 to (nsample - 1);
          sigma[i, i] = var[i] + var[i + 1];
        end;
        do i = 1 to (nsample - 2);
          sigma[i, i + 1] = -var[i + 1];
          sigma[i + 1, i] = sigma[i, i + 1];
        end;
        chisq = t(x) * inv(sigma) * x;
        df = nsample - 1;
        pvalue = 1 - probchi(chisq, df);
        out = bythisvalue || chisq || df || pvalue;
      end;
      varname = {"&by" 'chisq' 'df' 'pvalue'};
      if t = 1 then create rpt_outmultiztest from out[colname = varname];
      else edit rpt_outmultiztest var varname;
      append from out;
      close rpt_outmultiztest;
    end;
  quit;
%mend;

%macro rptcatalstinit(indata, varlst, outvar = catalst, label = '', usevarlabel = 1);
  data &indata;
    set &indata;
    length &outvar $ 250;
    call missing(&outvar);
    array varlst{*} &varlst;
    do i = 1 to dim(varlst);
      if varlst{i} = 1 then &outvar = catx(' + ', &outvar,
      %if &usevarlabel %then vlabel(varlst{i});
      %else vname(varlst{i}););
    end;
    label &outvar = &label;
    drop &varlst;
  run;
%mend;

%macro rptheaderpush(tb);
  %let rpt_header = 1;
  data tabheader;
    stop;
    set &tb;
  run;
  proc datasets lib = work nolist;
    delete &tb;
  quit;
%mend;

%macro rptpush(tblst);
  %let rpt_tid = %eval(&rpt_tid + 1);
  data tab&rpt_tid;
    set &tblst;
    sid = &rpt_sid;
    tid = &rpt_tid;
  run;
  proc datasets lib = work nolist;
    delete &tblst;
  quit;
%mend;

%macro rptpop(tblst);
  %let ntblst = %sysfunc(countw(&tblst));
  %do t = 1 %to &ntblst;
    %let tb = %scan(&tblst, &t);
    %if &rpt_tid > 0 %then %do;
      data &tb;
        set tab&rpt_tid;
        drop sid tid;
      run;
      proc datasets lib = work nolist;
        delete tab&rpt_tid;
      quit;
      %let rpt_tid = %eval(&rpt_tid - 1);
    %end;
    %else %put WARNING: No table left from the stack for &tb;
  %end;
%mend;

%macro rptreset();
  proc datasets lib = work nolist;
    delete tab1-tab&rpt_tid;
  quit;

  %let rpt_sid = 0;
  %let rpt_tid = 0;
  %let rpt_header = 0;
  %let rpt_sttvar = ;
  %let rpt_sttlbl = ;

  %let rpt_preset_test = ;
  %let rpt_preset_total = ;
%mend;

%macro rptdump(outtable);
  %* Precheck if report results exist;
  %if &rpt_tid <= 0 %then %do;
    %put ERROR: Report results unavailable;
    %abort;
  %end;

  data &outtable;
    set tab1-tab&rpt_tid;
    fid = &rpt_fid;
  run;

  %rptreset();
%mend;

%macro rptadddoc(outfile, orientation = portrait);
  %let rpt_fid = %eval(&rpt_fid + 1);
  %if &orientation = portrait %then %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile;
  %else %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile#;
%mend;

/* External macro functions */
/* -------------------------------------------- */
%macro rptpreset(test = , total = );
  %if &test  ne %then %let rpt_preset_test  = &test;
  %if &total ne %then %let rpt_preset_total = &total;
%mend;

%macro section(title, note = );
  %let rpt_sid = %eval(&rpt_sid + 1);
  data sectionheader;
    length varstr $ 250;
    varstr = &title;
    rwtype = -1;
    %if &note ~= %then %do;
      length rpt_col_note $ 500;
      rpt_col_note = &note;
    %end;
  run;
  %rptpush(sectionheader);
%mend;

%macro subsection(title, note = );
  data subsectionheader;
    length varstr $ 250;
    varstr = &title;
    rwtype = -2;
    %if &note ~= %then %do;
      length rpt_col_note $ 500;
      rpt_col_note = &note;
    %end;
  run;
  %rptpush(subsectionheader);
%mend;

%macro endsection();
  %let rpt_sid = %eval(&rpt_sid + 1);
%mend;

%macro header(
  indata,       /* Input dataset */
  strata,       /* Main group */
  total = 0     /* 1 - Show total column; 0 - No total column */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;
  %if &rpt_header %then %put WARNING: Header already exists;

  data init;
    set &indata(keep = &strata);
    where not missing(&strata);
  run;

  proc means data = init noprint;
    class &strata;
    var &strata;
    output out = cnt n = n;
  run;

  data cnt_result;
    %if not &total %then set cnt(where = (_type_ = 1));
    %else set cnt;;
    length col_id $ 50 col_idlabel $ 250;
    if _type_ = 0 then do;
      col_id = 'rpt_col_total';
      col_idlabel = cat('Total (N = ', n, ')');
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = cat(strip(vvalue(&strata)), ' (N = ', n, ')');
    end;
  run;
  proc transpose data = cnt_result out = cnt_bystrata(drop = _name_);
    var col_id;
    id col_id;
    idlabel col_idlabel;
  run;

  %rptheaderpush(cnt_bystrata);
%mend;

%macro cnt(
  indata,       /* Input dataset */
  strata,       /* Main group */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  label = 'No. of patients',
  rowpct = 0,   /* Display row percentage */
  format = 4.1, /* Format of the numbers */
  note =        /* Additional footnote */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  data init;
    set &indata(keep = &strata);
    where not missing(&strata);
  run;

  proc means data = init noprint;
    class &strata;
    var &strata;
    output out = cnt n = n;
  run;

  %if &rowpct %then %do;
    proc freq data = init noprint;
      table &strata / out = cntpct(drop = count);
    run;
    proc sql;
      alter table cnt add percent num;
      update cnt set percent = (select percent from cntpct where &strata = cnt.&strata) where _type_ = 1;
    quit;
  %end;

  data cnt_result;
    %if not &total %then set cnt(where = (_type_ = 1));
    %else set cnt;;
    length col_id result $ 50 col_idlabel varstr $ 250;
    rwtype = 0;
    varstr = &label;
    %if &rowpct %then %do;
      if 0 <= percent <= 100 then result = cat(n, ' (', strip(put(percent, &format)), ')');
      else result = cat(n);
    %end;
    %else %do;
      result = cat(n);
    %end;
    if _type_ = 0 then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = cnt_result;
    by rwtype varstr;
  proc transpose data = cnt_result out = cnt_bystrata(drop = _name_);
    by rwtype varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table cnt_bystrata add rpt_col_note varchar(500);
      update cnt_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %rptpush(cnt_bystrata);
%mend;

%macro nlevel(
  indata,       /* Input dataset */
  var,          /* Variable */
  strata,       /* Main group */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  label = 'No. of levels',
  note =        /* Additional footnote */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  data init;
    set &indata(keep = &strata &var);
    where not missing(&strata) and not missing(&var);
  run;

  proc sql noprint;
    create table nlevel as select &strata, count(distinct &var) as nlevel from init group by &strata;
    create table total_nlevel as select count(distinct &var) as nlevel from init;
  quit;

  data nlevel_result;
    %if not &total %then set nlevel;
    %else set nlevel total_nlevel(in = in_total);;
    length col_id result $ 50 col_idlabel varstr $ 250;
    rwtype = 0;
    varstr = &label;
    result = cat(nlevel);
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = nlevel_result;
    by rwtype varstr;
  proc transpose data = nlevel_result out = nlevel_bystrata(drop = _name_);
    by rwtype varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table nlevel_bystrata add rpt_col_note varchar(500);
      update nlevel_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %rptpush(nlevel_bystrata);
%mend;

%macro nlevelsubgrp(
  indata,       /* Input dataset */
  var,          /* Variable */
  subgrp,       /* Sub-group */
  strata,       /* Main group */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  label = 'No. of levels',
  missingtag = 'Missing',
  note =        /* Additional footnote */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  data init;
    set &indata(keep = &strata &subgrp &var);
    where not missing(&strata) and not missing(&var);
  run;

  proc sql noprint;
    create table header as select distinct &strata from init;
    create table nlevel as
    select h.&strata, s.&subgrp, coalesce(nlevel, 0) as nlevel
    from header h cross join (select distinct &subgrp from init) s
    left join (select &strata, &subgrp, count(distinct &var) as nlevel from init group by &strata, &subgrp) c
    on h.&strata = c.&strata and s.&subgrp = c.&subgrp;
    create table total_nlevel as select &subgrp, count(distinct &var) as nlevel from init group by &subgrp;
  quit;

  data nlevel_result;
    %if not &total %then set header(in = in_header) nlevel;
    %else set header(in = in_header) nlevel total_nlevel(in = in_total);;
    length col_id result $ 50 col_idlabel varstr $ 250;
    if in_header then do;
      rwtype = 0;
      varstr = &label;
    end;
    else do;
      rwtype = 1;
      if missing(&subgrp) and strip(vvalue(&subgrp)) in ('', '.') then varstr = &missingtag;
      else varstr = vvalue(&subgrp);
      result = cat(nlevel);
    end;
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = nlevel_result;
    by rwtype &subgrp varstr;
  proc transpose data = nlevel_result out = nlevel_bystrata(drop = _name_);
    by rwtype &subgrp varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table nlevel_bystrata add rpt_col_note varchar(500);
      update nlevel_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  proc sql noprint;
    create table nlevel_sorted(drop = &subgrp) as
    select * from nlevel_bystrata order by rwtype, missing(&subgrp), &subgrp;
  quit;

  %rptpush(nlevel_sorted);
%mend;

%macro cntcenter(
  indata,       /* Input dataset */
  cid,          /* Center ID */
  strata,       /* Main group */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  note =        /* Additional footnote */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %nlevel(
    &indata, &cid, &strata,
    total = &total,
    label = 'No. of centers',
    note = &note
  );
%mend;

%macro freq(
  indata,           /* Input dataset */
  varlst,           /* Variables to be analyzed */
  strata,           /* Main group */
  coln = 0,         /* 1 - Column total in header */
  colpct = 1,       /* 1 - Column percentages; 0 - Row percentages */
  nopct = 0,        /* 1 - No percentage; 0 - Show percentages */
  test = 0,         /* 1 - Pearsons chi-square test; 2 - Fishers exact test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  missing = 1,      /* 1 - Include missing in computations of percentages and statistics */
  missingtag = 'Missing',
  orderbytotal = 0, /* Order the rows by total counts */
  orderby = ,       /* Order the rows by specified values */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &colpct %then %let pctvar = pct_col;
  %else %let pctvar = pct_row;
  %if &missing %then %let missopt = missing;
  %else %let missopt = missprint;
  %if &tail and &nopct %then %let vtail = 'no.';
  %else %if &tail %then %let vtail = 'no. (%)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  proc means data = init noprint;
    class &strata;
    var &strata;
    output out = header n = count;
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);
    proc freq data = init noprint;
      table &var * &strata / sparse &missopt outpct out = freqtab&i(drop = percent rename = (&pctvar = percent)); %* By STRATA;
      table &var / &missopt out = total_freqtab&i; %* Overall;
    run;

    data freqtab_result&i;
      %if not &total %then set header(where = (_type_ = 1) in = in_header) freqtab&i;
      %else set header(in = in_header) freqtab&i total_freqtab&i(in = in_total);;
      length col_id result $ 50 col_idlabel varstr $ 250;
      if in_header then do;
        rwtype = 0;
        varstr = catx(' - ', vlabel(&var), &vtail);
        %if &coln %then result = cat('N = ', count);;
      end;
      else do;
        rwtype = 1;
        if missing(&var) and strip(vvalue(&var)) in ('', '.') then varstr = &missingtag;
        else varstr = vvalue(&var);
        %if &nopct %then %do;
          result = cat(count);
        %end;
        %else %do;
          if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
          else result = cat(count);
        %end;
      end;
      if (in_header and _type_ = 0) or in_total then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
    run;
    proc sort data = freqtab_result&i;
      by rwtype &var varstr;
    proc transpose data = freqtab_result&i out = freqtab_bystrata&i(drop = _name_);
      by rwtype &var varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %* Chi-square p-value;
    %if &test >= 1 %then %do;
      %if &test = 1 %then %let testtype = chisq;
      %else %if &test = 2 %then %do;
        ods exclude all;
        proc freq data = init nlevels;
          table &strata &var / noprint;
          ods output NLevels = freqtab_nlevels&i;
        run;
        ods exclude none;
        proc sql noprint;
          select nlevels, nnonmisslevels
          into :varlevels1 thru :varlevels2, :varnonmisslevels1 thru :varnonmisslevels2 from freqtab_nlevels&i;
        quit;
        %if &missing %then %do;
          %if &varlevels1 > 2 or &varlevels2 > 2 %then %let testtype = exactmc;
          %else %let testtype = exact;
        %end;
        %else %do;
          %if &varnonmisslevels1 > 2 or &varnonmisslevels2 > 2 %then %let testtype = exactmc;
          %else %let testtype = exact;
        %end;
      %end;
      %rpttest(init, &var, &strata, type = &testtype, testopt = &missopt);
      proc sql noprint;
        alter table freqtab_bystrata&i add rpt_col_pval varchar(20), plabel varchar(25);
        update freqtab_bystrata&i set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outtest),
        plabel = (select plabel from rpt_outtest) where rwtype = 0;
      quit;
    %end;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table freqtab_bystrata&i add rpt_col_note varchar(500);
        update freqtab_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %if &orderbytotal or &orderby ~= %then %do;
      %if &orderbytotal %then %do;
        data freqorderby&i;
          set total_freqtab&i;
          orderby = 100 - percent;
          keep &var orderby;
        run;
      %end;
      %else %if &orderby ~= %then %do;
        proc iml;
          vval = t({&orderby});
          nval = nrow(vval);
          vseq = t(do(1, nval, 1));
          outdata = vval || vseq;
          create freqorderby&i from outdata[colname = {"&var" 'orderby'}];
          append from outdata;
          close freqorderby&i;
        quit;
      %end;
      proc sql noprint;
        create table freqtab_sorted&i(drop = &var) as
        select f.* from freqtab_bystrata&i f
        left join freqorderby&i o on f.&var = o.&var
        order by f.rwtype, missing(o.orderby), o.orderby, missing(f.&var), f.&var;
      quit;
    %end;
    %else %do;
      proc sql noprint;
        create table freqtab_sorted&i(drop = &var) as
        select * from freqtab_bystrata&i
        order by rwtype, missing(&var), &var;
      quit;
    %end;

    %rptpush(freqtab_sorted&i);
  %end;
%mend;

%macro freqsubgrp(
  indata,           /* Input data set */
  varlst,           /* Variables to be analyzed */
  subgrp,           /* Sub-group variable */
  strata,           /* Main group */
  coln = 0,         /* 1 - Column total in header */
  subfreq = 0,      /* 1 - Show sub-group total and percentages */
  colpct = 1,       /* 1 - Show column percentages; 0 - Show row percentages */
  nopct = 0,        /* 1 - No percentage; 0 - Show percentages */
  pctbysub = 0,     /* 1 - Percentages per sub-groups; 0 - Percentages per the entire group */
  test = 0,         /* 1 - Pearsons chi-square test; 2 - Fishers exact test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  missing = 1,      /* 1 - Include missing in computations of percentages and statistics; 0 - Display missing only */
  missingtag = 'Missing',
  orderbytotal = 0, /* Order the rows by total counts */
  orderby = ,       /* Order the rows by specified values */
  ordersubby = ,    /* Order the sub-groups by specified values */
  hidesub = ,       /* Hide specified sub-groups */
  foldsub = ,       /* Fold specified sub-groups */
  foldemptysub = 0, /* Fold empty/all missing sub-groups */
  usesublabel = 0,  /* Use sub-group label as the entry label */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &colpct %then %let pctvar = pct_col;
  %else %let pctvar = pct_row;
  %if &missing %then %let missopt = missing;
  %else %let missopt = missprint;
  %if &foldsub ~= or &foldemptysub %then %let subfreq = 1;
  %if &tail and &nopct %then %let vtail = 'no.';
  %else %if &tail %then %let vtail = 'no. (%)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &subgrp &strata);
    where not missing(&strata);
  run;

  proc means data = init noprint;
    class &strata;
    var &strata;
    output out = header n = count;
  run;
  proc freq data = init noprint;
    table &subgrp * &strata / sparse &missopt outpct out = subfreqtab(drop = percent rename = (&pctvar = percent)); %* By STRATA;
    table &subgrp / &missopt out = total_subfreqtab; %* Overall;
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);
    %if &colpct and not &pctbysub %then %do;
      proc freq data = init noprint;
        table &strata * &subgrp * &var / outpct sparse &missopt out = freqtab&i(drop = percent rename = (pct_tabl = percent));
        table &subgrp * &var / &missopt out = total_freqtab&i; * Do not use SPARSE;
      run;
      proc sort data = freqtab&i; by &subgrp &var;
      proc sort data = total_freqtab&i; by &subgrp &var;
      data freqtab&i;
        merge freqtab&i total_freqtab&i(keep = &subgrp &var in = in_total);
        by &subgrp &var;
        if in_total;
      run;
    %end;
    %else %do;
      proc sort data = init; by &subgrp;
      proc freq data = init noprint;
        by &subgrp;
        table &var * &strata / outpct sparse &missopt out = freqtab&i(drop = percent rename = (&pctvar = percent));
        table &var / &missopt out = total_freqtab&i;
      run;
    %end;

    data freqtab_result&i;
      %if not &total %then set header(where = (_type_ = 1) in = in_header) subfreqtab(in = in_sub) freqtab&i;
      %else set header(in = in_header) subfreqtab(in = in_sub) freqtab&i total_subfreqtab(in = in_subtotal) total_freqtab&i(in = in_total);;
      length col_id result $ 50 col_idlabel varstr $ 250;
      if in_header then do;
        rwtype = 0;
        %if &usesublabel %then varstr = catx(' - ', vlabel(&subgrp), &vtail);
        %else varstr = catx(' - ', vlabel(&var), &vtail);;
        %if &coln %then result = cat('N = ', count);;
      end;
      else if in_sub or in_subtotal then do;
        rwtype = 1;
        if missing(&subgrp) and strip(vvalue(&subgrp)) in ('', '.') then varstr = &missingtag;
        else varstr = vvalue(&subgrp);
        %if &subfreq and &nopct %then %do;
          result = cat(count);
        %end;
        %else %if &subfreq %then %do;
          if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
          else result = cat(count);
        %end;
      end;
      else do;
        rwtype = 2;
        if missing(&var) and strip(vvalue(&var)) in ('', '.') then varstr = &missingtag;
        else varstr = vvalue(&var);
        %if &nopct %then %do;
          result = cat(count);
        %end;
        %else %do;
          if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
          else result = cat(count);
        %end;
      end;
      if (in_header and _type_ = 0) or in_subtotal or in_total then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
    run;
    proc sort data = freqtab_result&i;
      by rwtype &subgrp &var varstr;
    proc transpose data = freqtab_result&i out = freqtab_bystrata&i(drop = _name_);
      by rwtype &subgrp &var varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %if &hidesub ~= or &foldsub ~= or &foldemptysub %then %do;
      proc sql noprint;
        %if &hidesub ~= %then delete from freqtab_bystrata&i where rwtype in (1, 2) and &subgrp in (&hidesub);;
        %if &foldsub ~= %then delete from freqtab_bystrata&i where rwtype = 2 and &subgrp in (&foldsub);;
        %if &foldemptysub %then
        delete from freqtab_bystrata&i f where rwtype = 2 and (
        select count(&var) from freqtab_result&i where rwtype = 2 and &subgrp = f.&subgrp) = 0;;
      quit;
    %end;

    %if &test >= 1 %then %do;
      %if &test = 1 %then %let testtype = chisq;
      %else %if &test = 2 %then %let testtype = exact;
      %rpttest(init, &var, &strata, by = &subgrp, type = &testtype, testopt = &missopt);
      proc sql noprint;
        alter table freqtab_bystrata&i add rpt_col_pval varchar(20), plabel varchar(25);
        update freqtab_bystrata&i f set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outtest where &subgrp = f.&subgrp),
        plabel = (select plabel from rpt_outtest where &subgrp = f.&subgrp) where rwtype = 1;
      quit;
    %end;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table freqtab_bystrata&i add rpt_col_note varchar(500);
        update freqtab_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %if &ordersubby ~= %then %do;
      proc iml;
        vval = t({&ordersubby});
        nval = nrow(vval);
        vseq = t(do(1, nval, 1));
        outdata = vval || vseq;
        create freqordersubby&i from outdata[colname = {"&subgrp" 'orderby'}];
        append from outdata;
        close freqordersubby&i;
      quit;
    %end;
    %if &orderbytotal %then %do;
      data freqorderby&i;
        set total_freqtab&i;
        orderby = 100 - percent;
        keep &subgrp &var orderby;
      run;
    %end;
    %else %if &orderby ~= %then %do;
      proc iml;
        vval = t({&orderby});
        nval = nrow(vval);
        vseq = t(do(1, nval, 1));
        outdata = vval || vseq;
        create freqorderby&i from outdata[colname = {"&var" 'orderby'}];
        append from outdata;
        close freqorderby&i;
      quit;
    %end;
    %if &ordersubby ~= %then %do;
      %if &orderbytotal %then %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select f.* from freqtab_bystrata&i f
          left join freqordersubby&i s on f.&subgrp = s.&subgrp
          left join freqorderby&i o on f.&subgrp = o.&subgrp and f.&var = o.&var
          order by ifn(f.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(f.&subgrp), f.&subgrp,
          ifn(f.rwtype > 1, 1, 0), o.orderby, missing(f.&var), f.&var;
        quit;
      %end;
      %else %if &orderby ~= %then %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select f.* from freqtab_bystrata&i f
          left join freqordersubby&i s on f.&subgrp = s.&subgrp
          left join freqorderby&i o on f.&var = o.&var
          order by ifn(f.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(f.&subgrp), f.&subgrp,
          ifn(f.rwtype > 1, 1, 0), missing(o.orderby), o.orderby, missing(f.&var), f.&var;
        quit;
      %end;
      %else %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select * from freqtab_bystrata&i f
          left join freqordersubby&i s on f.&subgrp = s.&subgrp
          order by ifn(f.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(f.&subgrp), f.&subgrp,
          ifn(f.rwtype > 1, 1, 0), missing(f.&var), f.&var;
        quit;
      %end;
    %end;
    %else %do;
      %if &orderbytotal %then %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select f.* from freqtab_bystrata&i f
          left join freqorderby&i o on f.&subgrp = o.&subgrp and f.&var = o.&var
          order by ifn(f.rwtype > 0, 1, 0), missing(f.&subgrp), f.&subgrp,
          ifn(f.rwtype > 1, 1, 0), o.orderby, missing(f.&var), f.&var;
        quit;
      %end;
      %else %if &orderby ~= %then %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select f.* from freqtab_bystrata&i f
          left join freqorderby&i o on f.&var = o.&var
          order by ifn(f.rwtype > 0, 1, 0), missing(f.&subgrp), f.&subgrp,
          ifn(f.rwtype > 1, 1, 0), missing(o.orderby), o.orderby, missing(f.&var), f.&var;
        quit;
      %end;
      %else %do;
        proc sql noprint;
          create table freqtab_sorted&i(drop = &subgrp &var) as
          select * from freqtab_bystrata&i
          order by ifn(rwtype > 0, 1, 0), missing(&subgrp), &subgrp,
          ifn(rwtype > 1, 1, 0), missing(&var), &var;
        quit;
      %end;
    %end;

    %rptpush(freqtab_sorted&i);
  %end;
%mend;

%macro median(
  indata,       /* Input dataset */
  varlst,       /* Variables to be analyzed */
  strata,       /* Main group */
  test = 0,     /* 1 - Do baseline test; 0 - No test */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  note = ,      /* Additional footnote */
  tail = 1,     /* Append descriptive text to the label */
  format = 8.1  /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'median (min-max)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);
    proc means data = init noprint;
      class &strata;
      id &var;
      var &var;
      output out = median&i n = n min = min p50 = p50 max = max;
    run;
    data median_result&i;
      %if not &total %then set median&i(where = (_type_ = 1));
      %else set median&i;;
      length col_id result $ 50 col_idlabel varstr $ 250;
      rwtype = 0;
      varstr = catx(' - ', vlabel(&var), &vtail);
      if n > 0 then result = cat(strip(put(p50, &format)), ' (', strip(put(min, &format)), '-', strip(put(max, &format)), ')');
      else result = 'NE';
      if _type_ = 0 then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
    run;
    proc sort data = median_result&i;
      by rwtype varstr;
    proc transpose data = median_result&i out = median_bystrata&i(drop = _name_);
      by rwtype varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %* Perform the rank sum test;
    %if &test = 1 %then %do;
      %rpttest(init, &var, &strata, type = ranks);
      proc sql noprint;
        alter table median_bystrata&i add rpt_col_pval varchar(20), plabel varchar(25);
        update median_bystrata&i set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outtest),
        plabel = (select plabel from rpt_outtest) where rwtype = 0;
      quit;
    %end;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table median_bystrata&i add rpt_col_note varchar(500);
        update median_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %rptpush(median_bystrata&i);
  %end;
%mend;

%macro mediansubgrp(
  indata,           /* Input dataset */
  varlst,           /* Variables to be analyzed */
  subgrp,           /* Sub-group variable */
  strata,           /* Main group */
  test = 0,         /* 1 - Do baseline test; 0 - No test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  usesublabel = 0,  /* Use sub-group label as the entry label */
  note = ,          /* Additional footnote */
  missingtag = 'Missing',
  ordersubby = ,    /* Order the sub-groups by specified values */
  tail = 1,         /* Append descriptive text to the label */
  format = 8.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'median (min-max)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &subgrp &strata);
    where not missing(&strata);
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);
    proc means data = init noprint;
      class &strata &subgrp;
      id &var;
      var &var;
      output out = median&i n = n min = min p50 = p50 max = max;
    run;
    data median_result&i;
      %if not &total %then set median&i(where = (_type_ in (2, 3)));
      %else set median&i;;
      length col_id result $ 50 col_idlabel varstr $ 250;
      if _type_ in (0, 2) then do;
        rwtype = 0;
        %if &usesublabel %then varstr = catx(' - ', vlabel(&subgrp), &vtail);
        %else varstr = catx(' - ', vlabel(&var), &vtail);;
      end;
      else do;
        rwtype = 1;
        if missing(&subgrp) and strip(vvalue(&subgrp)) in ('', '.') then varstr = &missingtag;
        else varstr = vvalue(&subgrp);
        if n > 0 then result = cat(strip(put(p50, &format)), ' (', strip(put(min, &format)), '-', strip(put(max, &format)), ')');
        else result = 'NE';
      end;
      if _type_ in (0, 1) then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
    run;
    proc sort data = median_result&i;
      by &subgrp rwtype varstr;
    proc transpose data = median_result&i out = median_bystrata&i(drop = _name_);
      by &subgrp rwtype varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %* Perform the rank sum test;
    %if &test = 1 %then %do;
      %rpttest(init, &var, &strata, by = &subgrp, type = ranks);
      proc sql noprint;
        alter table median_bystrata&i add rpt_col_pval varchar(20), plabel varchar(25);
        update median_bystrata&i a set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outtest where a.&subgrp = &subgrp),
        plabel = (select plabel from rpt_outtest where a.&subgrp = &subgrp) where rwtype = 1;
      quit;
    %end;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table median_bystrata&i add rpt_col_note varchar(500);
        update median_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %if &ordersubby ~= %then %do;
      proc iml;
        vval = t({&ordersubby});
        nval = nrow(vval);
        vseq = t(do(1, nval, 1));
        outdata = vval || vseq;
        create median_ordersubby&i from outdata[colname = {"&subgrp" 'orderby'}];
        append from outdata;
        close median_ordersubby&i;
      quit;
      proc sql noprint;
        create table median_sorted&i(drop = &subgrp &var) as
        select * from median_bystrata&i f
        left join median_ordersubby&i s on f.&subgrp = s.&subgrp
        order by ifn(f.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(f.&subgrp), f.&subgrp,
        ifn(f.rwtype > 1, 1, 0), missing(f.&var), f.&var;
      quit;
    %end;
    %else %do;
      proc sql noprint;
        create table median_sorted&i(drop = &subgrp) as
        select * from median_bystrata&i order by rwtype, missing(&subgrp), &subgrp;
      quit;
    %end;

    %rptpush(median_sorted&i);
  %end;
%mend;

%macro medfreq(
  indata,           /* Input dataset */
  varkeylst,        /* Variable keyword list */
  strata,           /* Main group */
  test = 0,         /* 1 - Do baseline test; 0 - No test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  outstat = median_min_max,
  missing = 1,      /* 1 - Include missing in computations of percentages and statistics */
  missingtag = 'Missing',
  orderbytotal = 0, /* Order rows by total counts */
  orderby = ,       /* Order rows by specified values */
  tail = 1,         /* Append descriptive text to the label */
  mformat = 8.1,    /* Format of the univariate part */
  sformat = 8.2,    /* Format of the standard errors in the univariate part */
  fformat = 4.1     /* Format of the frequency part */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %let mvarlst = ;
  %let fvarlst = ;
  %let nvarkeys = %sysfunc(countw(%quote(&varkeylst), %str( )));
  %do i = 1 %to &nvarkeys;
    %let varkey = %scan(%quote(&varkeylst), &i, %str( ));
    %let mvarlst = &mvarlst %sysfunc(prxchange(%str(s/(.+)\|.+/$1/), 1, &varkey));
    %let fvarlst = &fvarlst %sysfunc(prxchange(%str(s/.+\|(.+)/$1/), 1, &varkey));
  %end;

  %univar(
    &indata, &mvarlst, &strata,
    total = &total,
    outstat = &outstat,
    format = &mformat,
    sformat = &sformat
  );
  %freq(
    &indata, &fvarlst, &strata,
    total = &total,
    test = &test,
    missing = &missing,
    missingtag = &missingtag,
    orderbytotal = &orderbytotal,
    orderby = &orderby,
    tail = &tail,
    format = &fformat,
    note = &note
  );

  %let mtablst = ;
  %let ftablst = ;
  %do i = &nvarkeys %to 1 %by -1;
    %let mtablst = &mtablst mtable&i;
    %let ftablst = &ftablst ftable&i;
  %end;
  %rptpop(&ftablst &mtablst);

  %do i = 1 %to &nvarkeys;
    data medfreq&i;
      set
      ftable&i(where = (rwtype = 0))
      mtable&i(where = (rwtype = 1))
      ftable&i(where = (rwtype = 1));
    run;
    %rptpush(medfreq&i);
  %end;
%mend;

%macro medlst(
  indata,       /* Input dataset */
  varlst,       /* Variable keyword list */
  strata,       /* Main group */
  label = '',   /* Label of the entry */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  test = 0,     /* 1 - Do baseline test; 0 - No test */
  tail = 1,     /* Append descriptive text to the label */
  format = 8.1  /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'median (min-max)';
  %else %let vtail = '';

  data mheader;
    length varstr $ 250;
    varstr = catx(' - ', &label, &vtail);
    rwtype = 0;
  run;

  %median(
    &indata, &varlst, &strata,
    total = &total,
    test = &test,
    format = &format,
    tail = 0
  );

  %let mtablst = ;
  %let nvars = %sysfunc(countw(%quote(&varlst), %str( )));
  %do i = &nvars %to 1 %by -1;
    %let mtablst = &mtablst mtable&i;
  %end;
  %rptpop(&mtablst);

  %do i = 1 %to &nvars;
    data mbody&i;
      set mtable&i;
      rwtype = 1;
    run;
  %end;

  data medlst_final;
    set mheader mbody1-mbody&nvars;
  run;
  %rptpush(medlst_final);
%mend;

%macro univarkey(
  indata,           /* Input dataset */
  varlst,           /* Variables to be analyzed */
  strata,           /* Main group */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  statkey = sum,    /* Keyword for the statistic */
  tail = 1,         /* Append descriptive text to the label */
  tailtag = 'sum',  /* Descriptive text for the statistic */
  format = 8.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = &tailtag;
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);

    proc means data = init noprint;
      class &strata;
      id &var;
      var &var;
      output out = univar&i n = n &statkey = &statkey;
    run;
    data univar_result&i;
      %if not &total %then set univar&i(where = (_type_ = 1));
      %else set univar&i;;
      length col_id result $ 50 col_idlabel varstr $ 250;
      rwtype = 0;
      varstr = catx(' - ', vlabel(&var), &vtail);
      if not missing(&statkey) then result = strip(put(&statkey, &format));
      else result = 'NE';
      if _type_ = 0 then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
    run;
    proc sort data = univar_result&i;
      by rwtype varstr;
    proc transpose data = univar_result&i out = univar_bystrata&i(drop = _name_);
      by rwtype varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table univar_bystrata&i add rpt_col_note varchar(500);
        update univar_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %rptpush(univar_bystrata&i);
  %end;
%mend;

%macro univar(
  indata,       /* Input dataset */
  varlst,       /* Variables to be analyzed */
  strata,       /* Main group */
  total = 0,    /* 1 - Show total column; 0 - No total column */
  note = ,      /* Additional footnote */
  outstat = n_total mean_std median_p25_p75 min_max,
  format = 8.1, /* Format of the numbers */
  sformat = 8.2 /* Format of the standard errors */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %let nstatkey = %sysfunc(countw(&outstat));

  proc format;
    value $statkeyfmt
    n_nmiss = 'n / Missing'
    n_total = 'n / N'
    n = 'n'
    nmiss = 'Missing'
    mean_std = 'Mean (SD)'
    mean = 'Mean'
    std = 'SD'
    median = 'Median'
    min = 'Min'
    max = 'Max'
    min_max = 'Min-Max'
    p5_p95 = '5th-95th pctl'
    p5 = '5th pctl'
    p95 = '95th pctl'
    p25_p75 = '25th-75th pctl'
    p25 = '25th pctl'
    p75 = '75th pctl'
    median_min_max = 'Median (min-max)'
    median_p25_p75 = 'Median (25th-75th pctl)';
  run;

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);
    proc means data = init noprint;
      class &strata;
      id &var;
      var &var;
      output out = univar&i
      n = n nmiss = nmiss mean = mean std = std
      median = median min = min max = max
      p5 = p5 p25 = p25 p75 = p75 p95 = p95;
    run;

    data univar_result&i;
      %if not &total %then set univar&i(where = (_type_ = 1));
      %else set univar&i;;
      length col_id result $ 50 col_idlabel varstr $ 250;
      if _type_ = 0 then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
      %* Header;
      rwtype = 0;
      varstr = vlabel(&var);
      output;
      %* Body;
      rwtype = 1;
      %do k = 1 %to &nstatkey;
        %let statkey = %lowcase(%scan(&outstat, &k));
        kid = &k;
        varstr = put("&statkey", $statkeyfmt.);
        %if &statkey = n or &statkey = nmiss %then %do;
          result = cat(&statkey);
        %end;
        %else %if &statkey = std %then %do;
          if n > 1 then result = strip(put(std, &sformat));
          else result = 'N/A';
        %end;
        %else %if &statkey = mean
        or &statkey = median or &statkey = min or &statkey = max
        or &statkey = p5 or &statkey = p25 or &statkey = p75 or &statkey = p95 %then %do;
          if n > 0 then result = strip(put(&statkey, &format));
          else result = 'N/A';
        %end;
        %else %if &statkey = n_nmiss %then %do;
          result = catx('/', n, nmiss);
        %end;
        %else %if &statkey = n_total %then %do;
          result = catx('/', n, n + nmiss);
        %end;
        %else %if &statkey = min_max or &statkey = p5_p95 or &statkey = p25_p75 %then %do;
          %let statkeypt1 = %scan(&statkey, 1, _);
          %let statkeypt2 = %scan(&statkey, 2, _);
          if n > 0 then result = catx('-', put(&statkeypt1, &format), put(&statkeypt2, &format));
          else result = 'N/A';
        %end;
        %else %if &statkey = mean_std %then %do;
          if n > 1 then result = cat(strip(put(mean, &format)), ' (', strip(put(std, &sformat)), ')');
          else if n = 1 then result = cat(strip(put(mean, &format)), ' (N/A)');
          else result = 'N/A';
        %end;
        %else %if &statkey = median_min_max or &statkey = median_p25_p75 %then %do;
          %let statkeypt1 = %scan(&statkey, 1, _);
          %let statkeypt2 = %scan(&statkey, 2, _);
          %let statkeypt3 = %scan(&statkey, 3, _);
          if n > 0 then result = cat(strip(put(&statkeypt1, &format)), ' (', strip(put(&statkeypt2, &format)), '-', strip(put(&statkeypt3, &format)), ')');
          else result = 'N/A';
        %end;
        %else %do;
          result = '';
        %end;
        output;
      %end;
    run;
    proc sort data = univar_result&i;
      by rwtype kid varstr;
    proc transpose data = univar_result&i out = univar_bystrata&i(drop = _name_ kid);
      by rwtype kid varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table univar_bystrata&i add rpt_col_note varchar(500);
        update univar_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %rptpush(univar_bystrata&i);
  %end;
%mend;

%macro prop(
  indata,           /* Input dataset */
  varlst,           /* Variables to be analyzed */
  strata,           /* Main group */
  showci = 1,       /* Display proportion CIs */
  denominator = 1,  /* 1 - Show denominator; 0 - Do not show denominator */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  test = 0,         /* 1 - Chi-square test; 2 - Fishers exact test */
  note = ,          /* Additional footnote */
  missing = 1,      /* 1 - Include missing in computations of proportions and statistics */
  tail = 1,         /* Append descriptive text to the label */
  fformat = 4.1,    /* Format of the frequency percentages */
  pformat = 8.1     /* Format of the proportions */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &missing %then %let missopt = missing;
  %else %let missopt = missprint;
  %if &tail and &denominator %then %let vtail = 'no./total no. (%)';
  %else %if &tail %then %let vtail = 'no. (%)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  %let nvarlst = %sysfunc(countw(&varlst));
  %do i = 1 %to &nvarlst;
    %let var = %scan(&varlst, &i);

    proc sql noprint;
      create table trtlevel&i as
      select distinct &strata, 1 as &var from init;
    quit;
    proc sort data = trtlevel&i; by &strata &var;
    proc sort data = init out = init&i(keep = &strata &var); by &strata &var;
    data init&i;
      merge trtlevel&i(in = in_level) init&i(in = in_data);
      by &strata &var;
      if in_level and not in_data then wgt = 0;
      else wgt = 1;
      format &var;
    run;

    proc sort data = init&i;
      by &strata;
    proc freq data = init&i noprint; %* By STRATA;
      by &strata;
      weight wgt / zeros;
      table &var / sparse binomial(level = '1' outlevel) &missopt out = freqtab&i;
      output out = bintab&i n nmiss binomial;
    run;
    proc freq data = init&i noprint; %* Overall;
      weight wgt / zeros;
      table &var / binomial(level = '1' outlevel) &missopt out = total_freqtab&i;
      output out = total_bintab&i n nmiss binomial;
    run;

    proc sql noprint;
      create table proptab&i as
      select b.*, f.&var, f.count, f.percent from bintab&i b
      left join freqtab&i f on b.&strata = f.&strata and b.levelvalue = f.&var;
      create table total_proptab&i as
      select b.*, f.&var, f.count, f.percent from total_bintab&i b
      left join total_freqtab&i f on b.levelvalue = f.&var;
    quit;

    data proptab_result&i;
      %if not &total %then set proptab&i;
      %else set proptab&i total_proptab&i(in = in_total);;
      length col_id result $ 50 col_idlabel varstr $ 250;
      if in_total then do;
        col_id = 'rpt_col_total';
        col_idlabel = 'Total';
      end;
      else do;
        col_id = cats('rpt_col', &strata);
        col_idlabel = strip(vvalue(&strata));
      end;
      %* Header;
      rwtype = 0;
      varstr = catx(' - ', vlabel(&var), &vtail);
      %if &denominator %then %do;
        %if &missing %then %let ntotal = n + nmiss;
        %else %let ntotal = n;
        if 0 <= percent <= 100 then result = cat(count, '/', &ntotal, ' (', strip(put(percent, &fformat)), ')');
        else result = cat(count, '/', &ntotal);
      %end;
      %else %do;
        if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &fformat)), ')');
        else result = cat(count);
      %end;
      output;
      %* Body;
      %if &showci %then %do;
        rwtype = 1;
        varstr = 'Proportion (95% CI)';
        result = cat(strip(put(_bin_, &pformat)), ' (', strip(put(xl_bin, &pformat)), '-', strip(put(xu_bin, &pformat)), ')');
        output;
      %end;
    run;
    proc sort data = proptab_result&i; by rwtype varstr;
    proc transpose data = proptab_result&i out = proptab_bystrata&i(drop = _name_);
      by rwtype varstr;
      id col_id;
      idlabel col_idlabel;
      var result;
    run;

    %* Chi-square p-value;
    %if &test >= 1 %then %do;
      %if &test = 1 %then %let testtype = chisq;
      %else %if &test = 2 %then %let testtype = exact;
      data init_proptest;
        set init;
        keep &var &strata;
        if missing(&var) then %if &missing %then &var = 0; %else delete;;
      run;
      %rpttest(init_proptest, &var, &strata, type = &testtype);
      proc sql noprint;
        alter table proptab_bystrata&i add rpt_col_pval varchar(20), plabel varchar(25);
        update proptab_bystrata&i set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outtest),
        plabel = (select plabel from rpt_outtest) where rwtype = 0;
      quit;
    %end;

    %if &note ~= %then %do;
      proc sql noprint;
        alter table proptab_bystrata&i add rpt_col_note varchar(500);
        update proptab_bystrata&i set rpt_col_note = &note where rwtype = 0;
      quit;
    %end;

    %rptpush(proptab_bystrata&i);
  %end;
%mend;

%macro proplst(
  indata,           /* Input dataset */
  varkeylst,        /* Variable list */
  strata,           /* Main group */
  label = '',       /* Header label */
  noheader = 0,     /* No header */
  denominator = 1,  /* 1 - Show denominator; 0 - Do not show denominator */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  missing = 1,      /* 1 - Include missing in computations of proportions */
  orderbytotal = 0, /* Order the rows by total counts */
  sparse = 0,       /* 1 - Display variables with zero count */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %let varlst = %sysfunc(prxchange(%str(s/>>|<<//), -1, %quote(&varkeylst)));
  %if &noheader %then %let hoffset = 0;
  %else %let hoffset = 1;
  %if &missing %then %let missopt = missing;
  %else %let missopt = missprint;
  %if &tail and &denominator %then %let vtail = 'no./total no. (%)';
  %else %if &tail %then %let vtail = 'no. (%)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata);
    where not missing(&strata);
  run;

  proc sql noprint;
    create table header as
    select distinct &strata from init;
  quit;

  %let offset = 0;
  %let varid = 0;
  %let propvarlst = ;
  %let proptablst = ;
  %let total_proptablst = ;
  %do i = 1 %to %sysfunc(countw(%quote(&varkeylst), %str( )));
    %let varkey = %scan(%quote(&varkeylst), &i, %str( ));
    %if %quote(&varkey) = %str(>>) %then %let offset = %eval(&offset + 1);
    %else %if %quote(&varkey) = %str(<<) %then %do;
      %if &offset > 0 %then %let offset = %eval(&offset - 1);
      %else %let offset = 0;
    %end;
    %else %do;
      %let var = &varkey;
      %let varid = %eval(&varid + 1);
      proc sql noprint;
        create table trtlevel&i as
        select distinct &strata, 1 as &var from init;
      quit;
      proc sort data = trtlevel&i; by &strata &var;
      proc sort data = init out = init&i(keep = &strata &var); by &strata &var;
      data init&i;
        merge trtlevel&i(in = in_level) init&i(in = in_data);
        by &strata &var;
        if in_level and not in_data then wgt = 0;
        else wgt = 1;
      run;

      proc freq data = init&i noprint;
        weight wgt / zeros;
        table &var * &strata / sparse &missopt outpct out = freqtab&i(drop = percent rename = (pct_col = percent));
        table &var / &missopt out = total_freqtab&i;
      run;
      proc means data = init&i noprint;
        class &strata;
        weight wgt;
        var &var;
        output out = ntab&i n = n nmiss = nmiss;
      run;
      proc sql noprint;
        create table proptab&i as
        select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from freqtab&i(where = (&var = 1)) f
        left join ntab&i(where = (_type_ = 1)) b on f.&strata = b.&strata;
        create table total_proptab&i as
        select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from total_freqtab&i(where = (&var = 1)) f
        cross join ntab&i(where = (_type_ = 0)) b;
      quit;
      %let propvarlst = &propvarlst &var;
      %let proptablst = &proptablst proptab&i;
      %let total_proptablst = &total_proptablst total_proptab&i;
    %end;
  %end;

  data proptab;
    set &proptablst;
  data total_proptab;
    set &total_proptablst;
  data proptab_result;
    %if not &noheader %then %do;
      %if not &total %then set header(in = in_header) proptab;
      %else set header(in = in_header) proptab total_proptab(in = in_total);;
    %end;
    %else %do;
      %if not &total %then set proptab;
      %else set proptab total_proptab(in = in_total);;
    %end;
    length col_id result $ 50 col_idlabel varstr $ 250;
    if in_header then do;
      rwtype = 0;
      varstr = catx(' - ', &label, &vtail);
    end;
    else do;
      %if not &sparse %then if count = 0 then delete;;
      rwtype = &hoffset + offset;
      array propvarlst{*} &propvarlst;
      varstr = vlabel(propvarlst{varid});
      %if &denominator %then %do;
        %if &missing %then %let ntotal = n + nmiss;
        %else %let ntotal = n;
        if 0 <= percent <= 100 then result = cat(count, '/', &ntotal, ' (', strip(put(percent, &format)), ')');
        else result = cat(count, '/', &ntotal);
      %end;
      %else %do;
        if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
        else result = cat(count);
      %end;
    end;
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = proptab_result;
    by rwtype varid varstr;
  proc transpose data = proptab_result out = proptab_bystrata(drop = _name_);
    by rwtype varid varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table proptab_bystrata add rpt_col_note varchar(500);
      update proptab_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %if &orderbytotal %then %do;
    proc sql noprint;
      select max(offset) + 1 into :noffsetlevels trimmed from total_proptab;
    quit;
    proc sort data = total_proptab;
      by varid;
    data proporderby;
      set total_proptab;
      by varid;
      retain orderby1-orderby&noffsetlevels;
      array orderbylst{&noffsetlevels} orderby1-orderby&noffsetlevels;
      do i = 1 to &noffsetlevels;
        if i = offset + 1 then orderbylst{i} = 100 - percent;
        else if i > offset + 1 then call missing(orderbylst{i});
      end;
      keep varid orderby1-orderby&noffsetlevels;
    run;
    proc sql noprint;
      create table proptab_sorted(drop = varid) as
      select p.* from proptab_bystrata p left join proporderby o on p.varid = o.varid
      order by ifn(p.rwtype >= &hoffset, 1, 0),
      %do f = 1 %to &noffsetlevels;
        o.orderby&f,
      %end;
      p.varid;
    quit;
  %end;
  %else %do;
    proc sql noprint;
      create table proptab_sorted(drop = varid) as
      select * from proptab_bystrata order by ifn(rwtype >= &hoffset, 1, 0), varid;
    quit;
  %end;

  %rptpush(proptab_sorted);
%mend;

%macro proplstsubgrp(
  indata,           /* Input data set */
  varkeylst,        /* Variable list */
  subgrp,           /* Sub-group variable */
  strata,           /* Main group */
  label = '',       /* Header label */
  subfreq = 1,      /* 1 - Show sub-group counts and percentages; 0 - Hide sub-group counts and percentages */
  pctbysub = 0,     /* 1 - Percentages per sub-groups; 0 - Percentages per the entire group */
  denominator = 1,  /* 1 - Show denominator; 0 - Do not show denominator */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  missing = 1,      /* 1 - Include missing in computations of percentages, proportions and statistics */
  missingtag = 'Missing',
  orderbytotal = 0, /* Order rows by total counts */
  ordersubby = ,    /* Order sub-groups by specified values */
  foldsub = ,       /* Fold specified sub-groups */
  usesublabel = 0,  /* Use sub-group label as header label */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %let varlst = %sysfunc(prxchange(%str(s/>>|<<//), -1, %quote(&varkeylst)));
  %if &missing %then %let missopt = missing;
  %else %let missopt = missprint;
  %if &missing %then %let missclassopt = missing;
  %else %let missclassopt = ;
  %if &tail and &denominator %then %let vtail = 'no./total no. (%)';
  %else %if &tail %then %let vtail = 'no. (%)';
  %else %let vtail = '';

  data init;
    set &indata(keep = &varlst &strata &subgrp);
    where not missing(&strata);
  run;

  proc sql noprint;
    create table header as
    select distinct &strata from init;
  quit;
  proc freq data = init noprint;
    table &subgrp * &strata / sparse &missopt outpct out = subfreqtab(drop = percent rename = (pct_col = percent)); %* By STRATA;
    table &subgrp / &missopt out = total_subfreqtab; %* Overall;
  run;

  %let offset = 0;
  %let varid = 0;
  %let propvarlst = ;
  %let proptablst = ;
  %let total_proptablst = ;
  %let nvarkeylst = %sysfunc(countw(%quote(&varkeylst), %str( )));
  %do i = 1 %to &nvarkeylst;
    %let varkey = %scan(%quote(&varkeylst), &i, %str( ));
    %if %quote(&varkey) = %str(>>) %then %let offset = %eval(&offset + 1);
    %else %if %quote(&varkey) = %str(<<) %then %do;
      %if &offset > 0 %then %let offset = %eval(&offset - 1);
      %else %let offset = 0;
    %end;
    %else %do;
      %let var = &varkey;
      %let varid = %eval(&varid + 1);
      %if not &pctbysub %then %do;
        proc freq data = init noprint;
          table &strata * &subgrp * &var / outpct sparse &missopt out = freqtab&i(drop = percent rename = (pct_tabl = percent));
          table &subgrp * &var / &missopt out = total_freqtab&i; * Do not use SPARSE;
        run;
        proc sort data = freqtab&i; by &subgrp &var;
        proc sort data = total_freqtab&i; by &subgrp &var;
        data freqtab&i;
          merge freqtab&i total_freqtab&i(keep = &subgrp &var in = in_total);
          by &subgrp &var;
          if in_total;
        run;

        proc means data = init noprint;
          class &strata &subgrp / &missclassopt;
          var &var;
          output out = ntab&i n = n nmiss = nmiss;
        run;
        proc sql noprint;
          create table proptab&i as
          select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from freqtab&i(where = (&var = 1)) f
          left join ntab&i(where = (_type_ = 2)) b on f.&strata = b.&strata;
          create table total_proptab&i as
          select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from total_freqtab&i(where = (&var = 1)) f
          cross join ntab&i(where = (_type_ = 0)) b;
        quit;
      %end;
      %else %do;
        proc sort data = init; by &subgrp;
        proc freq data = init noprint;
          by &subgrp;
          table &var * &strata / outpct sparse &missopt out = freqtab&i(drop = percent rename = (pct_col = percent)); %* By STRATA;
          table &var / &missopt out = total_freqtab&i; %* Overall;
        run;

        proc means data = init noprint;
          class &strata &subgrp / &missclassopt;
          var &var;
          output out = ntab&i n = n nmiss = nmiss;
        run;
        proc sql noprint;
          create table proptab&i as
          select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from freqtab&i(where = (&var = 1)) f
          left join ntab&i(where = (_type_ = 3)) b on f.&strata = b.&strata and f.&subgrp = b.&subgrp;
          create table total_proptab&i as
          select &varid as varid, &offset as offset, f.*, b.n, b.nmiss from total_freqtab&i(where = (&var = 1)) f
          left join ntab&i(where = (_type_ = 1)) b on f.&subgrp = b.&subgrp;
        quit;
      %end;
      %let propvarlst = &propvarlst &var;
      %let proptablst = &proptablst proptab&i;
      %let total_proptablst = &total_proptablst total_proptab&i;
    %end;
  %end;

  data proptab;
    set &proptablst;
  data total_proptab;
    set &total_proptablst;
  data proptab_result;
    %if not &total %then set header(in = in_header) subfreqtab(in = in_sub) proptab;
    %else set header(in = in_header) subfreqtab(in = in_sub) proptab
    total_subfreqtab(in = in_subtotal) total_proptab(in = in_total);;
    length col_id result $ 50 col_idlabel varstr $ 250;
    if in_header then do;
      rwtype = 0;
      %if &usesublabel %then varstr = catx(' - ', vlabel(&subgrp), &vtail);
      %else varstr = catx(' - ', &label, &vtail);;
    end;
    else if in_sub or in_subtotal then do;
      rwtype = 1;
      if missing(&subgrp) and strip(vvalue(&subgrp)) in ('', '.') then varstr = &missingtag;
      else varstr = vvalue(&subgrp);
      %if &subfreq %then %do;
        if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
        else result = cat(count);
      %end;
    end;
    else do;
      rwtype = 2 + offset;
      array propvarlst{*} &propvarlst;
      varstr = vlabel(propvarlst{varid});
      %if &denominator %then %do;
        %if &missing %then %let ntotal = n + nmiss;
        %else %let ntotal = n;
        if 0 <= percent <= 100 then result = cat(count, '/', &ntotal, ' (', strip(put(percent, &format)), ')');
        else result = cat(count, '/', &ntotal);
      %end;
      %else %do;
        if 0 <= percent <= 100 then result = cat(count, ' (', strip(put(percent, &format)), ')');
        else result = cat(count);
      %end;
    end;
    if in_subtotal or in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = proptab_result;
    by &subgrp rwtype varid varstr;
  proc transpose data = proptab_result out = proptab_bystrata(drop = _name_);
    by &subgrp rwtype varid varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &foldsub ~= %then %do;
    proc sql noprint;
      delete from proptab_bystrata where rwtype > 1 and &subgrp in (&foldsub);
    quit;
  %end;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table proptab_bystrata add rpt_col_note varchar(500);
      update proptab_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %if &ordersubby ~= %then %do;
    proc iml;
      vval = t({&ordersubby});
      nval = nrow(vval);
      vseq = t(do(1, nval, 1));
      outdata = vval || vseq;
      create propordersubby from outdata[colname = {"&subgrp" 'orderby'}];
      append from outdata;
      close propordersubby;
    quit;
  %end;
  %if &orderbytotal %then %do;
    proc sql noprint;
      select max(offset) + 1 into :noffsetlevels trimmed from total_proptab;
    quit;
    proc sort data = total_proptab;
      by varid;
    data proporderby;
      set total_proptab;
      by varid;
      retain orderby1-orderby&noffsetlevels;
      array orderbylst{&noffsetlevels} orderby1-orderby&noffsetlevels;
      do i = 1 to &noffsetlevels;
        if i = offset + 1 then orderbylst{i} = 100 - percent;
        else if i > offset + 1 then call missing(orderbylst{i});
      end;
      keep varid orderby1-orderby&noffsetlevels;
    run;
  %end;
  %if &ordersubby ~= %then %do;
    %if &orderbytotal %then %do;
      proc sql noprint;
        create table proptab_sorted(drop = &subgrp varid) as
        select p.* from proptab_bystrata p
        left join propordersubby s on p.&subgrp = s.&subgrp
        left join proporderby o on p.&subgrp = o.&subgrp and p.varid = o.varid
        order by ifn(p.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(p.&subgrp), p.&subgrp,
        ifn(p.rwtype > 1, 1, 0),
        %do f = 1 %to &noffsetlevels;
          o.orderby&f,
        %end;
        p.varid;
      quit;
    %end;
    %else %do;
      proc sql noprint;
        create table proptab_sorted(drop = &subgrp varid) as
        select p.* from proptab_bystrata p
        left join propordersubby s on p.&subgrp = s.&subgrp
        order by ifn(p.rwtype > 0, 1, 0), missing(s.orderby), s.orderby, missing(p.&subgrp), p.&subgrp,
        ifn(p.rwtype > 1, 1, 0), p.varid;
      quit;
    %end;
  %end;
  %else %do;
    %if &orderbytotal %then %do;
      proc sql noprint;
        create table proptab_sorted(drop = &subgrp varid) as
        select p.* from proptab_bystrata p
        left join proporderby o on p.&subgrp = o.&subgrp and p.varid = o.varid
        order by ifn(p.rwtype > 0, 1, 0), missing(p.&subgrp), p.&subgrp,
        ifn(p.rwtype > 1, 1, 0),
        %do f = 1 %to &noffsetlevels;
          o.orderby&f,
        %end;
        p.varid;
      quit;
    %end;
    %else %do;
      proc sql noprint;
        create table proptab_sorted(drop = &subgrp varid) as
        select * from proptab_bystrata
        order by ifn(rwtype > 0, 1, 0), missing(&subgrp), &subgrp, ifn(rwtype > 1, 1, 0), varid;
      quit;
    %end;
  %end;

  %rptpush(proptab_sorted);
%mend;

%macro catalst(
  indata,           /* Input dataset */
  varlst,           /* Variables to be analyzed */
  strata,           /* Main group */
  label = '',       /* Header label */
  usevarlabel = 1,  /* 1 - Use labels of the variables; 0 - Use variable names */
  nopct = 0,        /* 1 - No percentage; 0 - Show percentages */
  orderbytotal = 0, /* Order rows by total counts */
  test = 0,         /* 1 - Pearsons chi-square test; 2 - Fishers exact test; 0 - No test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  data init_catalst;
    set &indata(keep = &strata &varlst);
    where not missing(&strata);
  run;

  %rptcatalstinit(
    init_catalst, &varlst,
    label = &label,
    usevarlabel = &usevarlabel
  );

  %freq(
    init_catalst, catalst, &strata,
    nopct = &nopct,
    test = &test,
    total = &total,
    missingtag = 'None specified',
    orderbytotal = &orderbytotal,
    tail = &tail,
    note = &note,
    format = &format
  );
%mend;

%macro catalstsubgrp(
  indata,           /* Input dataset */
  varlst,           /* Variable list */
  subgrp,           /* Subgroup of row */
  strata,           /* Main group */
  label = '',       /* Header label */
  usevarlabel = 1,  /* 1 - Use labels of the variables; 0 - Use variable names */
  usesublabel = 0,  /* Use sub-group label as the entry label */
  subfreq = 0,      /* 1 - Show sub-group counts and percentages; 0 - Hide sub-group counts and percentages */
  nopct = 0,        /* 1 - No percentage; 0 - Show percentages */
  pctbysub = 0,     /* 1 - Percentages per sub-groups; 0 - Percentages per the entire group */
  test = 0,         /* 1 - Pearsons chi-square test; 2 - Fishers exact test; 0 - No test */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  foldsub = ,       /* Fold specified sub-groups */
  foldemptysub = 1, /* 1 - Fold empty SUBGRP */
  orderbytotal = 0, /* Order rows by total counts */
  ordersubby = ,    /* Order sub-groups by specified values */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  data init_catalst;
    set &indata(keep = &strata &subgrp &varlst);
    where not missing(&strata);
  run;

  %rptcatalstinit(
    init_catalst, &varlst,
    label = &label,
    usevarlabel = &usevarlabel
  );

  %freqsubgrp(
    init_catalst, catalst, &subgrp, &strata,
    usesublabel = &usesublabel,
    subfreq = &subfreq,
    nopct = &nopct,
    pctbysub = &pctbysub,
    test = &test,
    total = &total,
    foldsub = &foldsub,
    foldemptysub = &foldemptysub,
    missingtag = 'None specified',
    orderbytotal = &orderbytotal,
    ordersubby = &ordersubby,
    note = &note,
    tail = &tail,
    format = &format
  );
%mend;

%macro lifereg(
  indata,           /* Input dataset */
  strata,           /* Main group */
  event = dead,     /* Event of interest */
  censorcode = 0,   /* Value of right censoring */
  time = intxsurv,  /* Time to event */
  label = 'Incidence rate per person-time',
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  ci = 1,           /* 1 - Show CI; 0 - Hide CI */
  tail = 1,         /* Append descriptive text to the label */
  format = 8.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail and &ci %then %let vtail = 'est. (95% CI)';
  %else %if &tail %then %let vtail = 'est.';
  %else %let vtail = '';

  data lt_init;
    set &indata(keep = &strata &event &time);
    where not missing(&strata) and &event >= 0 and &time >= 0;
  run;

  ods exclude all;
  proc sort data = lt_init; by &strata;
  proc lifereg data = lt_init;
    by &strata;
    model &time * &event(&censorcode) = / dist = exponential;
    ods output ParameterEstimates = lt_out;
  run;
  proc lifereg data = lt_init;
    model &time * &event(&censorcode) = / dist = exponential;
    ods output ParameterEstimates = total_lt_out;
  run;
  ods exclude none;

  data lt_result;
    %if not &total %then set lt_out;
    %else set lt_out total_lt_out(in = in_total);;
    where parameter = 'Intercept';
    length col_id result $ 50 col_idlabel varstr $ 250;
    rwtype = 0;
    varstr = catx(' - ', &label, &vtail);
    irppt = exp(-estimate);
    irppt_lcl = exp(-uppercl);
    irppt_ucl = exp(-lowercl);
    %if &ci %then %do;
      result = cat(ifc(missing(irppt), 'NE', strip(put(irppt, &format))), ' (',
      ifc(missing(irppt_lcl), 'NE', strip(put(irppt_lcl, &format))), '-',
      ifc(missing(irppt_ucl), 'NE', strip(put(irppt_ucl, &format))), ')');
    %end;
    %else %do;
      result = ifc(missing(irppt), 'NE', strip(put(irppt, &format)));
    %end;
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = lt_result;
    by rwtype varstr;
  proc transpose data = lt_result out = lt_bystrata(drop = _name_);
    by rwtype varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table lt_bystrata add rpt_col_note varchar(500);
      update lt_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %rptpush(lt_bystrata);
%mend;

%macro lifetestbyqt(
  indata,           /* Input dataset */
  strata,           /* Main group */
  event = dead,     /* Event of interest */
  censorcode = 0,   /* Value of right censoring */
  failcode = ,      /* Value of the event */
  time = intxsurv,  /* Time to event */
  label = 'Percentiles of event time',
  conftype = loglog,
  test = 0,         /* 1 - Perform hypothesis testing */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 8.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'est. (95% CI)';
  %else %let vtail = '';

  data lt_init;
    set &indata(keep = &strata &event &time);
    where not missing(&strata) and &event >= 0 and &time >= 0;
  run;

  proc sql noprint;
    create table header as
    select distinct &strata from lt_init;
  quit;
  %if &failcode = %then %do;
    ods exclude all;
    proc lifetest data = lt_init conftype = &conftype;
      strata &strata;
      time &time * &event(&censorcode);
      ods output Quartiles = lt_median;
    run;
    proc lifetest data = lt_init conftype = &conftype;
      time &time * &event(&censorcode);
      ods output Quartiles = total_lt_median;
    run;
    ods exclude none;
  %end;
  %else %do;
    proc lifetest data = lt_init outcif = lt_output reduceout noprint;
      strata &strata;
      time &time * &event(&censorcode) / failcode = &failcode;
    run;
    proc iml;
      use lt_output;
      read all var {&strata &time cif} into xdata;
      close lt_output;
      bystrata = unique(xdata[, 1]);
      ns = ncol(bystrata); * N of strata;
      strata = colvec(repeat(t(bystrata), 1, 3));
      percent = repeat({25, 50, 75}, ns, 1);
      estimate = j(3 # ns, 1, .);
      do s = 1 to ns;
        sl = loc(strata = bystrata[s]);
        xsl = loc(xdata[, 1] = bystrata[s]);
        tmap = repeat(t(xdata[xsl, 3]), 3, 1) >= (percent[sl] / 100);
        tmin = j(3, 1, .);
        do p = 1 to 3;
          t_loc = loc(tmap[p, ]);
          if ncol(t_loc) > 0 then tmin[p] = min((xdata[xsl, 2])[t_loc]);
        end;
        estimate[sl] = tmin;
      end;
      outdata = strata || percent || estimate;
      create lt_median from outdata[colname = {"&strata" 'percent' 'estimate'}];
      append from outdata;
      close lt_median;
    quit;

    proc lifetest data = lt_init outcif = total_lt_output reduceout noprint;
      time &time * &event(&censorcode) / failcode = &failcode;
    run;
    proc iml;
      use total_lt_output;
      read all var {&time cif} into xdata;
      close total_lt_output;
      percent = {25, 50, 75};
      estimate = j(3, 1, .);
      tmap = repeat(t(xdata[, 2]), 3, 1) >= (percent / 100);
      do p = 1 to 3;
        t_loc = loc(tmap[p, ]);
        if ncol(t_loc) > 0 then estimate[p] = min((xdata[, 1])[t_loc]);
      end;
      outdata = percent || estimate;
      create total_lt_median from outdata[colname = {'percent' 'estimate'}];
      append from outdata;
      close total_lt_median;
    quit;
  %end;

  data lt_result;
    %if not &total %then set header(in = in_header) lt_median;
    %else set header(in = in_header) lt_median total_lt_median(in = in_total);;
    length col_id result $ 50 varstr $ 250;
    if in_header then do;
      rwtype = 0;
      varstr = catx(' - ', &label, &vtail);
    end;
    else do;
      rwtype = 1;
      varstr = cat(percent, '%');
      %if &failcode = %then %do;
        result = cat(ifc(missing(estimate), 'NE', strip(put(estimate, &format))), ' (',
        ifc(missing(lowerlimit), 'NE', strip(put(lowerlimit, &format))), '-',
        ifc(missing(upperlimit), 'NE', strip(put(upperlimit, &format))), ')');
      %end;
      %else %do;
        result = ifc(missing(estimate), 'NE', strip(put(estimate, &format)));
      %end;
    end;
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = lt_result;
    by rwtype percent varstr;
  proc transpose data = lt_result out = lt_bystrata(drop = _name_);
    by rwtype percent varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &test >= 1 %then %do;
    %if &failcode = %then %do;
      %rptttetest(lt_init, &event, &time, &strata,
      censorcode = &censorcode,
      type = logrank);
    %end;
    %else %do;
      %rptttetest(lt_init, &event, &time, &strata,
      censorcode = &censorcode,
      failcode = &failcode,
      type = gray);
    %end;
    proc sql noprint;
      alter table lt_bystrata add rpt_col_pval varchar(20), plabel varchar(25);
      update lt_bystrata set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outttetest),
      plabel = (select plabel from rpt_outttetest) where rwtype = 0;
    quit;
  %end;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table lt_bystrata add rpt_col_note varchar(500);
      update lt_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %rptpush(lt_bystrata);
%mend;

%macro lifetestbytime(
  indata,           /* Input dataset */
  strata,           /* Main group */
  event = dead,     /* Event of interest */
  censorcode = 0,   /* Value of right censoring */
  failcode = ,      /* Value of the event */
  time = intxsurv,  /* Time to event */
  timelist = 12 24 36,
  timeunit = 'Month',
  label = 'Probabilities of event free',
  conftype = asinsqrt,
  failure = 0,      /* 1 - Output failure estimates */
  test = 0,         /* 1 - Perform hypothesis testing */
  pwpvalue = 0,     /* 1 - Calculate point-wise p-values */
  total = 0,        /* 1 - Show total column; 0 - No total column */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 4.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'est. (95% CI)';
  %else %let vtail = '';

  data lt_init;
    set &indata(keep = &strata &event &time);
    where not missing(&strata) and &event >= 0 and &time >= 0;
  run;

  proc sql noprint;
    create table header as
    select distinct &strata from lt_init;
  quit;
  %if &failcode = %then %do;
    proc lifetest data = lt_init outsurv = lt_output reduceout
      timelist = &timelist conftype = &conftype stderr noprint;
      strata &strata;
      time &time * &event(&censorcode);
    run;
    proc lifetest data = lt_init outsurv = total_lt_output reduceout
      timelist = &timelist conftype = &conftype stderr noprint;
      time &time * &event(&censorcode);
    run;
  %end;
  %else %do;
    proc lifetest data = lt_init outcif = lt_output reduceout
      timelist = &timelist conftype = &conftype noprint;
      strata &strata;
      time &time * &event(&censorcode) / failcode = &failcode;
    run;
    proc lifetest data = lt_init outcif = total_lt_output reduceout
      timelist = &timelist conftype = &conftype noprint;
      time &time * &event(&censorcode) / failcode = &failcode;
    run;
  %end;

  %local lt_est lt_lcl lt_ucl;
  %if &failcode = %then %do;
    %if &failure %then %do;
      %let lt_est = failure;
      %let lt_lcl = cdf_lcl;
      %let lt_ucl = cdf_ucl;
      %let lt_stderr = sdf_stderr;
    %end;
    %else %do;
      %let lt_est = survival;
      %let lt_lcl = sdf_lcl;
      %let lt_ucl = sdf_ucl;
      %let lt_stderr = sdf_stderr;
    %end;
  %end;
  %else %do;
    %let lt_est = cif;
    %let lt_lcl = cif_lcl;
    %let lt_ucl = cif_ucl;
    %let lt_stderr = stderr;
  %end;

  data lt_result;
    %if not &total %then set header(in = in_header) lt_output;
    %else set header(in = in_header) lt_output total_lt_output(in = in_total);;
    length col_id result $ 50 col_idlabel varstr $ 250;
    if in_header then do;
      rwtype = 0;
      varstr = catx(' - ', &label, &vtail);
    end;
    else do;
      rwtype = 1;
      varstr = catx(' ', &timeunit, timelist);
      %if &failcode = and &failure %then %do;
        z = probit(1 - 0.05 / 2.0);
        failure = 1 - survival;
        if failure = 0 and sdf_stderr = 0 then do;
          call missing(cdf_lcl);
          call missing(cdf_ucl);
        end;
        else do;
          %if &conftype = asinsqrt %then %do;
            stderr = 0.5 * sdf_stderr / sqrt(failure * (1 - failure));
            cdf_lcl = sin(max(0, arsin(sqrt(failure)) - z * stderr)) ** 2;
            cdf_ucl = sin(min(3.1416 / 2, arsin(sqrt(failure)) + z * stderr)) ** 2;
          %end;
          %else %if &conftype = loglog %then %do;
            stderr = sdf_stderr / (failure * log(failure));
            cdf_lcl = failure ** exp(-z * stderr);
            cdf_ucl = failure ** exp(z * stderr);
          %end;
          %else %if &conftype = log %then %do;
            stderr = sdf_stderr / failure;
            cdf_lcl = failure * exp(-z * stderr);
            cdf_ucl = failure * exp(z * stderr);
          %end;
          %else %if &conftype = logit %then %do;
            stderr = sdf_stderr / (failure * (1 - failure));
            cdf_lcl = failure / (failure + (1 - failure) * exp(z * stderr));
            cdf_ucl = failure / (failure + (1 - failure) * exp(-z * stderr));
          %end;
        end;
      %end;
      result = cat(ifc(missing(&lt_est), 'NE', strip(put(100 * &lt_est, &format))), ' (',
      ifc(missing(&lt_lcl), 'NE', strip(put(100 * &lt_lcl, &format))), '-',
      ifc(missing(&lt_ucl), 'NE', strip(put(100 * &lt_ucl, &format))), ')');
    end;
    if in_total then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
  run;
  proc sort data = lt_result;
    by rwtype timelist varstr;
  proc transpose data = lt_result out = lt_bystrata(drop = _name_);
    by rwtype timelist varstr;
    id col_id;
    idlabel col_idlabel;
    var result;
  run;

  %if &test >= 1 %then %do;
    %if &failcode = %then %do;
      %rptttetest(lt_init, &event, &time, &strata,
      censorcode = &censorcode,
      type = logrank);
    %end;
    %else %do;
      %rptttetest(lt_init, &event, &time, &strata,
      censorcode = &censorcode,
      failcode = &failcode,
      type = gray);
    %end;
    proc sql noprint;
      alter table lt_bystrata add rpt_col_pval varchar(20), plabel varchar(25);
      update lt_bystrata set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outttetest),
      plabel = (select plabel from rpt_outttetest) where rwtype = 0;
    quit;

    %* Point-wise p-values;
    %if &pwpvalue %then %do;
      %rptmultiztest(testdata = lt_output, est = &lt_est, stderr = &lt_stderr, by = timelist);
      proc sql noprint;
        update lt_bystrata f set rpt_col_pval = (select put(pvalue, pvalue4.2) from rpt_outmultiztest where timelist = f.timelist),
        plabel = (select 'Point-wise test at a fixed point in time' from rpt_outmultiztest where timelist = f.timelist) where rwtype = 1;
      quit;
    %end;
  %end;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table lt_bystrata add rpt_col_note varchar(500);
      update lt_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  proc sql noprint;
    create table lt_sorted(drop = timelist) as
    select * from lt_bystrata order by rwtype, timelist;
  quit;

  %rptpush(lt_sorted);
%mend;

%macro mfs(
  indata,           /* Input dataset */
  strata,           /* Main group */
  event = dead,     /* Event of interest */
  time = intxsurv,  /* Time to event */
  label = 'Follow-up',
  total = 0,        /* 1 - Show total column; 0 - No total column */
  showq1q3 = 0,     /* Show Q1 and Q3 follow-up */
  note = ,          /* Additional footnote */
  tail = 1,         /* Append descriptive text to the label */
  format = 8.1      /* Format of the numbers */
  );

  %* Precheck the STRATA variable;
  %if &rpt_sttvar = %then %do;
    %let rpt_sttvar = &strata;
    %let dsid = %sysfunc(open(&indata, i));
    %let sttnum = %sysfunc(varnum(&dsid, &rpt_sttvar));
    %let rpt_sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
    %let rc = %sysfunc(close(&dsid));
  %end;
  %else %if &rpt_sttvar ne &strata %then %do;
    %put ERROR: Changed main group variable;
    %abort;
  %end;

  %if &tail %then %let vtail = 'median (range)';
  %else %let vtail = '';

  data mfs_init;
    set &indata(keep = &strata &event &time);
    where not missing(&strata) and &event >= 0 and &time >= 0;
  run;

  ods exclude all;
  proc lifetest data = mfs_init reduceout;
    strata &strata;
    time &time * &event(1);
    ods output Quartiles = mfs_median;
  run;
  proc lifetest data = mfs_init reduceout;
    time &time * &event(1);
    ods output Quartiles = total_mfs_median;
  run;
  ods exclude none;

  data mfs_median;
    set mfs_median total_mfs_median(in = in_total);
    drop transform lowerlimit upperlimit stratum;
    if in_total then _type_ = 0;
    else _type_ = 1;
  run;
  proc sort data = mfs_median;
    by _type_ &strata;
  proc transpose data = mfs_median out = mfs_median_t prefix = pc;
    by _type_ &strata;
    id percent;
    var estimate;
  run;
  proc means data = mfs_init noprint;
    where &event = 0;
    class &strata;
    var &time;
    output out = mfs_minmax min = pc0 max = pc100;
  run;
  proc sort data = mfs_median_t; by _type_ &strata;
  proc sort data = mfs_minmax; by _type_ &strata;
  data mfs;
    merge mfs_median_t mfs_minmax;
    by _type_ &strata;
  run;

  data mfs_result;
    %if not &total %then set mfs(where = (_type_ = 1));
    %else set mfs;;
    length rwid 8.;
    length col_id result $ 50 col_idlabel varstr $ 250;
    if _type_ = 0 then do;
      col_id = 'rpt_col_total';
      col_idlabel = 'Total';
    end;
    else do;
      col_id = cats('rpt_col', &strata);
      col_idlabel = strip(vvalue(&strata));
    end;
    %if &showq1q3 %then %do;
      %* Header;
      rwtype = 0;
      varstr = &label;
      output;
      %* Body;
      rwtype = 1;
      rwid = 1;
      varstr = 'Median (range)';
      result = cat(ifc(not missing(pc50), strip(put(pc50, &format)), 'NE'), ' (', strip(put(pc0, &format)), '-', strip(put(pc100, &format)), ')');
      output;
      rwid = 2;
      varstr = '25th-75th pctl';
      result = cat(ifc(not missing(pc25), strip(put(pc25, &format)), 'NE'), '-', ifc(not missing(pc75), strip(put(pc75, &format)), 'NE'));
      output;
    %end;
    %else %do;
      rwtype = 0;
      varstr = catx(' - ', &label, &vtail);
      result = cat(ifc(not missing(pc50), strip(put(pc50, &format)), 'NE'), ' (', strip(put(pc0, &format)), '-', strip(put(pc100, &format)), ')');
    %end;
  run;
  proc sort data = mfs_result;
    by rwtype rwid varstr;
  proc transpose data = mfs_result out = mfs_bystrata(drop = _name_ rwid);
    by rwtype rwid varstr;
    var result;
    id col_id;
    idlabel col_idlabel;
  run;

  %if &note ~= %then %do;
    proc sql noprint;
      alter table mfs_bystrata add rpt_col_note varchar(500);
      update mfs_bystrata set rpt_col_note = &note where rwtype = 0;
    quit;
  %end;

  %rptpush(mfs_bystrata);
%mend;

%macro export(
  outfile,                /* Output file name */
  title,                  /* Title of the table */
  footer = ,              /* Additional footnote */
  orientation = portrait, /* Orientation of the page */
  style = kenyrtf,        /* Style of the table */
  outfmt = rtf,           /* Format of the output file */
  regex = 's/^-?\d+([.-]\d+)?\s+//',
  mask = 0,               /* 1: Mask the output results */
  delay = 0               /* 1: Delay output; 0: Do not delay output */
  );

  %* Precheck if report results exist;
  %if &rpt_tid <= 0 %then %do;
    %put ERROR: Report results unavailable;
    %abort;
  %end;

  data outdata;
    %if &rpt_header %then set tabheader tab1-tab&rpt_tid end = eof;
    %else set tab1-tab&rpt_tid end = eof;;
    fid = &rpt_fid;
    array rpt_cols{*} rpt_col:;
    if eof then do;
      rpt_coln = 0;
      rpt_pcol = 0; rpt_ncol = 0; rpt_tcol = 0;
      do i = 1 to dim(rpt_cols);
        if vname(rpt_cols{i}) = 'rpt_col_pval' then rpt_pcol = 1;
        else if vname(rpt_cols{i}) = 'rpt_col_note' then rpt_ncol = 1;
        else if vname(rpt_cols{i}) = 'rpt_col_total' then do;
          rpt_tcol = 1;
          call symput('rpt_tcollbl', strip(vlabel(rpt_cols{i})));
        end;
        else do;
          rpt_coln = rpt_coln + 1;
          call symput(cat('rpt_colv', rpt_coln), vname(rpt_cols{i}));
          call symput(cat('rpt_colvlbl', rpt_coln), prxchange(&regex, 1, strip(vlabel(rpt_cols{i}))));
        end;
      end;
      call symput('rpt_pcol', rpt_pcol);
      call symput('rpt_ncol', rpt_ncol);
      call symput('rpt_tcol', rpt_tcol);
      call symput('rpt_coln', rpt_coln);
      call symput('nlines', _n_);
    end;
    drop i;
  run;

  %if &rpt_pcol %then %do;
    proc sql noprint;
      create table outplabel as
      select min(tid) as tid, plabel from outdata where not missing(plabel)
      group by plabel order by tid;
    quit;
    data outplabel;
      set outplabel end = eof;
      retain pfooter;
      length pfooterid $ 1 pfooter $ 250;
      pfooterid = byte(96 + _n_); %* Translate ASCII to letter;
      pfooter = catx(' ', pfooter, cat('^{super ', pfooterid, '} ', strip(plabel)));
      if eof then call symput('pfooter', quote(cat('Hypothesis testing: ', strip(pfooter))));
    run;
    proc sql;
      update outdata a set rpt_col_pval = (
      select cat(trim(a.rpt_col_pval), '^{super ', pfooterid, '}') from outplabel where plabel = a.plabel
      ) where not missing(rpt_col_pval);
    quit;
  %end;

  %if &rpt_ncol %then %do;
    proc sql noprint;
      create table outnotes as
      select min(tid) as tid, rpt_col_note from outdata where not missing(rpt_col_note)
      group by rpt_col_note order by tid;
    quit;
    data outnotes;
      set outnotes end = eof;
      length nfooter $ 500;
      nfooterid = _n_;
      nfooter = cat('^{super ', nfooterid, '} ', strip(rpt_col_note));
      call symput(cat('nfooter', nfooterid), quote(strip(nfooter)));
      if eof then call symput('nnfooter', nfooterid);
    run;
    proc sql;
      update outdata a set varstr = (
      select cat(trim(a.varstr), '^{super ', nfooterid, '}') from outnotes where rpt_col_note = a.rpt_col_note
      ) where not missing(rpt_col_note);
    quit;
  %end;

  %let m_size = 40; %* Define the size of left margin;
  %let c_size = 20; %* Define the size of each column;
  %let p_size =  8; %* Define the size of test column;

  %let ls = %eval(4 + &m_size + &rpt_coln * (4 + &c_size));
  %if &rpt_pcol %then %let ls = %eval(&ls + 4 + &p_size);
  %if &rpt_tcol %then %let ls = %eval(&ls + 4 + &c_size);
  %if &ls < 64 %then %let ls = 64;
  %else %if &ls > 256 %then %let ls = 256;
  %let ps = %eval(4 + &nlines);
  %if &rpt_pcol %then %let ps = %eval(&ps + 1);
  %if &rpt_ncol %then %let ps = %eval(&ps + &nnfooter);
  %if &footer ~= %then %let ps = %eval(&ps + 1);
  %if &ps < 15 %then %let ps = 15;
  %else %if &ps > 32767 %then %let ps = 32767;
  %let opt_save = %sysfunc(getoption(orientation, keyword))
  %sysfunc(getoption(linesize, keyword)) %sysfunc(getoption(pagesize, keyword));
  options orientation = &orientation ls = &ls ps = &ps label number;

  title &title;
  ods proclabel &title;
  %if &delay %then %do;
    %let rpt_fid = %eval(&rpt_fid + 1);
    %do %while(%sysfunc(prxmatch(%str(/\b&outfile#?\b/i), &rpt_output_delayed_files)));
      %if %sysfunc(prxmatch(%str(/\d+$/), &outfile)) %then %do;
        %let outfilepre = %sysfunc(prxchange(%str(s/^(\w+?)\d+$/$1/), 1, &outfile));
        %let outfileext = %eval(%sysfunc(prxchange(%str(s/^\w+?(\d+)$/$1/), 1, &outfile)) + 1);
      %end;
      %else %do;
        %let outfilepre = &outfile;
        %let outfileext = 1;
      %end;
      %let outfile = &outfilepre&outfileext;
    %end;
    %if &orientation = portrait %then %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile;
    %else %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile#;
    ods document name = &outfile(write);
  %end;
  %else %do;
    %if &outfmt = rtf %then ods rtf file = "&outfile..rtf" style = &style bodytitle;
    %else %if &outfmt = xls %then ods tagsets.excelxp file = "&outfile..xls" style = &style;;
  %end;
  ods escapechar = '^';

  proc report data = outdata contents = ''
    nowd split = '|' headline style(report) = [outputwidth = 100%];
    %let rpt_colvlst = fid sid tid rwtype varstr;
    %do i = 1 %to &rpt_coln;
      %let rpt_colvlst = &rpt_colvlst &&rpt_colv&i;
    %end;
    %if &rpt_pcol %then %let rpt_colvlst = &rpt_colvlst rpt_col_pval;
    %if &rpt_tcol %then %let rpt_colvlst = &rpt_colvlst rpt_col_total;
    column &rpt_colvlst;

    define fid / order noprint;
    define sid / order noprint;
    define tid / order noprint;
    define rwtype / display noprint;
    define varstr / 'Characteristic' style = [just = left] width = &m_size;
    %do i = 1 %to &rpt_coln;
      define &&rpt_colv&i / "&&rpt_colvlbl&i" style = [just = right] width = &c_size;
    %end;
    %if &rpt_pcol %then define rpt_col_pval / 'P Value' style = [just = right] width = &p_size;;
    %if &rpt_tcol %then define rpt_col_total / "&rpt_tcollbl" style = [just = right] width = &c_size;;

    compute varstr;
      if rwtype = 0 then varstr = prxchange(&regex, 1, strip(varstr));
      else if rwtype in (1: 3) then varstr = cat(repeat('  ', rwtype - 1), prxchange(&regex, 1, strip(varstr)));
      if rwtype in (0: 3) then call define(_col_, 'style/merge', cats('style = [leftmargin = ', 18 * rwtype, 'pt]'));
      else if rwtype = -1 then do;
        call define(_col_, 'style/merge', 'style = [font_weight = bold just = center]');
        call define(_row_, 'style/merge', 'style = [borderbottomstyle = solid borderbottomwidth = 1pt]');
      end;
      else if rwtype = -2 then do;
        call define(_col_, 'style/merge', 'style = [font_style = italic just = center]');
      end;
    endcomp;
    %if &rpt_pcol or &rpt_ncol or &footer ~= %then %do;
      compute after _page_ / style = [just = left font = fonts('FootFont')];
        %if &rpt_pcol %then line &pfooter;;
        %if &rpt_ncol %then %do l = 1 %to &nnfooter;
          line &&nfooter&l;
        %end;
        %if &footer ~= %then line &footer;;
      endcomp;
    %end;

    %if &mask %then %do;
      %let mask_colvlst = ;
      %do i = 1 %to &rpt_coln;
        %let mask_colvlst = &mask_colvlst &&rpt_colv&i;
      %end;
      %if &rpt_pcol %then %let mask_colvlst = &mask_colvlst rpt_col_pval;
      %if &rpt_tcol %then %let mask_colvlst = &mask_colvlst rpt_col_total;
      %let n_mask_colv = %sysfunc(countw(&mask_colvlst, %str( )));
      %do i = 1 %to &n_mask_colv;
        %let mask_colv = %scan(&mask_colvlst, &i, %str( ));
        compute &mask_colv;
          &mask_colv = prxchange('s/\d/x/', -1, &mask_colv);
        endcomp;
      %end;
    %end;

    break before fid / contents = '' page;
  run;

  %if &delay %then ods document close;
  %else %if &outfmt = rtf %then ods rtf close;
  %else %if &outfmt = xls %then ods tagsets.excelxp close;;

  title;
  options &opt_save;
  %rptreset();
%mend;

%macro export_listing(
  indata,                   /* Input data set */
  varkwdlst,                /* List of varible keywords */
  outfile,                  /* Output file name */
  title,                    /* Title of the output table */
  footer = ,                /* Footer of the output table */
  missingtag = 'Missing',
  orderby = ,               /* By group(s) to order */
  orientation = landscape,  /* Orientation of the output file */
  style = kenyrtf,          /* Style of the output table */
  outfmt = rtf,             /* Format of the output file */
  regex = 's/^-?\d+([.-]\d+)?\s+//',
  delay = 0                 /* 1: Delay output; 0: Do not delay output */
  );

  %let dsid = %sysfunc(open(&indata));
  %let nvar = %sysfunc(countw(&varkwdlst, %str( )));
  %do i = 1 %to &nvar;
    %let varkwd&i = %scan(&varkwdlst, &i, %str( ));
    %if %sysfunc(prxmatch(%str(/^[^|]+\|/), &&varkwd&i)) %then %do;
      %let var&i = %sysfunc(prxchange(%str(s/^([^|]+)\|(.+)/$1/), 1, &&varkwd&i));
      %let varfmt&i = %sysfunc(prxchange(%str(s/^([^|]+)\|(.+)/$2/), 1, &&varkwd&i));
    %end;
    %else %do;
      %let var&i = &&varkwd&i;
      %let varfmt&i = ;
    %end;
    %let varnum&i = %sysfunc(varnum(&dsid, &&var&i));
    %let vartyp&i = %sysfunc(vartype(&dsid, &&varnum&i));
    %if &&vartyp&i = N %then %let varlen&i = 20;
    %else %let varlen&i = %sysfunc(varlen(&dsid, &&varnum&i));
    %let varlbl&i = %qsysfunc(varlabel(&dsid, &&varnum&i));
    %if &&varlbl&i = %then %let varlbl&i = %upcase(&&var&i);
  %end;

  %let nvarby = %sysfunc(countw(&orderby, %str( )));
  %if &nvarby > 0 %then %do i = 1 %to &nvarby;
    %let varby&i = %scan(&orderby, &i, %str( ));
    %let varbynum&i = %sysfunc(varnum(&dsid, &&varby&i));
    %let varbytyp&i = %sysfunc(vartype(&dsid, &&varbynum&i));
  %end;
  %let rc = %sysfunc(close(&dsid));


  %let rpt_colvlst = fid;
  %do i = 1 %to &nvar;
    %let rpt_colvlst = &rpt_colvlst rpt_col&i;
  %end;

  data listing_init;
    set &indata;
    fid = &rpt_fid;
    %do i = 1 %to &nvar;
      %if &&vartyp&i = N %then length rpt_col&i $ 250;
      %else length rpt_col&i $ &&varlen&i;;
      %if &&varfmt&i = %then %do;
        if missing(&&var&i) and strip(vvalue(&&var&i)) in ('', '.') then rpt_col&i = &missingtag;
        else rpt_col&i = prxchange(&regex, 1, strip(vvalue(&&var&i)));
      %end;
      %else %do;
        if missing(&&var&i) and strip(vvalue(&&var&i)) in ('', '.') then rpt_col&i = &missingtag;
        else rpt_col&i = prxchange(&regex, 1, strip(put(&&var&i, &&varfmt&i)));
      %end;
    %end;
    keep &rpt_colvlst &orderby;
  run;

  proc sql;
    create table listing_sorted as
    select * from listing_init
    order by fid %if &nvarby > 0 %then %do i = 1 %to &nvarby;
      , missing(&&varby&i), &&varby&i
    %end;;
  quit;

  %let ls = 4;
  %do i = 1 %to &nvar;
    %if &&vartyp&i = N %then %let ls = %eval(&ls + 24);
    %else %if &&vartyp&i = C and &&varlen&i > 40 %then %let ls = %eval(&ls + 44);
    %else %if &&vartyp&i = C %then %let ls = %eval(&ls + &&varlen&i + 4);
  %end;
  %if &ls < 64 %then %let ls = 64;
  %else %if &ls > 256 %then %let ls = 256;
  %let opt_save = %sysfunc(getoption(orientation, keyword)) %sysfunc(getoption(linesize, keyword));
  options orientation = &orientation linesize = &ls label number;

  title &title;
  ods proclabel &title;
  %if &delay %then %do;
    %let rpt_fid = %eval(&rpt_fid + 1);
    %do %while(%sysfunc(prxmatch(%str(/\b&outfile#?\b/i), &rpt_output_delayed_files)));
      %if %sysfunc(prxmatch(%str(/\d+$/), &outfile)) %then %do;
        %let outfilepre = %sysfunc(prxchange(%str(s/^(\w+?)\d+$/$1/), 1, &outfile));
        %let outfileext = %eval(%sysfunc(prxchange(%str(s/^\w+?(\d+)$/$1/), 1, &outfile)) + 1);
      %end;
      %else %do;
        %let outfilepre = &outfile;
        %let outfileext = 1;
      %end;
      %let outfile = &outfilepre&outfileext;
    %end;
    %if &orientation = portrait %then %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile;
    %else %let rpt_output_delayed_files = &rpt_output_delayed_files &outfile#;
    ods document name = &outfile(write);
  %end;
  %else %do;
    %if &outfmt = rtf %then ods rtf file = "&outfile..rtf" style = &style bodytitle;
    %else %if &outfmt = xls %then ods tagsets.excelxp file = "&outfile..xls" style = &style;;
  %end;
  ods escapechar = '^';

  proc report data = listing_sorted contents = ''
    nowd split = '|' headline style(report) = [outputwidth = 100%];
    column &rpt_colvlst;
    define fid / order noprint;
    %do i = 1 %to &nvar;
      define rpt_col&i / "&&varlbl&i" style = [just = left] width =
      %if &&vartyp&i = N %then 20;
      %else %if &&vartyp&i = C and &&varlen&i > 40 %then 40;
      %else %if &&vartyp&i = C %then &&varlen&i;;
    %end;
    %if &footer ~= %then %do;
      compute after _page_ / style = [just = left font = fonts('FootFont')];
        line &footer;
      endcomp;
    %end;
    break before fid / contents = '' page;
  run;

  %if &delay %then ods document close;
  %else %if &outfmt = rtf %then ods rtf close;
  %else %if &outfmt = xls %then ods tagsets.excelxp close;;

  title;
  options &opt_save;
%mend;

%macro export_batch(
  outfile,                /* Output file name */
  title = ,               /* Title for the title page */
  toc = 0,                /* 1 - Generate table of contents */
  style = kenyrtf,        /* Style of the output table */
  outfmt = rtf            /* Format of the output file */
  );

  %* Precheck if report results exist;
  %if &rpt_output_delayed_files = %then %do;
    %put ERROR: Report results unavailable;
    %abort;
  %end;

  %if &toc %then %let rpt_tocopt_rtf = %str(contents = yes toc_data);
  %else %let rpt_tocopt_rtf = ;

  options number label;
  ods listing close;
  %if &outfmt = rtf %then ods rtf file = "&outfile..rtf" style = &style bodytitle nogtitle nogfootnote keepn &rpt_tocopt_rtf;
  %else %if &outfmt = xls %then ods tagsets.excelxp file = "&outfile..xls" style = &style nogtitle nogfootnote;;

  %if &title ~= %then %do;
    data titlepage;
      title = &title;
    run;
    proc report data = titlepage noheader
      style(report) = [rules = none frame = void outputwidth = 100%]
      style(column) = [font_weight = bold just = c];
    run;
  %end;

  %let nfiles = %sysfunc(countw(&rpt_output_delayed_files, %str( )));
  %do i = 1 %to &nfiles;
    %let rpt_output_key = %scan(&rpt_output_delayed_files, &i, %str( ));
    %let rpt_output_keylen = %length(&rpt_output_key);
    %if %substr(&rpt_output_key, &rpt_output_keylen, 1) = # %then %do;
      %let rpt_output_file = %substr(&rpt_output_key, 1, &rpt_output_keylen - 1);
      options orientation = landscape;
      %if &outfmt = rtf %then ods rtf;
      %else %if &outfmt = xls %then ods tagsets.excelxp;;
    %end;
    %else %do;
      %let rpt_output_file = &rpt_output_key;
      options orientation = portrait;
      %if &outfmt = rtf %then ods rtf;
      %else %if &outfmt = xls %then ods tagsets.excelxp;;
    %end;
    proc document;
      doc name = &rpt_output_file;
      replay;
      doc close;
    quit;
  %end;

  %if &outfmt = rtf %then ods rtf close;
  %else %if &outfmt = xls %then ods tagsets.excelxp close;;
  ods listing;

  %let rpt_fid = 1;
  %let rpt_output_delayed_files = ;
%mend;
