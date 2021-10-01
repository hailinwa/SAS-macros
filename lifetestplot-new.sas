%* Macro function for creating unadjusted plots
%* for lifetime data with or without competing risks
%* Maintainer: Zhen-Huan Hu <zhu@mcw.edu>
%* Last update: 2019-04-19;

%let pidx = 0;

%macro lifetestplot(indata =, outfile = figure, imagefmt = png, style = kenyplot,
  strata = , event = dead, competerisk = , intv = intxsurv, ltime = ,
  statfreq = 1, median = 0, pvalue = 1, cp = 0, atrisk = 0, xstart = 0, xend = 60, ypct = 0, timelist = ,
  title = , footnote = , title_inside = 1, xlabel = , ylabel = , color = 1, width = 640px, height = 480px, delay = 0);

  %* INDATA: Input datafile. Must specify;
  %* OUTFILE: Output filename. (default: plot);
  %* IMAGEFMT: Output ODS format;
  %* STYLE: ODS style of the figure;
  %* STRATA: Main group variable. (default: nil);
  %* EVENT: Event variable. (default: DEAD);
  %* COMPETERISK: Competing risk variable;
  %* INTV: Time to event variable. (default: INTXSURV);
  %* LTIME: Left truncation time;
  %* STATFREQ: Whether to display basic statistics of the outcome. (default: yes);
  %* MEDIAN: Whether to display median survival estimates. (default: no);
  %* PVALUE: Whether to show pvalue p-value in the legend. (default: yes);
  %* CP: Whether to mark censoring points on the survival curves. (default: no);
  %* ATRISK: Whether to include N at risk below the figure. (default: no);
  %* XSTART: Start mark on x-axis. (default: 0, i.e.: start at time 0 years);
  %* XEND: End mark on x-axis. (default: 60, i.e.: end at time 5 years);
  %* YPCT: Whether to use percentages on y-axis vs. rates. (default: no, i.e.: use rates);
  %* TIMELIST: Time points shown on the x-axis;
  %* TITLE: Title of the figure;
  %* FOOTNOTE: Footnote of the figure;
  %* TITLE_INSIDE: Show title inside the figure;
  %* XLABEL: Label of the X axis;
  %* YLABEL: Label of the Y axis;
  %* COLOR: Whether to use color in the output figure;
  %* WIDTH: Width of the figure;
  %* HEIGHT: Height of the figure;
  %* DELAY: Delay output to ODS DOCUMENT;

  %let pidx = %eval(&pidx + 1);

  %if &color = 0 %then %let lnpattern = %str((color = black));
  %else %if &color = 1 %then %let lnpattern = %str((pattern = solid));
  %else %if &color = 2 %then %let lnpattern = &style;

  %* Count the # of strata;
  %local colcnt;
  %if &strata = %then %let colcnt = 1;
  %else %do;
    proc sql noprint;
      select count(distinct &strata)
      into :colcnt trimmed from &indata where not missing(&strata);
    quit;
  %end;

  %let n_note_datasets = 0;
  %let note_datasets = ;

  proc format;
    value nil 1 = 'All subjects';
  run;

  %* Initialize;
  data ltplot_init;
    set &indata;
    %if &strata = %then %do;
      %let strata = nil;
      nil = 1;
      format nil nil.;
    %end;
    %else %do;
      if missing(&strata) then delete;
    %end;

    %* Create STATUS variable;
    %if &competerisk = %then %do;
      if &event = 1 then event_stat = 1;
      else if &event = 0 then event_stat = 0;
    %end;
    %else %do;
      if &event = 1 then event_stat = 1;
      else if &competerisk = 1 then event_stat = 2;
      else if &event = 0 and &competerisk = 0 then event_stat = 0;
    %end;
    if &intv <= 0 then call missing(&intv);
    if missing(&intv) and not missing(event_stat) then call missing(event_stat);
  run;

  %* Basic counts;
  %if &statfreq %then %do;
    proc means data = ltplot_init noprint;
      var event_stat;
      output out = ltplot_statcount n = n nmiss = nmiss;
    run;
    data ltplot_statcount_note;
      set ltplot_statcount;
      length note_label $ 25 note_text $ 75;
      note_label = 'Total N of subjects';
      note_text = cat(n + nmiss);
      keep note_label note_text;
    run;
    %let n_note_datasets = %eval(&n_note_datasets + 1);
    %let note_datasets = &note_datasets ltplot_statcount_note;

    proc freq data = ltplot_init noprint;
      where not missing(event_stat);
      table event_stat * &strata / out = ltplot_statfreq;
    run;
    proc sort data = ltplot_statfreq;
      by event_stat &strata;
    data ltplot_statfreq_note;
      set ltplot_statfreq;
      by event_stat &strata;
      length note_label $ 25 note_text $ 75;
      retain note_text;
      if first.event_stat then note_text = '';
      if first.event_stat and last.event_stat then note_text = cat(count);
      else note_text = catx(', ', note_text, catx(' = ', vvalue(&strata), count));
      if last.event_stat then do;
        if event_stat = 0 then note_label = 'N of censored';
        else if event_stat = 1 then note_label = 'N of events';
        else if event_stat = 2 then note_label = 'N of competing risks';
        output;
      end;
      keep note_label note_text;
    run;
    %let n_note_datasets = %eval(&n_note_datasets + 1);
    %let note_datasets = &note_datasets ltplot_statfreq_note;
  %end;

  %if &competerisk = %then %do;
    ods select none;
    ods output Quartiles = ltplot_median;
    %if &colcnt > 1 %then %do;
      ods output HomTests = ltplot_outtest;
    %end;
    proc lifetest data = ltplot_init outsurv = ltplot_output;
      where &intv > 0 and not missing(event_stat);
      time &intv * event_stat(0);
      strata &strata / test = logrank;
    run;
    ods select all;
  %end;
  %else %do;
    ods select none;
    %if &colcnt > 1 %then %do;
      ods output GrayTest = ltplot_outtest;
    %end;
    proc lifetest data = ltplot_init outcif = ltplot_output;
      where &intv > 0 and not missing(event_stat);
      time &intv * event_stat(0) / failcode = 1;
      strata &strata;
    run;
    ods select all;
  %end;

  %if &competerisk = %then %do;
    %let estimate = survival;
    %let censored = _censor_;
  %end;
  %else %do;
    %let estimate = cif;
    %let censored = censored;
  %end;

  %* Finalize plot data;
  proc sort data = ltplot_output; by &strata &intv;
  data ltplot_final;
    set ltplot_output; by &strata &intv;
    %* By proc lifetest algorithm, if the last few obs are censored,
    %* the survival rates are set to missing instead of carrying over previous values.
    %* For such obs, fill them with carrying over values.
    %* Only keep the last obs at the end of x-axis range.
    %* Patients who lived longer with be excluded;
    retain last_rate_carryover endpoint_flag;
    if first.&strata then endpoint_flag = 0;
    if &intv <= &xend then do;
      %* Update carry-over values;
      if &estimate >= 0 then last_rate_carryover = &estimate;
      else if &estimate < 0 then &estimate = last_rate_carryover;
    end;
    else if &intv > &xend and endpoint_flag = 0 then do;
      %* Change the current observation to XEND and set flag;
      &estimate = last_rate_carryover;
      &intv = &xend;
      endpoint_flag = 1;
    end;
    else if &intv > &xend and endpoint_flag = 1 then delete;
    if &censored = 1 then censored_estimate = &estimate;
    output;
  run;

  %* N at risk;
  %if &atrisk %then %do;
    %let ntp = %sysfunc(countw(&timelist, ' '));
    %if &ntp > 0 %then %do;
      ods select none;
      ods output ProductLimitEstimates = ltplot_atrisk(keep = &strata &intv timelist numberatrisk);
      proc lifetest data = ltplot_init atrisk timelist = &timelist reduceout;
        where &intv > 0 and not missing(event_stat);
        %if &competerisk = %then %do;
          time &intv * event_stat(0);
        %end;
        %else %do;
          time &intv * event_stat(0 2);
        %end;
        strata &strata;
      run;
      ods select all;

      proc sort data = ltplot_final; by &strata &intv;
      proc sort data = ltplot_atrisk; by &strata &intv;
      data ltplot_final;
        merge ltplot_final ltplot_atrisk(in = in_atrisk);
        by &strata &intv;
        if in_atrisk and missing(numberatrisk) then numberatrisk = 0;
      run;
    %end;
  %end;

  %* Median survival;
  %if &median and &competerisk = %then %do;
    proc sort data = ltplot_median; by percent &strata;
    data ltplot_median_note;
      set ltplot_median;
      where percent = 50;
      by percent &strata;
      length note_label $ 25 note_text cur_note_text $ 75;
      retain note_text;
      if first.percent then note_text = '';
      cur_note_text = cat(ifc(missing(estimate), 'NE', cat(round(estimate, .01))), ' (95% CI: ',
      ifc(missing(lowerlimit), 'NE', cat(round(lowerlimit, .01))), '-',
      ifc(missing(upperlimit), 'NE', cat(round(upperlimit, .01))), ')');
      if first.percent and last.percent then note_text = cur_note_text;
      else note_text = catx(', ', note_text, catx(' = ', vvalue(&strata), cur_note_text));
      if last.percent then do;
        note_label = 'Median survival est.';
        output;
      end;
      keep note_label note_text;
    run;
    %let n_note_datasets = %eval(&n_note_datasets + 1);
    %let note_datasets = &note_datasets ltplot_median_note;
  %end;

  %* Log-rank test/Grays test;
  %if &pvalue and &colcnt > 1 %then %do;
    data ltplot_outtest_note;
      set ltplot_outtest;
      length note_label $ 25 note_text $ 75;
      if probchisq >= 0.001 then note_text = put(probchisq, 5.3);
      else if probchisq > 0 then note_text = '< 0.001';
      %if &competerisk = %then note_label = 'Log-rank test p-value';
      %else note_label = 'Gray''s test p-value';;
      keep note_label note_text;
    run;
    %let n_note_datasets = %eval(&n_note_datasets + 1);
    %let note_datasets = &note_datasets ltplot_outtest_note;
  %end;

  %* Combine legends;
  %local nnotes;
  %if &n_note_datasets > 0 %then %do;
    data ltplot_notes;
      set &note_datasets;
    run;
    proc sql noprint;
      select count(note_text) into :nnotes trimmed from ltplot_notes;
      select note_label, note_text into
      :note_label1-:note_label&nnotes,
      :note_text1-:note_text&nnotes from ltplot_notes;
    quit;
    %let n_note_datasets = 0;
    %let note_datasets = ;
  %end;

  %* Label preparation;
  %if &ypct = 0 %then %let yscale = 1;
  %else %if &ypct = 1 %then %let yscale = 100;
  %if &xlabel = %then %let xlabel = 'Time';
  %if &ylabel = and &ypct = 0 %then %do;
    %if &competerisk = %then %let ylabel = 'Survival Probability';
    %else %let ylabel = 'Cumulative Incidence Rate';
  %end;
  %else %if &ylabel = and &ypct = 1 %then %do;
    %if &competerisk = %then %let ylabel = 'Survival Probability, %';
    %else %let ylabel = 'Cumulative Incidence Rate, %';
  %end;
  %if &title = %then %let title = '';
  %if &footnote = %then %let footnote = '';

  %* Set template;
  proc template;
    define statgraph lttplt&pidx;
      begingraph;
        %if &title_inside = 1 %then %do;
          entrytitle &title;
        %end;
        legenditem type = markerline name = 'censored' / markerattrs = (symbol = plus) label = 'Censored';
        layout overlay /
          xaxisopts = (label = &xlabel linearopts = (viewmin = &xstart viewmax = &xend %if &timelist ~= %then tickvaluelist = (&timelist);))
          yaxisopts = (label = &ylabel linearopts = (viewmin = 0 viewmax = &yscale));
          %* Plot;
          stepplot x = &intv y = eval(&estimate * &yscale) / group = &strata lineattrs = &lnpattern name = 'lsplot';
          %if &cp %then %do;
            scatterplot x = &intv y = eval(censored_estimate * &yscale) / group = &strata markerattrs = (symbol = plus);
          %end;
          %* Notes;
          %if &nnotes > 0 %then %do;
            layout gridded / columns = 2 order = rowmajor border = false autoalign = (topleft bottomleft topright bottomright);
              %do n = 1 %to &nnotes;
                entry halign = left "&&note_label&n";
                entry halign = left "&&note_text&n";
              %end;
            endlayout;
          %end;
          %* Inner margin;
          %if &atrisk %then %do;
            innermargin / separator = true;
              axistable x = timelist value = numberatrisk / class = &strata display = (values label) stat = mean title = 'N at risk';
            endinnermargin;
          %end;
          %* Inner legend;
          %if &cp %then %do;
            discretelegend 'censored' / location = inside autoalign = (topleft topright);
          %end;
          %* Outer legend;
          %if &colcnt > 1 %then %do;
            discretelegend 'lsplot' / location = outside valign = bottom halign = center;
          %end;
        endlayout;
        entryfootnote &footnote;
      endgraph;
    end;
  run;

  %* Output;
  %if &delay %then %do;
    title &title;
    ods proclabel &title;
    ods listing close;
    ods document name = &outfile(write);
    proc sgrender data = ltplot_final template = lttplt&pidx objectlabel = ''; run;
    ods listing;
    ods document close;
    title;
  %end;
  %else %do;
    title &title;
    ods proclabel &title;
    ods listing style = &style;
    ods graphics on / reset = all imagename = "&outfile" width = &width height = &height imagefmt = &imagefmt;
    proc sgrender data = ltplot_final template = lttplt&pidx objectlabel = ''; run;
    ods graphics off;
    title;
  %end;
%mend;
