/* Macro for generating forest plot comparing main effect conditioned on INTERACTION variables
/* Author: Hai-Lin Wang <hwang@mcw.edu>
/* Created: 2019-10-08
/*
/* Functions and their uses:
/* - Main function
/* %forestplot_i(): use PROC PHREG to obtain parameters, create template dataset and pass to %gtl_forest() to produce forest plot
/* - Internal function
/* %gtl_forest_i(): takes generated dataset and produce plot using PROC GRL
/*
/* Required variables:
/* Variables to be included in Cox model
/*
/* Note:
/* - This macro is NOT intended for showing hazard ratios of all co-variates from the Cox model 
/* - Cases missing main effect, event, time to event variables will be excluded
/* - Parameter &eventge restricts certain co-variate categories from display in the figure, however the interaction p-value considers all possible categories
/* - If &cuttime parameter is specified (indicating time-dependent main effect was split to 2 time periods):
/*    - Two figures will be generated for early and late main effect separately
/*    - For early figure, number of cases will be whoever at risk at day 0, number of events will be events prior to cutoff time
/*    - For late figure, number of cases will be whoever at risk at cutoff time, number of events will be events after cutoff time
/* - If the internal function %gtl_forest_i() is called directly to produce figures, then please use &outdata parameter of %forestplot_i()
/*   to see format of dataset to be passed to %gtl_forest_i()
/*
/* Reference:
/* https://www.pharmasug.org/proceedings/2017/QT/PharmaSUG-2017-QT15.pdf
/* ---------------------------------------------------------------------- */

%macro gtl_forest_i(
    plotdata = ,          /*dataset template containing all results for plotting*/
    lbltrta = ,           /*label for treatment group = 0 in figure, no quotes*/
    lbltrtb = ,           /*label for treatment group = 1 in figure, no quotes*/
    overallhr = ,         /*Overall main effect HR, pair with &refhr = 1 to create reference line*/
    refhr = ,             /*add reference line at HR of overall main effect. 1=Yes; ignored otherwise*/
    minbox = 4,           /*Minimum box size for plot*/
    maxbox = 12,          /*Maximum box size for plot*/
    hrtexty = ,           /*Vertical location of annotation text under the X-axis: big=upper; small=lower*/
    dpi = ,               /*do not set to more than 400 dpi as it will cause Java error */
    w = , h = ,           /*width and height of figure in inches*/
    title = ,             /*figure title, with quotes if call directly*/
    path = ,              /*directory of output file, default = same directory as SAS program, without quotes if call directly*/
    imgname = ,           /*name of figure file, no quotes if call directly*/
    imgfmt =              /*PNG / EFS / PDF*/
    );  

/*Compute plot parameters based on data*/
/*1. obtain max 95%CI of HR from list to determine range of x-axis, minimum 0.25 - 4*/
/*2. set right arrow sign under x-axis to reach largest HR mark on plot*/
    proc sql; select ceil(max(ucl)) into :maxucl from &plotdata; quit;
    %let autotick = 0.25 0.5 1 2 4;
    %let maxtick = 4;
    %do j = 1 %to 3; 
      %if %sysevalf(2**(&j + 1) - &maxucl) < 0 %then %do; 
        %let autotick = &autotick %eval(2**(&j + 2)); 
        %let maxtick = %eval(2**(&j + 2));
      %end; 
    %end;

/*create internal dataset for forest plot*/
    data anno(drop=indent);
     set &plotdata(keep=row varstr indent rename=(row=y1));
     length X1Space Y1Space $20;
    retain Function 'Text ' ID 'id1' X1Space 'DataPercent'
     Y1Space 'DataValue ' x1 x2 2 TextSize 7 Width 100 Anchor 'Left ';
     if indent;
     varstr=tranwrd(varstr, '>=', '(*ESC*){Unicode ''2265''x}');
     varstr=tranwrd(varstr, '<=', '(*ESC*){Unicode ''2264''x}');
     varstr=tranwrd(varstr, '+-', '(*ESC*){Unicode ''00b1''x}');
     label=tranwrd(varstr, '!=', '(*ESC*){Unicode ''2260''x}');
     run;
    /*--Used for text under x axis of HR scatter plot in column 7--*/
    data anno2;
    retain Function 'Arrow' ID 'id2' X1Space X2Space 'DataValue'
     FIllTransparency 0 Y1Space Y2Space 'LayoutPercent' Scale 1e-40
     LineThickness 1 y1 y2 &hrtexty Width 100 FillStyleElement 'GraphWalls'
     LineColor 'Black';
     x1 = 0.50; x2 = 0.25; output;
     x1 = 1.50; x2 = &maxtick; output;
     function = 'Text'; y1 = &hrtexty; y2 = &hrtexty;
     x1 = 0.9; anchor = 'Right'; label = "&lbltrtb Better"; Textsize=8; output;
     x1 = 1.1; Anchor = 'Left '; label = "&lbltrta Better"; Textsize=8; output;
    run;
    data anno; set anno anno2; run; 
    data forest2(drop=flag);
     set &plotdata nobs=nobs;
     Head = not indent;
     retain flag 0;
     if head then flag = mod(flag + 1, 2);
     if flag then ref=row;
     if indent then varstr = ' ';
     run;

/*Define template for forest plot*/
%if &title = %then %let title = "";

proc template;
 define statgraph Forest;
 dynamic /*_show_bands*/ _color _thk;
     begingraph;
       entrytitle &title;
       discreteattrmap name='text';
         value '1' / textattrs=(weight=bold); value other;
       enddiscreteattrmap;
       discreteattrvar attrvar=type var=head attrmap='text';

       layout lattice / columns=8 columnweights=(0.21 0.06 0.07 0.06 0.07 0.12 0.33 0.09);
       /*--Column headers--*/
       sidebar / align=top;
           layout lattice /
               rows=2 columns=5 columnweights=(0.21 0.12 0.13 0.40 0.14);
               entry textattrs=(size=8) halign=left "Main effect";
               entry textattrs=(size=8) halign=center "&lbltrta";
               entry textattrs=(size=8) halign=center "&lbltrtb";
               entry textattrs=(size=8) halign=left "&lbltrtb vs. &lbltrta";
               entry textattrs=(size=8) halign=center "Interaction";

               entry textattrs=(size=8) halign=left "Conditioned on";
               entry textattrs=(size=8) halign=center "Event / N (%)";
               entry textattrs=(size=8) halign=center "Event / N (%)";
               entry textattrs=(size=8) halign=left "HR (95% CI)";
               entry textattrs=(size=8) halign=center "P Value*" ;
           endlayout;
       endsidebar;
       /*--First Subgroup column, shows only the Y2 axis--*/
       layout overlay / walldisplay=none xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none
           tickvalueattrs=(weight=bold));
           annotate / id='id1';
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=varstr /
           display=(values) textgroup=type;
       endlayout;
       /*--Second column showing Treatment A Events/Count --*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=EventCountA /display=(values)
           valuejustify = center;
       endlayout;
       /*--Third column showing Treatment A Percent--*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=PctDisplayA /display=(values)
           valuejustify = left;
       endlayout;
       /*--Fourth column showing Treatment B Events/Count --*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=EventCountB /display=(values)
           valuejustify = center;
       endlayout;
       /*--Fifth column showing Treatment B Percent--*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=PctDisplayB /display=(values)
           valuejustify = left;
       endlayout;
       /*--Sixth column showing HRCIGroup--*/
           layout overlay / x2axisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=HRCI / display=(values);
       endlayout;
       /*--Seventh column showing Hazard ratio graph with 95% error bars--*/
       layout overlay / xaxisopts=(type=log
           label=' '
           labelattrs=(size=10)
           logopts=(tickvaluepriority=true
           tickvaluelist=(&autotick)))
           yaxisopts=(reverse=true display=none) walldisplay=none;
           annotate / id='id2';
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           scatterplot y=row x=HR / xerrorlower=lcl xerrorupper=ucl
           sizeresponse=SquareSize sizemin=&minbox sizemax=&maxbox
           markerattrs=(symbol=squarefilled);
           referenceline x=1;
           %if &refhr = 1 %then referenceline x=&overallhr / lineattrs = (pattern = 2);;
       endlayout;
       /*--Eigth column showing P-Values--*/
       layout overlay / x2axisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=pvalue / display=(values) valueformat=pvalue6.3
           valuejustify = right
           showmissing=false; /*false removes . for missing pvalues*/
       endlayout;
       endlayout;
       entryfootnote halign=left textattrs=(size=7)
       '* P-Value is the test of interaction between main effect and each co-variate.';
     endgraph;
 end;
run;

/*----Create Graph with PNG/EPS output-----*/
ods html close;
ods rtf close;
ods excel close;
ods listing gpath = "&path" image_dpi = &dpi;
    ods graphics / reset noscale IMAGEFMT=&imgfmt
        width=&w.in height=&h.in imagename="&imgname";
    proc sgrender data=forest2 template=Forest sganno=anno;
    dynamic _color='white' _thk=12 ; /*-Essentially invisible band-*/
    run;
%mend;


%macro forestplot_i(
    indata = ,            /*input dataset*/
    event = ,             /*event indicator, 0 (censor) vs. 1 (event)*/
    cuttime = ,           /*cutoff value of time to event if time-dependent main effect, only support 1 cut-point (early vs. late)*/
    intv = ,              /*time to event*/
    trtgp = ,             /*treatment group, 0 vs. 1, only support 2 main effect groups*/
    covlst = ,            /*list of co-variates considered in Cox model*/
    lbltrta = Trt A,      /*label for treatment group = 0 in figure, no quotes*/
    lbltrtb = Trt B,      /*label for treatment group = 1 in figure, no quotes*/
    eventge = 5,          /*exclude HR estimate from figure if number of events in corresponding subgroup for either treatment is less than specified, default = 5 events*/
    missing = ,           /*specify list of missing values if want to exclude missing co-variate category from figure, use %quote(x,y)*/
    refhr = 1,            /*add reference line at HR of overall main effect. 1=Yes; ignored otherwise*/
    minbox = 4,           /*Minimum box size for plot*/
    maxbox = 12,          /*Maximum box size for plot*/
    hrtexty = 3,          /*Vertical location of annotation text under the X-axis: big=upper; small=lower*/
    dpi = 300,            /*do not set to more than 400 dpi as it will cause Java error */
    w = 9, h = 6.5,       /*width and height of figure in inches*/
    title = "",           /*figure title, with quotes*/
    title_e = "",         /*figure title (time-dependent main effect, early), with quotes*/
    title_l = "",         /*figure title (time-dependent main effect, late), with quotes*/
    path = ,              /*directory of output file, default = same directory as SAS program*/
    imgname = Forestplot, /*name of figure file, no quotes*/
    imgname_e = Forestplot_early,
    imgname_l = Forestplot_late,
    imgfmt = png,         /*PNG / EFS / PDF*/
    outdata = ,           /*specify library and dataset name to output numbers used for forest plot, by default not created*/
  );  

data init; set &indata;
  where not missing(&trtgp);
run;
data init_mva; set init; format _all_;

%let tabid = 0;
%let varlst = &trtgp &covlst;
%let nvarlst = %sysfunc(countw(&varlst));
%let ncovlst = %sysfunc(countw(&covlst));

%do i = 1 %to &nvarlst;
    %let var = %sysfunc(scan(&varlst, &i));

/*Variable name header*/
    proc means data=init noprint;
    class &var;
    var &var;
    output out=header&i n=coln;
    run;
    data header&i; set header&i; where _type_=0;
      indent=0;
      varstr=vlabel(&var);

/*Number of Obs / events*/
    /*no time dependent main effect*/
    %if &cuttime = %then %do;
      proc freq data = init noprint;
        table &var / sparse out = freqtab&i;
        table &var*&trtgp*&event / sparse out = raw1&i;
      run;
      proc sort data=raw1&i; by &var &trtgp &event;
      proc transpose data=raw1&i out=neventtab&i prefix=n;
      by &var; var count;
      %if &var = &trtgp %then %do;
        proc sql; create table trtmax as
          select &trtgp, max(n1) as n1, max(n2) as n2, max(n3) as n3, max(n4) as n4
          from neventtab&i;
        quit;
        data neventtab&i; set trtmax;
          counta=n1+n3; eventa=n3; countb=n2+n4; eventb=n4;
        run;
      %end;
      %else %do;
        data neventtab&i; set neventtab&i;
          counta=n1+n2; eventa=n2; countb=n3+n4; eventb=n4;
        run;
      %end;

      data detail&i; merge freqtab&i neventtab&i; by &var;
      indent=1;
      varstr=vvalue(&var);
      data descrpt&i; set header&i detail&i; varname=vname(&var); varvalue=&var;

      /*number of total case to determine box size*/
      proc sql; select count(&event) into :event_cnt trimmed from &indata where &event = 1; quit;
    %end;

    /*time dependent main effect*/
    %else %do;
      data init_cut; set init;
        &event._e = (&event = 1 and 0 < &intv <= &cuttime);
        &event._l = (&event = 1 and &intv > &cuttime);
      run;
      proc freq data = init_cut noprint;
        table &var / sparse out = freqtab&i;
        table &var*&trtgp*&event._e / sparse out = raw1&i._e;
      run;
      proc freq data = init_cut noprint;
        where &intv > &cuttime;
        table &var*&trtgp*&event._l / sparse out = raw1&i._l;
      proc sort data=raw1&i._e; by &var &trtgp &event._e;
      proc transpose data=raw1&i._e out=neventtab&i._e prefix=n;
      by &var; var count;
      proc sort data=raw1&i._l; by &var &trtgp &event._l;
      proc transpose data=raw1&i._l out=neventtab&i._l prefix=n;
      by &var; var count;
      %if &var = &trtgp %then %do;
        proc sql; create table trtmax as
          select &trtgp, max(n1) as n1, max(n2) as n2, max(n3) as n3, max(n4) as n4
          from neventtab&i._e;
       quit;
        data neventtab&i._e; set trtmax;
          counta=n1+n3; eventa=n3; countb=n2+n4; eventb=n4;
        run;
        proc sql; create table trtmax as
          select &trtgp, max(n1) as n1, max(n2) as n2, max(n3) as n3, max(n4) as n4
          from neventtab&i._l;
        quit;
        data neventtab&i._l; set trtmax;
          counta=n1+n3; eventa=n3; countb=n2+n4; eventb=n4;
        run;
      %end;
      %else %do;
        data neventtab&i._e; set neventtab&i._e;
          counta=n1+n2; eventa=n2; countb=n3+n4; eventb=n4;
        run;
        data neventtab&i._l; set neventtab&i._l;
          counta=n1+n2; eventa=n2; countb=n3+n4; eventb=n4;
        run;
      %end;

      data detail&i._e; merge freqtab&i neventtab&i._e; by &var;
      indent=1;
      varstr=vvalue(&var);
      data detail&i._l; merge freqtab&i neventtab&i._l; by &var;
      indent=1;
      varstr=vvalue(&var);

      data descrpt&i._e; set header&i detail&i._e; varname=vname(&var); if varname="&trtgp" then varname=cat(vname(&var),"_e"); varvalue=&var; run;
      data descrpt&i._l; set header&i detail&i._l; varname=vname(&var); if varname="&trtgp" then varname=cat(vname(&var),"_l"); varvalue=&var; run;

      /*number of total case to determine box size*/
      proc sql; select count(&event) into :event_cnt_e trimmed from &indata where &event = 1 and &intv <= &cuttime; quit;
      proc sql; select count(&event) into :event_cnt_l trimmed from &indata where &event = 1 and &intv > &cuttime; quit;
      %put event_cnt_e event_cnt_l;
    %end;

/*Overall estimate of main effect*/
ods listing close;
    %if &var = &trtgp %then %do;
    proc phreg data=init_mva;
      ods output PHReg.ParameterEstimates=inthr1;
      class &trtgp &covlst / ref=first;
      %if &cuttime = %then %do;
        model &intv*&event(0) = &trtgp &covlst / rl=wald;
      %end;
      %else %do;
        model &intv*&event(0) = &trtgp._e &trtgp._l &covlst / rl=wald;
        t0=&cuttime; &trtgp._e=0; &trtgp._l=0; if &intv<=t0 then &trtgp._e=&trtgp; else if &intv>t0 then &trtgp._l=&trtgp;
      %end;
    run;
    data inthr1; set inthr1;
      varname=parameter;
      varvalue=classval0*1;
      waldlower=hrlowercl;
      waldupper=hruppercl;
      %if &cuttime NE %then %do; td=strip(varname); varvalue=1; %end;    
    %end;
    
/*Estimate of main effect conditioned on interaction variables*/
    %else %do;
    proc phreg data=init_mva;
      ods output PHReg.ModelANOVA=pvalue&i PHReg.HazardRatios=inthr&i;
      class &trtgp &covlst / ref=first;
      %if &cuttime = %then %do;
        model &intv*&event(0) = &trtgp &covlst &trtgp*&var / rl=wald;
        hazardratio &trtgp /at (&var=ALL) diff=ref;
      %end;
      %else %do;
        model &intv*&event(0) = &trtgp._e &trtgp._l &covlst &trtgp._e*&var &trtgp._l*&var / rl=wald;
        t0=&cuttime; &trtgp._e=0; &trtgp._l=0; if &intv<=t0 then &trtgp._e=&trtgp; else if &intv>t0 then &trtgp._l=&trtgp;
        hazardratio &trtgp._e /at (&var=ALL) diff=ref;
        hazardratio &trtgp._l /at (&var=ALL) diff=ref;
      %end;
    run;
    
    data inthr&i; set inthr&i;
      varname=prxchange('s/(.*At )(\w*)(=.*)/$2/',1,description);
      varvalue_c=prxchange('s/(.*=)(\d*)/$2/',1,description);
      varvalue=varvalue_c*1;
      %if &cuttime NE %then td=prxchange('s/^(\w+) (.*)/$1/',1,description);;
    data pvalue&i; set pvalue&i;
      if prxmatch('/\*/i',effect);
      %if &cuttime NE %then td=prxchange('s/(\w+)\*(\w+)/$1/',1,effect);;
      effect=vname(&var);
    %end;
ods listing;

/*Combine demographics, number of obs/events, HR, confidence interval altogether for a single variable*/
    %if &cuttime = %then %do;
      proc sql; create table tab&i as
        select a.*, b.hazardratio as hr, b.waldlower as lcl, b.waldupper as ucl 
               %if &var NE &trtgp %then , c.probchisq as pvalue;
               %if &var EQ &trtgp %then , . as pvalue;
        from descrpt&i as a
        left join inthr&i as b on a.varname=b.varname and a.varvalue=b.varvalue
        %if &var NE &trtgp %then left join pvalue&i as c on a.varname=c.effect;;
      quit;
    %end;
    %else %do;
      proc sql; create table tab&i._e as
        select a.*, b.hazardratio as hr, b.waldlower as lcl, b.waldupper as ucl 
               %if &var NE &trtgp %then , c.probchisq as pvalue;
               %if &var EQ &trtgp %then , . as pvalue;
        from descrpt&i._e as a
        left join inthr&i as b on a.varname=b.varname and a.varvalue=b.varvalue and b.td="&trtgp._e"
        %if &var NE &trtgp %then left join pvalue&i as c on a.varname=c.effect and c.td="&trtgp._e";;
      quit;
      proc sql; create table tab&i._l as
        select a.*, b.hazardratio as hr, b.waldlower as lcl, b.waldupper as ucl 
               %if &var NE &trtgp %then , c.probchisq as pvalue;
               %if &var EQ &trtgp %then , . as pvalue;
        from descrpt&i._l as a
        left join inthr&i as b on a.varname=b.varname and a.varvalue=b.varvalue and b.td="&trtgp._l"
        %if &var NE &trtgp %then left join pvalue&i as c on a.varname=c.effect and c.td="&trtgp._l";;
      quit;
      proc sql; create table tab&i as
        select a.indent, a.varstr, a.varname, a.varvalue, 
               a.counta as counta_e, a.eventa as eventa_e, a.countb as countb_e, a.eventb as eventb_e, a.hr as hr_e, a.lcl as lcl_e, a.ucl as ucl_e, 
               b.counta as counta_l, b.eventa as eventa_l, b.countb as countb_l, b.eventb as eventb_l, b.hr as hr_l, b.lcl as lcl_l, b.ucl as ucl_l,
               a.pvalue as pvalue_e, b.pvalue as pvalue_l
        from tab&i._e as a
        left join tab&i._l as b on a.varstr=b.varstr
        order by indent, missing(varvalue), varvalue;
      quit;
    %end;

  %let tabid = %eval(&tabid + 1);
%end;

/*Generate final output dataset for PROC GTL*/
%if &cuttime = %then %do;
data report; set tab1 - tab&tabid(keep = indent varstr varname varvalue counta eventa countb eventb hr lcl ucl pvalue);
  /*only show interaction p-value at variable header row*/
  if indent ^= 0 then pvalue=.;
  /*only show 1 line of overall main effect: trtA vs. trtB*/
  if varname = "&trtgp" and hr = . then delete;
  if varname = "&trtgp" then do; varstr = "Overall"; indent = 0; call symput('overallhr', hr); end;
  /*exclude row if event is less than a threshold number*/
  %if &eventge NE %then if indent = 1 and (eventa < &eventge or eventb < &eventge) then delete;;
  /*exclude missing co-variate category*/
  %if &missing NE %then if indent = 1 and varvalue in (&missing) then delete;;
data report; set report;
  row = _n_;
  if counta ne . then do;
    pcta = (eventa/counta)*100;
    pctb = (eventb/countb)*100;
    eventdisplaya = right(put(eventa, 4.0));
    countdisplaya = left(put(counta, 4.0));
    eventcounta = right(eventdisplaya || "/" || countdisplaya);
    pctdisplaya = "(" || put(pcta, 4.1) || ")";
    eventdisplayb = right(put(eventb, 4.0));
    countdisplayb = left(put(countb, 4.0));
    eventcountb = right(eventdisplayb || "/" || countdisplayb);
    eventa_eventb = right(eventdisplaya || "/" || eventdisplayb);
    pctdisplayb = "(" || put(pctb, 4.1) || ")";
    hrci = put(hr, 4.2) || " (" || put(lcl, 4.2) || " - " || put(ucl, 4.2) || ")";
    /* determine the marker size based on population size */
    squaresize = ((eventa + eventb) / &event_cnt) * &maxbox;
  end;
  varstr=prxchange('s/^-?\d+([.-]\d+)?\s+//',1,varstr);
  
format hr lcl ucl pvalue 7.2 counta eventa countb eventb 5. pcta pctb 4.1 eventdisplaya countdisplaya eventdisplayb countdisplayb $4.;
run;
%if &outdata NE %then %do; data &outdata; set report; run; %end;
%end;

%else %do;
data report_e; set tab1 - tab&tabid(rename=(counta_e=counta eventa_e=eventa countb_e=countb eventb_e=eventb hr_e=hr lcl_e=lcl ucl_e=ucl pvalue_e=pvalue )
                                    keep = indent varstr varname varvalue counta_e eventa_e countb_e eventb_e hr_e lcl_e ucl_e pvalue_e);
  /*only show interaction p-value at variable header row*/
  if indent^=0 then pvalue=.;
  /*only show 1 line of overall main effect: trtA vs. trtB*/
  if prxmatch("/&trtgp/",varname) and hr=. then delete;
  if prxmatch("/&trtgp/",varname) then do; varstr="Overall"; indent=0; call symput('overallhr_e', hr); end;
  /*exclude row if event is less than a threshold number*/
  %if &eventge NE %then if indent=1 and (eventa < &eventge or eventb < &eventge) then delete;;
  /*exclude missing co-variate category*/
  %if &missing NE %then if indent = 1 and varvalue = &missing then delete;;
data report_e; set report_e;
  row = _n_;
  if counta ne . then do;
    pcta = (eventa/counta)*100;
    pctb = (eventb/countb)*100;
    eventdisplaya = right(put(eventa, 4.0));
    countdisplaya = left(put(counta, 4.0));
    eventcounta = right(eventdisplaya || "/" || countdisplaya);
    pctdisplaya = "(" || put(pcta, 4.1) || ")";
    eventdisplayb = right(put(eventb, 4.0));
    countdisplayb = left(put(countb, 4.0));
    eventcountb = right(eventdisplayb || "/" || countdisplayb);
    eventa_eventb = right(eventdisplaya || "/" || eventdisplayb);
    pctdisplayb = "(" || put(pctb, 4.1) || ")";
    hrci = put(hr, 4.2) || " (" || put(lcl, 4.2) || " - " || put(ucl, 4.2) || ")";
    /* determine the marker size based on population size */
    squaresize = ((eventa + eventb) / &event_cnt_e) * &maxbox;
  end;
  varstr=prxchange('s/^-?\d+([.-]\d+)?\s+//',1,varstr);
format hr lcl ucl pvalue 7.2 counta eventa countb eventb 5. pcta pctb 4.1 eventdisplaya countdisplaya eventdisplayb countdisplayb $4.;
run;

data report_l; set tab1 - tab&tabid(rename=(counta_l=counta eventa_l=eventa countb_l=countb eventb_l=eventb hr_l=hr lcl_l=lcl ucl_l=ucl pvalue_l=pvalue )
                                    keep = indent varstr varname varvalue counta_l eventa_l countb_l eventb_l hr_l lcl_l ucl_l pvalue_l);
  /*only show interaction p-value at variable header row*/
  if indent^=0 then pvalue=.;
  /*only show 1 line of overall main effect: trtA vs. trtB*/
  if prxmatch("/&trtgp/",varname) and hr=. then delete;
  if prxmatch("/&trtgp/",varname) then do; varstr="Overall"; indent=0; call symput('overallhr_l', hr); end;
  /*exclude row if event is less than a threshold number*/
  %if &eventge NE %then if indent=1 and (eventa < &eventge or eventb < &eventge) then delete;;
  /*exclude missing co-variate category*/
  %if &missing NE %then if indent = 1 and varvalue = &missing then delete;;
data report_l; set report_l;
  row = _n_;
  if counta ne . then do;
    pcta = (eventa/counta)*100;
    pctb = (eventb/countb)*100;
    eventdisplaya = right(put(eventa, 4.0));
    countdisplaya = left(put(counta, 4.0));
    eventcounta = right(eventdisplaya || "/" || countdisplaya);
    pctdisplaya = "(" || put(pcta, 4.1) || ")";
    eventdisplayb = right(put(eventb, 4.0));
    countdisplayb = left(put(countb, 4.0));
    eventcountb = right(eventdisplayb || "/" || countdisplayb);
    eventa_eventb = right(eventdisplaya || "/" || eventdisplayb);
    pctdisplayb = "(" || put(pctb, 4.1) || ")";
    hrci = put(hr, 4.2) || " (" || put(lcl, 4.2) || " - " || put(ucl, 4.2) || ")";
    /* determine the marker size based on population size */
    squaresize = ((eventa + eventb) / &event_cnt_l) * &maxbox;
  end;
  varstr=prxchange('s/^-?\d+([.-]\d+)?\s+//',1,varstr);
format hr lcl ucl pvalue 7.2 counta eventa countb eventb 5. pcta pctb 4.1 eventdisplaya countdisplaya eventdisplayb countdisplayb $4.;
run;
%if &outdata NE %then %do; data &outdata._e; set report_e; data &outdata._l; set report_l; run; %end;
%end;

%if &cuttime = %then %do;
  %gtl_forest_i(plotdata = report, lbltrta = &lbltrta, lbltrtb = &lbltrtb, overallhr = &overallhr, refhr = &refhr, minbox = &minbox, maxbox = &maxbox, hrtexty = &hrtexty, 
              title = &title, dpi = &dpi, w = &w, h = &h, path = &path, imgname = &imgname, imgfmt = &imgfmt);
%end;
%else %do;
  %gtl_forest_i(plotdata = report_e, lbltrta = &lbltrta, lbltrtb = &lbltrtb, overallhr = &overallhr_e, refhr = &refhr, minbox = &minbox, maxbox = &maxbox, hrtexty = &hrtexty, 
              title = &title_e, dpi = &dpi, w = &w, h = &h, path = &path, imgname = &imgname_e, imgfmt = &imgfmt);
  %gtl_forest_i(plotdata = report_l, lbltrta = &lbltrta, lbltrtb = &lbltrtb, overallhr = &overallhr_l, refhr = &refhr, minbox = &minbox, maxbox = &maxbox, hrtexty = &hrtexty, 
              title = &title_e, dpi = &dpi, w = &w, h = &h, path = &path, imgname = &imgname_l, imgfmt = &imgfmt);
%end;

%mend;

