/* Macro for generating forest plot comparing main effect conditioned on INTERACTION variables
/* Author: Hai-Lin Wang <hwang@mcw.edu>
/* Created: 2019-10-08
/*
/* Functions and their uses:
/* - Main function
/* %forestplot_ph(): use PROC PHREG to obtain parameters, create template dataset and pass to %gtl_forest_ph() to produce forest plot
/* - Internal function
/* %gtl_forest_ph(): takes generated dataset and produce plot using PROC GRL
/*
/* Required variables:
/* Variables to be included in Cox model
/*
/* Note:
/* - This macro is for showing hazard ratios of all co-variates from the Cox model 
/* - Cases missing event, time to event variables will be excluded
/* - Parameter &eventge restricts certain co-variate categories from display in the figure, however the interaction p-value considers all possible categories
/* - If the internal function %gtl_forest_ph() is called directly to produce figures, then please use &outdata parameter of %forestplot_ph()
/*   to see format of dataset to be passed to %gtl_forest_ph()
/*
/* Reference:
/* https://www.pharmasug.org/proceedings/2017/QT/PharmaSUG-2017-QT15.pdf
/* ---------------------------------------------------------------------- */

%macro gtl_forest_ph(
    plotdata = ,          /*dataset template containing all results for plotting*/
    minbox = ,            /*minimum box size for plot*/
    maxbox = ,            /*maximum box size for plot*/
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
      varstr=tranwrd(varstr, '!=', '(*ESC*){Unicode ''2260''x}');
      varstr=tranwrd(varstr, '^9', '(*ESC*){Unicode ''2079''x}');
      label=varstr;
     run;

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

       layout lattice / columns=6 columnweights=(0.28 0.09 0.08 0.15 0.30 0.10);
       /*--Column headers--*/
       sidebar / align=top;
           layout lattice /
               rows=1 columns=4 columnweights=(0.29 0.17 0.43 0.11);
               entry textattrs=(size=8) halign=left "Co-variates";
               entry textattrs=(size=8) halign=left "Event / N (%)";
               entry textattrs=(size=8) halign=left "HR (95% CI)";
               entry textattrs=(size=8) halign=left "P Value" ;
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
       /*--Second column showing Events/Count --*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=EventCount /display=(values)
           valuejustify = center;
       endlayout;
       /*--Third column showing Percent--*/
       layout overlay / xaxisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=PctDisplay /display=(values)
           valuejustify = left;
       endlayout;
       /*--Fourth column showing HRCIGroup--*/
           layout overlay / x2axisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=HRCI / display=(values)
           valuejustify = center;
       endlayout;
       /*--Fifth column showing Hazard ratio graph with 95% error bars--*/
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
       endlayout;
       /*--Sixth column showing P-Values--*/
       layout overlay / x2axisopts=(display=none)
           yaxisopts=(reverse=true display=none) walldisplay=none;
           referenceline y=ref / lineattrs=(thickness=_thk color=_color);
           axistable y=row value=pvalue / display=(values) valueformat=pvalue6.3
           valuejustify = right
           showmissing=false; /*false removes . for missing pvalues*/
       endlayout;
       endlayout;
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


%macro forestplot_ph(
    indata = ,            /*input dataset*/
    event = ,             /*event indicator, 0 (censor) vs. 1 (event)*/
    intv = ,              /*time to event*/
    covlst = ,            /*list of co-variates considered in Cox model*/
    eventge = 5,          /*exclude HR estimate from figure if number of events in corresponding subgroup for either treatment is less than specified, default = 5 events*/
    minbox = 4,           /*minimum box size for plot*/
    maxbox = 12,          /*maximum box size for plot*/
    missing = ,           /*specify list of missing values if want to exclude missing co-variate category from figure, use %quote(x,y)*/
    hrtexty = 3,          /*Vertical location of annotation text under the X-axis: big=upper; small=lower*/
    dpi = 300,            /*do not set to more than 400 dpi as it will cause Java error */
    w = 9, h = 6.5,       /*width and height of figure in inches*/
    title = "",           /*figure title, with quotes*/
    path = ,              /*directory of output file, default = same directory as SAS program*/
    imgname = Forestplot, /*name of figure file, no quotes*/
    imgfmt = png,         /*PNG / EFS / PDF*/
    outdata = ,           /*specify library and dataset name to output numbers used for forest plot, by default not created*/
  );  

data init; set &indata; run;
data init_mva; set init; format _all_;

%let tabid = 0;
%let varlst = &covlst;
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
      proc freq data = init noprint;
        table &var / sparse out = freqtab&i;
        table &var*&event / sparse out = raw1&i;
      run;
      proc sort data=raw1&i; by &var &event;

      proc transpose data=raw1&i out=neventtab&i prefix=n;
      by &var; var count;

      data neventtab&i; set neventtab&i;
        count=n1+n2; event=n2;
      run;

      data detail&i; merge freqtab&i neventtab&i; by &var;
      indent=1;
      varstr=vvalue(&var);
      data descrpt&i; set header&i detail&i; varname=vname(&var); varvalue=&var;
  %let tabid = %eval(&tabid + 1);
%end;

data descrpt; set descrpt1 - descrpt&tabid(keep=indent varstr event count varname varvalue);
  rownum=_n_;
run;

/*Number of events for box size*/
proc sql; select count(&event) into :event_cnt trimmed from &indata where &event = 1; quit;

/*Cox model*/
ods listing close;
    proc phreg data=init_mva;
      ods output PHReg.ParameterEstimates=hr PHReg.ModelANOVA=pvalue;
      class &covlst / ref=first;
        model &intv*&event(0) = &covlst / rl=wald;
    run;
ods listing;

    data hr; set hr;
      varname=parameter;
      varvalue=classval0*1;
      waldlower=hrlowercl;
      waldupper=hruppercl;
    
    data pvalue; set pvalue;
      indent=0; /*for matching with varstr header line*/
    proc contents data=pvalue;

/*Combine demographics, number of obs/events, HR, confidence interval altogether*/
    proc sql; create table report as
      select a.*,b.probchisq as pvalue, c.hazardratio as hr, c.hrlowercl as lcl, c.hruppercl as ucl
      from descrpt as a 
      left join pvalue as b on a.varname=b.effect and a.indent=b.indent
      left join hr as c on a.varname=c.varname and a.varvalue=c.varvalue
      order by rownum;
    quit;

/*Generate final output dataset for PROC GTL*/
data report; set report;
  /*exclude row if event is less than a threshold number*/
  %if &eventge NE %then if indent = 1 and event < &eventge then delete;;
  /*exclude missing co-variate category*/
  %if &missing NE %then if indent = 1 and varvalue in (&missing) then delete;;
data report; set report;
  row = _n_;
  if count ne . then do;
    pct = (event/count)*100;
    eventdisplay = right(put(event, 4.0));
    countdisplay = left(put(count, 4.0));
    eventcount = right(eventdisplay || "/" || countdisplay);
    pctdisplay = "(" || put(pct, 4.1) || ")";
    hrci = put(hr, 4.2) || " (" || put(lcl, 4.2) || " - " || put(ucl, 4.2) || ")";
    /* determine the marker size based on population size */
    squaresize = (event / &event_cnt) * &maxbox;
  end;
  if indent=1 and missing(hr) then hrci = "Baseline";
  varstr=prxchange('s/^-?\d+([.-]\d+)?\s+//',1,varstr);


format hr lcl ucl pvalue 7.2 count event 5. pct 4.1 eventdisplay countdisplay $4.;
run;
%if &outdata NE %then %do; data &outdata; set report; run; %end;


%gtl_forest_ph(plotdata = report, minbox = &minbox, maxbox = &maxbox , hrtexty = &hrtexty, 
              dpi = &dpi, w = &w, h = &h, title = &title, path = &path, imgname = &imgname, imgfmt = &imgfmt);

%mend;




