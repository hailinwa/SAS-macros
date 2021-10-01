/* Macro for 1) estimating direct adjusted cumulative incidences
/* for K treatment groups at predetermined time points based on
/* stratified Fine-Gray's model, 2) simulating pair-wise confidence bands
/* and overall p-values.
/* -------------------------------------------- */
/* Author: Zhen-Huan Hu <zhu@mcw.edu>
/* Last change: 2018-07-31
/*
/* Functions and their basic uses:
/* -------------------------------------------- */
/* Parameters for %ADJCIF function:
/*  - INDATA:         Input data set
/*  - TIME:           Time to event
/*  - EVENT:          1 = Event of interest
/*                    2 = Competing risk
/*                    0 = Right censoring  
/*  - STRATA:         Treatment groups
/*  - COVLST:         List of covariates
/*  - SEED:           Random seed for simulation
/*  - NSIM:           N of simulations
/*  - ALPHA:          Alpha level
/*  - OUTDATA:        Output data set
/* -------------------------------------------- */

%macro adjcif(
  indata = ,            /* Input data set */
  time = ,              /* Time to event */
  event = ,             /* 1 = Event of interest, 2 = Competing risk, 0 = Right censoring */
  strata = ,            /* Treatment groups */
  covlst = ,            /* List of covariates */
  divcont = 0,          /* Divide continuous covariates */
  seed = 1986,          /* Random seed for simulation */
  starttime = ,         /* Start time for simulation */
  stoptime = ,          /* Stop time for simulation */
  nsim = 2000,          /* N of simulations */
  alpha = .05,          /* Alpha level */
  outdata = ,           /* Output data set */
  outsurvplot = 1,      /* Output survival plot */
  outdiffplot = 1,      /* Output survival diff plot(s) */
  style = statistical,  /* Output image style */
  imagefmt = png,       /* Output image format */
  noprint = 0           /* 0 = Show results, 1 = Hide results */
  );

  %let dsid = %sysfunc(open(&indata, i));
  %let eventnum = %sysfunc(varnum(&dsid, &event));
  %let eventlbl = %qsysfunc(varlabel(&dsid, &eventnum));
  %let sttnum = %sysfunc(varnum(&dsid, &strata));
  %let sttlbl = %qsysfunc(varlabel(&dsid, &sttnum));
  %let sttfmt = %qsysfunc(varfmt(&dsid, &sttnum));
  %let rc = %sysfunc(close(&dsid));
  %if &eventlbl = %then %let eventlbl = %upcase(&event);
  %if &sttlbl = %then %let sttlbl = %upcase(&strata);
  
  %if %sysfunc(prxmatch(%str(/\|/), &covlst)) %then %do;
    %let catelst = %sysfunc(prxchange(%str(s/^(.*)\|(.*)$/$1/), 1, &covlst));
    %let contlst = %sysfunc(prxchange(%str(s/^(.*)\|(.*)$/$2/), 1, &covlst));
  %end;
  %else %do;
    %let catelst = &covlst;
    %let contlst = ;
  %end;

  proc iml;
    use &indata;
    read all var {&time &event &strata} into xdata;
    %if &catelst ~= %then read all var {&catelst} into xcate;;
    %if &contlst ~= %then read all var {&contlst} into xcont;;
    close &indata;

    z = {};
    nl = nrow(xdata);
    %if &catelst ~= %then %do;
      nx = ncol(xcate);
      do v = 1 to nx;
        var = xcate[, v];
        vcat = unique(var);
        vdf = ncol(vcat) - 1; * DF;
        if vdf > 0 then do;
          cur_z = (repeat(vcat[, 1: vdf], nl, 1) = var);
          z = z || cur_z;
        end;
      end;
    %end;
    %if &contlst ~= %then %do;
      %if &divcont > 1 %then %do;
        nc = ncol(xcont);
        xcont_div = j(nl, nc, .);
        call qntl(xq, xcont, do(0, 1, (1 / &divcont)));
        nc_div = nrow(xq) - 1;
        do q = 1 to nc_div;
          xcont_div[loc(xcont >= xq[q, ] & xcont <= xq[q + 1, ])] = q;
        end;
        z = z || xcont_div;
      %end;
      %else %do;
        z = z || xcont;
      %end;
    %end;
    nz = ncol(z);
    znames = 'z' + strip(char(1: nz));
    store nz znames;

    out = xdata || z;
    out_vnames = {'time' 'event' 'strata'} || znames;
    create preproc from out[colname = out_vnames];
    append from out;
    close preproc;
  quit;

  proc phreg data = preproc covout outest = covout noprint;
    strata strata;
    model time * event(0) = z: / failcode = 1;
    output out = fgout xbeta = zbeta cif = cif;
  run;

  proc iml;
    load nz znames;
    use fgout;
    read all var {time event strata zbeta cif};
    read all var znames into z;
    close fgout;

    csh0 = -log(1 - cif) / exp(zbeta); * Baseline cumulative subdistribution hazard;
    adjcif = t(mean(1 - exp(-exp(zbeta) * t(csh0))));
    out = time || event || strata || csh0 || adjcif;
    vnames = {"&time" "&event" "&strata" 'csh0' 'adjcif'};
    create &outdata from out [colname = vnames];
    append from out;
    close &outdata;
  quit;

  proc sort; by &strata &time &event;
  proc print noobs;
  run;
%mend;
