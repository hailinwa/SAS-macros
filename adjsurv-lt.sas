/* Macro for 1) estimating direct adjusted survival functions at the
/* event times of K treatment groups for data with or without left truncation
/* based on stratified Cox model, 2) simulating pair-wise
/* confidence bands and simultaneous p-values.
/* -------------------------------------------- */
/* Author: Zhen-Huan Hu <zhu@mcw.edu>
/* Last change: 2019-08-01
/*
/* Parameters and their specifications:
/* -------------------------------------------- */
/*  - INDATA:         Input data set
/*  - TIME:           Time to event
/*  - EVENT:          1 = Event of interest
/*                    0 = Right censoring
/*  - STRATA:         Treatment groups
/*  - COVLST:         List of covariates
/*  - LTIME:          Time to left truncation
/*  - SEED:           Seed for random number generation
/*  - NSIM:           Number of simulations
/*  - MINTIME:        Minimum time for CB
/*  - MAXTIME:        Maximum time for CB
/*  - ALPHA:          Confidence level
/*  - TIMELIST:       Time points at which results are displayed
/*  - OUTDATA:        Output data set
/*  - OUTSURVPLOT:    1 = Output survival plot
/*                    0 = Suppress outputting survival plot
/*  - OUTDIFFPLOT:    1 = Output survival difference plot
/*                    0 = Suppress outputting survival difference plot
/*  - STYLE           Output image style
/*  - WIDTH           Output image width
/*  - HEIGHT          Output image height
/*  - IMAGEFMT:       Output image file format
/*  - NOPRINT:        0 = Show results
/*                    1 = Suppress results
/*
/* Notes:
/* -------------------------------------------- */
/* 1. When specifying COVLST, use a | sign to separate continuous covariates
/*    from categorical covariates.
/*    For example:
/*    - If A is categorical and B is continuous: COVLST = A | B
/*    - If both A and B are categorical: COVLST = A B
/*    - If both A and B are continuous: COVLST = | A B
/* -------------------------------------------- */
/* 2. Use LTIME to specify time to left truncation. The value of TIME must be
/*    larger than that of the LTIME for each individual.
/* -------------------------------------------- */
/* 3. Use MINTIME and MAXTIME to specify the time range for CB. If they are
/*    left unspecified, the shared minimum event time will be used as the
/*    minimum time, and the shared maximum time of having 10 subjects
/*    at risk will be used as the maximum time.
/* -------------------------------------------- */
/* 4. Specify TIMELIST to restrict displaying of outputs only at certain time
/*    points. This setting does not affect output data sets or plots.
/* -------------------------------------------- */
/* 5. Use OUTPUT to specify the name of the main output data set containing
/*    adjusted survival estimates. It is also used as prefixes of two
/*    additional data sets containing survival difference estimates and
/*    simultaneous p-values respectively. Assuming OUTPUT = ADJOUT, the two
/*    additional data sets will be ADJOUTDIFF and ADJOUTTEST.
/* -------------------------------------------- */

%macro adjsurv(
  indata = ,            /* Input data set */
  time = ,              /* Time to event */
  event = ,             /* 1 = Event of interest, 0 = Right censoring */
  strata = ,            /* Treatment groups */
  covlst = ,            /* List of covariates */
  ltime = ,             /* Left-truncation time */
  seed = 1986,          /* Random seed for simulation */
  nsim = 1000,          /* N of simulations */
  mintime = ,           /* Minimum time for CB */
  maxtime = ,           /* Maximum time for CB */
  alpha = .05,          /* Alpha level */
  timelist = ,          /* A list of displaying time points */
  outdata = adjout,     /* Output data set */
  outsurvplot = 0,      /* Output survival plot */
  outdiffplot = 0,      /* Output survival diff plot(s) */
  width = 800px,        /* Output image width */
  height = 600px,       /* Output image height */
  style = statistical,  /* Output image style */
  imagefmt = png,       /* Output image format */
  noprint = 0           /* 0 = Show results, 1 = Hide results */
  );

  %let dsid = %sysfunc(open(&indata, i));
  %let eventnum = %sysfunc(varnum(&dsid, &event));
  %let eventlbl = %qsysfunc(varlabel(&dsid, &eventnum));
  %let stratanum = %sysfunc(varnum(&dsid, &strata));
  %let stratalbl = %qsysfunc(varlabel(&dsid, &stratanum));
  %let stratafmt = %qsysfunc(varfmt(&dsid, &stratanum));
  %let rc = %sysfunc(close(&dsid));
  %if &eventlbl = %then %let eventlbl = %upcase(&event);
  %if &stratalbl = %then %let stratalbl = %upcase(&strata);

  %if %sysfunc(prxmatch(/\|/, &covlst)) %then %do;
    %let catelst = %qsysfunc(prxchange(s/^(.*)\|(.*)$/$1/, 1, &covlst));
    %let contlst = %qsysfunc(prxchange(s/^(.*)\|(.*)$/$2/, 1, &covlst));
  %end;
  %else %do;
    %let catelst = &covlst;
    %let contlst = ;
  %end;

  proc iml;
    use &indata;
    %if &ltime = %then read all var {&time &event &strata} into xdata;
    %else read all var {&time &event &strata &ltime} into xdata;;
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
      z = z || xcont;
    %end;
    nz = ncol(z);
    znames = 'z' + strip(char(1: nz));
    store nz znames;

    out = xdata || z;
    %if &ltime = %then out_vnames = {'time' 'event' 'strata'} || znames;
    %else out_vnames = {'time' 'event' 'strata' 'ltime'} || znames;;
    create preproc from out[colname = out_vnames];
    append from out;
    close preproc;
  quit;

  proc phreg data = preproc covout outest = covout noprint;
    strata strata;
    %if &ltime = %then model time * event(0) = z:;
    %else model time * event(0) = z: / entry = ltime;;
    output out = coxout xbeta = zbeta;
  run;

  proc iml;
    load nz znames;
    use covout where(_type_ = 'COV');
    read all var znames into covbeta; * Cov matrix for beta;
    close covout;
    use coxout;
    %if &ltime = %then read all var {time event strata zbeta};
    %else read all var {time event strata ltime zbeta};;
    read all var znames into z;
    close coxout;

    nl = nrow(z); * N of z;
    bystrata = unique(strata);
    ns = ncol(bystrata); * N of strata;

    * Create risk matrix;
    riskmat = j(nl, nl, .);
    do i = 1 to nl;
      %if &ltime = %then riskmat[i, ] = t(strata = strata[i] & time >= time[i]);
      %else riskmat[i, ] = t(strata = strata[i] & time >= time[i] & time[i] > ltime);;
    end;

    s0beta = riskmat * exp(zbeta);
    s1beta = riskmat * (exp(zbeta) # z);
    ebeta = s1beta / s0beta; * t x E(beta);
    free riskmat;

    * Create counting process matrix;
    pre_tlist = {0 &timelist};
    tlist = t(unique(pre_tlist));
    ntlist = nrow(tlist);
    ts0 = j(ntlist # ns, 2, .);
    ts0[, 1] = repeat(tlist, ns, 1);
    ts0[, 2] = shape(repeat(t(bystrata), 1, ntlist), ntlist # ns, 1);
    ts_block = ts0 // ((time || strata)[loc(event), ]);

    call sortndx(tdx, ts_block, 2: 1);
    tdx_unique = uniqueby(ts_block, 2: 1, tdx);
    nt = nrow(tdx_unique); * N of unique event times;
    time_sorted = ts_block[tdx[tdx_unique], 1];
    strata_sorted = ts_block[tdx[tdx_unique], 2];
    free ts0 ts_block;

    atrisk = j(nt, 1, .);
    histmat = j(nt, nl, .);
    do t = 1 to nt;
      %if &ltime = %then atrisk[t, ] = sum(strata = strata_sorted[t] & time >= time_sorted[t]);
      %else atrisk[t, ] = sum(strata = strata_sorted[t] & time >= time_sorted[t] & time_sorted[t] > ltime);;
      histmat[t, ] = t(strata = strata_sorted[t] & time <= time_sorted[t]); * t x I(X <= t);
    end;

    * Get unique z;
    call sortndx(zdx, z, 1: nz);
    zdx_unique = uniqueby(z, 1: nz, zdx);
    nl_weighted = nrow(zdx_unique);
    weight = (zdx_unique[2: nl_weighted] // (nl + 1)) - zdx_unique;
    z_weighted = z[zdx[zdx_unique], ];
    zbeta_weighted = zbeta[zdx[zdx_unique]];

    * Calculate direct adjusted survivals;
    h0 = event / s0beta; * t x Baseline h(t);
    h_weighted = h0 * t(exp(zbeta_weighted)); * t x h(t|z);
    s_weighted = exp(-(histmat * h_weighted)); * t x S(t|z);
    adjsurv = (s_weighted * weight) / nl; * t x Direct adjusted survivals;

    * Calculate V1;
    sszbeta = s_weighted * (weight # exp(zbeta_weighted));
    v1core = histmat * (h0 / s0beta);
    v1 = (sszbeta ## 2) # v1core / (nl ## 2);

    * Calculate V2;
    ssh = j(nt, nz, .); * t x Sum of S(t|z) multiplied by H matrix;
    do c = 1 to nz;
      ssh[, c] = (s_weighted # (
      histmat * ((repeat(t(z_weighted[, c]), nl, 1) - ebeta[, c]) # h_weighted)
      )) * weight;
    end;
    v2 = ((ssh * covbeta) # ssh)[, +] / (nl ## 2);

    adjvar = v1 + v2; * t x Variances;
    adjse = sqrt(adjvar); * t x SE;
    free h_weighted s_weighted zbeta_weighted z_weighted;

    * Calculate CL;
    z_alpha = probit(1 - &alpha / 2.0);
    cl = adjsurv + (z_alpha # adjse) * {-1 1};

    * Evaluate S1(t) - S2(t);
    sc = allcomb(ns, 2);
    call sort(sc, 1: 2);
    nsc = nrow(sc);
    strata_ref_sc = shape(bystrata[shape(sc, 1)], nsc, 2);

    time_pooled = {};
    byloc = {};
    byloc_offset = loc(time_sorted = 0) - 1;
    do s = 1 to nsc;
      s12pair = strata_ref_sc[s, ];
      time_sorted_s1 = time_sorted[loc(strata_sorted = s12pair[1])];
      time_sorted_s2 = time_sorted[loc(strata_sorted = s12pair[2])];
      time_pooled_s12 = t(unique(time_sorted[loc((
      strata_sorted = s12pair[1] | strata_sorted = s12pair[2]) & 
      time_sorted <= min(max(time_sorted_s1), max(time_sorted_s2)))]));
      time_pooled = time_pooled // time_pooled_s12;

      nt_pooled_s12 = nrow(time_pooled_s12);
      byloc_s12 = j(nt_pooled_s12, 2, .);
      byloc_s12[, 1] = (repeat(t(time_sorted_s1), nt_pooled_s12, 1) <= time_pooled_s12)[, +] + byloc_offset[sc[s, 1]];
      byloc_s12[, 2] = (repeat(t(time_sorted_s2), nt_pooled_s12, 1) <= time_pooled_s12)[, +] + byloc_offset[sc[s, 2]];
      byloc = byloc // byloc_s12;
    end;
    nt_pooled = nrow(time_pooled);
    strata_pair = j(nt_pooled, 2, .);
    strata_pair[, 1] = strata_sorted[byloc[, 1]];
    strata_pair[, 2] = strata_sorted[byloc[, 2]];

    wdiff = adjsurv[byloc[, 1]] - adjsurv[byloc[, 2]];
    theta = ssh[byloc[, 1], ] - ssh[byloc[, 2], ];
    vardiff = v1[byloc[, 1]] + v1[byloc[, 2]] + ((theta * covbeta) # theta)[, +] / (nl ## 2);
    sediff = sqrt(vardiff);
    free theta;

    * Calculate CL for S1(t) - S2(t);
    cldiff = wdiff + (z_alpha # sediff) * {-1 1};

    * Initialize simulations for CB;
    nsim = &nsim;
    gmat = j(nl, nsim, .);
    call randseed(&seed);
    call randgen(gmat, 'NORMAL');
    gevent = gmat # event;
    w1_sim = -(sszbeta # (histmat * (gevent / s0beta))) / nl;
    w2_sim = -(ssh * covbeta * (t(z - ebeta) * gevent)) / nl;
    free gevent gmat histmat;
    w_sim_add = w1_sim + w2_sim;
    w_sim_sub = w1_sim - w2_sim;
    free w1_sim w2_sim;

    * Define t1, t2 for CB;
    %if &mintime = %then %do;
      cb_prebyloc_t1 = (adjsurv < 1);
      cbdiff_prebyloc_t1 = (adjsurv[byloc[, 1]] < 1 & adjsurv[byloc[, 2]] < 1);
    %end;
    %else %do;
      cb_prebyloc_t1 = (time_sorted >= &mintime);
      cbdiff_prebyloc_t1 = (time_pooled >= &mintime);
    %end;
    %if &maxtime = %then %do;
      cb_prebyloc_t2 = (adjsurv > 0);
      cbdiff_prebyloc_t2 = (adjsurv[byloc[, 1]] > 0 & adjsurv[byloc[, 2]] > 0);
    %end;
    %else %do;
      cb_prebyloc_t2 = (time_sorted <= &maxtime);
      cbdiff_prebyloc_t2 = (time_pooled <= &maxtime);
    %end;
    cb_prebyloc_t12 = cb_prebyloc_t1 & cb_prebyloc_t2;
    cbdiff_prebyloc_t12 = cbdiff_prebyloc_t1 & cbdiff_prebyloc_t2;

    * Calculate CB for S(t);
    cb = j(nt, 2, .);
    do s = 1 to ns;
      cb_byloc = loc(cb_prebyloc_t12 & strata_sorted = bystrata[s]);
      qb = t((abs(w_sim_add[cb_byloc, ] / adjse[cb_byloc]))[<>, ]);
      call qntl(c_alpha, qb, 1 - &alpha, 2);
      cb[cb_byloc, ] = adjsurv[cb_byloc] + (c_alpha # adjse[cb_byloc]) * {-1 1};
    end;

    * Calculate CB for S1(t) - S2(t);
    cp = j(nsc, 3, .);
    cp[, 1] = nsim;

    cbdiff = j(nt_pooled, 2, .);
    wdiff_sim = w_sim_sub[byloc[, 1], ] + w_sim_add[byloc[, 2], ];
    do s = 1 to nsc;
      s12pair = strata_ref_sc[s, ];
      cbdiff_byloc = loc(cbdiff_prebyloc_t12 & strata_pair[, 1] = s12pair[1] & strata_pair[, 2] = s12pair[2]);
      qb = t((abs(wdiff_sim[cbdiff_byloc, ] / sediff[cbdiff_byloc]))[<>, ]);
      call qntl(c_alpha, qb, 1 - &alpha, 2);
      cbdiff[cbdiff_byloc, ] = wdiff[cbdiff_byloc] + (c_alpha # sediff[cbdiff_byloc]) * {-1 1};

      cp[s, 2] = c_alpha;
      qmax = max(abs(wdiff[cbdiff_byloc] / sediff[cbdiff_byloc]));
      cp[s, 3] = mean(qb > qmax); * P-value;
    end;
    free w_sim_add w_sim_sub wdiff_sim;

    outsurv = time_sorted || strata_sorted || adjsurv || adjse || cl || cb || atrisk;
    create &outdata from outsurv[colname = {"&time" "&strata" 'adjsurv' 'stderr' 'lcl' 'ucl' 'lcb' 'ucb' 'atrisk'}];
    append from outsurv;
    close &outdata;
    free outsurv;

    outdiff = time_pooled || strata_pair || wdiff || sediff || cldiff || cbdiff;
    create &outdata.diff from outdiff[colname = {"&time" "&strata.1" "&strata.2" 'adjdiff' 'stderr' 'lcl' 'ucl' 'lcb' 'ucb'}];
    append from outdiff;
    close &outdata.diff;
    free outdiff;

    outtest = strata_ref_sc || cp;
    create &outdata.test from outtest[colname = {"&strata.1" "&strata.2" 'nsim' 'c_alpha' 'p_value'}];
    append from outtest;
    close &outdata.test;
    free outtest;
  quit;

  %if &noprint = 0 %then %do;
    proc print data = &outdata.test heading = horizontal noobs;
      title "Hypothesis Tests";
      var &strata.1 &strata.2 nsim c_alpha p_value;
      format &strata.1 &strata.2 &stratafmt;
    proc sort data = &outdata;
      by &strata &time;
    proc print data = &outdata heading = horizontal noobs;
      %if &timelist ~= %then %do; where &time in (&timelist); %end;
      title "Direct Adjusted Survival Estimates";
      by &strata;
      var &time &strata adjsurv stderr lcl ucl lcb ucb atrisk;
      format &strata &stratafmt;
    run;
    proc sort data = &outdata.diff;
      by &strata.1 &strata.2 &time;
    proc print data = &outdata.diff heading = horizontal noobs;
      %if &timelist ~= %then %do; where &time in (&timelist); %end;
      title "Direct Adjusted Survival Difference Estimates";
      by &strata.1 &strata.2;
      var &time &strata.1 &strata.2 adjdiff stderr lcl ucl lcb ucb;
      format &strata.1 &strata.2 &stratafmt;
    run;
  %end;
  %if &outsurvplot > 0 or &outdiffplot > 0 %then %do;
    %let conf = %sysevalf(100 * (1 - &alpha));
    %if &mintime ~= %then %let minopt = min = &mintime; %else %let minopt = ;
    %if &maxtime ~= %then %let maxopt = max = &maxtime; %else %let maxopt = ;
    %if &outsurvplot > 0 %then %do;
      data survplot;
        set &outdata;
        label &strata = "&stratalbl";
        format &strata &stratafmt;
      run;
      ods listing style = &style;
      ods graphics on / reset = all imagename = "&outdata-survplot" imagefmt = &imagefmt width = &width height = &height;
      title "Direct Adjusted Survival Curves for &eventlbl with &conf% Simultaneous CB";
      %if &outsurvplot = 1 %then %do;
        proc sgpanel data = survplot;
          panelby &strata / skipemptycells;
          band x = &time lower = lcb upper = ucb / name = 'cb' type = step fill transparency = .5 legendlabel = "&conf% Simultaneous CB";
          band x = &time lower = lcl upper = ucl / name = 'cl' type = step nofill legendlabel = "&conf% Pointwise CI";
          step x = &time y = adjsurv / name = 'adjsurv' justify = left legendlabel = "Survival Estimates";
          colaxis label = "Time" &minopt &maxopt;
          rowaxis label = "Direct Adjusted Survival Probability" min = 0 max = 1;
          keylegend 'adjsurv' 'cl' 'cb' / position = bottom;
        run;
      %end;
      %else %if &outsurvplot = 2 %then %do;
        proc sgplot data = survplot;
          band x = &time lower = lcb upper = ucb / name = 'cb' group = &strata type = step fill transparency = .5 legendlabel = "&conf% Simultaneous CB";
          band x = &time lower = lcl upper = ucl / name = 'cl' group = &strata type = step nofill legendlabel = "&conf% Pointwise CI";
          step x = &time y = adjsurv / name = 'adjsurv' group = &strata justify = left;
          xaxis label = "Time" &minopt &maxopt;
          yaxis label = "Direct Adjusted Survival Probability" min = 0 max = 1;
          keylegend 'adjsurv' 'cl' 'cb' / position = bottom;
        run;
      %end;
      ods graphics off;
    %end;
    %if &outdiffplot = 1 %then %do;
      proc sort data = &outdata.test; by &strata.1 &strata.2;
      proc sort data = &outdata.diff; by &strata.1 &strata.2;
      data diffplot;
        merge &outdata.test &outdata.diff;
        by &strata.1 &strata.2;
        label &strata.1 = "&stratalbl 1" &strata.2 = "&stratalbl 2"
        nsim = "No. of Simulations"
        p_value = "Simultaneous P Value";
        format &strata.1 &strata.2 &stratafmt;
      run;
      ods listing style = &style;
      ods graphics on / reset = all imagename = "&outdata-diffplot" imagefmt = &imagefmt width = &width height = &height;
      title "Differences between Direct Adjusted Survival Curves for &eventlbl with &conf% Simultaneous CB";
      proc sgpanel data = diffplot;
        panelby &strata.1 &strata.2 / skipemptycells;
        band x = &time lower = lcb upper = ucb / name = 'cb' type = step fill transparency = .5 legendlabel = "&conf% Simultaneous CB";
        band x = &time lower = lcl upper = ucl / name = 'cl' type = step nofill legendlabel = "&conf% Pointwise CI";
        step x = &time y = adjdiff / name = 'adjdiff' justify = left legendlabel = "Survival Difference Estimates";
        colaxis label = "Time" &minopt &maxopt;
        rowaxis label = "Difference of Survival Probabilities" min = -0.5 max = 0.5;
        refline 0 / axis = y;
        inset nsim p_value / position = topleft;
        keylegend 'adjdiff' 'cl' 'cb' / position = bottom;
      run;
      ods graphics off;
    %end;
  %end;
%mend;
