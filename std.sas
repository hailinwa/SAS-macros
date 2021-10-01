/* Basic macro library for CIBMTR studies
 * Maintainer: Zhenhuan Hu <zhu@mcw.edu>
 * Last change: Sept, 2011

 * Functions and their uses:
 *
 * %revrs(string) Reverse a string
 * %rename(varlst =, ext = 2) Append an extension to variable names
 * %encrypt(id, dummyid) Create MD5 dummy ID
 * %chkted(indata, ted, varlst) Check TED for missing value
 * %donorgp(indata) Categorize HLA-donor groups
 * %colmin(indata, varname, macrovar) Retrieve the column min in a macro variable
 * %colmax(indata, varname, macrovar) Retrieve the column max in a macro variable
 * %retricat(indata, varname, macrovar) Retrieve category in a macro variable
 * %callmissing(varlst) Set the list of variables as missing
 * %condregi(indata) Classify conditioning regimen intensity
 */

%macro revrs(string);
  %local nstring;
  %do i = %length(&string) %to 1 %by -1;
    %let nstring = &nstring%qsubstr(&string, &i, 1);
  %end;
  &nstring
%mend;

%macro rename(varlst =, ext = 2);
  %let pointer = 1;
  %do %until(%superq(varmem) eq %str());
    %let varmem = %qsysfunc(scan(&varlst, &pointer));
    %if %superq(varmem) ne %str() %then %do;
      %unquote(&varmem&ext) = %unquote(&varmem);
      keep %unquote(&varmem&ext);
    %end;
    %let pointer = %eval(&pointer + 1);
  %end;
%mend;

%macro encrypt(id, dummyid);
  length &dummyid $ 16;
  &dummyid = put(&id, 16.);
  &dummyid = md5(&dummyid);
  format &dummyid $hex.;
%mend;

%macro chkted(indata, ted, varlst);
  %let nvarlst = %sysfunc(countw(&varlst, ' '));
  %let dvarlst = %sysfunc(prxchange(%str(s/->\w+//), -1, %nrbquote(&varlst)));
  %let rvarlst = %sysfunc(prxchange(%str(s/\w+->//), -1, %nrbquote(&varlst)));

  %* Set up a cache to improve speed;
  proc sql noprint;
    select 
    min(txtype), max(txtype),
    min(disease), max(disease),
    min(yeartx), max(yeartx)
    into 
    :ttpmin trimmed, :ttpmax trimmed,
    :dismin trimmed, :dismax trimmed,
    :ytxmin trimmed, :ytxmax trimmed
    from &indata;
  quit;
  data _tedcache_ (compress=binary);
    set &ted (keep = crid txtype datebmt disease &dvarlst);
    where txtype in (&ttpmin: &ttpmax)
    and &ytxmin <= year(datebmt) <= &ytxmax
    and disease in (&dismin: &dismax);
    rename datebmt = datetx;
    %do i = 1 %to &nvarlst;
      %let dvar = %sysfunc(scan(&dvarlst, &i));
      %let rvar = %sysfunc(scan(&rvarlst, &i));
      %if &dvar ne &rvar %then %do;
        rename &dvar = &rvar;
      %end;
    %end;
  run;

  data &indata (compress=binary);
    length crid datetx 8;

    if _n_ = 1 then do;
      declare hash h_ted(dataset:"_tedcache_");
      h_ted.definekey('crid', 'datetx');
      h_ted.definedata('crid', 'datetx');
      h_ted.definedone();
    end;

    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&rvarlst, &i));
      %let dsid = %sysfunc(open(_tedcache_, i));
      %let vnum = %sysfunc(varnum(&dsid, &var));
      %let vtype = %sysfunc(vartype(&dsid, &vnum));
      %let rc = %sysfunc(close(&dsid));

      %if &vtype = N %then %do;
        length &var 8;
      %end;
      %else %if &vtype = C %then %do;
        length &var $ 250;
      %end;

      if _n_ = 1 then do;
        declare hash h_ted_&var(dataset:"_tedcache_");
        h_ted_&var..definekey('crid', 'datetx');
        h_ted_&var..definedata("&var");
        h_ted_&var..definedone();
        call missing(&var);
      end;
    %end;
    
    set &indata;

    rc_ted = h_ted.find();

    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&rvarlst, &i));
      %let dsid = %sysfunc(open(_tedcache_, i));
      %let vnum = %sysfunc(varnum(&dsid, &var));
      %let vtype = %sysfunc(vartype(&dsid, &vnum));
      %let rc = %sysfunc(close(&dsid));

      %if &vtype = N %then %do;
        if missing(&var) then rc_ted_&var = h_ted_&var..find();
      %end;
      %else %if &vtype = C %then %do;
        if strip(&var) in ('', '.') then rc_ted_&var = h_ted_&var..find();
      %end;
    %end;
  run;

%mend;

%macro chkcrf(indata, crf, varlst);
  %let nvarlst = %sysfunc(countw(&varlst, ' '));
  %let dvarlst = %sysfunc(prxchange(%str(s/->\w+//), -1, %nrbquote(&varlst)));
  %let rvarlst = %sysfunc(prxchange(%str(s/\w+->//), -1, %nrbquote(&varlst)));
  
  %* Set up a cache to improve speed;
  proc sql noprint;
    select 
    min(txtype), max(txtype),
    min(disease), max(disease),
    min(yeartx), max(yeartx)
    into 
    :ttpmin trimmed, :ttpmax trimmed,
    :dismin trimmed, :dismax trimmed,
    :ytxmin trimmed, :ytxmax trimmed
    from &indata;
  quit;
  data _crfcache_ (compress=binary);
    set &crf (keep = crid txtype datetx disease &dvarlst);
    where txtype in (&ttpmin: &ttpmax)
    and &ytxmin <= year(datetx) <= &ytxmax
    and disease in (&dismin: &dismax);
    rename datetx = datebmt;
    %do i = 1 %to &nvarlst;
      %let dvar = %sysfunc(scan(&dvarlst, &i));
      %let rvar = %sysfunc(scan(&rvarlst, &i));
      %if &dvar ne &rvar %then %do;
        rename &dvar = &rvar;
      %end;
    %end;
  run;

  data &indata (compress=binary);
    length crid datebmt 8;

    if _n_ = 1 then do;
        declare hash h_crf(dataset:"_crfcache_");
        h_crf.definekey('crid', 'datebmt');
        h_crf.definedata('crid', 'datebmt');
        h_crf.definedone();
    end;

    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&rvarlst, &i));
      %let dsid = %sysfunc(open(_crfcache_, i));
      %let vnum = %sysfunc(varnum(&dsid, &var));
      %let vtype = %sysfunc(vartype(&dsid, &vnum));
      %let rc = %sysfunc(close(&dsid));

      %if &vtype = N %then %do;
        length &var 8;
      %end;
      %else %if &vtype = C %then %do;
        length &var $ 250;
      %end;

      if _n_ = 1 then do;
        declare hash h_crf_&var(dataset:"_crfcache_");
        h_crf_&var..definekey('crid', 'datebmt');
        h_crf_&var..definedata("&var");
        h_crf_&var..definedone();
        call missing(&var);
      end;
    %end;
    
    set &indata;

    rc_crf = h_crf.find();

    %do i = 1 %to &nvarlst;
      %let var = %sysfunc(scan(&rvarlst, &i));
      %let dsid = %sysfunc(open(_crfcache_, i));
      %let vnum = %sysfunc(varnum(&dsid, &var));
      %let vtype = %sysfunc(vartype(&dsid, &vnum));
      %let rc = %sysfunc(close(&dsid));

      %if &vtype = N %then %do;
        if missing(&var) then rc_crf_&var = h_crf_&var..find();
      %end;
      %else %if &vtype = C %then %do;
        if strip(&var) in ('', '.') then rc_crf_&var = h_crf_&var..find();
      %end;
    %end;
  run;
%mend;

%macro tagvar(var, varlbl, varfmt);
  label &var = &varlbl;
  format &var &varfmt;
%mend;

%macro colmin(indata, varname, macrovar);
  proc means data = &indata min max noprint;
    var &varname;
    output out = colmin_temp;

  proc transpose data = colmin_temp out = t_colmin_temp;
    var &varname;
    id _stat_;

  data t_colmin_temp;
    set t_colmin_temp;
    call symput("&macrovar", min);
  run;

  %put NOTING: Column min: &macrovar;
%mend;

%macro colmax(indata, varname, macrovar);
  proc means data = &indata min max noprint;
    var &varname;
    output out = colmax_temp;

  proc transpose data = colmax_temp out = t_colmax_temp;
    var &varname;
    id _stat_;

  data t_colmax_temp;
    set t_colmax_temp;
    call symput("&macrovar", max);
  run;

  %put NOTING: Column max: &macrovar;
%mend;

%macro retricat(indata, varname, macrovar);
  proc freq data = &indata noprint;
    table &varname / nocol norow nopct out = cat_table;
  
  data cat_table;
    length collst $ 50;
    set cat_table (keep = &varname);
    retain collst;
  
    collst = strip(cat(strip(collst), " ", put(&varname, 8.0)));
    call symput("macrovar", collst);
  run;
  
  %put NOTING: Now we have retrieved the category: &macrovar;
%mend;

%macro callmissing(varlst);
  %let varlst = %sysfunc(strip(&varlst));
  %let varlst = %sysfunc(prxchange(%str(s/\s+/, /), -1, &varlst));
  call missing(&varlst);
%mend;

%macro dropmissing(var);
  if missing(var) then do;
    delete; put "Note: drop CRID = " crid "due to missing &var";
  end;
%mend;

%macro getattrn(indata = , attrn = , debug = 0);
  %if %sysfunc(exist(&indata)) %then %do;
    %* open data set;
    %let did = %sysfunc(open(&indata));
    %* Get attribute;
    %let attvalue = %sysfunc(attrn(&did, &attrn));
    %* Close data set;
    %let close = %sysfunc(close(&did));
  %end;
  %else %let attvalue = -999;
  
  %if &debug = 1 %then %put getattrn1234 &attvalue;
  
  &attvalue

%mend getattrn;

%macro compressword(targetlist, keylist, debug = 0);
  %* TARGETLIST: string variable with keywords to be removed;
  %* KEYLIST: list of keywords;
  %local i;
  
  %* There is a problem dealing with space character in the TRANWRD function,
  therefore we need to replace space with a special character;
  
  %* Define space;
  %let space = %str( );
  %* Define special delimiter;
  %let dlmt = %str(~);
  %* Replace all spaces with delimiter in the target list;
  %let targetlist = %sysfunc(tranwrd(&targetlist, &space, &dlmt));
  %* Pad target list with one leading and one trailing delimiter so the first and last word can be matched;
  %let targetlist = &dlmt&targetlist&dlmt;
  %* Initialize keylist;
  %let keylist = %sysfunc(compbl(&keylist));
  %* Initialize pointer;
  %let i = 1;
  %* Get first key;
  %let key = %scan(&keylist, &i);
  
  %do %until (&key eq &space or &i = 999); %* 999 is an infinite loop breaker;
    %* Pad keyword with one leading and one trailing delimiter, so that the whole word is matched;
    %let key = &dlmt&key&dlmt;
    %let targetlist = %sysfunc(tranwrd(&targetlist, &key, &dlmt));
    %let i = %eval(&i + 1);
    %let key = %scan(&keylist, &i);
  %end;
  
  %* Replace any delimiter back to space;
  %let targetlist = %sysfunc(tranwrd(&targetlist, &dlmt, &space));
  %* Remove extra commas, if any;
  %if &targetlist ne &space %then %let targetlist = %sysfunc(compbl(&targetlist));
  
  &targetlist
%mend compressword;

%macro putnote(note);
  put "Note: crid = " crid ", " &note;
%mend;

%macro xls2sas(var, type = date);
  %if &type = date %then %do;
    %* SAS_date = Excel_date - 21916;
    &var = &var - 21916;
  %end;
  %else %if &type = time %then %do;
    %* SAS_time = Excel_time * 86400;
    &var = &var * 86400;
  %end;
  %else %if &type = both %then %do;
    %* SAS_date_time = (Excel_date_time - 21916) * 86400;
    &var = (&var - 21916) * 86400;
  %end;
%mend;
