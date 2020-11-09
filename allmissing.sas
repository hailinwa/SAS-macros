%macro allms(indata=, outdata=);  
proc format;
 value $missfmt ' '='Missing' '.'='Missing' other='Not Missing';
 value  missfmt  . ='Missing' other='Not Missing';
run;

proc sql; select count(crid) into :obscnt trimmed from &indata; quit;

ods listing close;
proc contents data=&indata position out=contents; run;
data contents; set contents; keep name label; run;

ods output onewayfreqs=numlst(keep=table frequency cumfrequency);
proc freq data=&indata;
format _NUMERIC_ missfmt.;
tables _NUMERIC_ / missprint nopercent; 
run;
data numlst; set numlst;
  if frequency=&obscnt and cumfrequency=.;
  varname=strip(prxchange('s/Table //',1,table));
  vtype="Numeric";
run;

ods output onewayfreqs=charlst(keep=table frequency cumfrequency);
proc freq data=&indata;
format _CHAR_ $missfmt.;
tables _CHAR_ / missprint nopercent;
run;
data charlst; set charlst;
  if frequency=&obscnt and cumfrequency=.;
  varname=strip(prxchange('s/Table //',1,table));
  vtype="Char";
run;
ods listing;

data lst; set numlst charlst;
  keep varname vtype;
run;

proc sql; create table lst2 as
select a.*,b.label
from lst as a left join contents as b
on a.varname=b.name;
quit;

data lst2; set lst2;
  length relabel $500;
  relabel=cat(strip(label)," [MAY NOT CONTAIN DATA]");
run;

ods excel close;
ods excel file="&indata list of all missing variables &sysdate..xlsx" options(frozen_headers='on');
proc print data=lst2; run; 

%if &outdata NE %then %do;
data tmp; set &indata; run;
data _null_; set lst2;
  if _n_=1 then call execute("data tmp; set tmp;");
  call execute("label "||varname||"="||'"'||strip(relabel)||'";');
run;
data &outdata(compress=binary); set tmp; run;
%end;

%mend;


