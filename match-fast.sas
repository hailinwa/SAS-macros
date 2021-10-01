%macro match(
  case =,
  ctrl =,
  outdata =,
  id = crid,
  ctrlpercase = 1,
  matchon =,
  noprint = 1
  );

  proc sql noprint;
    create table case as
    select *, ranuni(86311) as random
    from &case order by random;

    create table ctrl as 
    select *, ranuni(86409) as random
    from &ctrl order by random;

    create table init_match as 
    select a.&id as case_id, b.&id as ctrl_id, a.random as case_random, b.random as ctrl_random 
    from case a left join ctrl b on &matchon;

    create table scarce_count as
    select case_id, count(ctrl_id) as matched
    from init_match group by case_id order by matched, case_id;

    create table demand_count as
    select ctrl_id, count(case_id) as assigned
    from init_match where not missing(ctrl_id)
    group by ctrl_id order by ctrl_id;

    alter table init_match add matched num, assigned num;
    update init_match a set matched = (select b.matched from scarce_count b where a.case_id = b.case_id),
    assigned = (select c.assigned from demand_count c where a.ctrl_id = c.ctrl_id);
  quit;

  proc sort data = init_match; 
    by matched case_random case_id assigned ctrl_random;
  run;
  data final_match;
    length ctrl_id 8;
    if _n_ = 1 then do;
      declare hash used_ctrl_hash();
      used_ctrl_hash.definekey('ctrl_id');
      used_ctrl_hash.definedata('ctrl_id');
      used_ctrl_hash.definedone();
    end;

    set init_match;
    by matched case_random case_id assigned ctrl_random;
    retain count;
    if first.case_id then count = 0;
    if matched > 0 and count < &ctrlpercase and used_ctrl_hash.find() ~= 0 then do;
      count = count + 1;
      output final_match;
      used_ctrl_hash.add();
    end;
    if last.case_id then do;
      if count = 0 then do;
        put 'Warning: no match for: ' case_id;
        output final_match;
      end;
      else if count < &ctrlpercase then do;
        put 'Warning: not enough controls for: ' case_id;
      end;
    end;
  run;

  data &outdata;
    set final_match;
    group_id = case_id + 864931;
  run;

  %if &noprint eq 0 %then %do;
    proc means data = &outdata noprint;
      class case_id;
      var count;
      output out = match_count max = match_count;
    proc freq data = match_count;
      where _type_ = 1;
      table match_count / missing;
    run;
  %end;
%mend;
