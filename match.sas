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

    select count(distinct &id) into :ncase trimmed from &case; * Count cases;
    select count(distinct &id) into :nctrl trimmed from &ctrl; * Count controls;
  quit;

  %do i = 1 %to &ncase;
    proc sql noprint; 
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
    data _null_;
      set init_match (obs = 1);
      call symput('cur_case_id', case_id);
    run;
    data current_match_pairs_&i;
      set init_match; 
      by matched case_random case_id assigned ctrl_random;
      where case_id = &cur_case_id;
      retain count;
      if first.case_id and matched = 0 then do;
        count = 0;
        put 'Warning: no match for: ' case_id;
        call symput('n_cur_ctrl', 0);
        output current_match_pairs_&i;
      end;
      else do;
        if first.case_id then count = 1;
        if last.case_id then do;
          if count < &ctrlpercase then do;
            put 'Warning: not enough controls for: ' case_id;
            call symput('n_cur_ctrl', count);
          end;
          else call symput('n_cur_ctrl', &ctrlpercase);
        end;
        if count <= &ctrlpercase then do;
          %* Controls need to be at least as updated as cases;
          call symput(cat('cur_ctrl_id', strip(count)), ctrl_id);
          output current_match_pairs_&i;
          count = count + 1;
        end;
      end;
    run;

    data case; set case;
      if crid = &cur_case_id then delete;
    data ctrl; set ctrl;
      %if &n_cur_ctrl > 0 %then %do j = 1 %to &n_cur_ctrl;
        if crid = &&cur_ctrl_id&j then delete;
      %end;
    run;
  %end;

  data &outdata;
    set current_match_pairs_1 - current_match_pairs_&ncase;
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
