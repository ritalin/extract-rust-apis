create or replace temporary table tmp_fn_decl as
select distinct * from 'exp/**/fn_decl.json';

create or replace temporary table tmp_type_decl_ph1 as
select distinct * from 'exp/**/type_decl.json';

create or replace temporary table tmp_import_map as
select * from 'exp/**/import_map.json';

/* ---------- */

update tmp_fn_decl
set 
    owner = {'lookup_key': owner.lookup_key, 'generic_args': coalesce(owner.generic_args::varchar[], [])},
    ret_decl = {'lookup_key': ret_decl.lookup_key, 'generic_args': coalesce(ret_decl.generic_args::varchar[], [])},
    args = args.apply(arg -> {'lookup_key': arg.lookup_key, 'generic_args': coalesce(arg.generic_args::varchar[], []), 'sort_order': arg.sort_order})
;
