drop table if exists tmp_crate;
drop table if exists tmp_import_map;
drop table if exists tmp_fn_decl;
drop table if exists tmp_fn_decl_members_ph1;
drop table if exists tmp_fn_decl_members_ph2;
drop table if exists tmp_fn_decl_ph1;
drop table if exists tmp_fn_decl_ph2;
drop table if exists tmp_type_decl_ph1;
drop table if exists tmp_type_decl_ph2;

export database 'dump' (format json, array true);