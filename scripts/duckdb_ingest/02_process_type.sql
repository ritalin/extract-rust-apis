
create or replace temporary table tmp_type_decl_ph2 (
    id bigint not null primary key,
    lookup_key varchar not null, 
    symbol varchar not null,
    module_symbol varchar null,
    crate_symbol varchar null
);

insert into tmp_type_decl_ph2 by name
select distinct on (t1.lookup_key)
    nextval('type_symbol_seq') as id,
    t1.lookup_key as lookup_key, 
    t1.symbol as symbol, t1.module_symbol as module_symbol, t1.crate_symbol,
from (
    /* nominal */
    select lookup_key as from_lookup_key, lookup_key, decl.symbol, decl.module_symbol, decl.crate_symbol
    from tmp_type_decl_ph1 ts1
    where not exists (
        from tmp_import_map ts2
        where ts2.to_type = ts1.lookup_key
    )
    /* remap import type */
    union all
    select ts1.from_type, lookup_key, decl.symbol, decl.module_symbol, decl.crate_symbol
    from tmp_import_map ts1
    join tmp_type_decl_ph1 ts2 on ts1.to_type = ts2.lookup_key
) t1;

insert into tmp_type_decl_ph2 by name
select distinct on (lookup_key) 
    nextval('type_symbol_seq') as id,    
    v.*
from (
    /* slice */
    select 
        member.lookup_key as lookup_key, 
        member.symbol as symbol, member.module_symbol as module_symbol, member.crate_symbol as crate_symbol
    from (
        select unnest(decl.slice_member) as member 
        from tmp_type_decl_ph1
        where decl.slice_member is not null
        /* remap import type */
        union
        select unnest(ts2.decl.slice_member)
        from tmp_import_map ts1
        join tmp_type_decl_ph1 ts2 on ts1.to_type = ts2.lookup_key
        where ts2.decl.slice_member is not null
    )
    /* tuple */
    union all
    select
        member.lookup_key as lookup_key, 
        member.symbol as symbol, member.module_symbol as module_symbol, member.crate_symbol as crate_symbol
    from (
        select unnest(decl.tuple_members) as member 
        from tmp_type_decl_ph1
        where coalesce(decl.tuple_members, []) <> []
        /* remap import type */
        union
        select unnest(ts2.decl.tuple_members)
        from tmp_import_map ts1
        join tmp_type_decl_ph1 ts2 on ts1.to_type = ts2.lookup_key
        where coalesce(ts2.decl.tuple_members, []) <> []
    )
) v
where not exists (
    from tmp_type_decl_ph2 ts1
    where ts1.lookup_key = v.lookup_key
);
