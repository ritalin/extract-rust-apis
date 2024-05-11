create type type_kind as enum ('arg', 'return');
create type type_category as enum ('nominal', 'slice', 'tuple');

create type compound_item as struct (id BIGINT, item_index INT);

CREATE TABLE prototype(
    id BIGINT PRIMARY KEY, 
    symbol VARCHAR NOT NULL, 
    qual_symbol VARCHAR NOT NULL
);

CREATE TABLE module_symbol(
    id BIGINT PRIMARY KEY, 
    symbol VARCHAR NOT NULL
);
CREATE TABLE type_module_ref(
    module_id BIGINT, 
    type_id BIGINT, 
    PRIMARY KEY(module_id, type_id)
);

CREATE TABLE crate_symbol(
    id BIGINT PRIMARY KEY, 
    symbol VARCHAR NOT NULL
);
CREATE TABLE type_crate_ref(
    crate_id BIGINT, 
    type_id BIGINT, 
    PRIMARY KEY(crate_id, type_id)
);

CREATE TABLE type_symbol(
    id BIGINT PRIMARY KEY, 
    symbol VARCHAR NOT NULL
);

CREATE TABLE prototype_type_ref(
    prototype_id BIGINT, 
    type_id BIGINT, 
    kind type_kind, 
    category type_category, 
    PRIMARY KEY(prototype_id, type_id, kind, category)
);
CREATE or replace TABLE prototype_arg_order_ref(
    prototype_id BIGINT, 
    sort_order INTEGER, 
    type_ids compound_item[] NOT NULL, 
    category type_category not null,
    PRIMARY KEY(prototype_id, sort_order)
);
CREATE or replace TABLE prototype_return_ref (
    prototype_id BIGINT NOT NULL PRIMARY KEY, 
    type_ids compound_item[] NOT NULL, 
    category type_category not null,
);

CREATE TABLE prototype_owner_ref(
    owner_type_id BIGINT, 
    prototype_id BIGINT, 
    generic_args VARCHAR[] NOT NULL, 
    PRIMARY KEY(owner_type_id, prototype_id)
);

CREATE TABLE type_import_map(
    from_type_id BIGINT, 
    to_type_id BIGINT, 
    PRIMARY KEY(from_type_id, to_type_id)
);

CREATE TABLE deprecated(
    id BIGINT PRIMARY KEY, 
    since VARCHAR NOT NULL
);
CREATE TABLE deprecated_prototype_ref(
    prototype_id BIGINT, 
    deprecated_id BIGINT, 
    PRIMARY KEY(prototype_id, deprecated_id)
);

/* ---- */

create or replace temporary table tmp_fn_decl as 
select 
    row_number() over(order by "owner".lookup_key, proto) as id,
    proto, proto_qual, ret_decl, args, deprecated, "owner" 
from 'exp/**/fn_decl.json';

create temporary table tmp_type_decl_ph1 as
select * from 'exp/**/type_decl.json';

create or replace temporary table tmp_type_decl_ph2 as
select 
    row_number() over(order by lookup_key, parent_key nulls first) as id,
    v.*
from (
    select distinct on (v1.lookup_key, parent_key) v1.*
    from (
        select lookup_key, decl.symbol, decl.module_symbol, decl.crate_symbol, 'nominal'::type_category as category, null::varchar as parent_key
        from tmp_type_decl_ph1
        /* slice type member */
        union all
        select 
            member.lookup_key, member.symbol, member.module_symbol, member.crate_symbol, 'slice', lookup_key
        from (
            select lookup_key, unnest(decl.slice_member) as member 
            from tmp_type_decl_ph1
            where decl.slice_member is not null
        )
        /* tuple members */
        union all
        select 
            member.lookup_key, member.symbol, member.module_symbol, member.crate_symbol, 'tuple', lookup_key
        from (
            select lookup_key, unnest(decl.tuple_members) as member 
            from tmp_type_decl_ph1
            where decl.tuple_members is not null
        )    
    ) v1
) v;

create or replace temporary table tmp_import_map as
select * from 'exp/**/import_map.json';

/* ---------- */

insert into prototype (id, symbol, qual_symbol)
select id, proto, proto_qual from tmp_fn_decl;

insert into module_symbol (id, symbol)
select
    row_number() over(order by module_symbol) as id,
    module_symbol
from (
    select distinct module_symbol 
    from tmp_type_decl_ph2
    where module_symbol is not null
);

insert into crate_symbol (id, symbol)
select
    row_number() over(order by crate_symbol) as id,
    crate_symbol
from (
    select distinct crate_symbol 
    from tmp_type_decl_ph2
    where crate_symbol is not null
);

insert into type_symbol (id, symbol)
select id, symbol from tmp_type_decl_ph2;

insert into type_module_ref (module_id, type_id)
select t1.id, t2.id
from module_symbol t1
join tmp_type_decl_ph2 t2 on t1.symbol = t2.module_symbol;

insert into type_crate_ref (crate_id, type_id)
select t1.id, t2.id
from crate_symbol t1
join tmp_type_decl_ph2 t2 on t1.symbol = t2.crate_symbol;

insert into prototype_owner_ref (prototype_id, owner_type_id, generic_args)
select t1.id, t2.id, coalesce(t1.owner.generic_args, [])
from tmp_fn_decl t1
join tmp_type_decl_ph2 t2 on t1.owner.lookup_key = t2.lookup_key;

insert into prototype_arg_order_ref (prototype_id, type_ids, category, sort_order)
select t1.id, list({'id': t2.id, 'item_index': t2.item_index}), any_value(t2.category), t1.arg.sort_order
from (select id, unnest(args) as arg from tmp_fn_decl) t1
join lateral (
    /* nominal */
    select ts2.id, 0 as item_index, ts2.category, ts2.lookup_key
    from (
        select lookup_key
        from tmp_type_decl_ph1 
        where
            decl.slice_member is null 
            and decl.tuple_members is null
    ) ts1
    join tmp_type_decl_ph2 ts2 on ts1.lookup_key = ts2.lookup_key
    where 
        ts1.lookup_key = t1.arg.lookup_key
        and ts2.category = 'nominal'::type_category
    /* slice */
    union all    select ts2.id, 0 as item_index, ts2.category, ts2.lookup_key
    from (
        select 
            lookup_key as parent_key,
            unnest(decl.slice_member) as member
        from tmp_type_decl_ph1
        where decl.slice_member is not null 
    ) ts1
    join tmp_type_decl_ph2 ts2 
        on ts1.member.lookup_key = ts2.lookup_key
        and ts1.parent_key = ts2.parent_key   
    where 
        ts2.parent_key = t1.arg.lookup_key
        and ts2.category = 'slice'::type_category
    /* tuple */
    union all
    select ts2.id, ts1.item_index, ts2.category, ts2.lookup_key
    from (
        select 
            lookup_key as parent_key,
            apply(decl.tuple_members, (x, i) -> {'lookup_key': x.lookup_key, 'item_index': i}).unnest(recursive := true)
        from tmp_type_decl_ph1 
        where 
            lookup_key = t1.arg.lookup_key
            and decl.tuple_members is not null 
    ) ts1
    join tmp_type_decl_ph2 ts2 
        on ts1.lookup_key = ts2.lookup_key
        and ts1.parent_key = ts2.parent_key
    where ts2.category = 'tuple'::type_category
) t2 on true
group by t1.id, t1.arg.sort_order;

insert into prototype_return_ref (prototype_id, type_ids, category)
select t1.id, list({'id': t2.id, 'item_index': t2.item_index}), any_value(t2.category)
from tmp_fn_decl t1
join lateral (
    /* nominal */
    select ts2.id, 0 as item_index, ts2.category, ts2.lookup_key
    from (
        select lookup_key
        from tmp_type_decl_ph1 
        where
            decl.slice_member is null 
            and decl.tuple_members is null
    ) ts1
    join tmp_type_decl_ph2 ts2 on ts1.lookup_key = ts2.lookup_key
    where 
        ts1.lookup_key = t1.ret_decl.lookup_key
        and ts2.category = 'nominal'::type_category
    /* slice */
    union all
    select ts2.id, 0 as item_index, ts2.category, ts2.lookup_key
    from (
        select 
            lookup_key as parent_key,
            unnest(decl.slice_member) as member
        from tmp_type_decl_ph1
        where decl.slice_member is not null 
    ) ts1
    join tmp_type_decl_ph2 ts2 
        on ts1.member.lookup_key = ts2.lookup_key
        and ts1.parent_key = ts2.parent_key   
    where 
        ts2.parent_key = t1.ret_decl.lookup_key
        and ts2.category = 'slice'::type_category
    /* tuple */
    union all
    select ts2.id, ts1.item_index, ts2.category, ts2.lookup_key
    from (
        select 
            lookup_key as parent_key,
            apply(decl.tuple_members, (x, i) -> {'lookup_key': x.lookup_key, 'item_index': i}).unnest(recursive := true)
        from tmp_type_decl_ph1 
        where 
            lookup_key = t1.ret_decl.lookup_key
            and decl.tuple_members is not null 
    ) ts1
    join tmp_type_decl_ph2 ts2 
        on ts1.lookup_key = ts2.lookup_key
        and ts1.parent_key = ts2.parent_key
    where ts2.category = 'tuple'::type_category
) t2 on true
group by t1.id;

insert into prototype_type_ref (prototype_id, type_id, kind, category)
/* args */
select distinct t1.id, v.*
from (select id, unnest(args) as arg from tmp_fn_decl) t1
join lateral (
    /* args (nominal) */
    select ts1.id, 'arg'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.arg.lookup_key
        and ts1.category = 'nominal'::type_category
    /* args (slice) */
    union all
    select ts1.id, 'arg'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.arg.lookup_key
        and ts1.category = 'slice'::type_category
    /* args (tuple) */
    union all
    select ts1.id, 'arg'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.arg.lookup_key
        and ts1.category = 'tuple'::type_category
) v on true
/* return */
union all
select t1.id, v.*
from (select id, ret_decl as ret from tmp_fn_decl) t1
join lateral (
    /* return (nominal) */
    select 
        ts1.id, 'return'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.ret.lookup_key
        and ts1.category = 'nominal'::type_category
    /* return (slice) */
    union all
    select ts1.id, 'return'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.ret.lookup_key
        and ts1.category = 'slice'::type_category
    /* return (tuple) */
    union all
    select ts1.id, 'return'::type_kind, ts1.category
    from tmp_type_decl_ph2 ts1
    where 
        ts1.lookup_key = t1.ret.lookup_key
        and ts1.category = 'tuple'::type_category
) v on true;

insert into type_import_map (from_type_id, to_type_id)
select v1.id, v2.id
from tmp_import_map t1
join lateral (
    select ts1.id
    from tmp_type_decl_ph2 ts1
    where ts1.lookup_key = t1.from_type
) v1 on true
join lateral (
    select ts1.id
    from tmp_type_decl_ph2 ts1
    where ts1.lookup_key = t1.to_type
) v2 on true;

insert into deprecated (id, since)
select 
    row_number() over (order by since) as id, 
    v.*
from (
    select distinct deprecated as since from tmp_fn_decl
    where deprecated is not null
) v;

insert into deprecated_prototype_ref (prototype_id, deprecated_id)
select 
    v1.id, v2.id
from (
    select id, deprecated as since from tmp_fn_decl
    where deprecated is not null
) v1
join lateral (
    select id
    from deprecated ts1
    where ts1.since = v1.since
    limit 1
) v2 on true;

/* ---------- */

drop table tmp_fn_decl;
drop table tmp_import_map;
drop table tmp_type_decl_ph1;
drop table tmp_type_decl_ph2;

export database 'dump-rt' (format json, array true);
