create or replace temporary table tmp_fn_decl_members_ph1 (
    id bigint not null,
    kind type_kind not null,
    member_order int not null,
    member_category type_category not null,
    member_item_order int not null,
    member_item_type_id bigint not null,
    member_item_lookup_key varchar not null,
    member_item_symbol varchar not null,
    member_item_module varchar null, 
    member_item_crate varchar null,
    member_item_generic_args varchar[] null,
    primary key (id, kind, member_order, member_category, member_item_order),
);

create or replace temporary table tmp_fn_decl_ph1 as 
select
    nextval('prototype_seq') as id,
    t1.owner.lookup_key as owner_lookup_key,
    t1.owner.generic_args as owner_generic_args,
    t1.proto_qual as fn_lookup_key,
    t1.proto,
from tmp_fn_decl t1;

insert into tmp_fn_decl_members_ph1 (
    id, kind, member_order, member_category, 
    member_item_order, member_item_type_id, 
    member_item_lookup_key, member_item_symbol, member_item_module, member_item_crate,
    member_item_generic_args
)
select 
    t1.id, t2.kind, t2.member_order, 
    t3.member_category,
    t3.member_item_order,
    t4.id,
    t3.member_item_lookup_key,
    t3.member_item_symbol,
    t3.member_item_module,
    t3.member_item_crate,
    t3.member_item_generic_args,
from tmp_fn_decl_ph1 t1
join lateral (
    /* owner */
    select
        'owner'::type_kind as kind,
        1 as member_order,
        ts1.owner.lookup_key,
        ts1.owner.generic_args,
    from tmp_fn_decl ts1
    where
        ts1.owner.lookup_key = t1.owner_lookup_key
        and ts1.owner.generic_args = t1.owner_generic_args
        and ts1.proto_qual = t1.fn_lookup_key
    /* return */
    union all
    select
        'return'::type_kind as kind,
        1 as member_order,
        ts1.ret_decl.lookup_key,
        ts1.ret_decl.generic_args,
    from tmp_fn_decl ts1
    where
        ts1.owner.lookup_key = t1.owner_lookup_key
        and ts1.owner.generic_args = t1.owner_generic_args
        and ts1.proto_qual = t1.fn_lookup_key
    /* args */
    union all
    select
        'arg'::type_kind as kind,
        ts1.arg_decl.sort_order, 
        ts1.arg_decl.lookup_key,
        ts1.arg_decl.generic_args,
    from (
        select unnest(ts1.args) as arg_decl,
        from tmp_fn_decl ts1
        where
            ts1.owner.lookup_key = t1.owner_lookup_key
            and ts1.owner.generic_args = t1.owner_generic_args
            and ts1.proto_qual = t1.fn_lookup_key
    ) ts1
) t2 on true
join lateral (
    /* nominal */
    select
        'nominal'::type_category as member_category,
        1 as member_item_order, 
        ts1.lookup_key as member_item_lookup_key,
        ts1.decl.symbol as member_item_symbol,
        ts1.decl.module_symbol as member_item_module,
        ts1.decl.crate_symbol as member_item_crate,
        t2.generic_args as member_item_generic_args,
    from tmp_type_decl_ph1 ts1
    where 
        ts1.lookup_key = t2.lookup_key
        and ts1.decl.alias is null
        and ts1.decl.slice_member is null
        and coalesce(ts1.decl.tuple_members, []) = []
    /* slice */
    union all
    select
        'slice'::type_category, 
        1, 
        ts1.member.lookup_key,
        ts1.member.symbol, ts1.member.module_symbol, ts1.member.crate_symbol, []
    from (
        select unnest(ts1.decl.slice_member) as member
        from tmp_type_decl_ph1 ts1
        where 
            ts1.lookup_key = t2.lookup_key
            and ts1.decl.slice_member is not null
    ) ts1
    /* tuple */
    union all
    select
        'tuple'::type_category, 
        ts1.member.index, 
        ts1.member.item.lookup_key,
        ts1.member.item.symbol, ts1.member.item.module_symbol, ts1.member.item.crate_symbol, []
    from (
        select ts1.tuple_members.apply((x, i) -> {'item': x, 'index': i}).unnest() as member
        from (
            select ts1.decl.tuple_members from tmp_type_decl_ph1 ts1
            where 
                ts1.lookup_key = t2.lookup_key
                and coalesce(ts1.decl.tuple_members, []) <> []
        ) ts1
    ) ts1
) t3 on true
join lateral (
    select ts1.id
    from tmp_type_decl_ph2 ts1
    where ts1.lookup_key = t3.member_item_lookup_key
) t4 on true;

create or replace temporary table tmp_crate as 
select distinct decl.crate_symbol
from tmp_type_decl_ph1
where decl.crate_symbol is not null;

create or replace temporary table tmp_fn_decl_ph2 as 
select
    nextval('prototype_seq') as id,
    t2.crate_symbol,
    t1.id as from_id,
    t1.proto,
from tmp_fn_decl_ph1 t1
cross join tmp_crate t2
where exists (
    from tmp_fn_decl_members_ph1 ts1
    join tmp_import_map ts2 on ts1.member_item_lookup_key = ts2.from_type
    where 
        ts1.id = t1.id
        and ts2.crate_symbol = t2.crate_symbol
);

insert into tmp_fn_decl_members_ph1 (
    id, kind, member_order, member_category, 
    member_item_order, member_item_type_id,
    member_item_lookup_key, member_item_symbol, member_item_module, member_item_crate, member_item_generic_args
)
select
    t1.id, t3.kind, t3.member_order, 
    t3.member_category,
    t3.member_item_order,
    coalesce(t4.member_item_type_id, t3.member_item_type_id),
    coalesce(t4.member_item_lookup_key, t3.member_item_lookup_key),
    coalesce(t4.member_item_symbol, t3.member_item_symbol),
    coalesce(t4.member_item_module, t3.member_item_module),
    coalesce(t4.member_item_crate, t3.member_item_crate),
    t3.member_item_generic_args,
from tmp_fn_decl_ph2 t1
join tmp_fn_decl_ph1 t2 on t1.from_id = t2.id
join tmp_fn_decl_members_ph1 t3 on t2.id = t3.id
left outer join lateral (
    select
        coalesce(ts2.id, -1) as member_item_type_id,
        coalesce(ts2.lookup_key, '????') as member_item_lookup_key,
        coalesce(ts2.symbol, '????') as member_item_symbol,
        coalesce(ts2.module_symbol, '????') as member_item_module,
        coalesce(ts2.crate_symbol, '????') as member_item_crate,
    from tmp_import_map ts1
    left outer join tmp_type_decl_ph2 ts2 on ts1.to_type = ts2.lookup_key
    where 
        ts1.from_type = t3.member_item_lookup_key
        and ts1.crate_symbol = t1.crate_symbol
) t4 on true;

create or replace temporary table tmp_fn_decl_members_ph2 as
with member_fmt as not materialized (
    select
        t1.id, t1.kind, t1.member_category, t1.member_order, 
        t1.member_category, t1.member_item_order, 
        t1.member_item_symbol,
        format('{}{}{}{}',
            coalesce(t1.member_item_crate || '::', ''),
            coalesce(t1.member_item_module || '::', ''),
            coalesce((
                select format('<{}>::', string_agg(ts1.arg.item, ', ' order by ts1.arg.index))
                from (
                    select ts1.member_item_generic_args.apply((x, i) -> {'item': x, 'index': i}).unnest() as arg
                    from tmp_fn_decl_members_ph1 ts1
                    where 
                        ts1.id = t1.id
                        and ts1.kind = t1.kind
                        and ts1.member_order = t1.member_order
                        and ts1.member_item_order = t1.member_item_order
                        and ts1.member_item_generic_args <> []
                ) ts1
            ), ''),
            t1.member_item_symbol
        ) as member_item_qual_symbol,
    from tmp_fn_decl_members_ph1 t1
)
select v.* 
from (
    /* nominal */
    select
        ts1.id, ts1.kind, ts1.member_order, 
        any_value(ts1.member_item_symbol) as member_symbol,
        any_value(ts1.member_item_qual_symbol) as member_qual_symbol,
    from member_fmt ts1
    where ts1.member_category = 'nominal'::type_category
    group by ts1.id, ts1.kind, ts1.member_order
    /* slice */
    union all
    select
        ts1.id, ts1.kind, ts1.member_order, 
        format('[{}]', any_value(ts1.member_item_symbol)),
        format('[{}]', any_value(ts1.member_item_qual_symbol)),
    from member_fmt ts1
    where ts1.member_category = 'slice'::type_category
    group by ts1.id, ts1.kind, ts1.member_order
    /* tuple */
    union all
    select
        ts1.id, ts1.kind, ts1.member_order,
        format('({})', string_agg(ts1.member_item_symbol, ', ' order by ts1.member_item_order)),
        format('({})', string_agg(ts1.member_item_qual_symbol, ', ' order by ts1.member_item_order)),
    from member_fmt ts1
    where ts1.member_category = 'tuple'::type_category
    group by ts1.id, ts1.kind, ts1.member_order
) v;
