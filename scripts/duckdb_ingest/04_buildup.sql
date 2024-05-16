insert into prototype (id, symbol, qual_symbol, brief_symbol)
select 
    t1.id, t1.proto,
    format('{}::{}({}){}', 
        t2.member_qual_symbol, t1.proto, 
        coalesce(t4.member_qual_symbol, ''),
        coalesce(format(' -> {}', t3.member_qual_symbol), '')
    ) as qual_symbol,
    format('{}::{}({}){}', 
        t2.member_symbol, t1.proto, 
        coalesce(t4.member_symbol, ''),
        coalesce(format(' -> {}', t3.member_symbol), '')
    ) as brief_symbol
from (
    select id, proto from tmp_fn_decl_ph1
    union all
    select id, proto from tmp_fn_decl_ph2
) t1
join lateral (
    /* owner */
    select ts1.member_symbol, ts1.member_qual_symbol
    from tmp_fn_decl_members_ph2 ts1
    where
        ts1.id = t1.id
        and ts1.kind = 'owner'
) t2 on true
left outer join lateral (
    /* return */
    select ts1.member_symbol, ts1.member_qual_symbol
    from tmp_fn_decl_members_ph2 ts1
    where
        ts1.id = t1.id
        and ts1.kind = 'return'
) t3 on true
left outer join lateral (
    /* args */
    select 
        string_agg(ts1.member_symbol, ', ' order by ts1.member_order) as member_symbol,
        string_agg(ts1.member_qual_symbol, ', ' order by ts1.member_order) as member_qual_symbol,
    from tmp_fn_decl_members_ph2 ts1
    where
        ts1.id = t1.id
        and ts1.kind = 'arg'
) t4 on true;

insert into crate_symbol (id, symbol)
select nextval('crate_symbol_seq'), t1.*
from (
    select distinct t1.member_item_crate
    from tmp_fn_decl_members_ph1 t1
    where 
        t1.kind = 'owner'
        and t1.member_item_crate is not null
    order by t1.member_item_crate
) t1;

insert into type_symbol (id, symbol)
select distinct on (t1.member_item_symbol) t1.member_item_type_id, t1.member_item_symbol,
from tmp_fn_decl_members_ph1 t1
where t1.kind in ('arg', 'return');

insert into prototype_crate_ref (prototype_id, crate_id)
select t1.id, t2.id
from tmp_fn_decl_members_ph1 t1
join crate_symbol t2 on t1.member_item_crate = t2.symbol
where t1.kind = 'owner';

insert into prototype_type_ref (prototype_id, type_id, kind, category)
select distinct t1.id, t2.id, t1.kind, t1.member_category,
from tmp_fn_decl_members_ph1 t1
join type_symbol t2 on t1.member_item_symbol = t2.symbol
where t1.kind in ('arg', 'return');

insert into deprecated (since)
select distinct t1.deprecated as since from tmp_fn_decl t1
where t1.deprecated is not null
order by t1.deprecated;

insert into prototype_deprecated_ref (prototype_id, deprecated_id)
select v1.id, v2.id
from (
    select t1.id, t1.owner_lookup_key, t1.fn_lookup_key
    from tmp_fn_decl_ph1 t1
    union all
    select t1.id, t2.owner_lookup_key, t2.fn_lookup_key
    from tmp_fn_decl_ph2 t1
    join tmp_fn_decl_ph1 t2 on t1.from_id = t2.id
) v1
join lateral (
    select ts2.id
    from tmp_fn_decl ts1
    join deprecated ts2 on ts1.deprecated = ts2.since
    where 
        ts1.owner.lookup_key = v1.owner_lookup_key
        and ts1.proto_qual = v1.fn_lookup_key
) v2 on true;