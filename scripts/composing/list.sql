with 
    ph as materialized (
        select 
            3::bigint as crate_id, 
            null::varchar as arg_phrase,
            null::type_category as arg_cat1,
            null::type_category as arg_cat2,
            'Result'::varchar as return_phrase,
            null::type_category as ret_cat1,
            null::type_category as ret_cat2,
    ),
    mapped_type as not materialized (
        select 
            v.*, t3.symbol as module_symbol, t4.symbol as crate_symbol,
        from (
            select 
                t1.id as type_id,
                coalesce(vs1.id, t1.id) as mapped_type_id,
                coalesce(vs1.symbol, t1.symbol) as symbol, 
                coalesce(vs1.module_id, t2.module_id) as module_id, 
                coalesce(vs1.crate_id, t3.crate_id) as crate_id,
                coalesce(vs1.priority, 1) as priority,
            from type_symbol t1
            cross join ph
            left outer join type_module_ref t2 on t1.id = t2.type_id
            left outer join type_crate_ref t3 on t1.id = t3.type_id
            left outer join lateral (
                select ts2.id, ts2.symbol, ts3.module_id, ts4.crate_id, 0 as priority
                from type_import_map ts1
                cross join ph
                join type_symbol ts2 on ts1.to_type_id = ts2.id
                left outer join type_module_ref ts3 on ts3.type_id = ts2.id
                left outer join type_crate_ref ts4 on ts4.type_id = ts2.id
                where 
                    ts1.from_type_id = t1.id
                    and (ts4.crate_id = ph.crate_id) is not false
            ) vs1 on true
        ) v
        left outer join module_symbol t3 on v.module_id = t3.id
        left outer join crate_symbol t4 on v.crate_id = t4.id
    )
select 
    t1.id, t1.qual_symbol, "owner", "args", "returns", since,
from prototype t1
cross join ph
/* filter by phrase (future removing) */
join lateral (
    select list(t3.symbol) as filter_args
    from prototype_type_ref t2
    join type_symbol t3 on t2.type_id = t3.id
    where
        t1.id = t2.prototype_id
        and exists (
            from prototype_type_ref ts1
            join type_symbol ts2 on ts1.type_id = ts2.id
            where 
                ts1.prototype_id = t1.id
                and ts1.kind = 'return'::type_kind
                and (ts2.symbol = ph.return_phrase) is not false
                and ts1.category = any (select unnest(['nominal', ph.ret_cat1, ph.ret_cat2]))
        )
        and exists (
            from prototype_type_ref ts1
            join type_symbol ts2 on ts1.type_id = ts2.id
            where 
                ts1.prototype_id = t1.id
                and ts1.kind = 'arg'::type_kind
                and (ts2.symbol = ph.arg_phrase) is not false
                and ts1.category = any (select unnest(['nominal', ph.arg_cat1, ph.arg_cat2]))
        )
        and t2.kind = 'arg'::type_kind
    group by t2.prototype_id
) on true
/* join owner type info */
join lateral (
	select 
        {
            'symbol': max_by(ts2.symbol, ts2.priority), 
            'module_symbol': max_by(ts2.module_symbol, ts2.priority), 
            'crate_symbol': max_by(ts2.crate_symbol, ts2.priority),
            'generic_args': max_by(ts1.generic_args, ts2.priority),
        } as "owner"        
    from prototype_owner_ref ts1
    join mapped_type ts2 on ts1.owner_type_id = ts2.type_id
    where 
        ts1.prototype_id = t1.id
        and (ts2.crate_id = ph.crate_id) is not false
) on true
/* join return type info */
left join lateral ( --859
    select { 'items': list("item"), 'category': any_value(category) } as "returns" from (
        select 
            {
                'index': ts1.member.item_index,
                'symbol': max_by(ts2.symbol, ts2.priority), 
                'module_symbol': max_by(ts2.module_symbol, ts2.priority),
                'crate_symbol': max_by(ts2.crate_symbol, ts2.priority), 
            } as item,
            any_value(ts1.category) as category,
        from (
            select prototype_id, unnest(type_ids) as member, category 
            from prototype_return_ref
        ) ts1
        join mapped_type ts2 on ts1.member.id = ts2.type_id
        where ts1.prototype_id = t1.id
        group by ts1.member.item_index
    ) vs1
) on true
/* join arg type info */
left outer join lateral (
    select list(arg) as args 
    from (
        select {'items': list(vs1.item), 'category': any_value(vs1.category), 'sort_order': vs1.sort_order} as arg 
        from (
            select 
                {
                    'index': ts1.member.item_index,
                    'symbol': max_by(ts2.symbol, ts2.priority), 
                    'module_symbol': max_by(ts2.module_symbol, ts2.priority), 
                    'crate_symbol': max_by(ts2.crate_symbol, ts2.priority), 
                } as item,
                any_value(ts1.category) as category,
                ts1.sort_order,
            from (
                select prototype_id, unnest(type_ids) as member, category, sort_order 
                from prototype_arg_order_ref
            ) ts1 
            join mapped_type ts2 on ts1.member.id = ts2.type_id
            where ts1.prototype_id = t1.id
            group by ts1.sort_order, ts1.member.item_index
        ) vs1
        group by vs1.sort_order
    )
) on true
/* join deprecation */
left outer join lateral (
    select ts2.since
    from deprecated_prototype_ref ts1
    join deprecated ts2 on ts1.deprecated_id = ts2.id
    where ts1.prototype_id = t1.id
) on true
order by owner.crate_symbol, owner.module_symbol, owner.symbol, t1.symbol;