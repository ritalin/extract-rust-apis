create type type_kind as enum ('arg', 'return', 'owner');
create type type_category as enum ('nominal', 'slice', 'tuple');

create sequence crate_symbol_seq;
create sequence type_symbol_seq;
create sequence prototype_seq;
create sequence deprecated_seq;

create table type_symbol (
    id bigint not null primary key,
    symbol varchar not null,
);

create or replace table crate_symbol (
    id bigint not null primary key default nextval('crate_symbol_seq'),
    symbol varchar not null,
);

create table prototype (
    id bigint not null primary key,
    symbol varchar not null,
    qual_symbol varchar not null,
    brief_symbol varchar not null
);
create table prototype_crate_ref (
    crate_id bigint not null,
    prototype_id bigint not null,
    primary key (crate_id, prototype_id)
);

create or replace table prototype_type_ref (
    prototype_id BIGINT, 
    type_id BIGINT, 
    kind type_kind, 
    category type_category,
    PRIMARY KEY(prototype_id, type_id, kind, category)
);

CREATE or replace TABLE deprecated(
    id BIGINT PRIMARY KEY default nextval('deprecated_seq'), 
    since VARCHAR NOT NULL
);
CREATE TABLE prototype_deprecated_ref(
    prototype_id BIGINT, 
    deprecated_id BIGINT, 
    PRIMARY KEY(prototype_id, deprecated_id)
);
