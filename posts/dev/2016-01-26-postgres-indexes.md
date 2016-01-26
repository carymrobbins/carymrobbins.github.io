---
title: PostgreSQL Indexes
---

Many times, while we are creating our schemas, we preemptively add indexes to ensure
our queries will be blazingly fast before we even start writing them.  But come to find
out, not only does this rarely work, but you end up with false expectations regarding
performance.

Let's start with the following schema -

```sqlpostgresql
create table account(
  id serial primary key
);

create table customer(
  id serial primary key,
  account_id serial references account(id)
);

create table purchase(
  id serial primary key,
  account_id serial references account(id),
  customer_id serial references customer(id),
  amount numeric(10, 2),
  time timestamptz,
  favorite bool not null
);
```

For this exercise, I've created 3 accounts, 10,000 customers, and 10,000,000 purchases.

It's _obvious_ that we'd want to add indexes to our foreign key fields, so let's go ahead and do that.

```sqlpostgresql
create index ix_customer_account on customer(account_id);
create index ix_purchase_account on purchase(account_id);
create index ix_purchase_customer on purchase(customer_id);
```

_This is actually **not** a good idea.  While we very well may end up needing indexes like these,
we should always look at the query plan and confirm that the index is justified before adding it._

## Optimizing `count(*)`

Now let's say that we need to provide the count of favorite purchases for an account.
Let's check the query plan to see how this does.

```sqlpostgresql
explain analyze
  select count(*)
  from purchase
  where account_id = 1 and favorite;
```

```text
Aggregate  (cost=188196.41..188196.42 rows=1 width=0) (actual time=1109.149..1109.150 rows=1 loops=1)
  ->  Bitmap Heap Scan on purchase  (cost=62066.98..187350.98 rows=338173 width=0) (actual time=235.196..1093.048 rows=332813 loops=1)
        Recheck Cond: (account_id = 1)
        Rows Removed by Index Recheck: 4219293
        Filter: favorite
        Rows Removed by Filter: 3001198
        Heap Blocks: exact=30596 lossy=52738
        ->  Bitmap Index Scan on ix_purchase_account  (cost=0.00..61982.43 rows=3356000 width=0) (actual time=229.033..229.033 rows=3334011 loops=1)
              Index Cond: (account_id = 1)
Planning time: 0.051 ms
Execution time: 1109.173 ms
```

_Note: If you have trouble reading these, you can paste them into
[explain.depesz.com](http://explain.depesz.com/) which will give you a nicer format to work with._

These results aren't exactly great.  The **Bitmap Heap Scan** here shows us that we're having
to scan through more rows than we'd ought to.  In fact, if we create our indexes appropriately,
we can actually get the results we want directly from the index!

```sqlpostgresql
create index ix_purchase_account_favorite on purchase(account_id) where favorite;
```

Here we are using `where` to specify a [partial index](http://www.postgresql.org/docs/current/static/indexes-partial.html).
This can be useful so that your indexes remain smaller, reducing the size impact to the database.
This also gives Postgres additional information so it knows when it can use the index.

Ok, now let's plan our query again -

```text
Aggregate  (cost=94743.86..94743.87 rows=1 width=0) (actual time=886.781..886.781 rows=1 loops=1)
  ->  Bitmap Heap Scan on purchase  (cost=6337.27..93898.43 rows=338173 width=0) (actual time=44.876..868.883 rows=332813 loops=1)
        Recheck Cond: ((account_id = 1) AND favorite)
        Rows Removed by Index Recheck: 6114519
        Heap Blocks: exact=29172 lossy=52743
        ->  Bitmap Index Scan on ix_purchase_account_favorite  (cost=0.00..6252.72 rows=338173 width=0) (actual time=39.835..39.835 rows=332813 loops=1)
              Index Cond: (account_id = 1)
Planning time: 0.060 ms
Execution time: 886.807 ms
```

Wait, what?  Performance is a little better, but it's still doing a bitmap scan.  What we really
want is an **Index Only Scan**.  Come to find out, [Postgres can't always figure this out when
using partial indexes](http://www.postgresql.org/message-id/79C7D74D-59B0-4D97-A5E5-55553EF299AA@justatheory.com),
so we can give it more information by including the `favorite`
column -

```sqlpostgresql
drop index ix_purchase_account_favorite;
create index ix_purchase_account_favorite on purchase(account_id, favorite) where favorite;
```

Let's try our query again -

```text
Aggregate  (cost=1838.89..1838.90 rows=1 width=0) (actual time=47.028..47.028 rows=1 loops=1)
  ->  Index Only Scan using ix_purchase_account_favorite on purchase  (cost=0.42..1014.35 rows=329817 width=0) (actual time=0.023..31.922 rows=332813 loops=1)
        Index Cond: ((account_id = 1) AND (favorite = true))
        Heap Fetches: 0
Planning time: 0.063 ms
Execution time: 47.048 ms
```

Good!  Now that we are getting an index only scan, we can pretty much stop at this point.
This is generally the best you can do when optimizing a `count(*)` query.  Postgres is able
to look directly at the index instead of scanning any rows in the table.

## Optimizing `order by` with `limit`

Now we are tasked with getting the most recent, favorite purchase.

```sqlpostgresql
explain analyze
  select id, time
  from purchase
  where account_id = 1 and favorite
  order by time desc
  limit 1
```

```text
Limit  (cost=65199.64..65199.64 rows=1 width=12) (actual time=845.771..845.771 rows=1 loops=1)
  ->  Sort  (cost=65199.64..66024.18 rows=329817 width=12) (actual time=845.770..845.770 rows=1 loops=1)
        Sort Key: "time"
        Sort Method: top-N heapsort  Memory: 25kB
        ->  Bitmap Heap Scan on purchase  (cost=769.84..63550.56 rows=329817 width=12) (actual time=51.306..815.209 rows=332813 loops=1)
              Recheck Cond: (account_id = 1)
              Rows Removed by Index Recheck: 4216921
              Filter: favorite
              Rows Removed by Filter: 1897598
              Heap Blocks: exact=29172 lossy=52743
              ->  Bitmap Index Scan on ix_purchase_account_favorite  (cost=0.00..687.38 rows=32696 width=0) (actual time=43.954..43.954 rows=332813 loops=1)
                    Index Cond: ((account_id = 1) AND (favorite = true))
Planning time: 0.120 ms
Execution time: 845.798 ms
```

Interesting, it's using our new index, but it has to resort to a bitmap scan so it can
later sort the results.  Alternatively, we can provide a similar index with `time desc`
so we can optimize for the sort -

```sqlpostgresql
create index ix_purchase_latest_favorite on purchase(account_id, time desc)
  where favorite;
```

Now Postgres is able to use an **Index Scan** to fetch the result -

```text
Limit  (cost=0.42..1.41 rows=1 width=12) (actual time=0.018..0.019 rows=1 loops=1)
  ->  Index Scan using ix_purchase_latest_favorite on purchase  (cost=0.42..324659.07 rows=328677 width=12) (actual time=0.018..0.018 rows=1 loops=1)
        Index Cond: (account_id = 1)
Planning time: 0.080 ms
Execution time: 0.029 ms
```

Note that `limit 1` isn't necessarily required here; however, if you are fetching a lot of rows,
it's likely that Postgres will resort to a bitmap heap scan instead of an index scan.

## Gotchas

When adding indexes you may not see the plans you expect immediately.  Be sure to always
execute the query a few times to ensure statistics are updated.  You may even need to
run `vacuum analyze` on the table to rebuild statistics (although, don't do `vacuum full analyze`
unless you know that's what you need as this usually destroys statistics used for things like
index only scans).

So far, our original indexes aren't being used at all.  Let's see how big they are -

```sqlpostgresql
select ix, pg_size_pretty(pg_relation_size(ix))
from (
  values ('ix_customer_account'),('ix_purchase_account'),('ix_purchase_customer')
) as _(ix);
```

```text
          ix          | pg_size_pretty 
----------------------+----------------
 ix_customer_account  | 2208 kB
 ix_purchase_account  | 214 MB
 ix_purchase_customer | 214 MB
```

That's nearly half a GB for our test data.  This is pretty wasteful, so considering that production
systems contain a _lot_ more data than our toy, you can really save some space by not creating
wasteful indexes.

```sqlpostgresql
drop index ix_customer_account;
drop index ix_purchase_account;
drop index ix_purchase_customer;
```

In the future, let's not add indexes until we prove via our query plans that they are actually
beneficial.
