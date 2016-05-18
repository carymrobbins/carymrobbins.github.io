---
title: Safe Scripting with PostgreSQL
---

If you've ever worked with databases professionally, you should be well aware
of [SQL injection](https://en.wikipedia.org/wiki/SQL_injection).  When writing
software, your driver should provide facilities for creating
[prepared statements](https://en.wikipedia.org/wiki/Prepared_statement) so you
can safely pass user-supplied parameters without it wreaking 
havoc on your database.

<a href="https://xkcd.com/327/">
  <img
    alt="Her daughter is named Help I'm trapped in a driver's license factory."
    src="http://imgs.xkcd.com/comics/exploits_of_a_mom.png"
   />
</a>

However, sometimes you may have a task so trivial it could just be written in Bash.
PostgreSQL comes with the [psql](http://www.postgresql.org/docs/current/static/app-psql.html)
command-line tool, so we can just run our queries using that.
Your first thought may be to do something like this -

```bash
psql -c "select relkind from pg_class where relname = '$1'"
```

Note however that if the argument contained the value `'; drop table foo; '` you'd end up
with a dropped table!  Generally, your scripts may not be vulnerable to this, but when they
could be, it's best to take some precautions.

Luckily, `psql` can handle sanitizing input variables by using the `-v` flag.
From `man psql` -

```
-v assignment
--set=assignment
--variable=assignment
    Perform a variable assignment, like the \set meta-command. Note that you must separate name and
    value, if any, by an equal sign on the command line. To unset a variable leave off the equal sign.
    To set a variable with an empty value, use the equal sign but leave off the value. These
    assignments are done during a very early stage of start-up, so variables reserved for internal
    purposes might get overwritten later.
```

Here's a sanitized version of our original implementation -

```bash
psql -v name="$1" <<< "select relkind from pg_class where relname = :'name'"
```

There are a couple things to note regarding using variables -

* You **cannot** use the `-c` flag to pass in the SQL string.  Instead, redirect the string using `<<<`.
* While you _can_ use the `:var` syntax, you should generally prefer the `:'var'` syntax.
    This way variables are always quoted.  `psql` can infer the appropriate type of `'text'`
    values based on context, so this even works for numeric values.

If your SQL is a bit longer, you could store it in a separate file and just redirect it from there.

`foo.sql`
```sqlpostgresql
select relkind
from pg_class
where relname = :'name'
```

`run-query`
```bash
#!/bin/bash
sql_file="$(dirname $0)/foo.sql"
if [ ! -f "$sql_file" ]; then
  >&2 echo "SQL file not found: $sql_file"
  exit 1
fi
psql -X -v name="$1" < "$sql_file"
```

Also note that I'm providing the `-X` flag to avoid reading the user's `.psqlrc` file.
From `man psql` -

```
-X,
--no-psqlrc
    Do not read the start-up file (neither the system-wide psqlrc file nor the user's ~/.psqlrc file).
```

Now we can safely pass any values to your script without fear.

```
$ chmod +x run-query
$ ./run-query pg_type
 relkind 
---------
 r
(1 row)
$ ./run-query "'; drop table foo; '"
 relkind 
---------
(0 rows)
```

One caveat, variables don't seem to work with the `\copy` command -

```
$ psql -X -v a=1 <<< "\\copy ( select :'a' + 2 as foo ) to stdout with csv header"
ERROR:  syntax error at or near ":"
LINE 1: COPY  ( select : 'a' + 2 ) TO STDOUT with csv header
                       ^
```

They _do_ however work with the standard `copy` command

```
$ psql -X -v a=1 <<< "copy ( select :'a' + 2 as foo ) to stdout with csv header"
foo
3
```
