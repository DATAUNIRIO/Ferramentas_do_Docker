## sql_build(op, con, ...)
`sql_build` creates a `select_query` S3 object, that is rendered to a SQL string
by `sql_render`. The output from `sql_build` is designed to be easy to test, as
it's database diagnostic, and has a hierarchical structure.

### Arguments
* `op`  A sequence of [lazy operations](https://gist.github.com/yeesian/c616a56bed9accf6acf717c4ec888e6a#lazy-operations)
* `con` A database connection. The default `NULL` uses a set of
    rules that should be very similar to ANSI 92, and allows for testing
    without an active database connection.

* `...` Other arguments passed on to the methods. Not currently used.

### Usage
```R
sql_build(op, con, ...)
select_query(from, select = sql("*"), where = character(),
  group_by = character(), having = character(), order_by = character(),
  limit = NULL, distinct = FALSE)
join_query(x, y, type = "inner", by = NULL, suffix = c(".x", ".y"))
semi_join_query(x, y, anti = FALSE, by = NULL)
set_op_query(x, y, type = type)
sql_render(query, con = NULL, ...)
```
### Remarks
* `sql_build` is generic over the [lazy operations](https://gist.github.com/yeesian/c616a56bed9accf6acf717c4ec888e6a#lazy-operations), lazy_ops, and generates an S3 object that represents the query.
* `sql_render` takes a query object and then calls a function that is generic over the database. For example,
        `sql_build.op_mutate` generates a [`select_query`](https://gist.github.com/yeesian/c616a56bed9accf6acf717c4ec888e6a#sql-query), and [`sql_render.select_query`](https://gist.github.com/yeesian/c616a56bed9accf6acf717c4ec888e6a#sql_render) calls [`sql_select`](https://gist.github.com/yeesian/c616a56bed9accf6acf717c4ec888e6a#details-1), which has different methods for different databases.
* The default methods should generate ANSI 92 SQL where possible, so backends only need to override the methods
                if they are not ANSI compliant.

### Details
```R
# ----------------------------- Single table ops -----------------------------
sql_build.op_select(op, con, ...){
  vars <- select_vars_(op_vars(op$x), op$dots, include = op_grps(op$x))
  select_query(sql_build(op$x, con), ident(vars))}
  
sql_build.op_rename(op, con, ...) {
  vars <- rename_vars_(op_vars(op$x), op$dots)
  select_query(sql_build(op$x, con), ident(vars))}

sql_build.op_arrange(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con, op_vars(op$x))
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)
  select_query(sql_build(op$x, con), order_by = order_vars)}

sql_build.op_summarise(op, con, ...){
  select_vars <- translate_sql_(op$dots, con, op_vars(op$x), window = FALSE)
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)
  select_query(sql_build(op$x, con),
    select = c.sql(group_vars, select_vars, con = con),
    group_by = group_vars)}

sql_build.op_mutate(op, con, ...){
  vars <- op_vars(op$x)
  new_vars <- translate_sql_(op$dots, con, vars,
    vars_group = op_grps(op),
    vars_order = op_sort(op))
  old_vars <- ident(setdiff(vars, names(new_vars)))
  select_query(sql_build(op$x, con),
    select = c.sql(old_vars, new_vars, con = con))}

sql_build.op_head(op, con, ...){
  select_query(sql_build(op$x, con), limit = op$args$n)}

sql_build.op_group_by(op, con, ...){
  sql_build(op$x, con, ...)}

sql_build.op_ungroup(op, con, ...) {
  sql_build(op$x, con, ...)}

sql_build.op_filter(op, con, ...) {
  vars <- op_vars(op$x)
  where_sql <- translate_sql_(op$dots, con, vars = vars)
  select_query(sql_build(op$x, con),
    where = where_sql)}

sql_build.op_distinct(op, con, ...) {
  if (length(op$dots) == 0) {
    select_query(sql_build(op$x, con),
      distinct = TRUE)}
  else {
    if (op$args$.keep_all) {
      stop("Can't calculate distinct only on specified columns
            with SQL unless .keep_all is FALSE", call. = FALSE)}
    group_vars <- c.sql(ident(names(op$dots)), con = con)
    select_query(sql_build(op$x, con),
      select = group_vars,
      group_by = group_vars)}}

# ----------------------------- Dual table ops -----------------------------

sql_build.op_join(op, con, ...) {
  # Ensure tables have unique names
  x_names <- op_vars(op$x)
  y_names <- op_vars(op$y)
  by <- op$args$by
  uniques <- unique_names(x_names, y_names, by = by, suffix = op$args$suffix)
  if (is.null(uniques)) {
    x <- op$x; y <- op$y}
  else {
    # TODO: it would be better to construct an explicit FROM statement
    # that used the table names to disambiguate the fields names: this
    # would remove a layer of subqueries and would make sql_join more
    # flexible.
    x <- select_(op$x, .dots = setNames(x_names, uniques$x))
    y <- select_(op$y, .dots = setNames(y_names, uniques$y))
    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])}
  join_query(x, y,
    type = op$args$type,
    by = by)}

sql_build.op_semi_join(op, con, ...) {
  semi_join_query(op$x, op$y, anti = op$args$anti, by = op$args$by)}

sql_build.op_set_op(op, con, ...) {
  set_op_query(op$x, op$y, type = op$args$type)}
```

## sql-query
```R
select_query <- function(from,
                         select = sql("*"),
                         where = character(),
                         group_by = character(),
                         having = character(),
                         order_by = character(),
                         limit = NULL,
                         distinct = FALSE) {
  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      order_by = order_by,
      distinct = distinct,
      limit = limit
    ),
    class = c("select_query", "query")
  )
}

print.select_query <- function(x, ...) {
  cat("<SQL SELECT", if (x$distinct) " DISTINCT", ">\n", sep = "")
  cat("From:     ", x$from, "\n", sep = "")

  if (length(x$select))   cat("Select:   ", named_commas(x$select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas(x$order_by), "\n", sep = "")
  if (length(x$having))   cat("Having:   ", named_commas(x$having), "\n", sep = "")
  if (length(x$limit))    cat("Limit:    ", x$limit, "\n", sep = "")}

join_query <- function(x, y, type = "inner", by = NULL, suffix = c(".x", ".y")) {
  structure(
    list(
      x = x,
      y = y,
      type = type,
      by = by,
      suffix = suffix
    ),
    class = c("join_query", "query")
  )
}

print.join_query <- function(x, ...) {
  cat("<SQL JOIN (", toupper(x$type), ")>\n", sep = "")
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

semi_join_query <- function(x, y, anti = FALSE, by = NULL) {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by
    ),
    class = c("semi_join_query", "query")
  )
}

print.semi_join_query <- function(x, ...) {
  cat("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>\n", sep = "")
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

set_op_query <- function(x, y, type = type) {
  structure(
    list(
      x = x,
      y = y,
      type = type
    ),
    class = c("set_op_query", "query")
  )
}

print.set_op_query <- function(x, ...) {
  cat("<SQL ", x$type, ">\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}
```
## lazy operations
This set of S3 classes describe the action of dplyr verbs. These are
currently used for SQL sources to separate the description of operations
in R from their computation in SQL. This API is very new so is likely
to evolve in the future.

`op_vars` and `op_grps` compute the variables and groups from a sequence of lazy operations. `op_sort` tracks the order of the data for use in window functions.
```R
op_base_remote(src, x, vars = NULL) {
  # If not literal sql, must be a table identifier
  if (!is.sql(x)) { x <- ident(x) }
  if (is.null(vars)) { vars <- db_query_fields(src$con, x) }
  op_base("remote", src, x, vars)
}

print.op_base_remote(x, ...) {
  cat("Source: ", src_desc(x$src), "\n", sep = "")
  if (inherits(x$x, "ident")){ cat("From: ", x$x, "\n", sep = "") }
  else { cat("From: <derived table>\n") }
  cat("<Table: ", x$x, ">\n", sep = "")
}

op_base_local(df, env = parent.frame()) { op_base("local", src_df(env = env), df, names(df)) }

print.op_base_local(x, ...) { cat("<Local data frame> ", dim_desc(x$x), "\n", sep = "") }

op_base(name, src, x, vars) {
  stopifnot(is.character(vars))
  structure(
    list(
      src = src,
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", name), "op_base", "op")
  )
}

op_single(name, x, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_single", "op")
  )
}

add_op_single(name, .data, dots = list(), args = list()) {
  .data$ops <- op_single(name, x = .data$ops, dots = dots, args = args)
  .data
}

print.op_single(x, ...) {
  print(x$x)
  cat("-> ", x$name, "()\n", sep = "")
  for (dot in x$dots) {
    cat("   - ", deparse_trunc(dot$expr), "\n", sep = "")
  }
}

<!--# from R/utils.r-->
<!--deparse_trunc <- function(x, width = getOption("width")) {-->
<!--  text <- deparse(x, width.cutoff = width)-->
<!--  if (length(text) == 1 && nchar(text) < width) return(text)-->

<!--  paste0(substr(text[1], 1, width - 3), "...")-->
<!--}-->

op_double(name, x, y, args = list()) {
  structure(
    list(
      name = name,
      x = x,
      y = y,
      args = args
    ),
    class = c(paste0("op_", name), "op_double", "op")
  )
}

# op_grps -----------------------------------------------------------------

op_grps.op_base(op){ character() }
op_grps.op_group_by(op) {
  if (isTRUE(op$args$add)){ union(op_grps(op$x), names(op$dots)) }
  else { names(op$dots) }}
op_grps.op_ungroup(op) { NULL }
op_grps.op_summarise(op) {
  grps <- op_grps(op$x)
  if (length(grps) == 1) { NULL }
  else { grps[-length(grps)] }}
op_grps.op_single(op){ op_grps(op$x) }
op_grps.op_double(op){ op_grps(op$x) }
op_grps.tbl_lazy(op) { op_grps(op$ops) }

# op_vars -----------------------------------------------------------------

op_vars.op_base(op){ op$vars }
op_vars.op_select(op){ names(select_vars_(op_vars(op$x), op$dots, include = op_grps(op$x))) }
op_vars.op_rename(op){ names(rename_vars_(op_vars(op$x), op$dots)) }
op_vars.op_summarise(op) { c(op_grps(op$x), names(op$dots)) }
op_vars.op_mutate(op){ unique(c(op_vars(op$x), names(op$dots))) }
op_vars.op_single(op){ op_vars(op$x) }
op_vars.op_join <- function(op) {
  by <- op$args$by
  x_vars <- op_vars(op$x)
  y_vars <- op_vars(op$y)
  unique <- unique_names(x_vars, y_vars, by = by, suffix = op$args$suffix)
  if (is.null(unique)) { c(by$x, setdiff(x_vars, by$x), setdiff(y_vars, by$y)) }
  else { union(unique$x, unique$y) }
}
op_vars.op_semi_join(op){ op_vars(op$x) }
op_vars.op_set_op(op){ op_vars(op$x) }
op_vars.tbl_lazy(op){ op_vars(op$ops) }

# op_sort -----------------------------------------------------------------
# This is only used to determine the order for window functions
# so it purposely ignores grouping.

op_sort(op){ UseMethod("op_sort") }
op_sort.op_base(op){ NULL }
op_sort.op_summarise(op){ NULL }

op_sort.op_arrange(op){
  order_vars <- translate_sql_(op$dots, NULL, op_vars(op))
  c.sql(op_sort(op$x), order_vars, drop_null = TRUE)}

op_sort.op_single(op){ op_sort(op$x) }
op_sort.op_double(op){ op_sort(op$x) }
op_sort.tbl_lazy(op){ op_sort(op$ops) }
```

## sql_render
```R
sql_render.op(query, con = NULL, ...) {
  sql_render(sql_build(query, ...), con = con, ...)}

sql_render.tbl_sql(query, con = NULL, ...) {
  sql_render(sql_build(query$ops, query$src$con, ...), con = query$src$con, ...)}

sql_render.tbl_lazy(query, con = NULL, ...) {
  sql_render(sql_build(query$ops, con = NULL, ...), con = NULL, ...)}

sql_render.select_query(query, con = NULL, ..., root = FALSE) {
  from <- sql_subquery(con, sql_render(query$from, con, ..., root = root), name = NULL)

  sql_select(
    con, query$select, from, where = query$where, group_by = query$group_by,
    having = query$having, order_by = query$order_by, limit = query$limit,
    distinct = query$distinct,
    ...)}

sql_render.ident(query, con = NULL, ..., root = TRUE) {
  if (root) {
    sql_select(con, sql("*"), query)
  } else {
    query}}

sql_render.sql(query, con = NULL, ...) { query }

sql_render.join_query(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_subquery(con, sql_render(query$x, con, ..., root = root), name = NULL)
  from_y <- sql_subquery(con, sql_render(query$y, con, ..., root = root), name = NULL)

  sql_join(con, from_x, from_y, type = query$type, by = query$by)}

sql_render.semi_join_query(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_subquery(con, sql_render(query$x, con, ..., root = root), name = "_LEFT")
  from_y <- sql_subquery(con, sql_render(query$y, con, ..., root = root), name = "_RIGHT")

  sql_semi_join(con, from_x, from_y, anti = query$anti, by = query$by)}

sql_render.set_op_query(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_render(query$x, con, ..., root = TRUE)
  from_y <- sql_render(query$y, con, ..., root = TRUE)

  sql_set_op(con, from_x, from_y, method = query$type)}
```
## sql generics
These generics are used to run build various SQL queries. A default method
generates ANSI 92 compliant SQL, but variations in SQL across databases means
that it's likely that a backend will require at least a few methods.

### Arguments
* `con`	A database connection.

### Returns
A SQL string.

### Usage
```
sql_select(con, select, from, where = NULL, group_by = NULL,
  having = NULL, order_by = NULL, limit = NULL, distinct = FALSE, ...)
sql_subquery(con, from, name = random_table_name(), ...)
sql_join(con, x, y, type = "inner", by = NULL, ...)
sql_semi_join(con, x, y, anti = FALSE, by = NULL, ...)
sql_set_op(con, x, y, method)
sql_escape_string(con, x)
sql_escape_ident(con, x)
```

### Details
```R
sql_select(con, select, from, where = NULL,
                               group_by = NULL, having = NULL,
                               order_by = NULL, limit = NULL,
                               distinct = FALSE, ...){
  out <- vector("list", 7)
  names(out) <- c("select","from","where","group_by","having","order_by","limit")
  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    escape(select, collapse = ", ", con = con))
  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)
  if (length(where) > 0L) {
    assert_that(is.character(where))
    where_paren <- escape(where, parens = TRUE, con = con)
    out$where <- build_sql("WHERE ", sql_vector(where_paren, collapse = " AND "))}
  if (length(group_by) > 0L) {
    assert_that(is.character(group_by))
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))}
  if (length(having) > 0L) {
    assert_that(is.character(having))
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))}
  if (length(order_by) > 0L) {
    assert_that(is.character(order_by))
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))}
  if (!is.null(limit)) {
    assert_that(is.numeric(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ",
                           sql(format(trunc(limit), scientific = FALSE)),
                           con = con)}
  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)}

sql_subquery(con, from, name = unique_name(), ...) {
  if (is.ident(from)){
    setNames(from, name)}
  else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)}}

sql_join(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
    left = sql("LEFT"),
    inner = sql("INNER"),
    right = sql("RIGHT"),
    full = sql("FULL"),
    stop("Unknown join type:", type, call. = FALSE))
  using <- all(by$x == by$y)
  if (using) {
    cond <- build_sql("USING ", lapply(by$x, ident), con = con)
  } else {
    on <- sql_vector(paste0(sql_escape_ident(con, by$x), " = ", sql_escape_ident(con, by$y)),
      collapse = " AND ", parens = TRUE)
    cond <- build_sql("ON ", on, con = con)}
  build_sql(
    'SELECT * FROM ',x, "\n\n",
    join, " JOIN\n\n" ,
    y, "\n\n",
    cond,
    con = con)}

sql_semi_join(con, x, y, anti = FALSE, by = NULL, ...) {
  # X and Y are subqueries named _LEFT and _RIGHT
  left <- escape(ident("_LEFT"), con = con)
  right <- escape(ident("_RIGHT"), con = con)
  on <- sql_vector(
    paste0(left,  ".", sql_escape_ident(con, by$x), " = ",
           right, ".", sql_escape_ident(con, by$y)),
    collapse = " AND ",
    parens = TRUE,
    con = con)
  build_sql(
    'SELECT * FROM ', x, '\n\n',
    'WHERE ', if (anti) sql('NOT '), 'EXISTS (\n',
    '  SELECT 1 FROM ', y, '\n',
    '  WHERE ', on, '\n',
    ')',
    con = con)}

sql_set_op(con, x, y, method) {
  build_sql(x, "\n", sql(method), "\n", y)}

sql_escape_string(con, x) {
  sql_quote(x, "'")}

sql_escape_ident(con, x) {
  sql_quote(x, '"')}
```

## SQL escaping
These functions are critical when writing functions that translate R
functions to sql functions. Typically a conversion function should escape
all it's inputs and return an sql object.

### Arguments
* `...`    Character vectors that will be combined into a single SQL expression. `ident`
           flags its input as a identifier, to ensure that it gets the correct quoting.
* `x`      An object to escape. Existing sql vectors will be left as is, character
           vectors are escaped with single quotes, numeric vectors have trailing `.0`
           added if they're whole numbers, identifiers are escaped with double quotes.
* `parens` collapse Controls behaviour when multiple values are supplied. `parens`
*          should be a logical flag, or if `NA`, will wrap in parens if length > 1.

**Default behaviour**: lists are always wrapped in parens and separated by commas,
identifiers are separated by commas and never wrapped, atomic vectors are separated by
spaces and wrapped in parens if needed.

### Usage
```
# Doubles vs. integers
escape(1:5)
escape(c(1, 5.4))
# String vs known sql vs. sql identifier
escape("X")
escape(sql("X"))
escape(ident("X"))
# Escaping is idempotent
escape("X")
escape(escape("X"))
escape(escape(escape("X")))
```
### Details
```R
sql(...) {
  x <- c(...)
  if (length(x) == 0) {
    structure(character(), class = c("sql", "character"))
  } else {
    stopifnot(is.character(x))
    structure(x, class = c("sql", "character"))}}

ident(...) {
  x <- c(...)
  if (length(x) == 0) return(sql())
  stopifnot(is.character(x))

  structure(x, class = c("ident", "sql", "character"))}

c.sql(..., drop_null = FALSE, con = NULL) {
  input <- list(...)
  if (drop_null) input <- compact(input)

  sql(unlist(lapply(input, escape, collapse = NULL, con = con)))}

unique.sql(x, ...){ sql(NextMethod()) }

setOldClass(c("sql", "character"))
setOldClass(c("ident", "sql", "character"))
is.sql(x){ inherits(x, "sql") }
is.ident(x){ inherits(x, "ident") }

print.sql(x, ...){ cat(format(x, ...), sep = "\n") }
format.sql(x, ...){ paste0("<SQL> ", x) }
format.ident(x, ...){ paste0("<VAR> ", escape(x)) }

escape(x, parens = NA, collapse = " ", con = NULL) {
  UseMethod("escape") }

escape.ident(x, parens = FALSE, collapse = ", ", con = NULL) {
  y <- sql_escape_ident(con, x)
  sql_vector(names_to_as(y, con), parens, collapse)}

escape.logical(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  x[is.na(x)] <- "NULL"
  sql_vector(x, parens, collapse)}

escape.factor(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  escape.character(x, parens = parens, collapse = collapse, con = con)}

escape.Date(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  escape.character(x, parens = parens, collapse = collapse, con = con)}

escape.POSIXt(x, parens = NA, collapse = ", ", con = NULL) {
  x <- strftime(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  escape.character(x, parens = parens, collapse = collapse, con = con)}

escape.character(x, parens = NA, collapse = ", ", con = NULL) {
  sql_vector(sql_escape_string(con, x), parens, collapse, con = con)}

escape.double(x, parens = NA, collapse = ", ", con = NULL) {
  missing <- is.na(x)
  x <- ifelse(is.wholenumber(x), sprintf("%.1f", x), as.character(x))
  x[missing] <- "NULL"

  sql_vector(x, parens, collapse)}

escape.integer(x, parens = NA, collapse = ", ", con = NULL) {
  x[is.na(x)] <- "NULL"
  sql_vector(x, parens, collapse)}

escape.NULL(x, parens = NA, collapse = " ", con = NULL) { sql("NULL") }

escape.sql(x, parens = NULL, collapse = NULL, con = NULL) {
  sql_vector(x, isTRUE(parens), collapse, con = con)}

escape.list(x, parens = TRUE, collapse = ", ", con = NULL) {
  pieces <- vapply(x, escape, character(1), con = con)
  sql_vector(pieces, parens, collapse)}

sql_vector(x, parens = NA, collapse = " ", con = NULL) {
  if (is.na(parens)) { parens <- length(x) > 1L }

  x <- names_to_as(x, con = con)
  x <- paste(x, collapse = collapse)
  if (parens) x <- paste0("(", x, ")")
  
  sql(x)}

names_to_as(x, con = NULL) {
  names <- names2(x)
  as <- ifelse(names == '', '', paste0(' AS ', sql_escape_ident(con, names)))

  paste0(x, as)}
```
## build_sql
Build a SQL string.

This is a convenience function that should prevent sql injection attacks
(which in the context of dplyr are most likely to be accidental not
deliberate) by automatically escaping all expressions in the input, while
treating bare strings as sql. This is unlikely to prevent any serious
attack, but should make it unlikely that you produce invalid sql.

### Arguments
* `...` input to convert to SQL. Use `sql` to preserve user input as is (dangerous), and `ident` to label user input as sql identifiers (safe)
* `.env` the environment in which to evaluate the arguments. Should not be needed in typical use.
* `con` database connection; used to select correct quoting characters.

It makes use of the `dots` function ([explained here](https://github.com/hadley/adv-r/blob/master/Computing-on-the-language.rmd#capturing-unevaluated--capturing-dots), defined in the following way:
```
dots <- function(...){
    eval(substitute(alist(...)))
}
```
See also: https://github.com/hadley/pryr/blob/master/R/dots.r

### Examples
```
build_sql("SELECT * FROM TABLE")
x <- "TABLE"
build_sql("SELECT * FROM ", x)
build_sql("SELECT * FROM ", ident(x))
build_sql("SELECT * FROM ", sql(x))
# http://xkcd.com/327/
name <- "Robert'); DROP TABLE Students;--"
build_sql("INSERT INTO Students (Name) VALUES (", name, ")")
```
### Details
```R
build_sql(..., .env = parent.frame(), con = NULL) {
  escape_expr <- function(x) {
    # If it's a string, leave it as is
    if (is.character(x)) return(x)

    val <- eval(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) return("")

    escape(val, con = con)
  }

  pieces <- vapply(dots(...), escape_expr, character(1))
  sql(paste0(pieces, collapse = ""))
}

#' Helper function for quoting sql elements.
#'
#' If the quote character is present in the string, it will be doubled.
#' \code{NA}s will be replaced with NULL.
#'
#' @export
#' @param x Character vector to escape.
#' @param quote Single quoting character.
#' @export
#' @keywords internal
#' @examples
#' sql_quote("abc", "'")
#' sql_quote("I've had a good day", "'")
#' sql_quote(c("abc", NA), "'")
sql_quote(x, quote) {
  y <- gsub(quote, paste0(quote, quote), x, fixed = TRUE)
  y <- paste0(quote, y, quote)
  y[is.na(x)] <- "NULL"
  names(y) <- names(x)

  y
}
```

## translate_sql
Translate an expression to sql.

### Helpers
When creating a package that maps to a new SQL based src, you'll often
want to provide some additional mappings from common R commands to the
commands that your tbl provides. These three functions make that easy.

`sql_infix` and `sql_prefix` create default SQL infix and prefix functions
given the name of the SQL function. They don't perform any input checking,
but do correctly escape their input, and are useful for quickly providing
default wrappers for a new SQL variant.

* `scalar,aggregate,window` The three families of functions than a SQL variant can supply.
* `...`  .funs named functions, used to add custom converters from standard R functions to
         sql functions. Specify individually in `...`, or provide a list of `.funs`
* `.parent` the sql variant that this variant should inherit from. Defaults to `base_sql`
            which provides a standard set of mappings for the most common operators and functions.
* `f`       the name of the sql function as a string
* `n`       for \`sql_infix`, an optional number of arguments to expect. Will signal error if not correct.

**See also**: `sql` for an example of a more customised sql conversion function.

```R
sql_variant(scalar = sql_translator(),
            aggregate = sql_translator(),
            window = sql_translator()) {
  stopifnot(is.environment(scalar))
  stopifnot(is.environment(aggregate))
  stopifnot(is.environment(window))

  structure(list(scalar = scalar, aggregate = aggregate, window = window),
    class = "sql_variant")}

is.sql_variant(x) inherits(x, "sql_variant")

print.sql_variant(x, ...) {
  wrap_ls <- function(x, ...) {
    vars <- sort(ls(envir = x))
    wrapped <- strwrap(paste0(vars, collapse = ", "), ...)
    if (identical(wrapped, "")) return()
    paste0(wrapped, "\n", collapse = "")}

  cat("<sql_variant>\n")
  cat(wrap_ls(x$scalar,    prefix = "scalar:    "))
  cat(wrap_ls(x$aggregate, prefix = "aggregate: "))
  cat(wrap_ls(x$window,    prefix = "window:    "))}

names.sql_variant(x) {
  c(ls(envir = x$scalar), ls(envir = x$aggregate), ls(envir = x$window))}

sql_translator(..., .funs = list(),
                        .parent = new.env(parent = emptyenv())) {
  funs <- c(list(...), .funs)
  if (length(funs) == 0) return(.parent)

  list2env(funs, copy_env(.parent))}

copy_env(from, to = NULL, parent = parent.env(from)) {
  list2env(as.list(from), envir = to, parent = parent)}

sql_infix(f) {
  assert_that(is.string(f))

  f <- toupper(f)
  function(x, y) { build_sql(x, " ", sql(f), " ", y) }}

sql_prefix(f, n = NULL) {
  assert_that(is.string(f))

  f <- toupper(f)
  function(..., na.rm) {
    if (!missing(na.rm)) {
      message("na.rm not needed in SQL: NULL are always dropped", call. = FALSE)}

    args <- list(...)
    if (!is.null(n) && length(args) != n) {
      stop("Invalid number of args to SQL ", f, ". Expecting ", n,
        call. = FALSE)}
    if (any(names2(args) != "")) {
      warning("Named arguments ignored for SQL ", f, call. = FALSE)}
    
    build_sql(sql(f), args)}}

sql_not_supported(f) {
  assert_that(is.string(f))

  f <- toupper(f)
  
  function(...) { stop(f, " is not available in this SQL variant", call. = FALSE) }
}

win_rank(f) {
  force(f)
  
  function(order = NULL) {
    over(build_sql(sql(f), list()), partition_group(), order %||% partition_order())}}

win_recycled(f) {
  force(f)
  function(x) { over(build_sql(sql(f), list(x)), partition_group()) }}

win_cumulative(f) {
  force(f)
  function(x) { over(build_sql(sql(f), list(x)), partition_group(), partition_order(), frame = c(-Inf, 0))}}

win_absent(f) {
  force(f)
  function(...) { stop("Window function `", f, "()` is not supported by this database", call. = FALSE) }}

# Use a global variable to communicate state of partitioning between
# tbl and sql translator. This isn't the most amazing design, but it keeps
# things loosely coupled and is straightforward to understand.
partition <- new.env(parent = emptyenv())
partition$group_by <- NULL
partition$order_by <- NULL
partition$con <- NULL

set_partition_con(con) {
  old <- partition$con
  partition$con <- con
  invisible(old)}

set_partition_group(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- partition$group_by
  partition$group_by <- vars
  invisible(old)}

set_partition_order(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- partition$order_by
  partition$order_by <- vars
  invisible(old)}

set_partition(group_by, order_by, con = NULL) {
  old <- list(partition$group_by, partition$order_by)
  if (is.list(group_by)) {
    order_by <- group_by[[2]]
    group_by <- group_by[[1]]}

  partition$group_by <- group_by
  partition$order_by <- order_by
  partition$con <- con

  invisible(old)}

partition_group() partition$group_by
partition_order() partition$order_by
partition_con() partition$con
```
### base
The base translator, `base_sql`, provides custom mappings for `!` (to `NOT`), `&&` and `&` to
`AND`, `||` and `|` to `OR`, `^` to `POWER`, `\%>\%` to `\%`, `ceiling` to `CEIL`, `mean` to
`AVG`, `var` to `VARIANCE`, `tolower` to `LOWER`, `toupper` to `UPPER` and `nchar` to `length`.

`c` and `:` keep their usual R behaviour so you can easily create vectors that are passed to sql.

All other functions will be preserved as is. R's infix functions (e.g. `\%like\%`) will be
converted to their sql equivalents (e.g. `LIKE`). You can use this to access SQL string 
concatenation: `||` is mapped to `OR`, but `\%||\%` is mapped to `||`. To suppress this behaviour,
and force errors immediately when dplyr doesn't know how to translate a function it encounters, 
using set the `dplyr.strict_sql` option to `TRUE`. You can also use `sql` to insert a raw sql string.
```R

sql_if(cond, if_true, if_false = NULL) {
  build_sql(
    "CASE WHEN (", cond, ")",
    " THEN (", if_true, ")",
    if (!is.null(if_false)) build_sql(" ELSE (", if_false, ")"),
    " END")}

base_scalar <- sql_translator(
  `+`    = sql_infix("+"),
  `*`    = sql_infix("*"),
  `/`    = sql_infix("/"),
  `%%`   = sql_infix("%"),
  `^`    = sql_prefix("power", 2),
  `-`    = function(x, y = NULL) {
    if (is.null(y)) {
      if (is.numeric(x)) {
        -x }
      else {
        build_sql(sql("-"), x)}}
    else {
      build_sql(x, sql(" - "), y)}
  },
  `!=`    = sql_infix("!="),
  `==`    = sql_infix("="),
  `<`     = sql_infix("<"),
  `<=`    = sql_infix("<="),
  `>`     = sql_infix(">"),
  `>=`    = sql_infix(">="),
  `!`     = sql_prefix("not"),
  `&`     = sql_infix("and"),
  `&&`    = sql_infix("and"),
  `|`     = sql_infix("or"),
  `||`    = sql_infix("or"),
  xor     = function(x, y) {
    sql(sprintf("%1$s OR %2$s AND NOT (%1$s AND %2$s)", escape(x), escape(y)))
  },
  abs     = sql_prefix("abs", 1),
  acos    = sql_prefix("acos", 1),
  acosh   = sql_prefix("acosh", 1),
  asin    = sql_prefix("asin", 1),
  asinh   = sql_prefix("asinh", 1),
  atan    = sql_prefix("atan", 1),
  atan2   = sql_prefix("atan2", 2),
  atanh   = sql_prefix("atanh", 1),
  ceil    = sql_prefix("ceil", 1),
  ceiling = sql_prefix("ceil", 1),
  cos     = sql_prefix("cos", 1),
  cosh    = sql_prefix("cosh", 1),
  cot     = sql_prefix("cot", 1),
  coth    = sql_prefix("coth", 1),
  exp     = sql_prefix("exp", 1),
  floor   = sql_prefix("floor", 1),
  log     = function(x, base = exp(1)) {
    build_sql(sql("log"), list(x, base))
  },
  log10   = sql_prefix("log10", 1),
  round   = sql_prefix("round", 2),
  sign    = sql_prefix("sign", 1),
  sin     = sql_prefix("sin", 1),
  sinh    = sql_prefix("sinh", 1),
  sqrt    = sql_prefix("sqrt", 1),
  tan     = sql_prefix("tan", 1),
  tolower = sql_prefix("lower", 1),
  toupper = sql_prefix("upper", 1),
  nchar   = sql_prefix("length", 1),
  `if` = sql_if,
  if_else = sql_if,
  ifelse = sql_if,
  sql = function(...) sql(...),
  `(` = function(x) {
    build_sql("(", x, ")")},
  `{` = function(x) {
    build_sql("(", x, ")")},
  desc = function(x) {
    build_sql(x, sql(" DESC"))},
  is.null = function(x) {
    build_sql("(", x, ") IS NULL")},
  is.na = function(x) {
    build_sql("(", x, ") IS NULL")},
  na_if = sql_prefix("NULL_IF", 2),
  as.numeric = function(x) build_sql("CAST(", x, " AS NUMERIC)"),
  as.integer = function(x) build_sql("CAST(", x, " AS INTEGER)"),
  as.character = function(x) build_sql("CAST(", x, " AS TEXT)"),
  c = function(...) escape(c(...)),
  `:` = function(from, to) escape(from:to),
  between = function(x, left, right) {
    build_sql(x, " BETWEEN ", left, " AND ", right)},
  pmin = sql_prefix("min"),
  pmax = sql_prefix("max"),
  `__dplyr_colwise_fun` = function(...) {
    stop("colwise verbs only accept bare functions with local sources",
      call. = FALSE)
  }
)

base_symbols <- sql_translator(
  pi = sql("PI()"),
  `*` = sql("*"),
  `NULL` = sql("NULL")
)

base_agg <- sql_translator(
  # SQL-92 aggregates
  # http://db.apache.org/derby/docs/10.7/ref/rrefsqlj33923.html
  n          = sql_prefix("count"),
  mean       = sql_prefix("avg", 1),
  var        = sql_prefix("variance", 1),
  sum        = sql_prefix("sum", 1),
  min        = sql_prefix("min", 1),
  max        = sql_prefix("max", 1),
  n_distinct = function(x) {
    build_sql("COUNT(DISTINCT ", x, ")")
  }
)

base_win <- sql_translator(
  # rank functions have a single order argument that overrides the default
  row_number   = win_rank("row_number"),
  min_rank     = win_rank("rank"),
  rank         = win_rank("rank"),
  dense_rank   = win_rank("dense_rank"),
  percent_rank = win_rank("percent_rank"),
  cume_dist    = win_rank("cume_dist"),
  ntile        = function(order_by, n) {
    over(
      build_sql("NTILE", list(as.integer(n))),
      partition_group(),
      order_by %||% partition_order()
    )
  },

  # Recycled aggregate fuctions take single argument, don't need order and
  # include entire partition in frame.
  mean  = win_recycled("avg"),
  sum   = win_recycled("sum"),
  min   = win_recycled("min"),
  max   = win_recycled("max"),
  n     = function() {
    over(sql("COUNT(*)"), partition_group())
  },

  # Cumulative function are like recycled aggregates except that R names
  # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
  cummean = win_cumulative("mean"),
  cumsum  = win_cumulative("sum"),
  cummin  = win_cumulative("min"),
  cummax  = win_cumulative("max"),

  # Finally there are a few miscellaenous functions that don't follow any
  # particular pattern
  nth = function(x, order = NULL) {
    over(build_sql("NTH_VALUE", list(x)), partition_group(), order %||% partition$order())
  },
  first = function(x, order = NULL) {
    over(build_sql("FIRST_VALUE", list(x)), partition_group(), order %||% partition_order())
  },
  last = function(x, order = NULL) {
    over(build_sql("LAST_VALUE", list(x)), partition_group(), order %||% partition_order())
  },

  lead = function(x, n = 1L, default = NA, order = NULL) {
    over(
      build_sql("LEAD", list(x, n, default)),
      partition_group(),
      order %||% partition_order()
    )
  },
  lag = function(x, n = 1L, default = NA, order = NULL) {
    over(
      build_sql("LAG", list(x, n, default)),
      partition_group(),
      order %||% partition_order()
    )
  },

  order_by = function(order_by, expr) {
    old <- set_partition(partition_group(), order_by)
    on.exit(set_partition(old))

    expr
  }
)

base_no_win <- sql_translator(
  row_number   = win_absent("row_number"),
  min_rank     = win_absent("rank"),
  rank         = win_absent("rank"),
  dense_rank   = win_absent("dense_rank"),
  percent_rank = win_absent("percent_rank"),
  cume_dist    = win_absent("cume_dist"),
  ntile        = win_absent("ntile"),
  mean         = win_absent("avg"),
  sum          = win_absent("sum"),
  min          = win_absent("min"),
  max          = win_absent("max"),
  n            = win_absent("n"),
  cummean      = win_absent("mean"),
  cumsum       = win_absent("sum"),
  cummin       = win_absent("min"),
  cummax       = win_absent("max"),
  nth          = win_absent("nth_value"),
  first        = win_absent("first_value"),
  last         = win_absent("last_value"),
  lead         = win_absent("lead"),
  lag          = win_absent("lag"),
  order_by     = win_absent("order_by")
)
```

### Usage
```
translate_sql(..., con = NULL, vars = character(), vars_group = NULL,
  vars_order = NULL, window = TRUE)
translate_sql_(dots, con = NULL, vars = character(), vars_group = NULL,
  vars_order = NULL, window = TRUE)
```
### Arguments
* `..., dots`  Expressions to translate. sql_translate automatically quotes them for you. sql_translate_ expects a list of already quoted objects.

* `con`  An optional database connection to control the details of the translation. The default, NULL, generates ANSI SQL.

* `vars`  A character vector giving variable names in the remote data source. If this is supplied, translate_sql will call partial_eval to interpolate in the values from local variables.

* `vars_group, vars_order`	Grouping and ordering variables used for windowed functions.

* `window` Use FALSE to suppress generation of the OVER statement used for window functions. This is necessary when generating SQL for a grouped summary.

### Details
```R
translate_sql <- function(...,
                          con = NULL,
                          vars = character(),
                          vars_group = NULL,
                          vars_order = NULL,
                          window = TRUE) {
  dots <- lazyeval::lazy_dots(...)

  translate_sql_(dots,
    con = con,
    vars = vars,
    vars_group = vars_group,
    vars_order = vars_order,
    window = window
  )
}

translate_sql_ <- function(dots,
                           con = NULL,
                           vars = character(),
                           vars_group = NULL,
                           vars_order = NULL,
                           window = TRUE) {
  expr <- lazyeval::as.lazy_dots(dots, env = parent.frame())
  if (!any(has_names(expr))) {
    names(expr) <- NULL
  }

  if (length(vars) > 0) {
    # If variables are known, partially evaluate input
    expr <- partial_eval2(expr, vars)
  } else {
    # Otherwise just extract expressions, ignoring the environment
    # from which they came
    expr <- lapply(expr, "[[", "expr")
  }
  variant <- sql_translate_env(con)

  if (window) {
    old_con <- set_partition_con(con)
    on.exit(set_partition_con(old_con), add = TRUE)

    old_group <- set_partition_group(vars_group)
    on.exit(set_partition_group(old_group), add = TRUE)

    old_order <- set_partition_order(vars_order)
    on.exit(set_partition_order(old_order), add = TRUE)
  }

  pieces <- lapply(expr, function(x) {
    if (is.atomic(x)) return(escape(x, con = con))

    env <- sql_env(x, variant, con, window = window)
    escape(eval(x, envir = env))
  })

  sql(unlist(pieces))
}

sql_env <- function(expr, variant, con, window = FALSE,
                    strict = getOption("dplyr.strict_sql")) {
  stopifnot(is.sql_variant(variant))

  # Default for unknown functions
  if (!strict) {
    unknown <- setdiff(all_calls(expr), names(variant))
    default_env <- ceply(unknown, default_op, parent = emptyenv())
  } else {
    default_env <- new.env(parent = emptyenv())
  }


  # Known R -> SQL functions
  special_calls <- copy_env(variant$scalar, parent = default_env)
  if (!window) {
    special_calls2 <- copy_env(variant$aggregate, parent = special_calls)
  } else {
    special_calls2 <- copy_env(variant$window, parent = special_calls)
  }

  # Existing symbols in expression
  names <- all_names(expr)
  name_env <- ceply(names, function(x) escape(ident(x), con = con),
    parent = special_calls2)

  # Known sql expressions
  symbol_env <- copy_env(base_symbols, parent = name_env)
  symbol_env
}

default_op <- function(x) {
  assert_that(is.string(x))
  infix <- c("::", "$", "@", "^", "*", "/", "+", "-", ">", ">=", "<", "<=",
    "==", "!=", "!", "&", "&&", "|", "||", "~", "<-", "<<-")

  if (x %in% infix) {
    sql_infix(x)
  } else if (grepl("^%.*%$", x)) {
    x <- substr(x, 2, nchar(x) - 1)
    sql_infix(x)
  } else {
    sql_prefix(x)
  }
}


all_calls <- function(x) {
  if (!is.call(x)) return(NULL)

  fname <- as.character(x[[1]])
  unique(c(fname, unlist(lapply(x[-1], all_calls), use.names = FALSE)))
}

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

# character vector -> environment
ceply <- function(x, f, ..., parent = parent.frame()) {
  if (length(x) == 0) return(new.env(parent = parent))
  l <- lapply(x, f, ...)
  names(l) <- x
  # ‘names’ is a generic accessor function, and ‘names<-’ is a generic
  # replacement function.  The default methods get and set the
  # ‘"names"’ attribute of a vector (including a list) or pairlist.
  list2env(l, parent = parent)
  # list2env: From a _named_ ‘list l’, create an ‘environment’ containing
  # all list components as objects, or “multi-assign” from ‘l’ into a
  # pre-existing environment.
  # parent: (for the case ‘envir = NULL’): a parent frame aka enclosing
  # environment, see ‘new.env’
}
```