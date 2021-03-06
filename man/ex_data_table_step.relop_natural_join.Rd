% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relop_natural_join.R
\name{ex_data_table_step.relop_natural_join}
\alias{ex_data_table_step.relop_natural_join}
\title{Natural join.}
\usage{
\method{ex_data_table_step}{relop_natural_join}(
  optree,
  ...,
  tables = list(),
  source_usage = NULL,
  source_limit = NULL,
  env = parent.frame()
)
}
\arguments{
\item{optree}{relop operations tree.}

\item{...}{not used, force later arguments to bind by name.}

\item{tables}{named list map from table names used in nodes to data.tables and data.frames.}

\item{source_usage}{list mapping source table names to vectors of columns used.}

\item{source_limit}{if not null limit all table sources to no more than this many rows (used for debugging).}

\item{env}{environment to work in.}
}
\description{
\code{data.table} based implementation.
}
\examples{

d1 <- build_frame(
    "key", "val", "val1" |
      "a"  , 1  ,  10    |
      "b"  , 2  ,  11    |
      "c"  , 3  ,  12    )
d2 <- build_frame(
    "key", "val", "val2" |
      "a"  , 5  ,  13    |
      "b"  , 6  ,  14    |
      "d"  , 7  ,  15    )

# key matching join
optree <- natural_join(local_td(d1), local_td(d2),
                       jointype = "FULL", by = 'key')
ex_data_table(optree)

# full cross-product join
# (usually with jointype = "FULL", but "LEFT" is more
# compatible with rquery field merge semantics).
optree2 <- natural_join(local_td(d1), local_td(d2),
                        jointype = "LEFT", by = NULL)
ex_data_table(optree2)
# notice ALL non-"by" fields take coalese to left table.

}
