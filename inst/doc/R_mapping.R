## -----------------------------------------------------------------------------
library("rqdatatable")
library("wrapr")

dL <- build_frame(
  "subjectID", "surveyCategory"     , "assessmentTotal"|
    1          , "withdrawal behavior", 5              |
    1          , "positive re-framing", 2              |
    2          , "withdrawal behavior", 3              |
    2          , "positive re-framing", 4              |
    2          , "other"              , 0              )

scale <- 0.237
rquery_pipeline <- local_td(dL) %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := n(),
             rank := rank(),
             orderby = c("assessmentTotal", "surveyCategory"),
             reverse = c("assessmentTotal"),
             partitionby = 'subjectID')  %.>%
  orderby(., c("subjectID", "probability"))
res <- ex_data_table(rquery_pipeline, tables = list(dL = dL))
knitr::kable(res)

## -----------------------------------------------------------------------------
str(rqdatatable:::data_table_extend_fns)

