##' Take data, filter on a variable by given values, return unique values of another variable
##'
##' @title Get unique values of a variable for a subset of schools
##' @param data Df input data
##' @param n Branch of target
##' @param var_filter Variable to filter on (unquoted)
##' @param filter_values Values to filter var_filter by
##' @param var_pull Which variable to rerurn (unquoted)
##' @return a vector
##' @author Petr Bouchal
##' @export
sch_get_filtered_ids <- function(data, var_filter, filter_values, var_pull) {
  ids <- data %>%
    filter({{var_filter}} %in% filter_values) %>%
    pull({{var_pull}}) %>% unique()
  return(ids)
}
