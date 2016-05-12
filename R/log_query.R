#' Log the any query to the console
#'
#' Print a query to the console, along with time of query and the connection
#' details of the database connection used.
#'
#' @param query the query string
#' @param .db The database the query is being sent to
#'
#' @return None
log_query <- function(query, .db){
    # Determine if the query should be logged
    force_log <- attr(.db, "force_log")
    if (is.null(force_log)){
        log_query <- getOption("dbezr.force_log")
    } else{
        log_query <- force_log
    }

    # Log the query
    if (log_query){
        cat("Running Query (", format(lubridate::now()), " - ",
            cred_id(.db), "):\n", query, ";\n", sep = "")
  }
}
