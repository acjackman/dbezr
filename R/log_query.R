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
    if (is.na(force_log)){
        force_log <- getOption("dbezr.force_log")
    }

    # Log the query
    if (force_log){
        cat("Running Query (", format(lubridate::now()), " - ",
            cred_id(.db), "):\n", query, ";\n", sep = "")
  }
}

#' Force queries logging
#'
#' Force queries to be logged or not, regardless of the other settings.
#' @param .db credentials used for this query, if \code{NULL}, default is used
#' @rdname cred
#' @export
qlog_loud <- function(.db = NULL){
    .db <- get_db(.db)
    attr(.db, "force_log") <- TRUE
    .db
}

#' @rdname cred
#' @export
qlog_silent <- function(.db = NULL){
    .db <- get_db(.db)
    attr(.db, "force_log") <- FALSE
    .db
}
