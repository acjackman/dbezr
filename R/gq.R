#' Get query from database
#'
#' Retrieve query from registered database, or supply alternate credentials or connection
#' (\code{.cred} or \code{.conn} respectively.)
#'
#' @param ... strings to be combined to form SQL query. Query is combined with \code{paste0(..., collapse = " ")}.
#' @param .db database credentials to be used for the query.
#'
#' @return a \code{data.frame} with the results of the query.
#'
#' @export
gq <- function(..., .db = NULL) {
    # Turn the credentials provided into an actual database
    .db <- get_db(.db)
    reg_db <- registered_dbs[[cred_id(.db)]]

    query <- stringr::str_trim(paste0(..., collapse = " "))

    log_query(query, .db)

    show_warn <- attr(.db, "show_warn")
    if (is.na(show_warn)){
        show_warn <- getOption("dbezr.show_warn")
    }

    if (show_warn) {
        return(DBI::dbGetQuery(reg_db$connection, query))
    }
    else{
        return(suppressWarnings(DBI::dbGetQuery(reg_db$connection, query)))
    }
}

#' Force database warnings to be shown
#'
#' Force database warnings to be shown. Usually they are typecasting warnings,
#' and so are suppressed by default. This is a good debugging tool if queries
#' are throwing errors or not returning the expected output.
#' @param .db credentials used for this query, if \code{NULL}, default is used
dbwarn <- function(.db = NULL){
    .db <- get_db(.db)
    attr(.db, "show_warn") <- FALSE
    .db
}
