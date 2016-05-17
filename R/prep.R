#' Prepare data for Entry into database
#'
#' Prepare a vector of data to be inserted into the database
#'
#' @param x vector to prepare for database
#' @param .db the connection to use throughout the transaction
#'
#' @importFrom DBI dbQuoteString
#' @rdname prep
#' @export
prep <- function(x, .db) UseMethod("prep", x)

#' @rdname prep
#' @export
prep.character <- function(x, .db = NULL){
    .db <- get_db(.db)

    conn <- registered_dbs[[cred_id(.db)]]$connection
    do.call("dbQuoteString", list(conn, x)) %>%
        as.character %>% prep_nulls(x)
}
#' @rdname prep
#' @export
prep.POSIXct <- function(x, .db){
    db_date(x) %>% prep_nulls(x)
}
#' @rdname prep
#' @export
prep.POSIXlt <- function(x, .db){
    db_date(x) %>% prep_nulls(x)
}
#' @rdname prep
#' @export
prep.numeric <- function(x, .db){
    x %>% as.character %>% prep_nulls(x)
}
#' @rdname prep
#' @export
prep.logical <- function(x, .db){
    x %>% as.numeric %>% as.character %>% prep_nulls(x)
}
#' @rdname prep
#' @export
prep.default <- function(x, .db){
    x %>% as.character %>% prep_nulls(x)
}

#' @param orig original values
#' @rdname prep
#' @export
prep_nulls <- function(x, orig){
    x[is.na(orig)] <- "NULL"
    x
}


#' Format date for database query
#'
#' Format a date to use in a databse query. Includes quotes.
#'
#' @param date date to be formatted.
#'
#' @return date formatted as a string for database.
#'
#' @examples
#' db_date(lubridate::now())
#' \dontrun{
#' gq("SELECT * FROM table WHERE date > ", db_date(lubridate::now()))
#' }
#'
#' @export
db_date <- function(date){
  paste0("'", strftime(date, "%Y-%m-%d %H:%M:%S"), "'")
}


#' Convert database DateTime string to an R date
#'
#' Convert database DateTime format to an R date format. Timezone comes from
#' \code{dbezr.timezone} option.
#'
#' @param date_time date_time string passed to \code{parse_date_time}
#'
#' @export
parse_date_time <- function(date_time){
    lubridate::force_tz(
        lubridate::fast_strptime(date_time, "%Y-%m-%d %H:%M:%S"),
        tzone = getOption("dbezr.timezone")
    )
}

#' Convert database date string to an R date
#'
#' Convert database date format to an R date format. Timezone comes from
#' \code{dbezr.timezone} option.
#'
#' @param date date string passed to \code{ymd}
#'
#' @export
parse_date <- function(date){
    lubridate::force_tz(
        lubridate::fast_strptime(date, "%Y-%m-%d"),
        tzone = getOption("dbezr.timezone")
    )
}
