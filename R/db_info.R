#' Show tables in databse
#'
#' Show a list of tables in the databse
#'
#' @param schema schema to show tables from
#' @param .db credentials to be used by \code{gq}
#'
#' @export
shw_tbls <- function(schema = NA, .db = NULL){
    .db <- get_db(.db)

    if (.db$engine == "MySQL") {
        gq("SHOW TABLES", .db = qlog_silent(.db))
    } else if (.db$engine == "PostgreSQL") {
        if (is.na(schema))  schema <- "public"
        gq("SELECT * FROM pg_catalog.pg_tables where schemaname = '", schema,
            "'", .db = qlog_silent(.db))
    } else {
        stop("Invalid database engine")
    }
}


#' Show columns from a table in the databse
#'
#' Show a list of tables in the databse
#'
#' @param table table name to query for
#' @param schema schema to show tables from
#' @param .db credentials to be used by \code{gq}
#'
#' @export
shw_cols <- function(table, schema = NA, .db = NULL){
    .db <- get_db(.db)

    if (.db$engine == "MySQL") {
        gq("SHOW COLUMNS FROM ", table, .db = qlog_silent(.db))
    } else if (.db$engine == "PostgreSQL") {
        if (is.na(schema))  schema <- "public"

        cols <- paste("column_name", "data_type", "is_nullable",
            "column_default", "character_maximum_length", "numeric_precision",
            sep = ",")
        gq("SELECT ", cols,  " FROM information_schema.columns WHERE", "
            table_schema = '", schema, "' AND table_name = '", table,
            "' ORDER by ordinal_position", .db = qlog_silent(.db))
    } else {
        stop("Invalid database engine")
    }
}
