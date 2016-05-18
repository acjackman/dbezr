#' Begin/commit/rollback SQL transactions
#'
#' transactions require an open connection, so you will need to manually create the connection,
#' preform queries with \code{\link{gq}} and then and close the
#' connection.

#' @param force force the action to take place
#' @param .db the connection to use throughout the transaction

#' @rdname transactions
#' @export
trans_begin <- function(.db = NULL){
    .db <- get_db(.db)

    if (active_transaction(.db)){
        stop("a transaction is active, commit",
             " or rollback before beginning another")
    }
    if (.db$engine == "PostgreSQL"){
        gq("BEGIN TRANSACTION", .db = qlog_silent(.db))
    } else {
        DBI::dbBegin(registered_dbs[[cred_id(.db)]]$connection)
    }
    registered_dbs[[cred_id(.db)]]$trans_active <- TRUE
}
#' Commiting an SQL transaction saves the actions made durreing the transaction to the database.
#' @rdname transactions
#' @export
trans_commit <- function(force = FALSE, .db = NULL){
    .db <- get_db(.db)

    if (active_transaction(.db)){
        DBI::dbCommit(registered_dbs[[cred_id(.db)]]$connection)
        registered_dbs[[cred_id(.db)]]$trans_active <- FALSE
    } else {
        if (force){
            DBI::dbCommit(registered_dbs[[cred_id(.db)]]$connection)
            registered_dbs[[cred_id(.db)]]$trans_active <- FALSE
        } else {
            warning("no transaction is active,",
                "use force to commit regardless")
        }
    }
}

#' Rollback resets any transactions that might have happened durring the transaction.
#' @rdname transactions
#' @export
trans_rollback <- function(force = FALSE, .db = NULL){
    .db <- get_db(.db)

    if (active_transaction(.db)){
        DBI::dbRollback(registered_dbs[[cred_id(.db)]]$connection)
        registered_dbs[[cred_id(.db)]]$trans_active <- FALSE
    } else {
        if (force){
            DBI::dbRollback(registered_dbs[[cred_id(.db)]]$connection)
            registered_dbs[[cred_id(.db)]]$trans_active <- FALSE
        } else {
            warning("no transaction is active,",
                " use force to rollback regardless")
        }
    }
}

#' Determine if there is an active transaction
#' @rdname transactions
#' @export
trans_active <- function(force = FALSE, .db = NULL){
    .db <- get_db(.db)

    active_transaction(.db)
}

active_transaction <- function(.db){
    db_id <- cred_id(.db)
    registered_dbs[[db_id]]$trans_active
}
