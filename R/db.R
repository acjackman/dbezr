#' Register Database
#'
#' Register database to create a connection and use the database with \code{\link{gq}}.
#'
#' @param cred credentials object
#'
#' @rdname db
#' @export
db_cred <- function(cred){
    tmstp <- lubridate::now()
    attr(cred, "reg_tmstp") <- tmstp

    new_db <- structure(
        list(
            cred = cred,
            connection = create_con(cred),
            created = tmstp,
            trans_active = FALSE
        ), class = "reg_db")

    cid <- cred_id(cred)

    if (is.null(registered_dbs[[cid]])){
        # If can't find, then add
        registered_dbs[[cid]] <- new_db
    } else {
        # if currently there then disconnect the old, then add our new db
        if (is_valid_con(registered_dbs[[cid]]$connection)){
            disconnect_con(registered_dbs[[cid]]$connection)
        }

        registered_dbs[[cid]] <- new_db
    }

    # set as the default if not already done
    if (is.null(dbezr_set$db)){
        dbezr_set$db <- cid
    }

    cred
}

#' @param json a JSON credentials string.
#'
#' @rdname db
#' @export
db_json <- function(json){
    db_cred(cred_json(json))
}

#' @param file a file containg a JSON credentials string.
#' @rdname db
#' @export
db_file <- function(file){
    db_cred(cred_file(file))
}

#' Remove db object
#'
#' Removes a db from the registered list, disconnecting it's connection.
#'
#' @param cred credentials object
#'
#' @return \code{NULL}
#'
#' @examples
#' \dontrun{
#' rm_db(cred)
#' }
#'
#' @export
rm_db <- function(cred){
    rm_db_id(cred_id(cred))
}

#' @param id credential id string generated by cred_id
#' @rdname rm_db
#' @export
rm_db_id <- function(id){
    disconnect_con(registered_dbs[[id]]$connection)
    rm(list = id, envir = registered_dbs)
}

#' @rdname rm_db
#' @export
rm_db_all <- function(){
    rdbs <- ls(registered_dbs)
    if (length(rdbs) > 0) plyr::llply(rdbs, .fun = rm_db_id)
}

#' Get List of Database Credentials
#'
#' Get a list of databases registered with the package. All of these databases have open
#' connections.
#'
#' @examples
#' list_dbs()
#'
#' @importFrom magrittr "%>%"
#' @export
list_dbs <- function(){
    rdbs <- ls(registered_dbs)
    if (length(rdbs) > 0) {
            rdbs %>%
            plyr::llply(.fun = function(x){
                registered_dbs[[x]]$cred
            }) %>%
            structure(class = "db_credentials_list")
    }
}

#' Print list of database credentials
#'
#' Formats a list of database credentials for printing nicely by printing the individual databases
#' without the list structure.
#'
#' @inheritParams base::print
#'
#' @examples
#' list_dbs()
#'
#' @export
print.db_credentials_list <- function(x, ...){
    plyr::l_ply(x, .fun = print)
}


#' Retrieve the database
#'
#' Retrieve the database from the stored list, ensuring that there is a connection.
#' @param db credentials object for the database to be used, use the default
#'             database if null
#' @param .error throw an error if no database is found
get_db <- function(db = NULL, .error = TRUE){
    if (is.null(db)){
        if (is.null(dbezr_set$db) && .error) {
            stop("No database registered")
        } else if (is.null(dbezr_set$db) && !.error) {
            return(NULL)
        }

        # We can pull the default db
        reg_db <- registered_dbs[[dbezr_set$db]]
        db <- reg_db$cred
    } else {
        reg_db <- registered_dbs[[cred_id(db)]]
    }

    # Create connection if it doesn't exist, either because it hasn't been
    # registered or the connection is invalid
    if (is.null(reg_db) || !is_valid_con(reg_db$connection) ) {
        db <- db_cred(db)
    }

    db
}
