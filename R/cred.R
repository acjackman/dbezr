#' Create databse credentials object
#'
#' Combine all the required elements to create a databse connection into
#' a single object. Options can be set interatively via \code{\link{qlog_loud}},
#' \code{\link{qlog_silent}}, and  \code{\link{dbwarn}}.
#'
#' @param user database username
#' @param password databse password
#' @param host database server hostname
#' @param port databse server port number
#' @param dbname database name to connect to
#' @param engine specify database engine to use
#' @param .qlog force logging of queries to console
#' @param .show_warn show warnings from running query, usually typecasting.
#'        See \code{\link{dbwarn}}
#'
#' @return a \code{db_credentials} obeject
#'
#' @examples
#' cred("bob","passwd","127.0.0.1",3306,"foo")
#'
#' @export
cred <- function(user, password, host, dbname, port = 3306, engine="MySQL",
                 .qlog = NULL, .show_warn = FALSE){
    if(is.null(user) || is.null(password) || is.null(host) || is.null(port)
       || is.null(dbname)){
        stop("Missing an argument")
    }

    structure(data.frame(user = user, password = password, host = host,
            port = port, dbname = dbname),
             class="db_credentials", show_warn = FALSE, force_log = .qlog)
}

# Create a unique identifier string for a db_credentials object
cred_id <- function(cred){
    paste0(cred$user, "@", cred$host, ":", cred$port, "/", cred$dbname)
}

#' Print db_credentials object
#'
#' Print a db_credentials object, traditionally hiding the password.
#'
#' @param x db_credentials to print.
#' @param show_password show password, defaults to \code{FALSE}.
#' @param ... ignored.
#'
#' @export
print.db_credentials <- function(x, show_password=FALSE, ...){
    res <- paste0("db_cred: ", cred_id(x))
    if(show_password){
        res <- paste0(res, " using \"", x$password, "\"")
    }
    cat(res,"\n")
    invisible(x)
}
