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
#' @return a \code{db_credentials} object
#'
#' @examples
#' cred("bob","passwd","127.0.0.1","foo")
#'
#' @importFrom assertive.types assert_is_character
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_numeric
#' @export
cred <- function(user, password, host, dbname, port = NA, engine = NA,
                 .qlog = NA, .show_warn = NA){

    assert_is_character(user)
    assert_is_character(password)
    assert_is_character(host)
    assert_is_character(dbname)

    # Set defaults
    .show_warn <- ifelse(is.na(.show_warn), FALSE, .show_warn)
    assert_is_a_bool(.show_warn)

    engine <- ifelse(!(engine %in% c("MySQL", "PostgreSQL")), "MySQL", engine)
    if (engine == "MySQL") {
        port <- ifelse(is.na(port), 3306, as.numeric(port))
        assert_is_numeric(port)
    } else if (engine == "PostgreSQL"){
        port <- ifelse(is.na(port), 5432, as.numeric(port))
        assert_is_numeric(port)
    } else{
        stop("Invalid database engine specified")
    }

    structure(list(user = user, password = password, host = host,
            port = port, dbname = dbname, engine = engine),
            class = "db_credentials", show_warn = .show_warn, force_log = .qlog)
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
    if (show_password) {
        res <- paste0(res, " using \"", x$password, "\"")
    }
    cat(res, "\n")
    invisible(x)
}

#' Create database credentials from JSON
#'
#' Retrieve database credentials from a structured JSON and create a
#' db_credentials object
#'
#' @param json JSON credentials string.
#'
#' @examples
#' json <- paste0('{"user": "usr", "password": "pass123", "host": "127.0.0.1",',
#'                ' "dbname": "foo"}')
#' foo <- cred_json(json)
#' @rdname cred
#' @export
cred_json <- function(json){
    j <- tryCatch(jsonlite::fromJSON(json), error = function(e){
        stop("Problem parsing JSON please check it for syntax.",
             call. = FALSE)
    })

    required_fields <- c("user", "password", "dbname", "host")

    if (!all(required_fields %in% names(j))){
        stop("String is missing a required parameter.",
             "Please check that it specifies",
             "a username, password, host, and dbname. Port defaults to 3306")


    }

    optional_fields <- c("port", "engine", ".qlog", ".show_warn")

    plyr::l_ply(optional_fields, .fun = function(x) {
        if (is.null(j[[x]])) j[[x]] <<- NA
    })

    cred(
        user = j$user,
        password = j$password,
        host = j$host,
        dbname = j$dbname,
        port = j$port,
        engine = j$engine,
        .qlog = j$.qlog,
        .show_warn = j$.show_warn)

}

#' Retrieve database credentials from a file
#'
#' Retrieve database credentials from a file. Uses cred_json to parse.
#'
#' @param file filepath to JSON credentials file
#'
#' @rdname cred
#' @export
cred_file <- function(file){
    if (!file.exists(file)) {
        stop("Credentials file does not exist.")
    }

    # jsonlite package will handle being handed a file,
    # so there's nothing else to do
    cred_json(file)
}

#' Retrive database credentials from a system environment variable
#'
#' Retrieve database credentials from a system environment variable.
#' Uses cred_json to parse.
#'
#' @param var_name system variable name
#'
#' @rdname cred
#' @export
cred_env <- function(var_name){
    j <- Sys.getenv(var_name)
    if (j == "") {
        stop(paste0(var_name, " is not set"))
    }
    cred_json(j)
}
