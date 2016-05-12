create_con <- function(cred){
    assert_is_character(cred$engine)

    if (cred$engine == "MySQL"){
        RMySQL::dbConnect(RMySQL::MySQL(),
                         user = cred$user,
                         password = cred$password,
                         host = cred$host,
                         port = cred$port,
                         dbname = cred$dbname)
    } else if (cred$engine == "PostgreSQL"){
        RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                       user = cred$user,
                       password = cred$password,
                       host = cred$host,
                       port = cred$port,
                       dbname = cred$dbname)
    }
}

disconnect_con <- function(conn){
    DBI::dbDisconnect(conn)
}




is_valid_con <- function(conn){
    tryCatch({
        DBI::dbGetQuery(conn, "SELECT TRUE")
        return(TRUE)
    },
    error = function(e){
        return(FALSE)
    })
}
