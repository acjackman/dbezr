create_con <- function(cred){
    if(cred$engine == "MySQL"){
        RMySQL::dbConnect(RMySQL::MySQL(),
                         user = cred$user,
                         password = cred$password,
                         host = cred$host,
                         port = cred$port,
                         dbname= cred$dbname)
    }
}

disconnect_con <- function(conn){
    DBI::dbDisconnect(conn)
}

cred_env <- function(){
    cred_json(Sys.getenv("DBEZR_TEST_MYSQL"))
}
