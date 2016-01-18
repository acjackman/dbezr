context("connections")

test_database <- function(json_string){
    if (json_string != ""){
        # Try and establish connection
        tryCatch({
            .cred <- cred_json(json_string)
        },
        error=function(e){
            skip("Malformed JSON, can't extract credentials")
        })
        tryCatch({
            disconnect_con(create_con(.cred))
        },
        error=function(e){
            skip("Unable to connect database")
        })
    } else{
        skip("No test database provided")
    }
}

test_database_mysql <- function(){
    test_database(Sys.getenv("DBEZR_TEST_MYSQL")) # nolint
}

test_database_postgres <- function(){
    test_database(Sys.getenv("DBEZR_TEST_POSTGRES")) # nolint
}

test_that("connect to MySQL test database", {
    test_database_mysql()
    expect_true(TRUE)
})

test_that("connect to the Postgres test database",{
    test_database_postgres()
    expect_true(TRUE)
})
