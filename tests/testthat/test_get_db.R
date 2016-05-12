context("get_db")

mysql_cred <- function(){
    json_string <- Sys.getenv("DBEZR_TEST_MYSQL")
    if (json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
            },
            error = function(e){
                skip(
                    paste0("Unable to connect. ", capture.output(print(.cred)))
                )
            })
        },
        error = function(e){
            skip("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No MySQL test database provided")
    }
    .cred
}

pgsql_cred <- function(){
    json_string <- Sys.getenv("DBEZR_TEST_POSTGRES")
    if (json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
            },
            error = function(e){
                skip(
                    paste0("Unable to connect. ", capture.output(print(.cred)))
                )
            })
        },
        error = function(e){
            skip("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No Postgres test database provided")
    }
    .cred
}

test_that("get_db throws error if no db is registered", {
    rm_db_all()
    dbezr_set$db <- NULL
    expect_error(get_db(), "No database registered")
})

test_that("default db is set to retrieve", {
    reg_psql <- db_cred(pgsql_cred())
    reg_mysq <- db_cred(mysql_cred())

    expect_equal(dbezr_set$db, cred_id(reg_psql))
    rm_db_all()
})

test_that("get_db retrieves the default", {
    reg_psql <- db_cred(pgsql_cred())
    reg_mysq <- db_cred(mysql_cred())

    expect_equal(cred_id(get_db()), cred_id(reg_psql))
    rm_db_all()
})

test_that("get_db retrieves the passed through db", {
    reg_psql <- db_cred(pgsql_cred())
    reg_mysq <- db_cred(mysql_cred())

    expect_equal(cred_id(get_db(reg_mysq)), cred_id(reg_mysq))
    rm_db_all()
})
