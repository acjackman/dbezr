context("connections")

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


test_that("connect to MySQL test database", {
    json_string <- Sys.getenv("DBEZR_TEST_MYSQL")
    if (json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
                expect_true(TRUE)
            },
            error = function(e){
                fail(
                    paste0("Unable to connect. ", capture.output(print(.cred)))
                )
            })
        },
        error = function(e){
            fail("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No MySQL test database provided")
    }
})

test_that("connect to PostgreSQL test database", {
    json_string <- Sys.getenv("DBEZR_TEST_POSTGRES")
    if (json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
                expect_true(TRUE)
            },
            error = function(e){
                fail(
                    paste0("Unable to connect. ", capture.output(print(.cred)))
                )
            })
        },
        error = function(e){
            fail("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No PostgreSQL test database provided")
    }
})


test_that("is_valid_con tests valid connection for MySQL", {
    .cred <- mysql_cred()

    con <- create_con(.cred)
    expect_true(is_valid_con(con))

    disconnect_con(con)
    expect_false(is_valid_con(con))
})

test_that("is_valid_con tests valid connection for PostgreSQL", {
    .cred <- pgsql_cred()

    con <- create_con(.cred)
    expect_true(is_valid_con(con))

    disconnect_con(con)
    expect_false(is_valid_con(con))
})
