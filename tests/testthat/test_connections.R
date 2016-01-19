context("connections")

test_that("connect to MySQL test database", {
    json_string <- Sys.getenv("DBEZR_TEST_MYSQL")
    if(json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
                expect_true(TRUE)
            },
            error=function(e){
                skip(paste0("Unable to connect. ",capture.output(print(.cred))))
            })
        },
        error=function(e){
            fail("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No MySQL test database provided")
    }
})

test_that("connect to PostgreSQL test database", {
    json_string <- Sys.getenv("DBEZR_TEST_POSTGRES")
    if(json_string != ""){
        tryCatch({
            .cred <- cred_json(json_string)
            tryCatch({
                disconnect_con(create_con(.cred))
                expect_true(TRUE)
            },
            error=function(e){
                skip(paste0("Unable to connect. ",capture.output(print(.cred))))
            })
        },
        error=function(e){
            fail("Malformed JSON, can't extract credentials")
        })
    } else{
        skip("No PostgreSQL test database provided")
    }
})
