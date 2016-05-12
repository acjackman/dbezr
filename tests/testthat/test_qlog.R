context("log_query - qlog")

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

crd <- pgsql_cred()
crd_log <- crd
crd_log$force_log <- TRUE
crd_nlg <- crd
crd_nlg$force_log <- FALSE

test_that("qlog_loud overrides package logging default", {
    current_setting <- getOption("dbezr.force_log")

    # Turn logging on by default
    options(list("dbezr.force_log" = TRUE))
    expect_true(attr(qlog_loud(crd), "force_log"))

    # Turn logging off by default
    options(list("dbezr.force_log" = FALSE))
    expect_true(attr(qlog_loud(crd), "force_log"))

    # reset setting
    options(list("dbezr.force_log" = current_setting))
})

test_that("qlog_loud overrides connection logging default", {
    expect_true(attr(qlog_loud(crd), "force_log"))
    expect_true(attr(qlog_loud(crd_nlg), "force_log"))
})

test_that("qlog_silent overrides package logging default", {
    current_setting <- getOption("dbezr.force_log")

    # Turn logging on by default
    options(list("dbezr.force_log" = TRUE))
    expect_false(attr(qlog_silent(crd), "force_log"))

    # Turn logging off by default
    options(list("dbezr.force_log" = FALSE))
    expect_false(attr(qlog_silent(crd), "force_log"))

    # reset setting
    options(list("dbezr.force_log" = current_setting))
})

test_that("qlog_silent overrides connection logging default", {
    expect_false(attr(qlog_silent(crd), "force_log"))
    expect_false(attr(qlog_silent(crd_log), "force_log"))
})
