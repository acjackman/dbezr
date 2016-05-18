context("database info")

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

test_that("Get table list Postgres", {
    psql_db <- pgsql_cred()
    psql_id <- cred_id(psql_db)

    # Ensure there are no tables
    trans_begin(.db = psql_db)
    gq("drop schema public cascade", .db = qlog_silent(psql_db))
    gq("create schema public;", .db = qlog_silent(psql_db))

    expect_true(is.data.frame(shw_tbls(.db = psql_db)))
    expect_equal(nrow(shw_tbls(.db = psql_db)), 0)

    gq("CREATE TABLE tmp (foo integer)", .db = qlog_silent(psql_db))
    expect_equal(nrow(shw_tbls(.db = psql_db)), 1)

    # Rollback trasaction
    trans_rollback(.db = psql_db)
})

test_that("Get table list MySQL", {
    skip("Implement MySQL test")
})

test_that("Get Column list Postgres", {
    psql_db <- pgsql_cred()
    psql_id <- cred_id(psql_db)

    # Ensure there are no tables
    trans_begin(.db = psql_db)
    gq("drop schema public cascade", .db = qlog_silent(psql_db))
    gq("create schema public;", .db = qlog_silent(psql_db))

    gq("CREATE TABLE tmp (foo integer)", .db = qlog_silent(psql_db))

    col_tbl <- shw_cols("tmp", .db = psql_db)
    expect_true(is.data.frame(col_tbl))
    expect_equal(nrow(col_tbl), 1)

    # Rollback trasaction
    trans_rollback(.db = psql_db)
})

test_that("Get column list MySQL", {
    skip("Implement MySQL test")
})
