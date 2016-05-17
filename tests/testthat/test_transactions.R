context("Transactions")

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

test_that("Transactions can rollback with Postgres", {
    psql_db <- pgsql_cred()
    psql_id <- cred_id(psql_db)

    num_tbls <- function(db){
    qry <- "SELECT * FROM pg_catalog.pg_tables where schemaname = 'public'"
    nrow(gq(qry, .db = qlog_silent(psql_db)))
    }

    # Ensure there are no tables
    gq("drop schema public cascade", .db = qlog_silent(psql_db))
    gq("create schema public;", .db = qlog_silent(psql_db))
    expect_equal(num_tbls(psql_db), 0)
    expect_false(registered_dbs[[psql_id]]$trans_active)

    # Start the transaction
    trans_begin(.db = psql_db)

    expect_true(registered_dbs[[psql_id]]$trans_active)

    gq("CREATE TABLE tmp (foo integer)", .db = qlog_silent(psql_db))
    # Table should be created
    expect_equal(num_tbls(psql_db), 1)

    # Rollback trasaction
    trans_rollback(.db = psql_db)

    expect_equal(num_tbls(psql_db), 0)
    expect_false(registered_dbs[[psql_id]]$trans_active)

})

test_that("Transactions can commit with Postgres", {
    psql_db <- pgsql_cred()
    psql_id <- cred_id(psql_db)

    num_tbls <- function(db){
    qry <- "SELECT * FROM pg_catalog.pg_tables where schemaname = 'public'"
    nrow(gq(qry, .db = qlog_silent(psql_db)))
    }

    # Ensure there are no tables
    gq("drop schema public cascade", .db = qlog_silent(psql_db))
    gq("create schema public;", .db = qlog_silent(psql_db))
    expect_equal(num_tbls(psql_db), 0)
    expect_false(registered_dbs[[psql_id]]$trans_active)

    # Start the transaction
    trans_begin(.db = psql_db)

    expect_true(registered_dbs[[psql_id]]$trans_active)

    gq("CREATE TABLE tmp (foo integer)", .db = qlog_silent(psql_db))
    # Table should be created
    expect_equal(num_tbls(psql_db), 1)

    # Rollback trasaction
    trans_commit(.db = psql_db)

    expect_equal(num_tbls(psql_db), 1)
    expect_false(registered_dbs[[psql_id]]$trans_active)

    gq("DROP TABLE tmp", .db = qlog_silent(psql_db))
})
