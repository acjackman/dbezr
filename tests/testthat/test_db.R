context("registered_db - db")

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

test_that("registration environment exists", {
    expect_true(exists("registered_dbs",  asNamespace("dbezr")))
    expect_true(exists("dbezr_set", asNamespace("dbezr")))
    expect_true(exists("db", dbezr:::dbezr_set))
})

test_that("db can be created from a credentials object and then removed", {
    .cred <- pgsql_cred()

    tm <- lubridate::now()
    con <- create_con(.cred)

    tryCatch({
        with_mock(
            `lubridate::now` = function() tm,
            `create_con` = function(x) con,

            # nolint start
            # register the credentials
            {db_cred <- db_cred(.cred)},

            # Returned credentials should have an updated timestamp
            {up_cred <- .cred},
            {attr(up_cred, "reg_tmstp") <- lubridate::now()},
            # nolint end

            expect_equal(db_cred, up_cred),

            # They should show be in the registered environment,
            expect_true(exists(cred_id(.cred), dbezr:::registered_dbs)),

            # object structure should match
            expect_equivalent(dbezr:::registered_dbs[[cred_id(.cred)]],
                structure(
                    list(
                        cred = .cred,
                        connection = create_con(cred),
                        created = lubridate::now(),
                        trans_active = FALSE
                    ), class="dbchk_results")
            ),
            { rm_db(db_cred) }, # nolint
            expect_true(length(ls(dbezr:::registered_dbs)) == 0)
        )
    }
    , finally = {
        tryCatch({disconnect_con(con)}, error= function(e) pass()) # nolint
    })
})

test_that("rm_db_id removes database from registered list", {
    .cred <- pgsql_cred()

    tm <- lubridate::now()
    con <- create_con(.cred)

    tryCatch({
        with_mock(
            `lubridate::now` = function() tm,
            `create_con` = function(x) con,
            # register the credentials
            {db_cred <- db_cred(.cred)}, # nolint

            # Verify there's something to remove
            expect_true(exists(cred_id(.cred), dbezr:::registered_dbs)),
            expect_silent(rm_db_id(cred_id(db_cred))),
            expect_true(length(ls(dbezr:::registered_dbs))  == 0),
            expect_false(is_valid_con(con))
        )
    }
    , finally = {
        tryCatch({disconnect_con(con)}, error= function(e) pass()) # nolint
    })
})

test_that("rm_db removes database from registered list", {
    .cred <- pgsql_cred()

    tm <- lubridate::now()
    con <- create_con(.cred)

    tryCatch({
        with_mock(
            `lubridate::now` = function() tm,
            `create_con` = function(x) con,

            # nolint start
            # register the credentials
            {db_cred <- db_cred(.cred)},

            # nolint end

            # Verify there's something to remove
            expect_true(exists(cred_id(.cred), dbezr:::registered_dbs)),
            expect_silent(rm_db(db_cred)),
            expect_true(length(ls(dbezr:::registered_dbs))  == 0),
            expect_false(is_valid_con(con))
        )
    }
    , finally = {
        tryCatch({disconnect_con(con)}, error= function(e) pass()) # nolint
    })
})

test_that("rm_db_all removes all databases from the list", {
    .cred_p <- db_cred(pgsql_cred())
    .cred_m <- db_cred(mysql_cred())

    expect_true(exists(cred_id(.cred_p), dbezr:::registered_dbs))
    expect_true(exists(cred_id(.cred_m), dbezr:::registered_dbs))

    expect_silent(rm_db_all())

    expect_true(length(ls(dbezr:::registered_dbs)) == 0)
})
