context("query_building - prep")


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

test_that("Character column is cleaned", {
    rm_db_all()
    db_cred(pgsql_cred())

    # Single Value
    expect_equal(prep("Bob"), "'Bob'")

    # Vector
    expect_equal(prep(c("Bob", "George")), c("'Bob'", "'George'"))
})

test_that("Character column is cleaned properly with NULL values", {
    rm_db_all()
    db_cred(pgsql_cred())

    # Single Null Values
    expect_equal(prep(c(NA, "George", "Fred")), c("NULL", "'George'", "'Fred'"))
    expect_equal(prep(c("Bob", NA, "Fred")), c("'Bob'", "NULL", "'Fred'"))
    expect_equal(prep(c("Bob", "George", NA)), c("'Bob'", "'George'", "NULL"))

    # Whole Vector NULL
    expect_equal(prep(c(NA, NA)), c("NULL", "NULL"))
})

test_that("Character column is cleaned for possible Injection attacks", {
    rm_db_all()
    db_cred(pgsql_cred())

    expect_equal(prep("Robert'); DROP TABLE Students;--"),
                 "'Robert''); DROP TABLE Students;--'")
})


test_that("Date column is cleaned", {
    today <- parse_date("2016-01-02")
    today_s <- "'2016-01-02 00:00:00'"

    now <- parse_date_time("2016-01-03 04:05:06")
    now_s <- "'2016-01-03 04:05:06'"

    # Single Value
    expect_equal(prep(today), today_s)

    # Vector
    t_vect <- lubridate::with_tz(c(today, now), "UTC")
    expect_equal(prep(t_vect), c(today_s, now_s))
})

test_that("Date column is cleaned properly with NULL values", {
    today <- parse_date("2016-01-02")
    today_s <- "'2016-01-02 00:00:00'"

    now <- parse_date_time("2016-01-03 04:05:06")
    now_s <- "'2016-01-03 04:05:06'"

    # Single Null Values
    first_val <- lubridate::with_tz(c(now, today, now), "UTC")
    first_val[1] <- NA
    secnd_val <- lubridate::with_tz(c(today, NA, now), "UTC")
    third_val <- lubridate::with_tz(c(today, now, NA), "UTC")
    expect_equal(prep(first_val), c("NULL", today_s, now_s))
    expect_equal(prep(secnd_val), c(today_s, "NULL", now_s))
    expect_equal(prep(third_val), c(today_s, now_s, "NULL"))

    # Whole Vector NULL
    expect_equal(prep(c(NA, NA)), c("NULL", "NULL"))
})


test_that("Numeric column is cleaned", {
    # Single Value
    expect_equal(prep(1), "1")

    # Vector
    expect_equal(prep(c(1, 2, 3)), c("1", "2", "3"))
})

test_that("Double column is cleaned", {
    # Single Value
    expect_equal(prep(as.double(1)), "1")

    # Vector
    expect_equal(prep(as.double(c(1, 2, 3))), c("1", "2", "3"))
})

test_that("Integer column is cleaned", {
    # Single Value
    expect_equal(prep(1L), "1")

    # Vector
    expect_equal(prep(c(1L, 2L, 3L)), c("1", "2", "3"))
})

test_that("Numeric column is cleaned properly with NULL values", {
    # Single Null Values
    expect_equal(prep(c(NA, 2, 3)), c("NULL", "2", "3"))
    expect_equal(prep(c(1, NA, 3)), c("1", "NULL", "3"))
    expect_equal(prep(c(1, 2, NA)), c("1", "2", "NULL"))

    # Whole Vector NULL
    expect_equal(prep(c(NA, NA)), c("NULL", "NULL"))
})


test_that("Logical column is cleaned", {
    # Single Value
    expect_equal(prep(TRUE), "1")
    expect_equal(prep(FALSE), "0")

    # Vector
    expect_equal(prep(c(TRUE, FALSE)), c("1", "0"))
})

test_that("Logical column is cleaned properly with NULL values", {
    # Single Null Values
    expect_equal(prep(c(NA, FALSE)), c("NULL", "0"))
    expect_equal(prep(c(TRUE, NA)), c("1", "NULL"))

    # Whole Vector NULL
    expect_equal(prep(c(NA, NA)), c("NULL", "NULL"))
})
