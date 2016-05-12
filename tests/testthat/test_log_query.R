context("log_query")

crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)
crd_log <- cred("bob", "passwd", "127.0.0.1", "foo", 3306, .qlog = TRUE)
crd_nlg <- cred("bob", "passwd", "127.0.0.1", "foo", 3306, .qlog = FALSE)
query_string <- "SHOW TABLES"

test_that("Log prints properly when forced", {
    tm <- lubridate::now()

    with_mock(
        `lubridate::now` = function() tm,
        `dbezr:::create_con` = function(x) con,

        # nolint start
        # Build answer string
        {
            # Correct answer is a regular expression
            cor_ans <- paste0("Running Query \\(", format(lubridate::now()),
                              " - ", cred_id(crd_log), "\\):\n", query_string,
                              ";")
        },
        # nolint end
        expect_output(log_query(query_string, crd_log), cor_ans)
    )
})

test_that("Log does not print when suppressed", {
    expect_silent(log_query(query_string, crd_nlg))
})

test_that("Log follows option if not specified", {
    current_setting <- getOption("dbezr.force_log")

    # Turn logging on by default
    options(list("dbezr.force_log" = TRUE))
    tm <- lubridate::now()

    with_mock(
        `lubridate::now` = function() tm,
        `dbezr:::create_con` = function(x) con,

        # nolint start
        # Build answer string
        {
            # Correct answer is a regular expression
            cor_ans <- paste0("Running Query \\(", format(lubridate::now()),
                              " - ", cred_id(crd_log), "\\):\n", query_string,
                              ";")
        },
        # nolint end
        expect_output(log_query(query_string, crd_log), cor_ans)
    )

    # Turn logging off by default
    options(list("dbezr.force_log" = FALSE))
    expect_silent(log_query(query_string, crd_nlg))

    # reset setting
    options(list("dbezr.force_log" = current_setting))
})

test_that("Log maintains newlines in query ", {
    query_string <- "SELECT a, b, c\n FROM tbl"
    tm <- lubridate::now()

    with_mock(
        `lubridate::now` = function() tm,
        `dbezr:::create_con` = function(x) con,

        # nolint start
        # Build answer string
        {
            # Correct answer is a regular expression
            cor_ans <- paste0("Running Query \\(", format(lubridate::now()),
                              " - ", cred_id(crd_log), "\\):\n", query_string,
                              ";")
        },
        # nolint end
        expect_output(log_query(query_string, crd_log), cor_ans)
    )
})
