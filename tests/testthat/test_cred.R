context("db_credentials - cred")

clst <- list(
    user = "bob",
    password = "passwd",
    host = "127.0.0.1",
    port = 3306,
    dbname = "foo",
    engine = "MySQL")

test_that("cred creates a credentials object",{

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      clst$port, clst$engine),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})


test_that("cred creates a credentials object with a character port",{
    char_port <- "8000"
    num_port <- 8000

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      char_port, clst$engine),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = num_port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})

test_that("cred defaults engine to MySQL", {

    expect_equal(
        cred(clst$user, clst$password, clst$host, clst$dbname, clst$port),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})

test_that("cred defaults port to 3306 if not specified on MySQL Engine", {

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      engine = clst$engine),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})

test_that("cred allows setting show_warn", {

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      engine = clst$engine, .show_warn = TRUE),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = TRUE, force_log = NA)
    )
})

test_that("cred allows setting .qlog to loud", {

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      engine = clst$engine, .qlog = TRUE),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = TRUE)
    )
})

test_that("cred allows setting .qlog to silent", {

    expect_equal(cred(clst$user, clst$password, clst$host, clst$dbname,
                      engine = clst$engine, .qlog = FALSE),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = FALSE)
    )
})
