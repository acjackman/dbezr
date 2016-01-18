context("db_credentials - cred_json")

clst <- list(
    user = "bob",
    password = "passwd",
    host = "127.0.0.1",
    port = 3306,
    dbname = "foo",
    engine = "MySQL")

test_that("cred_json parses string", {
    js <- as.character(jsonlite::toJSON(clst))
    expect_equal(cred_json(js),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})

test_that("cred_json throws error for empty ", {
    expect_error(cred_json(jsonlite::toJSON(list())))
})

test_that("cred_json allows setting .qlog", {

    jtrue <- paste0('{"user": "bob", "password": "passwd",',
        '"host": "127.0.0.1", "port": 3306, "dbname": "foo",',
        '"engine":"MySQL",".qlog": true}', collapse="")

    expect_equal(cred_json(jtrue),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = TRUE)
    )
})

test_that("cred_json allows setting .show_warn", {
    jtrue <- paste0('{"user": "bob", "password": "passwd",',
        '"host": "127.0.0.1", "port": 3306, "dbname": "foo",',
        '"engine":"MySQL",".show_warn": true}', collapse="")

    expect_equal(cred_json(jtrue),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = TRUE, force_log = NA)
    )
})

test_that("cred_json accepts port with character type ", {
    jtrue <- paste0('{"user": "bob", "password": "passwd",',
        '"host": "127.0.0.1", "port": "3306", "dbname": "foo",',
        '"engine":"MySQL"}', collapse="")

    expect_equal(cred_json(jtrue),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )
})
