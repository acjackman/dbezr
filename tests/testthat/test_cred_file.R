context("db_credentials - cred_file")

clst <- list(
    user = "bob",
    password = "passwd",
    host = "127.0.0.1",
    port = 500,
    dbname = "foo",
    engine = "MySQL")

path <- "test_file.json"

test_that("cred_file throws error if file doesn't exist", {
    expect_error(cred_file(path))
})

test_that("cred_file reads from a file",{
    writeLines(jsonlite::toJSON(clst, pretty=TRUE), path)

    expect_equal(cred_file(path),
        structure(
            data.frame(
                user = clst$user, password = clst$password, host = clst$host,
                port = clst$port, dbname = clst$dbname,
                engine = clst$engine),
            class="db_credentials", show_warn = FALSE, force_log = NA)
    )

    file.remove(path)
})
