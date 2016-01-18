context("db_credentials")

test_that("cred creates a credentials object",{
    usr <- "bob"
    pswd <- "passwd"
    host <- "127.0.0.1"
    dbname <- "foo"
    port <- 3306
    engine <- "MySQL"

    expect_equal(cred(usr, pswd, host, dbname, port, engine),
        structure(data.frame(user = usr, password = pswd, host = host,
                    port = port, dbname = dbname, engine = engine),
             class="db_credentials", show_warn = FALSE, .qlog = NULL))
})

test_that("cred defaults engine to MySQL", {
    usr <- "bob"
    pswd <- "passwd"
    host <- "127.0.0.1"
    dbname <- "foo"
    port <- 3306
    engine <- "MySQL"

    expect_equal(cred(usr, pswd, host, dbname, port),
        structure(data.frame(user = usr, password = pswd, host = host,
                    port = port, dbname = dbname, engine = engine),
             class="db_credentials", show_warn = FALSE, .qlog = NULL))
})

test_that("cred defaults port to 3306 if not specified on MySQL Engine", {
    usr <- "bob"
    pswd <- "passwd"
    host <- "127.0.0.1"
    dbname <- "foo"
    port <- 3306
    engine <- "MySQL"

    expect_equal(cred(usr, pswd, host, dbname, engine=engine),
        structure(data.frame(user = usr, password = pswd, host = host,
                port = port, dbname = dbname, engine = engine),
             class="db_credentials", show_warn = FALSE, .qlog = NULL))
})

test_that("db_credentials print properly", {
    crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)

    expect_output(crd, "db_cred: bob@127.0.0.1:3306/foo")
})

test_that("db_credentials print with paswd", {
    crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)

    expect_output(crd, "db_cred: bob@127.0.0.1:3306/foo")
})
