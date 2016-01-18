context("db_credentials - print.cred")

test_that("cred objects print properly", {
    crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)

    expect_output(crd, "db_cred: bob@127.0.0.1:3306/foo")
})

test_that("cred objects print with paswd", {
    crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)

    expect_output(crd, "db_cred: bob@127.0.0.1:3306/foo")
})
