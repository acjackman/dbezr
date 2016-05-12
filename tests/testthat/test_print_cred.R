context("db_credentials - print.cred")

crd <- cred("bob", "passwd", "127.0.0.1", "foo", 3306)

test_that("cred objects print properly", {
    expect_output(print(crd), "db_cred: bob@127.0.0.1:3306/foo")
})

test_that("cred objects print with paswd", {
    expect_output(print(crd), "db_cred: bob@127.0.0.1:3306/foo")
})
