registered_dbs <- new.env(hash=TRUE)
dbezr_set <- new.env(hash=TRUE)
assign("db", NA, envir=dbezr_set)

.onLoad <- function(libname, pkgname) {
    reg.finalizer(registered_dbs, f=function(x) rm_db_all(), onexit = TRUE)

    op <- options()

    # Set package options if the don't already exist
    op.dbezr <- list(
        # default to not logging queries to the console if dbezr's not running
        # interactively
        dbezr.force_log = interactive(),

        # Specify lubridate compatible time_zone for converting dates
        # from database
        dbezr.timezone = "UTC"
    )

    toset <- !(names(op.dbezr) %in% names(op))
    if(any(toset)) options(op.dbezr[toset])

    invisible()
}
