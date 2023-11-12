PACKAGE_NAME <- NULL

#' @importFrom cachem cache_disk
#'
#' @importFrom memoise memoise
.onLoad <-
    function(libname, pkgname)
{
    PACKAGE_NAME <<- pkgname

    ## cache web requests, and aggregate web requests
    ols4_request_one <<- memoise(ols4_request_one, cache = cache())
    ols4_request_perform <<- memoise(ols4_request_perform, cache = cache())

    ## assertions
    is_string <<- BiocBaseUtils::isScalarCharacter
    is_number <<- BiocBaseUtils::isScalarNumber
    is_TRUEorFALSE <<- BiocBaseUtils::isTRUEorFALSE
}
