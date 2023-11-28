PACKAGE_NAME <- NULL

#' @importFrom cachem cache_disk
#'
#' @importFrom memoise memoise
.onLoad <-
    function(libname, pkgname)
{
    PACKAGE_NAME <<- pkgname

    ## cache web requests, and aggregate web requests
    ## ols4_request_one <<- memoise(ols4_request_one, cache = cache())
    ols4_request_perform <<- memoise(ols4_request_perform, cache = cache())
    ## in-memory cache
    is_ontology <<- memoise(is_ontology)

    ## assertions
    is_string <<- BiocBaseUtils::isScalarCharacter
    is_character <<- BiocBaseUtils::isCharacter
    is_number <<- BiocBaseUtils::isScalarNumber
    is_TRUEorFALSE <<- BiocBaseUtils::isTRUEorFALSE
    select_some <<- BiocBaseUtils::selectSome
}
