#' @importFrom jsonlite parse_json
jmespath_to_r <-
    function(data, path, ..., simplify = FALSE)
{
    txt <- jmespath(data, path, ...)
    jsonlite::parse_json(txt, simplifyVector = simplify)
}
