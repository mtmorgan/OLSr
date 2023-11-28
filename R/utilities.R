## assertions; assigned in .onLoad

is_string <-
    is_character <-
        is_number <-
            is_TRUEorFALSE <-
                NULL

is_ontology <-
    function(x)
{
    is_string(x) &&
        tolower(x) %in% get_ontologies()$id
}

## predicates for filter and select(which(...))

is_varying_column <-
    function(x)
{
    ## 0- and 1-length columns 'vary'? i.e., are not filtered
    if (length(x) <= 1L) {
        TRUE
    } else {
        length(unique(x)) != 1
    }
}

## transformations for mutate()

as_description_string <-
    function(x)
{
    string <- vapply(x, toString, character(1))
    string[!nzchar(string)] <- NA_character_
    string
}

## user agent for http requests

pkg_user_agent <-
    function()
{
    file_path <- system.file(package = PACKAGE_NAME, "DESCRIPTION")
    dcf <- read.dcf(file_path, c("Package", "Version", "URL"))
    glue("{Package}/{Version} ({URL})", .envir = as.data.frame(dcf))
}

## pretty-printin

select_some <- NULL # assigned in .onLoad
