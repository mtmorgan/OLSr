## assertions; assigned in .onLoad

is_string <-
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
