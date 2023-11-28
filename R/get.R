#' @rdname get
#'
#' @title Retrieve Ontologies, Terms, and their Relatives
#'
#' @description `get_ontologies()` queries the OLS for information on
#'     all available ontologies.
#'
#' @details
#'
#' The functions documented on this page provide a programmatic
#' interface to the EMBL-EBI Ontology Lookup Service at
#' \url{https://www.ebi.ac.uk/ols4/}. The API is described at
#' \url{https://www.ebi.ac.uk/ols4/help}
#'
#' The functions use an on-disk cache of results retrieved from the
#' internet to speed interactive analysis. Generally, initial queries
#' are 'slow', but subsequent identical queries are very fast. Details
#' of the cache, some edge cases where the cache can get in the way of
#' current results, and strategies for cache management are summarized
#' in the 'Cache Management' section of the vignette.
#'
#' In an attempt to simplify navigation of results, values returned
#' from API calls are presented as a `tibble` with invariant columns
#' removed.
#'
#' Data returned by the OLS is often hierarchical, resulting in `list`
#' columns in the tibble. In some cases (e.g., `get_ontologies()`) the
#' list columns have been un-nested (using `tidyr::unnest_wider()`) to
#' provide users with relevant information. Downstream processing
#' steps may also find it beneficial to understand 'tidy' approaches
#' to working with hierarchical data in tibbles, as outlined in
#' chapter 23 of 'R For Data Science' (2e),
#' \url{https://r4ds.hadley.nz/rectangling}. This is illustrated in
#' the "Hierarchical Data" section of the vignette.
#'
#' @return
#'
#' The `get_*()` functions return tibbles summarizing information
#' retrieved from the OLS. The meaning of individual columns is as in
#' the service; columns are *not* renamed, but have been re-ordered to
#' prioritize useful information.
#'
#' @examples
#' onto <- get_ontologies()
#' onto
#'
#' @importFrom dplyr select filter mutate where any_of starts_with
#'     everything .data
#'
#' @export
get_ontologies <-
    function()
{
    ## information redundant with 'config'
    drop_columns <- c("ontologyId", "version", "baseUris", "namespace")
    ols4_request_perform("/api/ontologies") |>
        ## invariant columns 'lang', 'status', 'message', 'fileHash',
        ## 'loadAttempts'
        select(where(is_varying_column)) |>
        select(!any_of(drop_columns)) |>
        unnest_wider("config") |>
        select(
            "id", "title", "description", "version", starts_with("number"),
            everything()
        )
}

#' @rdname get
#'
#' @description `get_ontology()` retrieves
#'     information on a single ontology.
#'
#' @param ontology character(1) `id` (from `get_ontologies()`) of the
#'     ontology of interst.
#'
#' @examples
#'
#' get_ontology("cl") |>
#'     glimpse()
#'
#' @export
get_ontology <-
    function(ontology)
{
    path <- glue("/api/ontologies/{ontology}")
    ols4_request_perform(path)
}

#' @rdname get
#'
#' @description `get_roots()` and `get_terms()` return all 'roots' and
#'     terms in an ontology.
#'
#' @examples
#' get_roots("cl")
#'
#' @importFrom dplyr mutate
#'
#' @export
get_roots <-
    function(ontology)
{
    stopifnot(
        is_ontology(ontology)
    )

    path <- glue("/api/ontologies/{ontology}/terms/roots")
    ols4_request_perform(path) |>
        select(where(is_varying_column)) |>
        mutate(description = as_description_string(.data$description)) |>
        select("obo_id", "label", "description", everything())
}

#' @rdname get
#'
#' @param all_ontologies logical(1) when `FALSE` (default), only
#'     terms, parents, etc., defined in `ontology` are returned. When
#'     `TRUE`, terms from all ontologies are associated with terms in
#'     `ontology` are returned.
#'
#' @examples
#' terms <- get_terms("cl")
#' terms
#'
#' @export
get_terms <-
    function(ontology, all_ontologies = FALSE)
{
    stopifnot(
        is_ontology(ontology)
    )

    path <- glue("/api/ontologies/{ontology}/terms")
    drop_columns <- c(
        "is_obsolete",
        if (!all_ontologies) "is_defining_ontology"
    )
    ols4_request_perform(path) |>
        filter(
            !.data$is_obsolete,
            all_ontologies | .data$is_defining_ontology
        ) |>
        select(where(is_varying_column)) |>
        select(!any_of(drop_columns)) |>
        select("label", "obo_id", "description", everything()) |>
        mutate(description = as_description_string(.data$description))
}

#' @rdname get
#'
#' @description `get_term()` returns a tibble with detailed
#'     information about a single term.
#'
#' @param id character(1) the term identifier, usually 'obo_id' (e.g.,
#'     "CL:0002494") but for `get_term()` as specified by `form`.
#'
#' @param form character(1) the form of the identifier, as describe in Details.
#'
#' @details
#'
#' For `get_term()`, the identifier `id` can be one of three
#' forms. The `id` and `obo_id` forms are synonyms and follow the
#' pattern ontology abbrevation, `:`, and term id, e.g., "CL:0002494".
#' The `short_form` is typically like `obo_id` but with `:` replaced
#' by `_`. An `iri` is the purl resource locator, typically
#' "http://purl.obolibrary.org/...".
#'
#' @examples
#' CL0002494 <- get_term("cl", "CL:0002494")
#' CL0002494 |>
#'     glimpse()
#'
#' @export
get_term <-
    function(ontology, id, form = c("id", "obo_id", "short_form", "iri"))
{
    ontology <- tolower(ontology)
    form <- match.arg(form)
    path <- glue("/api/terms/findByIdAndIsDefiningOntology?{form}={id}")
    term <- ols4_request_perform(path)

    term |>
        select("obo_id", "label", "description", everything()) |>
        mutate(description = as_description_string(.data$description))
}

get_relatives <-
    function(ontology, id, relation)
{
    relations <- c(
        "parents", "children", "ancestors", "descendants",
        "hierarchicalAncestors", "hierarchicalDescendants"
    )
    stopifnot(
        is_ontology(ontology),
        is_string(id),
        is_string(relation),
        relation %in% relations
    )

    path <- glue("/api/ontologies/{ontology}/{relation}?id={id}")
    ols4_request_perform(path)
}

clean_relatives <-
    function(relatives, all_ontologies)
{
    if (!NROW(relatives))
        return(relatives)

    drop_columns <- c(
        "lang", "ontology_name", "ontology_prefix", "ontology_iri",
        "is_preferred_root", "is_obsolete", "term_replaced_by",
        if (!all_ontologies) "is_defining_ontology"
    )

    relatives |>
        filter(
            !.data$is_obsolete,
            all_ontologies | .data$is_defining_ontology
        ) |>
        select(!any_of(drop_columns)) |>
        select("obo_id", "label", "description", everything()) |>
        mutate(description = as_description_string(.data$description))
}

get_clean_relatives <-
    function(ontology, id, relation, all_ontologies)
{
    stopifnot(
        is_TRUEorFALSE(all_ontologies)
    )
    relatives <- get_relatives(ontology, id, relation)
    clean_relatives(relatives, all_ontologies)
}

#' @rdname get
#'
#' @description `get_parents()`, `get_ancestors()`, `get_children()`,
#'     and `get_descendants()` retrieve parents, ancestors, children
#'     and descedants of a single term.
#'
#' @details
#'
#' When no relatives are found, `get_parents()` etc. return a tibble
#' with 0 rows (and sometimes 0 columns).
#'
#' @examples
#' get_parents("cl", "CL:0002350")
#'
#' @export
get_parents <-
    function(ontology, id, all_ontologies = FALSE)
{
    get_clean_relatives(ontology, id, "parents", all_ontologies)
}

#' @rdname get
#'
#' @examples
#' get_ancestors("cl", "CL:0002494")
#' get_ancestors("cl", "CL:0002350")
#'
#' @export
get_ancestors <-
    function(ontology, id, all_ontologies = FALSE)
{
    get_clean_relatives(ontology, id, "ancestors", all_ontologies)
}

#' @rdname get
#'
#' @examples
#' get_children("cl", "CL:0002494")
#' get_children("cl", "CL:0002350")    # no children, 0 x 0 tibble
#'
#' @export
get_children <-
    function(ontology, id, all_ontologies = FALSE)
{
    get_clean_relatives(ontology, id, "children", all_ontologies)
}

#' @rdname get
#'
#' @examples
#' get_descendants("cl", "CL:0002494")
#' get_descendants("cl", "CL:0002350") # no descedants, 0 x 0 tibble
#'
#' @export
get_descendants <-
    function(ontology, id, all_ontologies = FALSE)
{
    get_clean_relatives(ontology, id, "descendants", all_ontologies)
}
