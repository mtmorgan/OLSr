#' @importFrom httr2 request req_user_agent req_perform
#'     resp_check_status resp_body_string
#'
#' @importFrom memoise memoise
ols4_request_one <-
    function(url)
{
    stopifnot(is_string(url))

    response <-
        request(url) |>
        req_user_agent(pkg_user_agent()) |>
        req_perform()

    body <-
        response |>
        resp_check_status() |>
        resp_body_string()

    body
}

#' @importFrom glue glue
#'
#' @importFrom progress progress_bar
#'
#' @importFrom rjsoncons jmespath
#'
#' @importFrom dplyr bind_rows
ols4_request_perform <-
    function(path, page = 0L, size = 500L)
{
    OLS4 <- "https://www.ebi.ac.uk/ols4"
    stopifnot(
        is_string(path),
        is_number(page), page >= 0,
        is_number(size), size > 0, size <= 500
    )

    url <- glue(
        OLS4, path,
        ## append '?size=' or '&size=' as appropriate
        ifelse(grepl("?", path, fixed = TRUE), "&", "?"),
        "page={page}&size={size}"
    )

    pb <- NULL
    all_responses <- NULL
    repeat {
        response <- ols4_request_one(url)
        if (is.null(all_responses)) {
            total_pages <- jmespath(response, "page.totalPages", as = "R")
            if (is.null(total_pages)) { # not paged
                all_responses[[1]] <- ols4_response_to_tbl(response)
                break
            }
            if (total_pages == 0L)      # no results
                break
            all_responses <- vector("list", total_pages)
            pb <- progress_bar$new(total = total_pages)
        }

        page_number <- # 1-based R
            jmespath(response, "page.number", as = "R") + 1L
        all_responses[[page_number]] <- ols4_response_nested_to_tbl(response)
        pb$tick()

        if (!jmespath(response, "contains(keys(_links), 'next')", as = "R"))
            break
        url <- jmespath(response, "_links.next.href")
    }

    bind_rows(all_responses)
}

#' @importFrom jsonlite parse_json
#'
#' @importFrom dplyr as_tibble all_of
#'
#' @importFrom tidyr unnest_wider
#'
## hierarchical data, e.g., 'onologities' in get_ontologies()
ols4_response_nested_to_tbl <-
    function(data)
{
    stopifnot(
        is_string(data)
    )

    key <- jmespath(data, "keys(_embedded)", as = "R")
    if (!is_string(key)) {
        txt <- spdl::fmt(
            "expected length 1 'key', but is length {}", length(key)
        )
        spdl::error(txt)
        stop(txt)
    }

    tbl <-
        parse_json(data)[["_embedded"]] |>
        as_tibble() |>
        unnest_wider(col = all_of(key))

    tbl
}

## unnested data
ols4_response_to_tbl <-
    function(data)
{
    stopifnot(
        is_string(data)
    )

    tbl <-
        parse_json(data) |>
        ## replace top-level 'NULL' values with NA
        lapply(\(x) ifelse(is.null(x), NA, x)) |>
        as_tibble()

    tbl
}
