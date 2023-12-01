#' @rdname cache
#'
#' @title OLSr Cache Management
#'
#' @description `cache_directory()` returns the on-disk location of
#'     the cache used by OLSr.
#'
#' @details
#'
#' OLSr uses an on-disk cache to improve performance and reliability.
#' Each query to the OLS server is cached on first use, so that
#' subsequent responses can be read from the local disk. This greatly
#' increases performance, and allows use of previously retrieved
#' resources when off-line.
#'
#' A risk of using a cache is that the cache contains an 'old' version
#' of the response. Circumvent this risk by manually manipulating the
#' cache, e.g., removing objects that a known to be too old.
#'
#' OLSr uses the memoise <https://CRAN.R-project.org/package=memoise>
#' and cachem <https://CRAN.R-project.org/package=cachem> packages to
#' implement the cache. The disk cache is implemented using cachem's
#' `cache_disk()` function using the default cache size (1 Gb) and
#' `"lsu"` ('least recently used') eviction policy. Objects remain in
#' the cache for 30 days.
#'
#' The cache is created in a package-specific directory, located at
#' `cache_directory()`.
#'
#' @return cache_directory() returns the character(1) path to the
#'     location of the cache on disk.
#'
#' @examples
#' cache_directory()
#'
#' @export
cache_directory <-
    function()
{
    tools::R_user_dir(PACKAGE_NAME, "cache")
}

#' @rdname cache
#'
#' @description `cache_summary()` reports the number and size of files
#'     in the cache, the oldest file, etc.
#'
#' @return
#'
#' `cache_summary()` returns a list with the following elements:
#'
#' - `directory`: the location of the cache, i.e., `cache_directory()`.
#'
#' - `n_files`: the number of files in the cache.
#'
#' - `cache_size`: the sum of file sizes in the cache.
#'
#' - `largest_file`, `largest_size`: the largest file and its size in
#'   the cache.
#'
#' - `oldest_file`, `oldest_age`: the name and age (as a `difftime`
#'   object) of the oldest (based on `ctime`) file in the path.
#' - `least_used_file`, `least_used_age`: the name and age of the file
#'   that was modified least recently, based on `mtime`.
#'
#' `print.cache_summary()` displays this information in a readable
#' way.
#'
#' @examples
#' cache_summary()
#'
#' @importFrom dplyr arrange desc slice
#'
#' @export
cache_summary <-
    function()
{
    info <- cache_info()
    oldest_file <-
        info |>
        arrange(.data$ctime) |>
        slice(1)
    least_used_file <-
        info |>
        arrange(.data$mtime) |>
        slice(1)
    largest_file <-
        info |>
        arrange(desc(.data$size)) |>
        slice(1)

    structure(
        list(
            directory = cache_directory(),
            n_files = NROW(info),
            cache_size = sum(info$size),
            oldest_file = oldest_file$file,
            oldest_age = Sys.time() - oldest_file$ctime,
            least_used_file = least_used_file$file,
            least_used_age = Sys.time() - least_used_file$mtime,
            largest_file = largest_file$file,
            largest_size = largest_file$size
        ),
        class = "cache_summary"
    )
}

#' @rdname cache
#'
#' @description `cache_info()` reports information about each file in
#'     the cache.
#'
#' @return
#'
#' `cache_info()` returns a tibble with columns summarizing
#'
#' - `file`: the file name of the cache entry. This is a hash, based
#'   on the function and signature, of the cached object. It is not
#'   easily possible to associate individual calls in *R* to file
#'   names.
#'
#' - `size` the size in bytes of the cached object.
#'
#' - `mtime`, `ctime`, `atime`: the modification, creation and
#'   last-access time of the file.
#'
#' @examples
#' cache_info()
#'
#' ## cache objects created in the last 24 hours
#' cache_info() |>
#'     filter(mtime > Sys.time() - 24 * 60 * 60)
#'
#' \dontrun{
#' ## remove the most recently modified object, forcing this to be recreated
#' file_name <-
#'     cache_info() |>
#'     arrange(desc(mtime)) |>
#'     slice(1) |>
#'     pull(file)
#' unlink(file.path(cache_directory(), file_name))
#' }
#'
#' @export
cache_info <-
    function()
{
    file_info <-
        cache_directory() |>
        dir(full.names = TRUE) |>
        file.info()

    file_info |>
        as_tibble(rownames = "file") |>
        select(where(is_varying_column)) |>
        mutate(file = basename(file))
}

#' @rdname cache
#'
#' @description `cache()` returns an object that can be used in cache
#'     management.
#'
#' @details
#'
#' See the help page `?cache_disk` for information on using the object
#' returned by `cache()`.
#'
#' @return `cache()` returns an object defined in the cachem package.
#'
#' @examples
#' cache()
#'
#' @export
cache <-
    function()
{
    ## use a disk cache for memoised functions; expire objects after
    ## 30 days
    cache_disk(cache_directory(), max_age = 60 * 60 * 24 * 30)
}

## helper functions for print.cache_summary()
cache_size_as_text <-
    function(x, units = "auto")
{
    if (!length(x)) {
        return("0 B")
    }

    format(structure(x, class = "object_size"), units = units)
}

cache_age_as_text <-
    function(x)
{
    if (!length(x))
        return ("0 secs")

    age <- as.integer(x)
    units <- attr(x, "units")
    glue("{age} {units}")
}

cache_file_as_text <-
    function(x)
{
    if (length(x)) glue("({x})") else ""
}

#' @rdname cache
#'
#' @param x for `print.cache_summary()`, an object returned by
#'     `cache_summary().
#'
#' @param ... ignored by `print.cache_summary()`.
#'
#' @export
print.cache_summary <-
    function(x, ...)
{
    cache_size <- cache_size_as_text(x$cache_size)
    largest_file <- cache_file_as_text(x$largest_file)
    largest_size <- cache_size_as_text(x$largest_size)
    oldest_file <- cache_file_as_text(x$oldest_file)
    oldest_age <- cache_age_as_text(x$oldest_age)
    least_used_file <- cache_file_as_text(x$least_used_file)
    least_used_age <- cache_age_as_text(x$least_used_age)

    cat(glue("
        cache directory: {x$directory}
        number of files: {x$n_files}
        total cache size: {cache_size}
        largest file: {largest_size} {largest_file}
        oldest file: {oldest_age} {oldest_file}
        least used file: {least_used_age} {least_used_file}"
    ), "\n")
}
