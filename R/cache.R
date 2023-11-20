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
#' @description `cache_info()` summarizes information about each file
#'     in the cache.
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
