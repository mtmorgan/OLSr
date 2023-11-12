cache_directory <-
    function()
{
    tools::R_user_dir(PACKAGE_NAME, "cache")
}

cache <-
    function()
{
    ## use a disk cache for memoised functions; expire objects after
    ## 30 days
    cache_disk(cache_directory(), max_age = 60 * 60 * 24 * 30)
}

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
