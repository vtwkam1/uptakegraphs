## code to prepare `DATASET` dataset goes here
rlang::check_installed("fs")
rlang::check_installed("rmapshaper")
rlang::check_installed("janitor")

ccg_shape <- sf::read_sf(fs::path_package("uptakegraphs", "extdata", "CCG_April_2019_EN_BUC.geojson")) %>%
    rmapshaper::ms_simplify(keep = 0.1,
                            # Stops small polygons from disappearing
                            keep_shapes = TRUE) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    janitor::clean_names()

subicb_shape <- sf::read_sf(fs::path_package("uptakegraphs", "extdata", "SubICB_April_2023_EN_BSC.geojson")) %>%
    rmapshaper::ms_simplify(keep = 0.1,
                            # Stops small polygons from disappearing
                            keep_shapes = TRUE) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    janitor::clean_names()

icb_shape <- sf::read_sf(fs::path_package("uptakegraphs", "extdata", "ICB_April_2023_EN_BSC.geojson")) %>%
    rmapshaper::ms_simplify(keep = 0.1,
                            # Stops small polygons from disappearing
                            keep_shapes = TRUE) %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>%
    janitor::clean_names()


usethis::use_data(ccg_shape, subicb_shape, icb_shape, overwrite = TRUE, internal = TRUE)
