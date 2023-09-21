#' Create leaflet map using create_map()
#'
#' @param metric_df
#' @param input_geog_tabs

create_leaflet_map <- function(metric_df, input_geog_tabs) {

    max_year <- year(max(metric_df$period_end_date))

    if (input_geog_tabs == "Sub-ICB") {
        if (max_year == 2019) {
            create_map(metric_df, "sub_icb_2019")
        } else if (max_year == 2023) {
            create_map(metric_df, "sub_icb_2023")
        }
    }

}

