#' Create a heat map in leaflet.
#'
#' @description
#' `create_leaflet_map()` runs `create_map()` at the right geographical level (ICB or sub-ICB).
#'
#' `create_map()` contains the code to create a map in leaflet.
#'
#' Currently only compatible with proportions.
#'
#' @param df Dataframe
#' @param input_geog_tabs Area level (e.g. ICB, provider) to plot.
#' @param unit Area level (e.g. ICB, provider) to plot.
#'
#' @return Leaflet object
#'
#' @examples
#'
#' @export

create_leaflet_map <- function(df, input_geog_tabs) {

    max_year <- lubridate::year(max(df$period_end_date))

    if (input_geog_tabs == "Sub-ICB") {
        if (max_year == 2019) {
            create_map(df, "sub_icb_2019")
        } else if (max_year == 2023) {
            create_map(df, "sub_icb_2023")
        }
    } else if (input_geog_tabs == "ICB") {
        create_map(df, "icb")
    }

}

#' @rdname create_leaflet_map
#' @export

create_map <- function(df, unit) {

    # Variables to use for each type area level
    vars <- list(sub_icb_2019 = list(filter = "Sub-ICB",
                                     geojson = ccg_shape,
                                     join_col = "ccg19cd",
                                     hover_label = "CCG",
                                     hover_label_col = "ccg19nm"),
                 sub_icb_2023 = list(filter = "Sub-ICB",
                                     geojson = subicb_shape,
                                     join_col = "sicbl23cd",
                                     hover_label = "Sub-ICB location",
                                     hover_label_col = "sicbl23nm"),
                 icb = list(filter = "ICB",
                            geojson = icb_shape,
                            join_col = "icb23cd",
                            hover_label = "ICB",
                            hover_label_col = "icb23nm"))

    # Keep only latest year of data
    df <- df %>%
        dplyr::filter(.data$metric_category == vars[[unit]][["filter"]],
                      .data$period_start_date > max(.data$period_start_date) - lubridate::years(1)) %>%
        dplyr::group_by(.data$area_name, .data$area_gss_code) %>%
        # Possible that some areas will not have full year of data
        dplyr::summarise(period_start_date = min(.data$period_start_date),
                         period_end_date = max(.data$period_end_date),
                         numerator = sum(.data$numerator),
                         denominator = sum(.data$denominator),
                         indicator_unit = unique(.data$indicator_unit)[[1]],
                  .groups = "drop") %>%
        dplyr::mutate(indicator = .data$numerator/.data$denominator)

    # Get indicator unit
    indicator_unit <- unique(df$indicator_unit)

    # Load in the a .geoJSON file containing the relevant geographical shapes and simplify
    # these to increase plotting speed. In the code below, we are keeping 10% of the total points.
    shapes <- vars[[unit]][["geojson"]]

    # Combine these shapes into a single shape for the whole of England.
    # Setting the sf_use_s2() function to FALSE prevents the use of spherical geometry
    sf::sf_use_s2(FALSE)

    # Set column to join on
    temp_join_col <- vars[[unit]][["join_col"]]

    # Join our data to the dataframe containing the relevant sub-ICB location shapes. Ensure the
    # dataframe containing the shapes is the first argument in the join, as we want to preserve
    # the class of this table. If not the geometry column containing the shapes will be dropped.
    map_table <- dplyr::left_join(shapes, df, by = dplyr::join_by(!!temp_join_col == "area_gss_code"))

    # Set up colour palette using the NICE sequential palette
    pal <- leaflet::colorBin(grDevices::colorRampPalette(niceRplots::nice_pal("seq"))(5),
                    domain = map_table$indicator,
                    bins = 5,
                    na.color = niceRplots::nice_cols("black_50"))

    # Set which column should be used for the place's hover label
    temp_hover_label_col <- vars[[unit]][["hover_label_col"]]

    # Create chart
    leaflet_map <- map_table %>%
        # Set up degree of zoom using controls and scroll
        leaflet::leaflet(options = leaflet::leafletOptions(zoomDelta = 0.25,
                                         zoomSnap = 0.25)) %>%
        # Set view to centre on England and set appropriate zoom
        leaflet::setView(lat = 53,
                lng = -1.5,
                zoom = 5.8) %>%
        # Add the base map tile
        leaflet::addProviderTiles(provider = "CartoDB.Positron") %>%
        # Add our shapes
        leaflet::addPolygons(fillColor = ~pal(indicator),
                    fillOpacity = 1,
                    color = niceRplots::nice_cols("black_100"),
                    weight = 0.5,
                    opacity = 1,
                    # Add functionality to highlight shape when hovered over
                    highlight = leaflet::highlightOptions(weight = 3,
                                                 color = niceRplots::nice_cols("positive_yellow_100"),
                                                 fillOpacity = 1,
                                                 bringToFront = TRUE),
                    # Add hover labels to the shapes
                    label = ~lapply(paste0("<strong>", vars[[unit]][["hover_label"]], ": </strong>",
                                           map_table[[temp_hover_label_col]],
                                           "<br><strong>", indicator_unit, "</strong> ",
                                           round(map_table$indicator*100, 1)),
                                    htmltools::HTML),
                    # Set font options
                    labelOptions = leaflet::labelOptions(textsize = "12px",
                                                style = list("font-family" = "Inter Regular"))) %>%
        leaflet::addLegend(position = "bottomleft",
                  pal = pal,
                  values = ~map_table$indicator,
                  title = indicator_unit,
                  labFormat = leaflet::labelFormat(transform = function(x) x*100),
                  opacity = 1)

    leaflet_map
}
