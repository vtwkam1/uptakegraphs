#' Create a heat map in leaflet.
#'
#' @description
#' `create_leaflet_map()` runs `create_map()` at the right geographical level (ICB or sub-ICB).
#'
#' `create_map()` contains the code to create a map in leaflet.
#'
#' @param metric_df Dataframe
#' @param input_geog_tabs Area level (e.g. ICB, provider) to plot.
#' @param unit Area level (e.g. ICB, provider) to plot.
#'
#' @return Leaflet object
#'
#' @examples
#'
#' @export

create_leaflet_map <- function(metric_df, input_geog_tabs) {

    max_year <- year(max(metric_df$period_end_date))

    if (input_geog_tabs == "Sub-ICB") {
        if (max_year == 2019) {
            create_map(metric_df, "sub_icb_2019")
        } else if (max_year == 2023) {
            create_map(metric_df, "sub_icb_2023")
        }
    } else if (input_geog_tabs == "ICB") {
        create_map(metric_df, "icb")
    }

}

#' @rdname create_leaflet_map

create_map <- function(metric_df, unit) {

    # Variables to use for each type area level
    vars <- list(sub_icb_2019 = list(filter = "Sub-ICB",
                                     geojson_file = "data/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BUC_2022_4644984930930678212.geojson",
                                     join_col = "ccg19cd",
                                     hover_label = "CCG",
                                     hover_label_col = "ccg19nm"),
                 sub_icb_2023 = list(filter = "Sub-ICB",
                                     geojson_file = "data/Sub_Integrated_Care_Board_Locations_April_2023_EN_BSC_2087764644089335899.geojson",
                                     join_col = "sicbl23cd",
                                     hover_label = "Sub-ICB location",
                                     hover_label_col = "sicbl23nm"),
                 icb = list(filter = "ICB",
                            geojson_file = "data/Integrated_Care_Boards_April_2023_EN_BSC_7929772807133590321.geojson",
                            join_col = "icb23cd",
                            hover_label = "ICB",
                            hover_label_col = "icb23nm"))

    # Keep only latest year of data
    metric_df <- metric_df %>%
        filter(metric_category == vars[[unit]][["filter"]],
               period_start_date > max(period_start_date) - years(1)) %>%
        group_by(area_name, area_gss_code) %>%
        # Possible that some areas will not have full year of data
        summarise(period_start_date = min(period_start_date),
                  period_end_date = max(period_end_date),
                  numerator = sum(numerator),
                  denominator = sum(denominator),
                  .groups = "drop") %>%
        mutate(indicator = numerator/denominator)

    # Load in the a .geoJSON file containing the relevant geographical shapes and simplify
    # these to increase plotting speed. In the code below, we are keeping 10% of the total points.
    shapes <- sf::read_sf(vars[[unit]][["geojson_file"]]) %>%
        ms_simplify(keep = 0.1,
                    # Stops small polygons from disappearing
                    keep_shapes = TRUE) %>%
        st_transform('+proj=longlat +datum=WGS84')

    names(shapes) <- make_clean_names(names(shapes))

    # Combine these shapes into a single shape for the whole of England.
    # Setting the sf_use_s2() function to FALSE prevents the use of spherical geometry
    sf::sf_use_s2(FALSE)

    # Set column to join on
    temp_join_col <- vars[[unit]][["join_col"]]

    # Join our data to the dataframe containing the relevant sub-ICB location shapes. Ensure the
    # dataframe containing the shapes is the first argument in the join, as we want to preserve
    # the class of this table. If not the geometry column containing the shapes will be dropped.
    map_table <- left_join(shapes, metric_df, by = join_by(!!temp_join_col == "area_gss_code"))

    # Set up colour palette using the NICE sequential palette
    pal <- colorBin(colorRampPalette(nice_pal("seq"))(5),
                    domain = map_table$indicator,
                    bins = 5,
                    na.color = nice_cols("black_50"))

    # Set which column should be used for the place's hover label
    temp_hover_label_col <- vars[[unit]][["hover_label_col"]]

    # Create chart
    leaflet_map <- map_table %>%
        # Set up degree of zoom using controls and scroll
        leaflet(options = leafletOptions(zoomDelta = 0.25,
                                         zoomSnap = 0.25)) %>%
        # Set view to centre on England and set appropriate zoom
        setView(lat = 53,
                lng = -1.5,
                zoom = 5.8) %>%
        # Add the base map tile
        addProviderTiles(provider = "CartoDB.Positron") %>%
        # Add our shapes
        addPolygons(fillColor = ~pal(indicator),
                    fillOpacity = 1,
                    color = nice_cols("black_100"),
                    weight = 0.5,
                    opacity = 1,
                    # Add functionality to highlight shape when hovered over
                    highlight = highlightOptions(weight = 3,
                                                 color = nice_cols("positive_yellow_100"),
                                                 fillOpacity = 1,
                                                 bringToFront = TRUE),
                    # Add hover labels to the shapes
                    label = ~lapply(paste0("<strong>", vars[[unit]][["hover_label"]], ": </strong>", map_table[[temp_hover_label_col]],
                                           "<br><strong>Uptake:</strong> ",
                                           label_percent(0.1)(map_table$indicator)),
                                    htmltools::HTML),
                    # Set font options
                    labelOptions = labelOptions(textsize = "12px",
                                                style = list("font-family" = "Inter Regular"))) %>%
        addLegend(position = "bottomleft",
                  pal = pal,
                  values = ~map_table$indicator,
                  title = "Uptake",
                  labFormat = labelFormat(suffix = "%",
                                          transform = function(x) x*100),
                  opacity = 1)

    leaflet_map
}
