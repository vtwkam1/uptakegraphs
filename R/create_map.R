#' Create map
#'
#' @param metric_df
#' @param unit

create_map <- function(metric_df, unit) {

    vars <- list(sub_icb_2019 = list(filter = "Sub-ICB",
                                     geojson_file = "data/Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BUC_2022_4644984930930678212.geojson",
                                     join_col = "ccg19cd",
                                     hover_label = "CCG",
                                     hover_label_col = "ccg19nm"),
                 sub_icb_2023 = list(filter = "Sub-ICB",
                                     geojson_file = "data/Sub_Integrated_Care_Board_Locations_April_2023_EN_BSC_2087764644089335899.geojson",
                                     join_col = "sicbl23cd",
                                     hover_label = "Sub-ICB location",
                                     hover_label_col = "sicbl23nm"))

    metric_df <- metric_df %>%
        filter(metric_category == vars[[unit]][["filter"]],
               period_end_date == max(period_end_date))

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
    map_table <- left_join(shapes, metric_df, by = join_by(!!temp_join_col == "geo_code"))

    # Set up colour palette using the NICE sequential palette
    pal <- colorBin(colorRampPalette(nice_pal("seq"))(5),
                    domain = map_table$proportion,
                    bins = 5,
                    reverse = TRUE, # Darker colours for low uptake
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
        addPolygons(fillColor = ~pal(proportion),
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
                                           label_percent(0.1)(map_table$proportion)),
                                    htmltools::HTML),
                    # Set font options
                    labelOptions = labelOptions(textsize = "12px",
                                                style = list("font-family" = "Inter Regular"))) %>%
        addLegend(position = "bottomleft",
                  pal = pal,
                  values = ~map_table$proportion,
                  title = "Uptake",
                  labFormat = labelFormat(suffix = "%",
                                          transform = function(x) x*100),
                  opacity = 1)

    leaflet_map
}
