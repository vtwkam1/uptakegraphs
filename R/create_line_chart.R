#' Create line chart
#'
#' @description
#' Creates a line chart with time (`period_end_date` column) on the x-axis
#' and uptake (`indicator` column) on the y-axis. If groups are supplied,
#' (more than one level in `metric_category_group`), each will be plotted in a different colour.
#'
#' `line_chart_proportion()` plots proportion data. `indicator` column values should
#' be between 0 and 1. Y-axis is 0-100%.
#'
#' `line_chart_count()` plots count data. `indicator` column values can be any numerical
#' value. Y-axis is from 0 to whatever Plotly thinks is appropriate.
#'
#' `create_line_chart()`:
#' * Filters data for the appropriate indicator (e.g. All, ICB, Ethnicity)
#' * Selects the colours for the number of groups
#' * Runs the correct line_chart_ function
#'
#' @param df Dataframe
#' @param input_explore_tabs What data to plot (e.g. All, ICB, Ethnicity).
#' @param selected_area_name Defaults to NULL. If a specific area is selected, e.g. Greater Manchester ICB,
#' plots that area's data only.
#' @param colours Character vector of HEX codes to use as line colours.
#' @param indicator_type Must be either "Count" or "Proportion"
#' @param indicator_unit Units for indicator, used to label y-axis title.
#'
#' @return Plotly chart
#'
#' @examples
#'
#' @export

create_line_chart <- function(df, input_explore_tabs, selected_area_name = NULL){

    # Filter data for only metric_category of interest, e.g. "Ethnicity", "Sex"
    if (is.null(selected_area_name)) {
        tmp_df <- df %>%
            dplyr::filter(.data$metric_category == input_explore_tabs)
    } else {
        # If specific area selected, keep its data and national data
        tmp_df <- df %>%
            dplyr::filter(.data$metric_category == input_explore_tabs | (.data$area_name == "England" & .data$metric_category == "All")) %>%
            # Rename metric_category_group with area_name for ease of plotting
            dplyr::mutate(metric_category_group = factor(.data$area_name) %>% forcats::fct_relevel(c("England")))
    }

    # Get indicator type for this metric (i.e. proportion or count)
    indicator_type <- tmp_df %>%
        dplyr::distinct(.data$indicator_type) %>%
        dplyr::pull("indicator_type") %>%
        purrr::pluck(1)

    # Test - indicator type must be either "Proportion" or "Count"

    # Get indicator unit for this metric (i.e. percentage (%) or count or median etc)
    indicator_unit <- tmp_df %>%
        dplyr::distinct(.data$indicator_unit) %>%
        dplyr::pull("indicator_unit") %>%
        purrr::pluck(1)

    # Set colours depending on number of groups
    n_groups <- length(unique(tmp_df$metric_category_group))

    if(n_groups <= 8) {
        # If 8 groups or less
        # Palette mixing NICE brand colours with Government Analysis Service categorical palette
        colours <- c("#228096", "#D07B4D", "#37906D", "#00436C", "#801650", "#3D3D3D", "#A285D1", "#EAD054")
    } else if (n_groups <= 10) {
        # If 10 groups or less
        # Paul Tol's muted qualitative colour scheme
        colours <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499", "#DDDDDD")
    } else {
        # If more than 10 groups
        # Interpolate Paul Tol's smooth rainbow colour scheme
        pal <- c("#E8ECFB", "#DDD8EF", "#D1C1E1", "#C3A8D1", "#B58FC2", "#A778B4",
                 "#9B62A7", "#8C4E99", "#6F4C9B", "#6059A9", "#5568B8", "#4E79C5",
                 "#4D8AC6", "#4E96BC", "#549EB3", "#59A5A9", "#60AB9E", "#69B190",
                 "#77B77D", "#8CBC68", "#A6BE54", "#BEBC48", "#D1B541", "#DDAA3C",
                 "#E49C39", "#E78C35", "#E67932", "#E4632D", "#DF4828", "#DA2222",
                 "#B8221E", "#95211B", "#721E17", "#521A13", "#666666")

        colours <- grDevices::colorRampPalette(pal, interpolate = "linear")(n_groups)
    }

    # If no subgroups, no need for subplots
    if (sum(is.na(tmp_df$metric_category_subgroup) > 0)) {

        # Get colours for the number of unique metric_category_groups in this metric_category
        colours <- colours %>% magrittr::extract(1:length(unique(tmp_df$metric_category_group)))

        # Generate count or proportion chart, depending on metric indicator type
        if (indicator_type == "Proportion") {
            line_chart_proportion(tmp_df, colours, indicator_unit)
        } else if (indicator_type == "Count") {
            line_chart_count(tmp_df, colours, indicator_unit)
        }

    } else { # Subplots required

        line_chart_facet(tmp_df, colours, indicator_type, indicator_unit)
    }

}

#' @rdname create_line_chart

# Depending on date intervals in data, set x-axis ticks and tick labels
date_axis_intervals <- function(df) {
    # If the data is in financial years (checking the first row only), create x-axis labels in financial year format (e.g. "2018/19")
    if (lubridate::month(df$period_start_date[[1]]) == 4 & lubridate::interval(df$period_start_date[[1]], df$period_end_date[[1]] + lubridate::days(1))/lubridate::years(1) == 1) {
        fin_year <- TRUE
        tickvals <- seq(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T), by = "years")
        ticktext <- purrr::map_chr(tickvals, \(x) paste0(lubridate::year(x) - 1, "/", stringr::str_extract(as.character(lubridate::year(x)), "\\d{2}$")))
    # If the data spans four years or more, only label every year on the x-axis
    } else if ((lubridate::interval(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T)) / lubridate::years(1)) >= 4) {
        fin_year <- FALSE
        tickvals <- seq(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T), by = "years")
        ticktext <- purrr::map_chr(tickvals, \(x) format(x, "%b %Y"))
    } else {
        # Label every 6 months
        fin_year <- FALSE
        tickvals <- seq(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T), by = "6 months")
        ticktext <- purrr::map_chr(tickvals, \(x) format(x, "%b %Y"))
    }

    return(list(fin_year = fin_year, tickvals = tickvals, ticktext = ticktext))

}

#' @rdname create_line_chart
#' @export

line_chart_proportion <- function(df, colours, indicator_unit) {

    # Determine x-axis date intervals and labels
    date_xaxis <- date_axis_intervals(df)

    df %>%
        # Generate hover date labels depending on if data is for a financial year
        dplyr::mutate(hoverdate = ifelse(rep(date_xaxis$fin_year, nrow(df)),
                                         paste0(lubridate::year(.data$period_end_date) - 1, "/", stringr::str_extract(as.character(lubridate::year(.data$period_end_date)), "\\d{2}$")),
                                         format(.data$period_end_date, "%b %Y"))) %>%
        plotly::plot_ly(x = ~period_end_date,
                y = ~indicator*100,
                color = ~metric_category_group,
                type = "scatter",
                # Line with marker dot at each data point
                mode = "lines+markers",
                # Make line and dots teal
                colors = colours,
                hoverinfo = "text",
                hovertext = ~paste0(metric_category_group,
                                    "<br>",
                                    hoverdate,
                                    "<br>Numerator: ",
                                    scales::label_comma()(numerator),
                                    "<br>Denominator: ",
                                    scales::label_comma()(denominator),
                                    "<br>", indicator_unit, ": ",
                                    round(indicator*100, 1))) %>%
        plotly::layout(#showlegend = FALSE,
            # hovermode = "x unified",
            xaxis = list(type = "date", # Specify x axis is date
                         # Show x axis ticks
                         ticks = "outside",
                         tickmode = "array",
                         tickvals = date_xaxis$tickvals,
                         ticktext = date_xaxis$ticktext
            ),
            # Y axis ticks with commas as thousands separators
            yaxis = list(tickformat = ",",
                         range = ~c(0, 101)
            ),
            legend = list(font = list(size = 12))) %>%
        niceRplots::nice_plotly_theme(x_title = "",
                          y_title = indicator_unit)
}

#' @rdname create_line_chart
#' @export

line_chart_count <- function(df, colours, indicator_unit) {

    # Determine x-axis date intervals and labels
    date_xaxis <- date_axis_intervals(df)

    df %>%
        # Generate hover date labels depending on if data is for a financial year
        dplyr::mutate(hoverdate = ifelse(rep(date_xaxis$fin_year, nrow(df)),
                                         paste0(lubridate::year(.data$period_end_date) - 1, "/", stringr::str_extract(as.character(lubridate::year(.data$period_end_date)), "\\d{2}$")),
                                         format(.data$period_end_date, "%b %Y"))) %>%
        plotly::plot_ly(x = ~period_end_date,
                y = ~indicator,
                color = ~metric_category_group,
                type = "scatter",
                # Line with marker dot at each data point
                mode = "lines+markers",
                # Make line and dots teal
                colors = colours,
                hoverinfo = "text",
                hovertext = ~paste0(metric_category_group,
                                    "<br>",
                                    hoverdate,
                                    "<br>", indicator_unit, ": ",
                                    scales::label_comma()(indicator))) %>%
        plotly::layout(#showlegend = FALSE,
            xaxis = list(type = "date", # Specify x axis is date
                         # Show x axis ticks
                         ticks = "outside",
                         tickmode = "array",
                         tickvals = date_xaxis$tickvals,
                         ticktext = date_xaxis$ticktext
            ),
            # Y axis ticks with commas as thousands separators
            yaxis = list(tickformat = ",",
                         rangemode = "tozero"
            ),
            legend = list(font = list(size = 12))) %>%
        niceRplots::nice_plotly_theme(x_title = "",
                          y_title = indicator_unit)
}

#' @rdname create_line_chart
#' @export
#'

line_chart_facet <- function(df, colours, indicator_type, indicator_unit) {

    # Get the two variable names, e.g. "Sex" and "age" for "Sex and age"
    metric_category_var <- df %>%
        dplyr::distinct(.data$metric_category) %>%
        dplyr::pull("metric_category") %>%
        purrr::pluck(1)

    metric_category_group_var <- stringr::str_to_sentence(stringr::str_extract(metric_category_var, ".+(?= and )"))
    metric_category_subgroup_var <- stringr::str_to_sentence(stringr::str_extract(metric_category_var, "(?<= and ).+"))

    # Determine x-axis date intervals and labels
    date_xaxis <- date_axis_intervals(df)

    # Set ymax depending on indicator type
    if (indicator_type == "Proportion") {

        ymax <- 100

        # Multiply by 100 so plotting percentage
        df <- df %>%
            dplyr::mutate(indicator = round(.data$indicator*100, 1))

    } else if (indicator_type == "Count") {

        max_ind <- max(df$indicator, na.rm = T)

        power_10 <- floor(log10(max_ind))

        ymax <- ceiling(max_ind/10^power_10) * 10^power_10
    }

    facet_chart <- df %>%
        dplyr::mutate(hoverdate = ifelse(rep(date_xaxis$fin_year, nrow(df)),
                                         paste0(lubridate::year(.data$period_end_date) - 1, "/", stringr::str_extract(as.character(lubridate::year(.data$period_end_date)), "\\d{2}$")),
                                         format(.data$period_end_date, "%b %Y"))) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$period_end_date,
                                     y = .data$indicator,
                                     color = .data$metric_category_subgroup,
                                     group = 1,
                                     # Create hover text
                                     text = paste0(metric_category_group_var, ": ", .data$metric_category_group, "<br>",
                                                   metric_category_subgroup_var, ": ", .data$metric_category_subgroup, "<br>",
                                                   .data$hoverdate,"<br>",
                                                   indicator_unit, ": ", scales::label_comma()(.data$indicator)))) +
        # Add lines
        ggplot2::geom_line(linewidth = 0.8) +
        # Add points
        ggplot2::geom_point(size = 1.5) +
        # Set y-axis limits and remove padding
        ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,ymax)) +
        # Set x-axis labels to show as month year, and to occur at 6-monthly intervals
        ggplot2::scale_x_date(date_labels = date_xaxis$ticktext,
                              breaks = date_xaxis$tickvals
                              # date_breaks = paste0(x_interval, " months")
                              ) +
        # Manually set the colour of the lines and points
        ggplot2::scale_color_manual(values = colours) +
        # Apply facet wrapping to put each chemical into an individual plot
        ggplot2::facet_wrap(~metric_category_group) +
        # Apply the NICE theme
        niceRplots::nice_gg_theme(x_title = FALSE,
                                  axis_lines = "none",
                                  panel_border = TRUE,
                                  show_legend = TRUE) +
        # Add labels
        ggplot2::labs(y = indicator_unit,
                      # Remove legend title
                      colour = "")

    # Convert to ggplotly
    plotly::ggplotly(facet_chart,
                     # Use labels generated in aes()
                     tooltip = c("text"),
                     # Helps x-axis date breaks show properly
                     dynamicTicks = FALSE) %>%
        plotly::layout(legend = list(orientation = 'h',
                                     xref = "container",
                                     xanchor = "right",
                                     x = 1,
                                     yref = "paper",
                                     yanchor = "top",
                                     y = 1.2)) %>%
        plotly::config(modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso", "zoomIn2d",
                                          "zoomOut2d", "autoscale", "resetscale", "hovercompare", "hoverclosest"),
               displaylogo = FALSE)
}
