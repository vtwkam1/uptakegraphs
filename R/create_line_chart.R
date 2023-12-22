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

    # Get colours for the number of unique metric_category_groups in this metric_category
    colours <- c("#228096", "#D07B4D", "#37906D", "#00436C", "#801650", "#3D3D3D", "#A285D1", "#EAD054") %>%
        purrr::pluck(1:length(unique(tmp_df$metric_category_group)))

    # Get indicator type for this metric (i.e. proportion or count)
    indicator_type <- tmp_df %>%
        dplyr::distinct(.data$indicator_type) %>%
        dplyr::pull("indicator_type") %>%
        purrr::pluck(1)

    # Generate count or proportion chart, depending on metric indicator type
    if (indicator_type == "Proportion") {
        line_chart_proportion(tmp_df, colours)
    } else if (indicator_type == "Count") {
        line_chart_count(tmp_df, colours)
    }
}

#' @rdname create_line_chart
#' @export

line_chart_proportion <- function(df, colours) {

    # If the data spans four years or more, only label every year on the x-axis
    if ((lubridate::interval(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T)) / lubridate::years(1)) >= 4) {
        x_interval <- "M12"
    } else {
        # Label every 6 months
        x_interval <- "M6"
    }

    df %>%
        plotly::plot_ly(x = ~period_end_date,
                y = ~indicator*100,
                color = ~metric_category_group,
                type = "scatter",
                # Line with marker dot at each data point
                mode = "lines+markers",
                # Make line and dots teal
                colors = colours,
                #marker = list(color = "#228096"),
                # Edit hover text. Use hovertext instead of hovertemplate as date
                # tick labels edited to show month and year in two separate lines,
                # layout(tickformat = "%b\n%Y"), but don't want new line for year in
                # hover text
                hoverinfo = "text",
                hovertext = ~paste0(metric_category_group,
                                    "<br>",
                                    format(period_end_date, "%b %Y"),
                                    "<br>Numerator: ",
                                    scales::label_comma()(numerator),
                                    "<br>Denominator: ",
                                    scales::label_comma()(denominator),
                                    "<br>Proportion (%): ",
                                    round(indicator*100, 1))) %>%
        plotly::layout(#showlegend = FALSE,
            # hovermode = "x unified",
            xaxis = list(type = "date", # Specify x axis is date
                         # Show x axis ticks
                         ticks = "outside",
                         # Format ticks as abbreviated month name and full year, e.g. Jan 2018
                         tickformat = "%b\n%Y",
                         # Set first tick
                         tick0 = ~min(period_end_date),
                         # Tick every 6 months
                         dtick = x_interval
                         # showgrid = FALSE
                         # title = list(font = list(size = 12),
                         #              text = "",
                         #              standoff = 10),
                         # tickfont = list(size = 12),
                         # gridwidth = 1.5,
                         # gridcolor = "#e6e6e6",
                         # zerolinewidth = 1.5
            ),
            # Y axis ticks with commas as thousands separators
            yaxis = list(tickformat = ",",
                         range = ~c(0, 101)
                         # showgrid = TRUE,
                         # title = list(font = list(size = 12),
                         #              text = "Proportion (%)",
                         #              standoff = 5),
                         # tickfont = list(size = 12),
                         # gridwidth = 1.5,
                         # gridcolor = "#e6e6e6",
                         # zerolinewidth = 1.5
            ),
            legend = list(font = list(size = 12))) %>%
        # config(modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso", "zoomIn2d",
        #                                   "zoomOut2d", "autoscale", "resetscale", "hovercompare", "hoverclosest"),
        #        displaylogo = FALSE)
        niceRplots::nice_plotly_theme(x_title = "",
                          y_title = "Proportion (%)")
}

#' @rdname create_line_chart
#' @export

line_chart_count <- function(df, colours) {

    # If the data spans more than four years, only label every year on the x-axis
    if ((lubridate::interval(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T)) / lubridate::years(1)) > 4) {
        x_interval <- "M12"
    } else {
        # Label every 6 months
        x_interval <- "M6"
    }

    df %>%
        plotly::plot_ly(x = ~period_end_date,
                y = ~indicator,
                color = ~metric_category_group,
                type = "scatter",
                # Line with marker dot at each data point
                mode = "lines+markers",
                # Make line and dots teal
                colors = colours,
                #marker = list(color = "#228096"),
                # Edit hover text. Use hovertext instead of hovertemplate as date
                # tick labels edited to show month and year in two separate lines,
                # layout(tickformat = "%b\n%Y"), but don't want new line for year in
                # hover text
                hoverinfo = "text",
                hovertext = ~paste0(metric_category_group,
                                    "<br>",
                                    format(period_end_date, "%b %Y"),
                                    "<br>Count: ",
                                    scales::label_comma()(indicator))) %>%
        plotly::layout(#showlegend = FALSE,
            xaxis = list(type = "date", # Specify x axis is date
                         # Show x axis ticks
                         ticks = "outside",
                         # Format ticks as abbreviated month name and full year, e.g. Jan 2018
                         tickformat = "%b\n%Y",
                         # Set first tick
                         tick0 = ~min(period_end_date),
                         # Tick every 6 months
                         dtick = x_interval
                         # showgrid = FALSE,
                         # title = list(font = list(size = 12),
                         #              text = "",
                         #              standoff = 10),
                         # tickfont = list(size = 12),
                         # gridwidth = 1.5,
                         # gridcolor = "#e6e6e6",
                         # zerolinewidth = 1.5
            ),
            # Y axis ticks with commas as thousands separators
            yaxis = list(tickformat = ",",
                         rangemode = "tozero"
                         # showgrid = TRUE,
                         # title = list(font = list(size = 12),
                         #              text = "Count",
                         #              standoff = 5),
                         # tickfont = list(size = 12),
                         # gridwidth = 1.5,
                         # gridcolor = "#e6e6e6",
                         # zerolinewidth = 1.5
            ),
            legend = list(font = list(size = 12))) %>%
        niceRplots::nice_plotly_theme(x_title = "",
                          y_title = "Count")
}
