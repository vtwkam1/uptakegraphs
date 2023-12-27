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

    # Set colours
    colours <- c("#228096", "#D07B4D", "#37906D", "#00436C", "#801650", "#3D3D3D", "#A285D1", "#EAD054")

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

date_axis_intervals <- function(df) {
    # If the data spans four years or more, only label every year on the x-axis
    if ((lubridate::interval(min(df$period_end_date, na.rm = T), max(df$period_end_date, na.rm = T)) / lubridate::years(1)) >= 4) {
        x_interval <- "12"
    } else {
        # Label every 6 months
        x_interval <- "6"
    }

    return(x_interval)

}

#' @rdname create_line_chart
#' @export

line_chart_proportion <- function(df, colours, indicator_unit) {

    # Calculate x interval needed - 6 or 12 months
    x_interval <- date_axis_intervals(df)

    df %>%
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
                                    format(period_end_date, "%b %Y"),
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
                         # Format ticks as abbreviated month name and full year, e.g. Jan 2018
                         tickformat = "%b\n%Y",
                         # Set first tick
                         tick0 = ~min(period_end_date),
                         # Tick every 6/12 months
                         dtick = paste0("M", x_interval)
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

    # Calculate x interval needed - 6 or 12 months
    x_interval <- date_axis_intervals(df)

    df %>%
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
                                    format(period_end_date, "%b %Y"),
                                    "<br>", indicator_unit, ": ",
                                    scales::label_comma()(indicator))) %>%
        plotly::layout(#showlegend = FALSE,
            xaxis = list(type = "date", # Specify x axis is date
                         # Show x axis ticks
                         ticks = "outside",
                         # Format ticks as abbreviated month name and full year, e.g. Jan 2018
                         tickformat = "%b\n%Y",
                         # Set first tick
                         tick0 = ~min(period_end_date),
                         # Tick every 6/12 months
                         dtick = paste0("M", x_interval)
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

    # Calculate x interval needed - 6 or 12 months
    x_interval <- date_axis_intervals(df)

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
        ggplot2::ggplot(ggplot2::aes(x = .data$period_end_date,
                                     y = .data$indicator,
                                     color = .data$metric_category_subgroup,
                                     group = 1,
                                     # Create hover text
                                     text = paste0(metric_category_subgroup_var, ": ", .data$metric_category_subgroup, "<br>",
                                                   format(.data$period_end_date, "%b %Y"),"<br>",
                                                   indicator_unit, ": ", scales::label_comma()(.data$indicator)))) +
        # Add lines
        ggplot2::geom_line(linewidth = 0.8) +
        # Add points
        ggplot2::geom_point(size = 1.5) +
        # Set y-axis limits and remove padding
        ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,ymax)) +
        # Set x-axis labels to show as month year, and to occur at 6-monthly intervals
        ggplot2::scale_x_date(date_labels = "%b\n%Y",
                              breaks = seq(min(df$period_end_date, na.rm = TRUE),
                                           max(df$period_end_date, na.rm = TRUE),
                                           by = paste0(x_interval, " months")),
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
