#' Create bar chart for geographies
#'
#' @description
#' Creates a bar chart with all the area level units (e.g. ICBs, providers) on the x-axis,
#' and their uptake on the y-axis. Also plots the median uptake value as a horizontal line.
#'
#' `geog_bar_proportion()` plots proportion data. `indicator` column values should
#' be between 0 and 1. Y-axis is 0-100%.
#'
#' `geog_bar_count()` plots count data. `indicator` column values can be any numerical
#' value. Y-axis is from 0 to whatever Plotly thinks is appropriate.
#'
#' `create_geog_bar()`:
#' * Filters data for the appropriate indicator (e.g. ICB, All, Ethnicity)
#' * selects the last year of data for proportion data or
#'  the latest time period of data for count data
#' * Calculates the median
#' * Runs the correct geog_bar_ function
#'
#' @param df Dataframe
#' @param input_geog_tabs Area level (e.g. ICB, provider) to plot.
#' @param median_var Median indicator value for given area level split.
#'
#' @return Plotly chart
#'
#' @examples
#'
#' @export

create_geog_bar <- function(df, input_geog_tabs){

    # Keep only data for right metric_category
    tmp_df <- df %>%
        filter(metric_category == input_geog_tabs)

    # Get indicator type for this metric (i.e. proportion or count)
    indicator_type <- tmp_df %>%
        distinct(indicator_type) %>%
        pull(indicator_type) %>%
        pluck(1)

    # Generate count or proportion chart, depending on metric indicator type
    if (indicator_type == "Proportion") {

        # Calculate proportion for latest year of data combined
        tmp_df <- tmp_df %>%
            filter(period_start_date > max(period_start_date, na.rm = T) - years(1)) %>%
            group_by(area_name) %>%
            # Possible that some areas will not have full year of data
            summarise(period_start_date = min(period_start_date, na.rm = T),
                      period_end_date = max(period_end_date, na.rm = T),
                      numerator = sum(numerator),
                      denominator = sum(denominator),
                      .groups = "drop") %>%
            mutate(indicator = numerator/denominator)

        # Get median
        median_var <- tmp_df$indicator %>%
            median(na.rm = TRUE)

        # Plot chart
        geog_bar_proportion(tmp_df, input_geog_tabs, median_var)

    } else if (indicator_type == "Count") {

        # Calculate count for latest period of data
        # Do not sum latest year as this would count people many times
        tmp_df <- tmp_df %>%
            filter(period_start_date == max(period_start_date, na.rm = T))

        # Get median
        median_var <- tmp_df$indicator %>%
            median(na.rm = TRUE)

        # Plot chart
        geog_bar_count(tmp_df, input_geog_tabs, median_var)
    }
}

#' @rdname create_geog_bar

geog_bar_proportion <- function(df, input_geog_tabs, median_var) {
    tmp <- df %>%
        plot_ly(x = ~reorder(area_name, indicator), # Reorder area_name in order of size of indicator
                y = ~indicator*100,
                type = "bar",
                # Edit hover text
                color = I("#228096"),
                stroke = I("#FFFFFF"),
                hoverinfo = "text",
                hovertext = ~paste0("Name: ", area_name,
                                    "<br>Numerator: ", scales::label_comma()(numerator),
                                    "<br>Denominator: ", scales::label_comma()(denominator),
                                    "<br>Proportion: ", scales::label_percent(accuracy = 0.1)(indicator))) %>%
        add_lines(y = median_var*100,
                  name = "National median",
                  line = list(dash = 'dot',
                              color = "#000000"),
                  hovertext = paste0("National median: ", scales::label_percent(accuracy = 0.01, scale = 100)(median_var))) %>%
        add_annotations(xref = "paper",
                        yref ="y",
                        x = 0.02,
                        y = median_var*100,
                        yshift = 12,
                        text = "<b>National median</b>",
                        showarrow = FALSE) %>%
        layout(yaxis = list(range = c(0, 101)),
               xaxis = list(showticklabels = FALSE),
               hovermode = "x unified") %>%
        # Add NICE theme and set axis titles
        nice_plotly_theme(x_title = paste0(input_geog_tabs, "s"),
                          axis_ticks = "none",
                          y_title = "Proportion (%)",
                          show_legend = FALSE)

    return(tmp)
}

#' @rdname create_geog_bar

geog_bar_count <- function(df, input_geog_tabs, median_var) {
    tmp <- df %>%
        plot_ly(x = ~reorder(area_name, indicator), # Reorder area_name in order of size of indicator
                y = ~indicator,
                type = "bar",
                # Edit hover text
                color = I("#228096"),
                stroke = I("#FFFFFF"),
                hoverinfo = "text",
                hovertext = ~paste0("Name: ", area_name,
                                    "<br>Count: ", scales::label_comma()(indicator))) %>%
        add_lines(y = median_var,
                  name = "National median",
                  line = list(dash = 'dot',
                              color = "#000000"),
                  hovertext = paste0("National median: ", scales::label_comma()(median_var))) %>%
        add_annotations(xref = "paper",
                        yref ="y",
                        x = 0.02,
                        y = median_var,
                        yshift = 12,
                        text = "<b>National median</b>",
                        showarrow = FALSE) %>%
        layout(yaxis = list(tickformat = ",",
                            rangemode = "tozero"),
               xaxis = list(showticklabels = FALSE),
               hovermode = "x unified") %>%
        # Add NICE theme and set axis titles
        nice_plotly_theme(x_title = paste0(input_geog_tabs, "s"),
                          axis_ticks = "none",
                          y_title = "Count",
                          show_legend = FALSE)

    return(tmp)
}


