#' Create bar chart for geographies
#'
#' @param df
#' @param ods_lookup_df
#' @param input_geog_tabs

create_geog_bar <- function(df, ods_lookup_df, input_geog_tabs){

    median_var <- df %>%
        filter(metric_category == input_geog_tabs,
               period_end_date == max(period_end_date)) %>%
        pull(proportion) %>%
        median(na.rm = TRUE)

    tmp <- df %>%
        filter(metric_category == input_geog_tabs,
               period_end_date == max(period_end_date)) %>%
        left_join(ods_lookup_df, by = c("metric_category_group" = "ods_code")) %>%
        plot_ly(x = ~reorder(metric_category_group, proportion),
                y = ~proportion*100,
                type = "bar",
                # Edit hover text
                color = I("#228096"),
                stroke = I("#FFFFFF"),
                hoverinfo = "text",
                hovertext = ~paste0("Name: ", metric_category_group,
                                    # "<br>ODS code: ", metric_category_group,
                                    "<br>Numerator: ", scales::label_comma()(numerator),
                                    "<br>Denominator: ", scales::label_comma()(denominator),
                                    "<br>Proportion: ", scales::label_percent(accuracy = 0.1)(proportion))) %>%
        add_lines(y = median_var*100,
                  name = "National Median",
                  line = list(dash = 'dot',
                              color = "#000000"),
                  hovertext = paste0("National Median: ", scales::label_percent(accuracy = 0.01, scale = 100)(median_var))) %>%
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

