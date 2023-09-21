#' Create line chart
#'
#' @param filtered_df
#' @param input_explore_tabs

create_line_chart <- function(filtered_df, input_explore_tabs){

    cols <- c("#228096", "#D07B4D", "#37906D", "#00436C", "#801650", "#3D3D3D", "#A285D1")
    tmp_df <- filtered_df %>%
        filter(metric_category == input_explore_tabs)

    tmp_df %>%
        plot_ly(x = ~period_end_date,
                y = ~proportion*100,
                color = ~metric_category_group,
                type = "scatter",
                # Line with marker dot at each data point
                mode = "lines+markers",
                # Make line and dots teal
                colors = cols[1:length(unique(tmp_df$metric_category_group))],
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
                                    round(proportion*100, 1))) %>%
        layout(#showlegend = FALSE,
               hovermode = "x unified",
               xaxis = list(type = "date", # Specify x axis is date
                            # Show x axis ticks
                            ticks = "outside",
                            # Format ticks as abbreviated month name and full year, e.g. Jan 2018
                            tickformat = "%b\n%Y",
                            # Set first tick
                            tick0 = ~min(period_end_date),
                            # Tick every 6 months
                            #dtick = "M6",
                            showgrid = FALSE,
                            title = list(font = list(size = 12),
                                         text = "",
                                         standoff = 10),
                            tickfont = list(size = 12),
                            gridwidth = 1.5,
                            gridcolor = "#e6e6e6",
                            zerolinewidth = 1.5),
               # Y axis ticks with commas as thousands separators
               yaxis = list(tickformat = ",",
                            range = ~c(0, 101),
                            showgrid = TRUE,
                            title = list(font = list(size = 12),
                                         text = "Proportion (%)",
                                         standoff = 5),
                            tickfont = list(size = 12),
                            gridwidth = 1.5,
                            gridcolor = "#e6e6e6",
                            zerolinewidth = 1.5),
               legend = list(font = list(size = 12))) %>%
        config(modeBarButtonsToRemove = c("zoom", "pan", "select", "lasso", "zoomIn2d",
                                          "zoomOut2d", "autoscale", "resetscale", "hovercompare", "hoverclosest"),
               displaylogo = FALSE)
}
