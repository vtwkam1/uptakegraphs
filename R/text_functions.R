#' Create caption for uptake graphs
#'
#' @description
#' Creates a caption for uptake graphs

#' @param df Dataframe
#' @param input_explore_tabs What data to plot (e.g. All, ICB, Ethnicity).
#' @param selected_area_level England or region/ICB/provider
#' @param selected_area_name Defaults to NULL. If a specific area is selected, e.g. Greater Manchester ICB,
#' plots that area's data only.
#' @param geog_bar Defaults to FALSE. If true, dates only cover last year.
#'
#' @return String
#'
#' @examples
#'
#' @export


# Function to create the text for the chart statistical title
get_stat_title_text <- function(df, input_explore_tabs, selected_area_level, selected_area_name = NULL, geog_bar = FALSE){

    # Cleaning should be done separately from visualisation
    # All inputs should have dates as yyyy-mm-dd

    date_text_fn <- function(min_period_start_date, min_period_end_date, max_period_start_date, max_period_end_date) {
        if (lubridate::month(min_period_start_date) == 4 & lubridate::interval(min_period_start_date, min_period_end_date + lubridate::days(1))/lubridate::years(1) == 1) {
            if (lubridate::interval(min_period_start_date, max_period_end_date + lubridate::days(1))/lubridate::years(1) > 1) {
                paste0(paste0(lubridate::year(min_period_start_date), "/", stringr::str_extract(as.character(lubridate::year(min_period_start_date) + 1), "\\d{2}$")),
                       " to ",
                       paste0(lubridate::year(max_period_start_date), "/", stringr::str_extract(as.character(lubridate::year(max_period_start_date) + 1), "\\d{2}$")))
            } else {
                paste0(lubridate::year(max_period_start_date), "/", stringr::str_extract(as.character(lubridate::year(max_period_start_date) + 1), "\\d{2}$"))
            }
        }
        else if (min_period_start_date == max_period_start_date) {
            format(min_period_start_date, "%B %Y")
        } else {
            paste0(format(min_period_start_date, "%B %Y"),
                   " to ",
                   format(max_period_end_date, "%B %Y"))
        }
    }

    header_text_fn <- function(df = NULL,
                               split_by = NULL,
                               area_name = NULL,
                               min_period_start_date,
                               min_period_end_date,
                               max_period_start_date,
                               max_period_end_date) {
        paste0(stringr::str_remove(unique(df$metric_desc), "\\.$"), ", ",
               split_by,
               area_name, ", ",
               date_text_fn(min_period_start_date, min_period_end_date,
                            max_period_start_date, max_period_end_date))
    }

    # Header text variables depend on selected area level, if area-level tabs or if splits
    if (selected_area_level == "England") {
        df <- df %>%
            dplyr::filter(.data$metric_category == input_explore_tabs)

        min_period_start_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::pull(.data$period_start_date) %>%
            purrr::pluck(1)

        min_period_end_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::pull(.data$period_end_date) %>%
            purrr::pluck(1)

        max_period_start_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::slice_tail() %>%
            dplyr::pull(.data$period_start_date) %>%
            purrr::pluck(1)

        max_period_end_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::slice_tail() %>%
            dplyr::pull(.data$period_end_date) %>%
            purrr::pluck(1)

        area_name_txt <- selected_area_level

        if (input_explore_tabs == "All") {

            split_by_txt <- NULL

        } else if (geog_bar) {

            # Get indicator type for this metric (i.e. proportion or count)
            indicator_type <- df %>%
                dplyr::distinct(.data$indicator_type) %>%
                dplyr::pull(.data$indicator_type) %>%
                purrr::pluck(1)

            split_by_txt <- paste0("split by ",
                                   dplyr::if_else(!stringr::str_detect(stringr::str_to_lower(input_explore_tabs), "icb|pcn"),
                                                  stringr::str_replace(input_explore_tabs, "^\\w{1}", tolower),
                                           input_explore_tabs),
                                   ", ")

            # Ensure date range correct as only latest year of data used for proportion bar charts
            if (indicator_type == "Proportion") {
                # We use latest year of data for proportion bar charts in geographies
                min_period_start_date <- df %>%
                    dplyr::filter(.data$period_start_date > max(.data$period_start_date) - lubridate::years(1)) %>%
                    dplyr::pull(.data$period_start_date) %>%
                    min(na.rm = TRUE)

                min_period_end_date <- min_period_start_date + lubridate::years(1) - lubridate::days(1)

                max_period_start_date <- min_period_start_date

                max_period_end_date <- min_period_end_date

            } else if (indicator_type == "Count") {
                # We use latest period of data for count bar charts in geographies
                min_period_start_date <- max(df$period_start_date, na.rm = TRUE)
            }

        } else {
            split_by_txt <- paste0("split by ",
                                   stringr::str_replace(input_explore_tabs, "^\\w{1}", tolower),
                                   ", ")
        }
    } else { # If a specific area is selected
        df <- df %>%
            dplyr::filter(.data$metric_category == input_explore_tabs | .data$area_name == "England" & .data$metric_category_group == "All")

        min_period_start_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::pull(.data$period_start_date) %>%
            purrr::pluck(1)

        min_period_end_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::pull(.data$period_end_date) %>%
            purrr::pluck(1)

        max_period_start_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::slice_tail() %>%
            dplyr::pull(.data$period_start_date) %>%
            purrr::pluck(1)

        max_period_end_date <- df %>%
            dplyr::arrange(.data$period_start_date) %>%
            dplyr::slice_tail() %>%
            dplyr::pull(.data$period_end_date) %>%
            purrr::pluck(1)

        if (stringr::str_detect(stringr::str_to_lower(input_explore_tabs), "region|icb|provider|pcn")) {
            area_name_txt <- paste0(selected_area_name, " versus national level")

            split_by_txt <- NULL
        } else {
            area_name_txt <- selected_area_name

                min_period_start_date <- df %>%
                dplyr::filter(.data$area_name == selected_area_name) %>%
                dplyr::arrange(.data$period_start_date) %>%
                dplyr::pull(.data$period_start_date) %>%
                purrr::pluck(1)

            min_period_end_date <- df %>%
                dplyr::filter(.data$area_name == selected_area_name) %>%
                dplyr::arrange(.data$period_start_date) %>%
                dplyr::pull(.data$period_end_date) %>%
                purrr::pluck(1)

            max_period_start_date <- df %>%
                dplyr::filter(.data$area_name == selected_area_name) %>%
                dplyr::arrange(.data$period_start_date) %>%
                dplyr::slice_tail() %>%
                dplyr::pull(.data$period_start_date) %>%
                purrr::pluck(1)

            max_period_end_date <- df %>%
                dplyr::filter(.data$area_name == selected_area_name) %>%
                dplyr::arrange(.data$period_start_date) %>%
                dplyr::slice_tail() %>%
                dplyr::pull(.data$period_end_date) %>%
                purrr::pluck(1)

            split_by_txt <- paste0("split by ",
                                   stringr::str_replace(input_explore_tabs, "^\\w{1}", tolower),
                                   ", ")
        }
    }

    header_text <- header_text_fn(df, split_by_txt, area_name_txt, min_period_start_date,
                                  min_period_end_date, max_period_start_date, max_period_end_date)

    return(header_text)
}


# Function to get text for the data source (sits below to chart)
get_source_text <- function(df){

    temp <- paste0("Source: ",
                   unique(df$data_source),
                   " - ",
                   unique(df$data_provider))

    return(temp)
}

# Function to get text for the interpretation notes (sits below chart)
get_caution_text <- function(df){

    temp <- paste0("Note: ",
                   unique(df$caution))

    return(temp)
}

