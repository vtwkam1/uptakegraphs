#' Suppress small numbers and round to 5
#'
#' @description
#' `replace_and_round()` replaces all values between 1 and 10 (inclusive) with 10
#' and rounds all numbers above 10 to the nearest 5.
#'
#' @param x Number to round or suppress
#'
#' @return Numeric
#'
#' @examples
#' replace_and_round(6)
#'
#' @export

replace_and_round <- function(x) {
    ifelse(x >= 1 & x <= 10, 10, round(x/5)*5)
}

#' Apply disclosure control to standard table
#'
#' @description
#' `apply_disclosure_control()` applies disclosure control to data in the standard data structure.
#' For the denominator and numerator columns, it applies the `replace_and_round()` function. It then
#' recalculates the indicator column using the rounded values.
#'
#' @param df Dataframe in the standard uptake data table structure.
#'
#' @return Dataframe
#'
#' @examples
#'
#' @export

apply_disclosure_control <- function(df) {

    # Check that input df contains columns called numerator and denominator
    if(!"numerator" %in% colnames(df) | !"denominator" %in% colnames(df)) {
        stop('Input error: Dataframe needs to have columns named "numerator" and "denominator"')
    }

    # Apply replace_and_round function to numerator and denominator columns. Recalculate indicator
    df %>%
        dplyr::mutate(numerator = replace_and_round(numerator),
               denominator = replace_and_round(denominator),
               indicator = round(numerator/denominator, 4)
               )

}
