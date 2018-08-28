#' @title "ah_data_summarise" - Summarises requested Apple Health information from the extracted data frame to actually be useful
#'
#' @description Summarises requested Apple Health information from the extracted data frame to actually be useful
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @param health_data A data frame containing extracted health data, in a format generated by the ah_import_xml.r function
#' @param type_filter Parameter to select the element "type" of health data to extract. Named so as the XML file groups elements by "type". See your data frame to see what types are available.
#' @param summary_period A period of time to summarise. Default is hour
#'
#' @return summary_data A data frame containing the selected type elements
#'
#' @examples
#' # PUT EXAMPLES HERE
#'
#' @note Common types to pass to type_filter include:
#' @note BodyMassIndex
#' @note BodyMass
#' @note Height
#' @note HeartRate
#' @note DistanceWalkingRunning
#' @note StepCount
#'
#' @export

ah_data_summarise <- function( health_data , type_filter = NULL , summary_period = "hour"  ) {

  # Error checks

  if ( is.null(health_data) )
    stop("You must specify data frame containing health data.")

  if ( is.null(type_filter) )
    stop("You need to select one filter for this function to be useful.")

  if ( !summary_period %in% c("hour", "day") )
    stop("You must specify a valid period of time to summarise by.")

  # End of error checks



  # Start of summarising functions

  # Summarises per hour
  if( summary_period == "hour" ){

    summary_data <- health_data %>%
      dplyr::filter( .data$type == type_filter ) %>%
      dplyr::group_by( .data$date , .data$hour , .data$sourceName) %>%
      dplyr::summarise( value = sum(.data$value) )

  }


  # Summarises per day
  if( summary_period == "day" ){

    summary_data <- health_data %>%
      dplyr::filter( .data$type == type_filter ) %>%
      dplyr::group_by( .data$date , .data$sourceName) %>%
      dplyr::summarise( value = sum(.data$value) )

  }

  # End of summary functions






  # Return summary data

  return( summary_data )

  ## END OF FUNCTION

}
