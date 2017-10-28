#' @title Import Apple Health data from the exported XML file
#'
#' @description Imports Apple Health data from its exported XML file into R. Cleans up the data and type identifiers. Based on R scripts as in the references.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import XML
#' @import tidyr
#'
#' @references (1) http://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/#4
#' @references (2) https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
#' @references (3) http://rpubs.com/Ranthony__/visualizing-iphone-health-app-data-in-R
#'
#' @param filename A XML file of the exported Apple Health data
#'
#' @return health_data A data frame containing the extracted health data
#'
#' @examples
#' # Generates two random measurements
#' # import_apple_health_xml("export.xml")
#' # apple_health_data <- import_apple_health_xml("export.xml")
#'
#' @export

import_apple_health_xml <- function( filename ) {

  # Load exported apple health XML file
  xml_health_data <- xmlParse( filename )

  # Transform xml file to data frame - select the Record rows from the xml file
  health_data <- XML:::xmlAttrsToDataFrame(xml_health_data["//Record"])

  # Make the 'value' variable numeric
  health_data$value <- as.numeric(as.character(health_data$value))

  # Make endDate in a date time variable POSIXct using lubridate with London time zone
  health_data$endDate <-ymd_hms(health_data$endDate,tz="Europe/London")

  # add in columns for: month, year, date, day of week, hour
  health_data$month     <- format(health_data$endDate,"%m")
  health_data$year      <- format(health_data$endDate,"%Y")
  health_data$date      <- format(health_data$endDate,"%Y-%m-%d")
  health_data$dayofweek <- wday(health_data$endDate, label=TRUE, abbr=FALSE)
  health_data$hour      <- format(health_data$endDate,"%H")

  # Clean up the type identifier so it is easier to parse
  health_data$type <- gsub('HKQuantityTypeIdentifier', "" , health_data$type )
  health_data$type <- gsub('HKCategoryTypeIdentifier', "" , health_data$type )

  return( health_data )

  # END OF FUNCTION

}

