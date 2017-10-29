#' @title "ah_import_xml": Import Apple Health data from the exported XML file
#'
#' @description Imports Apple Health data from its exported XML file into R. Cleans up the data and type identifiers.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import xml2
#' @import dplyr
#' @import purrr
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate wday
#'
#' @param filename The name of a XML file containing exported Apple Health data, written in inverted commas
#'
#' @return health_data A data frame containing the extracted health data
#'
#' @references http://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/#4
#' @references https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
#' @references http://rpubs.com/Ranthony__/visualizing-iphone-health-app-data-in-R
#' @references http://www.tdda.info/in-defence-of-xml-exporting-and-analysing-apple-health-data
#'
#' @examples
#' # Loads the data
#' # ah_import_xml("export.xml")
#' # Loads the data, and moves it into a data frame
#' # health_data <- ah_import_xml("export.xml")
#'
#' @export

ah_import_xml <- function( filename ) {

  if ( is.null(filename) )
    stop("You must specify an input filename!")

  # Load exported apple health XML file
  xml_health_data <- read_xml( filename )

  # Extracts the health records, selects the 'Record' elements
  # And then transforms into a data frame using the 'purrr' library
  health_data <- xml_find_all( xml_health_data , "//Record") %>% map(xml_attrs) %>% map_df(as.list)
  # Does the same, but for the one element which contains the personal data
  # Currently not exported back out, but kept in for future use
  # personal_data <- xml_find_all( xml_health_data , "//Me") %>% xml_attrs()

  # Ways that I tried and didn't work
  # xml_find_all( xml_health_data , "//Record") # this will just give the raw XML
  # xml_find_all( xml_health_data , "//Record") %>% xml_attrs() # this will do it as a list

  # Subset the data to get rid of the columns we don't need
  # We take endDate, but could easily take startDate
  health_data <- health_data[ c( "type" , "value" , "unit" , "endDate"  ) ]

  # Make the 'value' variable numeric
  health_data$value <- as.numeric(as.character(health_data$value))

  # Clean up the type identifier so it is easier to parse
  health_data$type <- gsub('HKQuantityTypeIdentifier', "" , health_data$type )
  health_data$type <- gsub('HKCategoryTypeIdentifier', "" , health_data$type )

  # Make endDate in a date time variable POSIXct using lubridate with London time zone
  health_data$endDate <- ymd_hms( health_data$endDate )

  # add in columns for: month, year, date, day of week, hour
  health_data$month     <- format( health_data$endDate,"%m" )
  health_data$year      <- format( health_data$endDate,"%Y" )
  health_data$date      <- format( health_data$endDate,"%Y-%m-%d" )
  health_data$dayofweek <- wday( health_data$endDate, label=TRUE, abbr=FALSE )
  health_data$hour      <- format( health_data$endDate,"%H" )

  return( health_data )

  # END OF FUNCTION

}
