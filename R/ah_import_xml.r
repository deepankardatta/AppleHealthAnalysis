#' @title "ah_import_xml": Import Apple Health data from the exported XML file
#'
#' @description Imports Apple Health data from its exported XML file into R. Cleans up the data and type identifiers.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import xml2
#' @import purrr
#' @import lubridate
#' @importFrom gdata humanReadable
#'
#' @param import_filename The name of a XML file containing exported Apple Health data, written in inverted commas
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

ah_import_xml <- function( import_filename ) {

  if ( is.null(import_filename) )
    stop("You must specify an input filename.")

  # Get file size data and estimates time
  file_info <- file.info(import_filename)
  message( paste0( "File size: " , gdata::humanReadable(file_info$size) ) )
  message( paste0( "Estimated import time at 5MiB/s: " ,
                 round(file_info$size / (5*(1024)^2 )) ,
                 " seconds") )

  # Start timer
  start_time <- Sys.time()
  message( paste0( "Import started at: " , start_time ) )

  # Load exported apple health XML file
  xml_health_data <- read_xml( import_filename )

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
  health_data <- health_data[ c( "type" , "sourceName" , "value" , "unit" , "endDate"  ) ]

  # Make the 'value' variable numeric
  # (Explore why I have written the function this way later)
  health_data$value <- as.numeric(as.character(health_data$value))

  # Clean up the type identifier so it is easier to parse
  health_data$type <- gsub('HKQuantityTypeIdentifier', "" , health_data$type )
  health_data$type <- gsub('HKCategoryTypeIdentifier', "" , health_data$type )

  # Make the type identifier to a factor - this makes it easier for me down the line to sort it
  health_data$type       <- health_data$type %>% as.factor()

  # Clean up source names (some seem to contain special characters) and make factor
  health_data$sourceName <- gsub("[^A-Za-z0-9 ]", "", health_data$sourceName, perl = TRUE) %>% as.factor

  # Use lubridate package to format the dates
  health_data$endDate    <- health_data$endDate %>% ymd_hms()

  # add in columns for dates and times
  health_data$year       <- health_data$endDate %>% year() %>% as.factor()
  health_data$month      <- health_data$endDate %>% month() %>% as.factor()
  health_data$month_name <- health_data$endDate %>% month(label = TRUE, abbr = FALSE ) %>% as.factor()
  health_data$day        <- health_data$endDate %>% mday() %>% as.factor()
  health_data$day_name   <- health_data$endDate %>% wday( label=TRUE, abbr=FALSE ) %>% as.factor()
  health_data$date       <- health_data$endDate %>% as_date()
  health_data$hour       <- health_data$endDate %>% hour() %>% as.factor()
  health_data$minutes    <- health_data$endDate %>% minute() %>% as.factor()

  # Stop timer and calculate running times
  end_time <- Sys.time()
  message( paste0( "Import finished at: " , end_time ) )
  running_time <- difftime( end_time , start_time , units = "secs" )
  message( paste0( "Time for import: " , round(running_time) , " seconds" ) )
  message( paste0( "File size/running time: " ,
                 gdata::humanReadable(file_info$size/as.double(running_time) ) ,
                 " per second") )

  return( health_data )

  # END OF FUNCTION

}
