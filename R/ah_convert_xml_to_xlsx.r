#' @title "ah_convert_xml_to_xlsx": Converts Apple Health data from XML to XLSX format
#'
#' @description Converts Apple Health data from XML to XLSX format in one step.
#' @description You can easily do this manually but wrote it to save time for people.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import openxlsx
#'
#' @param export_filename A name for saving the file, written in inverted commas
#'
#' @inheritParams ah_import_xml
#'
#' @include ah_import_xml.r
#'
#' @examples
#' # Loads the data
#' # ah_convert_xml_to_xlsx( "export.xml" , "excel_conversion.xlsx" )
#'
#' @export

ah_convert_xml_to_xlsx <- function( filename , export_filename ) {

  if ( is.null(export_filename) )
    stop("You must specify an export filename!")

  health_data <- ah_import_xml( filename )
  write.xlsx( health_data , export_filename )

  #END OF FUNCTION

}
