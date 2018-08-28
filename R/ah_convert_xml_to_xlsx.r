#' @title "ah_convert_xml_to_xlsx": Converts Apple Health data from XML to XLSX format
#'
#' @description Converts Apple Health data from XML to XLSX format in one step.
#' @description You can easily do this manually but wrote it to save time for people.
#' @description This imports parameters from two other functions. This function does nothing with the health_data parameter though.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @inheritParams ah_import_xml
#' @inheritParams ah_export_xslx
#'
#' @include ah_import_xml.r
#' @include ah_export_xslx.r
#'
#' @param import_filename The name of a XML file containing exported Apple Health data
#' @param export_filename The name of the XLSX output filename
#'
#' @examples
#' # Loads the data
#' # ah_convert_xml_to_xlsx( "export.xml" , "excel_conversion.xlsx" )
#'
#' @export

ah_convert_xml_to_xlsx <- function( import_filename ,
                                    export_filename ) {

  if ( is.null(import_filename) )
    stop("You must specify an import filename.")

  if ( is.null(export_filename) )
    stop("You must specify an export filename.")

  # End of error checks




  # What the function actually does

  health_data <- ah_import_xml( import_filename )
  openxlsx::write.xlsx( health_data , export_filename )

  #END OF FUNCTION

}
