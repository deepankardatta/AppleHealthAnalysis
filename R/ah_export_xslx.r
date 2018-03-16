#' @title "ah_export_xlsx": Writes imported Apple Health dataframe to XLSX format
#'
#' @description Writes imported Apple Health dataframe to XLSX format
#' @description This is easy with openxslx but thought I'd write a wrapper for me
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @import openxlsx
#'
#' @param health_data A data frame containing the data needed to be exported
#' @param export_filename A name for saving the file, written in inverted commas
#'
#' @examples
#' # Loads the data
#' # ah_export_xlsx( health_data , "excel_conversion.xlsx" )
#'
#' @export

ah_export_xlsx <- function( health_data ,
                            export_filename ) {

  # Error checks

  if ( is.null(health_data) )
    stop("You must specify data frame containing health data.")

  if ( is.null(export_filename) )
    stop("You must specify an export filename.")

  # End of error checks




  # What the function actually does

  write.xlsx( health_data , export_filename )

  #END OF FUNCTION

}
