#' @title DTM2TIF
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @description DTM2TIF converts data stored in the PLANS DTM format into a TIFF image and creates a world file that provides coordinate system reference data for the image. Such images can be imported into GIS software or used in other analysis processes.
#' @param inputfile is a character.Name for output canopy surface file (stored in PLANS DTM format with .dtm extension).
#' @param outputfile Character. Name for the converted file. If outputfile is omitted, the output file name will be constructed from the inputfile name and the extension .xyz.
#' If the /csv switch is used, the extension will be .csv.
#' @param switches False as default. To insert a switch, it must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value (numeric).
#'
#' \itemize{
#'    \item /mask - Produces a mask image showing the areas in the DTM with valid data values. In the mask image, a value of 0 indicates a cell with invalid data (NODATA value) and a value of 255 indicates a cell with a valid data value.
#'   \item /csv - Output XYZ points in comma separated value format (CSV). If /csv is used with no outputfile, an extension of .csv will be used to form the output file name.
#'   \item /noheader - Do not include the column headings in CSV output files. Ignored if /csv is not used
#'    }
#' @details DTM2TIF creates grayscale TIFF images that represent the data stored in a PLANS format DTM file. The range of values in the DTM file is scaled to correspond to gray values ranging from 1 to 255 in the TIFF image.
#' The gray level value of 0 is reserved to indicate NODATA areas in the DTM file (values less than 0.0).
#' DTM2TIF creates a world file to provide coordinates system information for the TIFF image. The world file is named using the same file name as the TIFF image but with the extension .tfw.
#' @return No return value. This function return the command prompt running the FUSION command
#' @import dplyr sjmisc
#' @examples
#' inputfile<-'Z:/dtmfile.dtm'
#' outputfile<- 'Z:/outputfile.xyz'
#' DTM2TIF(inputfile = inputfile,outputfile = outputfile, switches = NULL)
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

DTM2TIF<-function(fusion.path='C:/FUSION/', inputfile,
                  outputfile, switches=NULL){
  if (!(is.character(inputfile) & is.character(outputfile))){
    stop('arguments must be numeric.')
  } else {
    ifelse(is.null(switches)==TRUE, switches<-'',switches<-switches)
    DTM2TIF<-paste(paste0(fusion.path,'DTM2TIF'),switches, inputfile, outputfile) %>% as.matrix()
    apply(DTM2TIF,1,system)

  }}
