#' @title DTM2ENVI
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @description DTM2ENVI converts data stored in the PLANS DTM format into ENVI standard format raster files. Such files can be imported into GIS software such as ENVI and ArcInfo.
#' @param inputfile is a character.Name for output canopy surface file (stored in PLANS DTM format with .dtm extension).
#' @param outputfile Character. Name for the converted file. If outputfile is omitted, the output file name will be constructed from the inputfile name and the extension .xyz.
#' If the /csv switch is used, the extension will be .csv.
#' @param switches False as default. To insert a switch, it must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value (numeric).
#'
#' \itemize{
#'    \item /south - Specifies that data are located in the southern hemisphere.
#'  }
#' @details The ENVI data file is created using the same numeric format as the PLANS DTM file. All PLANS DTM data types are supported. Geo-referencing information is included in the ENVI header file using the “map info” tag.
#' Areas in the DTM grid that have no data will be “marked” with a value of -9999.0 in the ENVI format file and the appropriate value will be included in the “data ignore value” tag in the ENVI header file.
#' @return No return value. This function return the command prompt running the FUSION command
#' @import dplyr sjmisc
#' @examples
#' inputfile<-'Z:/dtmfile.dtm'
#' outputfile<- 'Z:/outputfile.xyz'
#' DTM2ENVI(inputfile = inputfile,outputfile = outputfile, switches = NULL)
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

DTM2ENVI<-function(fusion.path='C:/FUSION/', inputfile,
                  outputfile, switches=NULL){
  if (!(is.character(inputfile) & is.character(outputfile))){
    stop('arguments must be numeric.')
  } else {
    ifelse(is.null(switches)==TRUE, switches<-'',switches<-switches)
    DTM2ENVI<-paste(paste0(fusion.path,'DTM2ENVI'),switches, inputfile, outputfile) %>% as.matrix()
    apply(DTM2ENVI,1,system)

  }}
