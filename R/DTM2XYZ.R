#' @title DTM2XYZ
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @description DTM2XYZ converts data stored in the PLANS DTM format into ASCII text files containing XYZ points.
#' Such files can be imported into GIS software as point data with the elevation as an attribute or used in other analysis processes.
#' @param inputfile is a character.Name for output canopy surface file (stored in PLANS DTM format with .dtm extension).
#' @param outputfile Character. Name for the converted file. If outputfile is omitted, the output file name will be constructed from the inputfile name and the extension .xyz.
#' If the /csv switch is used, the extension will be .csv.
#' @param switches False as default. To insert a switch, it must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value (numeric).
#'
#' \itemize{
#'    \item /void - Output points from DTM with NODATA value (default is to omit). NODATA value is -9999.0 for the elevation
#'   \item /csv - Output XYZ points in comma separated value format (CSV). If /csv is used with no outputfile, an extension of .csv will be used to form the output file name.
#'   \item /noheader - Do not include the column headings in CSV output files. Ignored if /csv is not used
#'    }
#' @details The XYZ point file consists of one record for each grid point. Each record contains the X, Y, and elevation for the DTM grid point. If creating an ASCII text file, the values are separated by spaces and if creating a CSV format file, by commas.
#' For CSV files, the first line contains column labels unless the /noheader switch is specified.
#' If four or more of the directional searches find a valid elevation, the hole is filled using the average of all the values.
#' @return No return value. This function return the command prompt running the FUSION command
#' @import dplyr sjmisc
#' @examples
#' inputfile<-'Z:/dtmfile.dtm'
#' outputfile<- 'Z:/outputfile.xyz'
#' DTM2XYZ(inputfile = inputfile,outputfile = outputfile, switches = NULL)
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

DTM2XYZ<-function(fusion.path='C:/FUSION/', inputfile,
                  outputfile, switches=NULL){
    if (!(is.character(inputfile) & is.character(outputfile))){
    stop('arguments must be numeric.')
  } else {
    ifelse(is.null(switches)==TRUE, switches<-'',switches<-switches)
    DTM2XYZ<-paste(paste0(fusion.path,'DTM2XYZ'),switches,inputfile, outputfile) %>% as.matrix()
    apply(DTM2XYZ,1,system)

  }}
