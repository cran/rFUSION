#' @title DTMHeader
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @description DTMHeader is an interactive program. It is described in the Command Line Utility section because it provides a means to examine and modify PLANS DTM file header information. DTMHeader allows you to easily view and change the header information for a PLANS DTM file. To make it most convenient, associate the .dtm extension with DTMHeader so you can simply double-click a .dtm file to view the header. The values in the header that can be modified are:
#' Planimetric units,
#' Elevation units,
#' Descriptive name,
#' Coordinate system and zone,
#' Horizontal datum,
#' Vertical datum.
#' @param filename is a character.Name for output canopy surface file (stored in PLANS DTM format with .dtm extension).
#' @details The ENVI data file is created using the same numeric format as the PLANS DTM file. All PLANS DTM data types are supported. Geo-referencing information is included in the ENVI header file using the “map info” tag.
#' Areas in the DTM grid that have no data will be “marked” with a value of -9999.0 in the ENVI format file and the appropriate value will be included in the “data ignore value” tag in the ENVI header file.
#' @return No return value. This function return the command prompt running the FUSION command
#' @import dplyr sjmisc
#' @examples
#' filename<-'Z:/filename.dtm'
#' DTMHeader(filename = filename)
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

DTMHeader<-function(fusion.path='C:/FUSION/', filename){
  if (!(is.character(filename))){
    stop('arguments must be numeric.')
  } else {
    DTMHeader<-paste(paste0(fusion.path,'DTMHeader'),filename) %>% as.matrix()
    apply(DTMHeader,1,system)

  }}
