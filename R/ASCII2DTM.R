#' @title ASCII2DTM
#' @description ASCII2DTM converts raster data stored in ESRI ASCII raster format into a PLANS format data file. Data in the input ASCII raster file can represent a surface or raster data. ASCII2DTM converts areas containing NODATA values into areas with negative elevation values in the output data file.
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @param surfacefile Character. Name for output canopy surface file (stored in PLANS DTM format with .dtm extension).
#' @param xyunits Character. Units for LIDAR data XY:
#'  \itemize{
#'   \item M - for meters.
#'   \item F - for feet.
#'   }
#' @param zunits Character. Units for LIDAR data elevations:
#'  \itemize{
#'   \item M - for meters.
#'   \item F - for feet.
#'   }
#' @param coordsys Numeric. Coordinate system for the canopy model:
#' \itemize{
#'   \item 0 - for unknown.
#'   \item 1 - for UTM.
#'   \item 2 - for state plane.
#'     }
#' @param zone Numeric. Coordinate system zone for the canopy model (0 for unknown).
#' @param horizdatum Numeric. Horizontal datum for the canopy model.
#'  \itemize{
#'   \item 0 - for unknown.
#'   \item 1 - for NAD27.
#'   \item 2 -for NAD83.
#'     }
#' @param vertdatum Numeric. LIDAR data file name or template or name of a text file containing a list of file names (list file must have .txt extension).
#'  \itemize{
#'   \item 0 - for unknown.
#'   \item 1 - for NGVD29.
#'   \item 2 - for NAVD88.
#'   \item 3 - for GRS80.
#'     }
#' @param Gridfile Character. Name of the ESRI ASCII raster file containing surface data.
#' @param switches Character. NULL as default. To insert a switch, it must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value (numeric).
#'
#' \itemize{
#'    \item /multiplier:# - Multiply all data values in the input surface by the constant.
#'   \item /offset:# - Add the constant to all data values in the input surface. The constant can be negative.
#'   \item /nan - Create FUSION index files for the output file.
#'    }
#' @details ASCII2DTM recognizes both the (xllcorner, yllcorner) and (xllcenter, yllcenter) methods for specifying the location of the raster data. The PLANS DTM format used in FUSION
#' always assumes that the data point (grid point) in the lower left corner is the model origin and adjusts the location of the raster data accordingly. \cr
#' ASCII2DTM examines the ASCII raster file to determine whether the elevation values are integers or floating point numbers. It creates the PLANS DTM file using either integer or 4-byte floating point values for the elevations. \cr
#' ASCII2DTM always assumes that the data stored in ASCII raster format is interpreted as a raster. That is, the value is representative of the entire grid cell. For data that represent a surface where the values are actually elevations at specific points, the origin of the DTM file is set to the center of the lower left cell in the grid. \cr
#' If you receive surface data in ESRI’s GRID format it is possible to use GDAL (http://www.gdal.org/) to convert the GRID data into ASCII raster format. Refer to Appendix D: Building multi-processor workflows using AreaProcessor for more details.\cr
#' If you are using DTM2ASCII to convert data from the PLANS DTM format into ASCII raster format, you should always use the /raster switch in DTM2ASCII to ensure that you can convert the data back to the PLANS DTM format using ASCII2DTM.
#'
#' @return No return value. This function return the command prompt running the FUSION command
#'
#' @import dplyr sjmisc
#' @examples
#' surfacefile<-'Z:/dtmfile.dtm'
#' Gridfile<- 'Z:/gridfile.asc'
#' ASCII2DTM(surfacefile = surfacefile,xyunits = 'M',zunits = 'M',
#'           coordsys = 1,zone = 0, horizdatum = 0, vertdatum = 0,
#'                     Gridfile = Gridfile, switches = NULL)
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

ASCII2DTM<-function(fusion.path='C:/FUSION/', surfacefile,
                    xyunits, zunits, coordsys, zone, horizdatum, vertdatum, Gridfile, switches=NULL){
    if (!(is.numeric(horizdatum) & is.numeric(vertdatum))){
    stop('arguments must be numeric.')
  } else {
    ifelse(is.null(switches)==TRUE, switches<-'',switches<-switches)
    ascii2dtm<-paste(paste0(fusion.path,'ASCII2DTM'), switches,surfacefile,
                    xyunits, zunits, coordsys, zone, horizdatum, vertdatum, Gridfile) %>% as.matrix()
    apply(ascii2dtm,1,system)

  }}


