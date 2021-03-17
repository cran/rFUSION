#' @title filterData
#' @param fusion.path Character. By default: C:/FUSION/. Path where the program FUSION is installed.
#' @description FilterData applies various filters to return data files to produce new return data files with only the returns that meet the filter requirements.
#' The most common application for FilterType is to remove “outliers” from return data files. Other filter options overlay the return data with a user-specified
#' grid and produce output return files that contain only the returns with the minimum or maximum elevation for each grid cell.
#'
#' @param fusion.path Character. The directory where FUSION is installed. By defult: C:/FUSION/
#' @param FilterType Character. Filtering algorithm used to remove returns from the DataFile(s).  \cr
#' The following options (by name) are supported:
#' \itemize{
#'   \item outlier - removes returns above or below the mean elevation plus or minus FilterParms * standard deviation of the elevations
#'   \item outlier2 - More robust outlier removal (experimental)
#'   \item minimum - removes all returns except the return with the minimum elevation
#'   \item maximum - removes all returns except the return with the maximum elevation
#'   }
#'
#' @param FilterParms Numeric. Parameters specific to the filtering method.
#' For outlier this is the multiplier applied to the standard deviation. For minimum and maximum,
#' FilterParms is ignored (but a value must be included on the command line...use 0)
#' @param WindowSize Numeric. Size of the window used to compute the standard deviation of elevations or the minimum/maximum return
#' @param OutputFile Character. Name of the output file. If any part of the name includes spaces, include the entire name in double quotation marks.
#' Must have the extension.
#' @param DataFile Character. LIDAR data file name or template or name of a text file containing a list of file names (list file must have .txt extension).
#' @param switches Character. Default is NULL. To insert a switch, each switch must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value depending on see switch.
#'  \itemize{
#'   \item /lda - Write output files using FUSION's LDA format when using LAS input files. The default behavior after FUSION version 3.00 is to write data in LAS format when the input data are in LAS format. When using input data in a format other than LAS, sample files are written in LDA format.
#'   \item /layers - Output intermediate raster data layers.
#'   \item /index - Create FUSION index files for the output file.
#'   \item /invert - Inverts the elevations for points so the logic will work for points below ground. Use with outlier2.
#'   \item /minsd:# - Minimum standard deviation for points within a comparison window for filtering to take place. Default is 1.0 (same units as
#'         elevation data). This switch is only useful when using the outlier filter.
#'   \item /minpts:# - Minimum number of points in the comparison window for filtering to take place. This option can be used with all filters but must specify at least 3 points when used with the outlier filter.
#'   \item /minrange:# - Minimum range in elevations within a window for outlier filtering to take place. Default is 150.0 elevation units Used only with the outlier2 filter.
#'   \item /mingap:# - Minimum vertical distance that define a gap. Used to isolate points above the majority of points in the filter window. Used only with the outlier2 filter
#'   \item /gapratio:# - Proportion of points in window that can be above a vertical gap. Ranges from 0.0 to 1.0 Used only with the outlier2 filter.
#'   \item /class:string - Used with LAS format files only. Specifies that only points with classification values listed are to be included in the subsample. Classification values should be separated by a comma e.g. (2,3,4,5) and can range from 0 to 31. If the first character of string is “~”, all classes except those listed will be used.
#'   \item /ignoreoverlap - Ignore points with the overlap flag set (LAX V1.4+ format).
#'   \item /precision:scaleX,scaleY,scaleZ - Control the scale factor used for X, Y, and Z values in output LAS files. These values will override the values in the source LAS files. There is rarely any need for the scale parameters to be smaller than 0.001.
#'   \item /reclass:# - Change the classification code for points identified as outliers and write them to the output file. The optional value is the classification code assigned to the points. Only valid when used with the outlier and outlier2 filters. The code must be between brackets.
#'   }
#' @details FilterData was developed to help LIDAR data users eliminate outliers from files delivered by vendors. In general, vendors identify outliers (returns above expected elevations for vegetation and structures or returns below the ground surface)
#' and either use the LAS classification field to label the return as an outlier or delete them from the files delivered to their client.
#' However, sometimes not all outliers are removed. The presence of unlabeled outliers can cause problems for bare-earth filtering algorithms and vegetation analysis as well as other analyses. FilterData offers a way for the data user to produce “clean” data files for use in subsequent analyses.
#' \cr FilterData provides an outlier filter that identifies and removes returns based on the range of observed elevation values in the comparison window. In operation, the outlier filter works by computing the mean elevation and standard deviation of elevations for each cell in the comparison grid.
#' Then, individual return elevations are compared to range defined as follows:\cr
#' \deqn{mean elevation ± (FilterParms * ElevationStandardDeviation)}
#' \cr Only returns with elevations within the range are written to the output file. Generally, using a range of ± 5.0 * Standard deviation and a large window size (100 m) eliminates most outliers. In areas if steep terrain with returns from birds, a range of ± 3 * Standard deviation may produce better results.
#' The outlier filter can also be used on return files produced using the maximum filter to eliminate high returns from small objects such as transmission towers and lines. Flat areas with no above-ground features can result in a very low standard deviation of the return elevations.
#' For data files containing such areas, it may be necessary to use the /minsd:# switch to control filtering in cells with small standard deviations. The default is to use a minimum threshold standard deviation of 1.0 (same units as the return elevations). For most areas, this will be sufficient. If you specify a smaller threshold, you may find that all returns within the comparison window are removed.
#' \cr FilterData also provides a minimum and maximum classification feature that produces output files that contain only the return with the minimum or maximum elevations for each cell in the comparison grid.
#' @return No return value. This function return the command prompt running the FUSION command
#' @examples
#' filterData(FilterType = 'outlier2',FilterParms = 3,WindowSize =  5,
#' OutputFile = 'Z:/filterdata.las',DataFile = 'Z:/datafile.las', switches = '/invert /layers')
#' @references McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
#' @export

filterData<-function(fusion.path='C:/FUSION/', FilterType,
                     FilterParms, WindowSize, OutputFile, DataFile, switches=NULL){

  if (!(is.numeric(FilterParms) || is.numeric(WindowSize))){
    stop('FilterParms and WindowSize arguments must be numeric.')}

  if (is.numeric(OutputFile)|| is.numeric(DataFile)){
      stop('OutputFile and DataFile arguments must be a path (character)')}

  if (str_contains(switches, 'invert') ==TRUE & FilterType != 'outlier2'){
    stop('The switch /invert must be used with the FilterType outlier2')}

  if (str_contains(switches, 'minsd') ==TRUE & FilterType != 'outlier'){
    stop('The switch /minsd must be used with the FilterType outlier')}

  if (str_contains(switches, 'minrange:') ==TRUE & FilterType != 'outlier2'){
    stop('The switch /minrange: must be used with the FilterType outlier2')}

  if (str_contains(switches, 'minpts') ==TRUE & FilterType != 'outlier2'){
    print('The switch /minpts must be used with the FilterType outlier2')}

  if (str_contains(switches, 'mingap:') ==TRUE & FilterType != 'outlier2'){
    stop('The switch /mingap: must be used with the FilterType outlier2')}

  if (str_contains(switches, 'gapratio') ==TRUE & FilterType != 'outlier2'){
    print('The switch /gapratio must be used with the FilterType outlier2')}

  if (str_contains(switches, 'reclass')== TRUE & any(FilterType == 'outlier2' | FilterType == 'outlier')==FALSE){
      print('The switch /reclass must be used with the FilterType outlier2 or oulier')
  } else {
    ifelse(is.null(switches)==TRUE, switches<-'',switches<-switches)
    filterData<-paste(paste0(fusion.path,'FilterData'), switches, FilterType, FilterParms, WindowSize,
                    OutputFile, DataFile) %>% as.matrix()
    apply(filterData,1,system)

  }}


