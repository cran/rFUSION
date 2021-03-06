% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DTMDescribe.R
\name{DTMDescribe}
\alias{DTMDescribe}
\title{DTMDescribe}
\usage{
DTMDescribe(fusion.path = "C:/FUSION/", inputfile, outputfile, switches = NULL)
}
\arguments{
\item{fusion.path}{Character. By default: C:/FUSION/. Path where the program FUSION is installed.}

\item{inputfile}{is a character.DTM file name, DTM file template, or name of a text file containing a list of file names (must have .txt extension).}

\item{outputfile}{Character. Name for the output ASCII CSV file. If no extension is provided, an extension (.csv) will be added.}

\item{switches}{False as default. To insert a switch, it must have a '/' before of the names. If you want to insert two or more switches, they must be separated by an empty space. When a # is displayed, should be replaced by the desired value (numeric).

\itemize{
\item /stats - Compute descriptive statistics for the data values in the DTM.
}}
}
\value{
No return value. This function return the command prompt running the FUSION command
}
\description{
DTMDescribe reads header information for PLANS format DTM files and outputs the information to an ASCII text file compatible with most spreadsheet and database programs. DTMDescribe can provide information for a single file or multiple files.
Such files can be imported into GIS software as point data with the elevation as an attribute or used in other analysis processes.
}
\details{
DTMDescribe produced output files in comma separated value (CSV) format and includes column labels in the first line of the file. The following header information from the DTM file is included in the CSV file:
File name
Descriptive name
Origin (X, Y)
Upper right (X, Y)
Number of columns
Number of rows
Column spacing
Row spacing
Minimum data value
Maximum data value
Horizontal units
Vertical units
Variable type
Coordinate system
Coordinate zone
Horizontal datum
Vertical datum
}
\examples{
inputfile<-'Z:/dtmfile.dtm'
outputfile<- 'Z:/outputfile.xyz'
DTMDescribe(inputfile = inputfile,outputfile = outputfile, switches = NULL)
}
\references{
McGaughey, R.J. FUSION/LDV: Software for LIDAR Data Analysis and Visualization. 2021.
}
