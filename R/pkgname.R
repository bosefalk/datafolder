#' @section Overview:
#'
#' \code{\link{datafolder_update}} creates a list of the file names and their md5 hashes for all files in a data folder (and all subfolders), and
#' writes this to a .csv file (default is docs/data_folder_content.csv). This csv file can then be synced to git remotes - this way although the data
#' itself is not synced, the data files expected by the code at each commit is documented.
#'
#' When pulling the remote we can check that the contents of our local repository data folder matches this list. \code{\link{datafolder_check}}
#' prints any mismatches in some detail - which files are missing, which files appear in data/ but not the csv list, which have been
#' renamed and so on - allowing you to manually copy the data to match what the code expects through some other secure channel. If run
#' with \code{datafolder_check(stop_on_error = FALSE} raises a warning when mismatches occur instead of an error.
#'
#' This doesn't work very well with projects where the data is frequently updated in which case an alternative solution is probably appropriate.
#' These functions assume that your R project is structured
#' with a main project working directory, and all data is located within a specific
#' subfolder inside this project (i.e. data/), and there is another folder for general documentation / configuration (i.e. docs/). For an example structure see
#' \url{http://projecttemplate.net/architecture.html}
#'
#' @section Functions:
#'
#' \code{\link{datafolder_update}}: Write docs/data_folder_content.csv listing the files in data/ and their md5 hashes
#'
#' \code{\link{datafolder_check}}: Check docs/data_folder_content.csv against the actual data/ files and list any mismatches
#'
#' @section Example workflow:
#' \itemize{
#' \item Create a new R project, and in this project create a "data" and a "docs" folder
#' \item Add a dummy text file to the data/ directory, and git commit this (to create an empty data/ directory when cloned on another machine)
#' \item Add data/ and all subfolders to .gitignore
#' \item Place your actual datafiles in data/
#' \item Run \code{datafolder_update()} to generate docs/data_folder_content.csv
#' \item Commit this .csv file (git should now ignore any files in data/)
#' \item When pulling in the future, run datafolder_check() to make sure everything is up to date, and use the printed output to manually copy missing or changed data files
#' \item Run datafolder_update() if you've added new data files to be tracked
#' }
#'
#'
#' @section Location:
#'
#' \url{https://github.com/bosefalk/datafolder}
#'
#' \code{devtools::install_github("bosefalk/datafolder")}
#'
#' @keywords internal
"_PACKAGE"


