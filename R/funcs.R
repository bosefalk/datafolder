#' Check local data folder is up-to-date
#'
#' We don't want to sync the actual data to git remotes. Instead, we list the files expected in the data/ subfolder in docs/data_folder_content.csv
#' with their md5 checksums.
#' This .csv should be synced to git, and this function run when git pulling, to see if the actual data folder needs updating. This function compares
#' docs/data_folder_content.csv to the actual contents in data/, and outputs an error or warning if things are not matching, with a list
#' of mismatches.
#'
#' @param stop_on_error if TRUE (default), stops execution with an error message when a file is listed in data_folder_content.csv but doesn't match
#' the contents of the data folder. If set to FALSE, instead outputs a warning and continues execution of the code.
#' @param data_folder data folder path
#' @param docs_folder subfolder where the data_folder_content.csv file is located. Currently cannot be the working directory root.
#'
#'
#' @details The md5 checksum is created using \code{openssl::md5(file(<filename>))}
#'
#' Generating or updating docs/data_folder_content.csv is done using \code{\link{datafolder_update}}
#'
#' @return If no mismatches has been found, returns nothing. If a mismatch is found an error or warning is raised, with a message listing
#'  the files which are missing, changed, renamed, or are new in the data folder and not yet recorded. If the number of files to list is >15
#'  a temp file is opened with this information instead of printing it to console
#'
#' @seealso \code{\link{datafolder_update}}
#'
#' @export
#' @importFrom openssl md5
#' @importFrom dplyr anti_join
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom rlang .data
datafolder_check <- function(stop_on_error = TRUE, docs_folder = "docs", data_folder = "data") {

  # Check workspace is as expected
  if (!docs_folder %in% list.files(recursive = TRUE, include.dirs = TRUE)) {
    stop("Missing ", docs_folder, " folder")
  }
  if (!data_folder %in% list.files(recursive = TRUE, include.dirs = TRUE)) {
    stop("Missing ", data_folder, " folder")
  }

  if (!file.exists(file.path(docs_folder, "data_folder_content.csv"))) {
    stop(paste0(file.path(docs_folder, "data_folder_content.csv"), " does not exist so no checking is done,
                   run datafolder_update() to generate list from current data folder"))
  }

  if (length(list.files(data_folder)) == 0) {
    stop(data_folder, " folder is empty, exiting datafolder_check()")
  }

  # Check data_folder_content.csv is as expected
  data_folder_content <- utils::read.csv(file.path(docs_folder, "data_folder_content.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  if(!"filepath" %in% colnames(data_folder_content) | !"md5" %in% colnames(data_folder_content)) {
    stop(paste("filepath or md5 column missing from", file.path(docs_folder, "data_folder_content.csv")))
  }

  # If it's not by default read as character strings something is wrong with the input file, which is why character types
  # are not defined at csv read-in
  if (class(data_folder_content$filepath) != "character" | class(data_folder_content$md5) != "character") {
    stop(paste("filepath or md5 column in", file.path(docs_folder, "data_folder_content.csv"), "not read as character strings"))
  }


  # datafolder are the actual files in the data/ folder, data_folder_content is the list of what should
  # be there from the docs .csv file
  datafolder <- data.frame(filepath = list.files(data_folder, recursive = TRUE), stringsAsFactors = FALSE)
  for (i in 1:nrow(datafolder)) {
    datafolder$md5[i] <- as.character(openssl::md5(file(file.path(data_folder, datafolder$filepath[i]))))

  }

  # First look at what appears in the csv but not the data folder
  changes <- dplyr::anti_join(data_folder_content, datafolder, by = c("filepath", "md5"))

  # Files where the filename doesn't exist are considered new, where the filename exists but md5 is different
  # are changed, and where md5 is same but filename is new are considered renamed
  new_files <- dplyr::filter(changes, !.data$filepath %in% datafolder$filepath & !.data$md5 %in% datafolder$md5)
  changed_files <- dplyr::filter(changes, .data$filepath %in% datafolder$filepath & !.data$md5 %in% datafolder$md5)
  changed_files <- dplyr::left_join(changed_files, datafolder, by = "filepath", suffix = c(".new", ".old"))
  renamed_files <- dplyr::filter(changes, !.data$filepath %in% datafolder$filepath & .data$md5 %in% datafolder$md5)
  renamed_files <- dplyr::left_join(renamed_files, datafolder, by = "md5", suffix = c(".new", ".old"))

  # These are files which appear in the data folder but are not listed in data_folder_content
  data_new_files <- dplyr::anti_join(datafolder, data_folder_content, by = c("filepath", "md5"))
  # Renamed and changed files will already be listed by changed_files and renamed_files above, so only interested in
  # new files which appear in the data folder
  data_new_files <- dplyr::filter(data_new_files, !.data$filepath %in% data_folder_content$filepath & !.data$md5 %in% data_folder_content$md5)

  # Construct error message to be passed to message()
  error_msg <- function(nf = new_files, cf = changed_files, rf = renamed_files) {

    error_string <- paste0("Missing or outdated files in ", data_folder,
                           ":\nIf data folder is accurate run datafolder::datafolder_update()\n")

    if (nrow(nf) > 0) {
      error_string <- paste0(error_string, "New files:\n")
      error_string <- paste0(error_string, paste0(utils::capture.output(new_files), collapse = "\n"), "\n")
    }

    if (nrow(cf) > 0) {
      error_string <- paste0(error_string, "Changed files:\n")
      error_string <- paste0(error_string, paste0(utils::capture.output(changed_files), collapse = "\n"), "\n")

    }

    if (nrow(rf) > 0) {
      error_string <- paste0(error_string, "Renamed files:\n")
      error_string <- paste0(error_string, paste0(utils::capture.output(renamed_files), collapse = "\n"), "\n")
    }

    return(error_string)
  }

  # If the number of discrepancies is too large (>=15) instead of cluttering the console the details are written to a temp file which
  # is displayed
  if (nrow(data_new_files) + nrow(changes) > 15) {
    error_file <- tempfile()
  }

  # New files appearing in the data folder
  if (nrow(data_new_files) > 0) {
    dnf_string <- "New files appeared in data folder, run datafolder::datafolder_update()\n"
    dnf_string <- paste0(dnf_string, paste0(utils::capture.output(data_new_files), collapse = "\n"), "\n")

    if (exists("error_file")) {
      sink(error_file)
      cat(dnf_string)
      sink()

      if (nrow(changes) == 0) { # if there are only added files the error block which displays the file below won't trigger
        file.show(error_file, title = "datafolder_check() messages")
      }

    } else {
      message(dnf_string)
    }
  }

  # If there are discepancies between the csv and actual data file, send a message with the details and either throw an error or
  # a warning depending on the stop_on_error flag
  if (nrow(changes) > 0) {

    error_string <- error_msg()

    if (exists("error_file")) { # Too long to display as a message, open up a file with details of what is missing

      sink(error_file, append = TRUE)
      cat(error_string)
      sink()
      file.show(error_file, title = "datafolder_check() messages")

    } else {
      message(error_string)
    }

    if (stop_on_error == TRUE) {
      stop(paste0("data folder file list doesn't match ", file.path(docs_folder, "data_folder_content.csv")))
    } else {
      warning(paste0("data folder file list doesn't match ", file.path(docs_folder, "data_folder_content.csv")))
    }
  }

  # If everything matches return nothing
}

#' Save data folder content list to sync to git
#'
#' Creates the docs/data_folder_content.csv file which can be synced to git, and used by \code{\link{datafolder_check}} to make sure the
#' local data folder is up-to-date with the rest of the code
#'
#' @param data_folder data folder path
#' @param docs_folder subfolder where the data_folder_content.csv file is located. Currently cannot be the working directory root.
#'
#' @details The md5 checksum is created using \code{openssl::md5(file(<filename>))}
#'
#' @return Nothing, writes docs/data_folder_content.csv directly. This csv has two columns, \code{filepath}, and \code{md5}
#'
#' @seealso \code{\link{datafolder_check}}
#'
#' @export
#' @importFrom openssl md5
datafolder_update <- function(docs_folder = "docs", data_folder = "data") {

  # Check workspace is as expected
  if (!docs_folder %in% list.files(recursive = TRUE, include.dirs = TRUE)) {
    stop("Missing ", docs_folder, " folder")
  }
  if (!data_folder %in% list.files(recursive = TRUE, include.dirs = TRUE)) {
    stop("Missing ", data_folder, " folder")
  }

  if (length(list.files(data_folder)) == 0) {
    stop(data_folder, " folder is empty, exiting datafolder_check()")
  }




  datafolder <- data.frame(filepath = list.files(data_folder, recursive = TRUE), stringsAsFactors = FALSE)
  for (i in 1:nrow(datafolder)) {
    datafolder$md5[i] <- as.character(openssl::md5(file(file.path(data_folder, datafolder$filepath[i]))))

  }
  utils::write.csv(datafolder, file = file.path(docs_folder, "data_folder_content.csv"), row.names = FALSE, fileEncoding = "UTF-8")
}
