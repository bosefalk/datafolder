context("Testing data folder content checks")
library(testthat)

old_wd <- getwd()


# Setup test environment --------------------------------------------------

# Set up test environment in a fresh temp directory, where the files in data/ matches docs/data_folder_content.csv,
# as a baseline we can adjust
baseloc <- tempfile()
dir.create(baseloc)
setwd(baseloc)
dir.create("data")
dir.create("data/subdir")
dir.create("docs")

# Create three data files in the data subdirectory
write.csv(
  data.frame(colA = c("A", "B"),
             colB = c(13, 90)),
  file = "data/fileA.csv",
  row.names = FALSE
)
write.table(
  data.frame(colC = c("C", "D"),
             colD = c(20, 10)),
  file = "data/fileB.txt",
  row.names = FALSE
)
write.csv(
  data.frame(colDate = c(as.Date("2018-01-02"), as.Date("2019-02-02")),
             colFactor = as.factor(c("factorA", "factorB"))),
  file = "data/subdir/fileC.csv",
  row.names = FALSE
)

# Create a data_folder_content matching these files
datafiles <- data.frame(filepath = list.files("data", recursive = TRUE), stringsAsFactors = FALSE)
for (i in 1:nrow(datafiles)) {
  datafiles$md5[i] <- as.character(openssl::md5(file(paste0("data/", datafiles$filepath[i]))))

}
write.csv(
  datafiles,
  file = "docs/data_folder_content.csv",
  row.names = FALSE
)

# Function to make a copy of the clean base directory and move the working directory to it
copy_baseloc <- function() {
  tmp_testdir <- tempfile()
  dir.create(tmp_testdir)
  file.copy(paste0(baseloc, "//"), tmp_testdir, recursive = TRUE)
  setwd(tmp_testdir)
}


# Run tests ---------------------------------------------------------------

test_that("Correctly identify missing, changed and renamed files in updated docs/data_folder_content.csv", {

  # csv and data files match, no output expected
  copy_baseloc()
  expect_silent(datafolder_check())


  # Remove one file from data folder
  copy_baseloc()
  file.remove("data/fileB.txt")
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "New files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileB.txt")


  # Change content of a file
  # Change one cell in fileA.csv, save it in a temp location and update data_folder_content.csv with the md5 from
  # this new temporary fileA.csv
  copy_baseloc()
  fileA <- read.csv("data/fileA.csv", stringsAsFactors = FALSE)
  fileA$colB[1] <- 15
  tmp_fileA <- tempfile(fileext = ".csv")
  write.csv(fileA, tmp_fileA, row.names = FALSE)
  fileA_newmd5 <- as.character(openssl::md5(file(tmp_fileA)))
  data_folder_content <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)
  data_folder_content$md5[data_folder_content$filepath == "fileA.csv"] <- fileA_newmd5
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Changed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileA.csv")


  # Renamed files, rename fileC.csv to fileX.csv in data_folder_content
  copy_baseloc()
  data_folder_content <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)
  data_folder_content$filepath[3] <- "subdir/fileX.csv"
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Renamed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileX.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileC.csv")


  # One missing file, one changed file and one renamed file:
  copy_baseloc()

  file.remove("data/fileB.txt")

  fileA <- read.csv("data/fileA.csv", stringsAsFactors = FALSE)
  fileA$colB[1] <- 15
  tmp_fileA <- tempfile(fileext = ".csv")
  write.csv(fileA, tmp_fileA, row.names = FALSE)
  fileA_newmd5 <- as.character(openssl::md5(file(tmp_fileA)))
  data_folder_content <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)
  data_folder_content$md5[data_folder_content$filepath == "fileA.csv"] <- fileA_newmd5

  data_folder_content$filepath[3] <- "subdir/fileX.csv"

  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )

  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "New files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileB.txt")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Changed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileA.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Renamed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileX.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileC.csv")





})


test_that("data files updated but not reflected in data_folder_content.csv", {

  # New file added to data but not reflected in data_folder_content.csv
  copy_baseloc()
  write.csv(
    data.frame(colT = c("T", "U"),
               colU = c(3, 7)),
    file = "data/fileD.csv",
    row.names = FALSE
  )
  expect_message(datafolder_check(), "New files appeared in data folder")
  expect_message(datafolder_check(), "fileD.csv")


  # Changing a file should give the same error as if a new data_folder_content.csv was downloaded and had an updated md5
  copy_baseloc()
  fileA <- read.csv("data/fileA.csv", stringsAsFactors = FALSE)
  fileA$colB[1] <- 15
  write.csv(fileA, "data/fileA.csv", row.names = FALSE)
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Changed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileA.csv")


  # Same with renaming a file
  copy_baseloc()
  file.rename("data/subdir/fileC.csv", "data/subdir/fileX.csv")
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Renamed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileX.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileC.csv")

  # One new file in data, one renamed and one changed:
  copy_baseloc()
  write.csv(
    data.frame(colT = c("T", "U"),
               colU = c(3, 7)),
    file = "data/fileD.csv",
    row.names = FALSE
  )
  fileA <- read.csv("data/fileA.csv", stringsAsFactors = FALSE)
  fileA$colB[1] <- 15
  write.csv(fileA, "data/fileA.csv", row.names = FALSE)
  file.rename("data/subdir/fileC.csv", "data/subdir/fileX.csv")
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Missing or outdated files in data:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Changed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileA.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Renamed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileX.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileC.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "New files appeared in data folder")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileD.csv")


})

test_that("If there are too many different files, open a tempfile with list instead of printing to console", {

  # 20 new files listed in .csv
  copy_baseloc()
  new_dfc <- data.frame(filepath = replicate(20, tempfile(tmpdir = "", fileext = ".csv")),
                        md5 = replicate(20, as.character(openssl::md5(tempfile()))),
                        stringsAsFactors = FALSE)
  new_dfc$filepath <- gsub("/", "", new_dfc$filepath)
  data_folder_content <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)
  data_folder_content <- rbind(data_folder_content, new_dfc)
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), NA) # Expect no messages as it should have opened a new window
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")

  # 20 new files added to data subfolder but not listed in csv
  copy_baseloc()
  rnd_strings <- replicate(20, paste0(sample(LETTERS, 50, replace = TRUE), collapse = ""))
  for (l in rnd_strings) {
    writeLines(l, tempfile(tmpdir = "data", fileext = ".csv"))
  }
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), NA)

  # 10 files added to data folder and 10 new entries added to .csv
  copy_baseloc()
  new_dfc <- data.frame(filepath = replicate(10, tempfile(tmpdir = "", fileext = ".csv")),
                        md5 = replicate(10, as.character(openssl::md5(tempfile()))),
                        stringsAsFactors = FALSE)
  new_dfc$filepath <- gsub("/", "", new_dfc$filepath)
  data_folder_content <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)
  data_folder_content <- rbind(data_folder_content, new_dfc)
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  rnd_strings <- replicate(10, paste0(sample(LETTERS, 50, replace = TRUE), collapse = ""))
  for (l in rnd_strings) {
    writeLines(l, tempfile(tmpdir = "data", fileext = ".csv"))
  }
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), NA)
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
})


test_that("datafolder_update correctly writes a csv", {
  copy_baseloc()
  file.remove("docs/data_folder_content.csv")
  datafolder_update()
  expect_silent(datafolder_check())

  copy_baseloc()
  file.remove("docs/data_folder_content.csv")
  file.remove("data/fileB.txt")
  datafolder_update()
  expect_silent(datafolder_check())


})


test_that("datafolder_check() handles missing expected inputs", {

  # No docs/data_folder_content.csv
  copy_baseloc()
  file.remove("docs/data_folder_content.csv")
  expect_error(datafolder_check(), "docs/data_folder_content.csv does not exist ")

  # No docs folder at all
  copy_baseloc()
  unlink("docs", recursive = TRUE)
  expect_error(datafolder_check(), "Missing docs folder")

  # No data folder
  copy_baseloc()
  unlink("data", recursive = TRUE)
  expect_error(datafolder_check(), "Missing data folder")

  # Empty data folder
  copy_baseloc()
  unlink("data", recursive = TRUE)
  dir.create("data")
  expect_error(datafolder_check(), "data folder is empty, exiting datafolder_check()")

  # Empty data files, should work fine just list all files as changed due to the md5 being the same
  copy_baseloc()
  file.remove("data/fileA.csv")
  file.remove("data/fileB.txt")
  file.remove("data/subdir/fileC.csv")
  file.create("data/fileA.csv")
  file.create("data/fileB.txt")
  file.create("data/subdir/fileC.csv")
  expect_error(datafolder_check(), "data folder file list doesn't match docs/data_folder_content.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "Changed files:")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileA.csv")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileB.txt")
  expect_message(suppressWarnings(datafolder_check(stop_on_error = FALSE)), "fileC.csv")

  # Missing columns and malformed data in data_folder_content.csv
  copy_baseloc()
  good_dfc <- read.csv("docs/data_folder_content.csv", stringsAsFactors = FALSE)

  data_folder_content <- good_dfc
  data_folder_content$md5 <- c(1, 23, 52)
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "path or md5 column in docs/data_folder_content.csv not read as character strings")

  data_folder_content <- good_dfc
  data_folder_content$filepath <- NA_character_
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "path or md5 column in docs/data_folder_content.csv not read as character strings")

  data_folder_content <- good_dfc
  data_folder_content$filepath <- NULL
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "path or md5 column missing from docs/data_folder_content.csv")

  data_folder_content <- good_dfc
  data_folder_content$md5 <- NULL
  write.csv(
    data_folder_content,
    file = "docs/data_folder_content.csv",
    row.names = FALSE
  )
  expect_error(datafolder_check(), "path or md5 column missing from docs/data_folder_content.csv")


})

test_that("accepts non-default docs locations", {

  # csv and data files match, no output expected, with new docs location
  copy_baseloc()
  dir.create("docsnew")
  file.copy(file.path("docs", "data_folder_content.csv"),
            "docsnew", recursive = TRUE)
  unlink("docs", recursive = TRUE)

  expect_error(datafolder_check())
  expect_silent(datafolder_check(docs_folder = "docsnew"))

  # Test with subdirectories of docs
  dir.create(file.path("docsnew", "subdir"))
  file.rename(file.path("docsnew", "data_folder_content.csv"),
              file.path("docsnew", "subdir", "data_folder_content.csv"))
  expect_error(datafolder_check(docs_folder = "docsnew"))
  expect_silent(datafolder_check(docs_folder = file.path("docsnew", "subdir")))

})

test_that("accepts non-default data locations", {

  # csv and data files match, no output expected, with new data location
  copy_baseloc()
  dir.create("datanew")
  file.copy(list.files("data", full.names = TRUE),
            "datanew", recursive = TRUE)
  unlink("data", recursive = TRUE)


  expect_error(datafolder_check())
  expect_silent(datafolder_check(data_folder = "datanew"))

  # Test with subdirectories of data
  dir.create(file.path("datanew", "datasubdir"))
  dir.create(file.path("datanew", "datasubdir", "subdir"))
  file.rename(file.path("datanew", list.files("datanew", recursive = TRUE)),
              file.path("datanew", "datasubdir", list.files("datanew", recursive = TRUE)))
  expect_error(datafolder_check(data_folder = "datanew"))
  expect_silent(datafolder_check(data_folder = "datanew/datasubdir"))

})

test_that("datafolder_update can handle non-default locations", {
  copy_baseloc()
  file.remove("docs/data_folder_content.csv")
  unlink("docs", recursive = TRUE)
  dir.create("docsnew")
  expect_error(datafolder_update())
  datafolder_update(docs_folder = "docsnew")
  expect_silent(datafolder_check(docs_folder = "docsnew"))

})

# Return from test environment --------------------------------------------

# Return to the original working directory after the tests have been carried out
setwd(old_wd)
detach("package:testthat")

