# datafolder: List and check contents of data subfolder

### Description

This is intended as a light-weight solution to the following problem: You can sync code to a git remote, but do not want to sync the actual data. It creates a list of the contents of a data subfolder, writes this to a csv file which can be synced to git remotes, with functions to check for mismatches between this .csv and the actual data folder contents.

### Overview

`datafolder_update()` creates a list of the file names and their md5 hashes for all files in a data folder (and all subfolders), and writes this to a .csv file (default is docs/data_folder_content.csv). This csv file can then be synced to git remotes - this way although the data itself is not synced, the data files expected by the code at each commit is documented.

When pulling the remote we can check that the actual contents of out local repository data folder matches this list. `datafolder_check()` prints any mismatches in some detail - which files are missing, which files appear in data/ but not the csv list, which have been renamed and so on - so you can manually copy the data to match what the code expects through some other secure channel. If run with `datafolder_check(stop_on_error = FALSE)` raises a warning when mismatches occur instead of an error.

This doesn't work very well with projects where the data is frequently updated in which case an alternative solution is probably appropriate. These functions assume that your R project is structured with a main project working directory, and all data is located within a specific subfolder inside this project (i.e. data/), and there is another folder for general documentation / configuration (i.e. docs/). For an example structure see <http://projecttemplate.net/architecture.html>

### Functions

`datafolder_update()`: Write docs/data_folder_content.csv listing the files in data/ and their md5 hashes

`datafolder_check()`: Check docs/data_folder_content.csv against the actual data/ files and list any mismatches

### Example workflow

* Create a new R project, and in this project create a "data" and a "docs" folder
* Add a dummy text file to the data/ directory, and git commit this (to create an empty data/ directory when cloned on another machine)
* Add data/ and all subfolders to .gitignore
* Place your actual datafiles in data/
* Run `datafolder_update()` to generate docs/data_folder_content.csv
* Commit this .csv file (git should now ignore any files in data/)
* When pulling in the future, run `datafolder_check()` to make sure everything is up to date, and use the printed output to manually copy missing or changed data files
* Run `datafolder_update()` if you've added new data files to be tracked

### Location

<https://github.com/bosefalk/datafolder>

`devtools::install_github("bosefalk/datafolder")`


### Author(s)

Maintainer: Bose Falk <falk@dkms.de>
