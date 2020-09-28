library(stringr)
library(zip)

# file with protected sheets
file <- "input/raw_data/MCNA_VIII_2020_-_latest_version_-_False_-_2020-08-10-05-51-47.xlsx"

# file name and path after removing protection
file_unlocked <- str_replace(basename(file), ".xlsx$", "_unlocked.xlsx")
file_unlocked_path <- file.path(getwd(), "data", file_unlocked)

# create temporary directory in project folder
# so we see what is going on
temp_dir <- "_tmp"

# remove and recreate _tmp folder in case it already exists
unlink(temp_dir, recursive = T) 
dir.create(temp_dir)

# unzip Excel file into temp folder
unzip(file, exdir = temp_dir)

# get full path to XML files for all worksheets 
worksheet_paths <- list.files(
  paste0(temp_dir, "/xl/worksheets"), 
  full.name = TRUE, 
  pattern = ".xml")

# remove the XML node which contains the sheet protection 
# We might of course use e.g. xml2 to parse the XML file, but this simple approach will suffice here
for (ws in worksheet_paths) {
  x <- readLines(ws, encoding = "windows1")
  # the "sheetProtection" node contains the hashed password "<sheetProtection SOME INFO />"
  # we simply remove the whole node 
  out <- str_replace(x, "<sheetProtection.*?/>", "")  
  writeLines(out, ws)  
}

# create a new zip, i.e. Excel file, containing the modified XML files
old_wd <- setwd(temp_dir)
f <- list.files(recursive = T, full.names = F, all.files = T, no..=T)
# as the Excel file is a zip file, we can directly replace the .zip extension by .xlsx
zip::zip(file_unlocked_path, files = f) # utils::zip does not work for some reason
setwd(old_wd)

# clean up and remove temporary directory
unlink(temp_dir, recursive = T)
