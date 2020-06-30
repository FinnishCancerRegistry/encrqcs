


# taken from ENCR JRC Call for Data, 2015, version 1.1
column_specifications <- data.table::fread(
  "data-raw/column_specifications.csv", encoding = "UTF-8"
)

usethis::use_data(column_specifications, internal = TRUE, overwrite = TRUE)
