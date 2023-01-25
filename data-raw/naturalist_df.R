## code to prepare `transects_ik` dataset goes here


naturalist_df <- read.csv2("./data-raw/naturalist_df.csv",
                           fileEncoding = "UTF-8-BOM") %>%
  dplyr::mutate(date = lubridate::dmy(date))

usethis::use_data(naturalist_df, overwrite = TRUE)
