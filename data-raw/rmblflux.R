
# Download raw data -------------------------------------------------------

dir.create("inst/extdata/almont_peak_day")
dest <- 'inst/extdata/almont_peak_day/06122020_almont_day_1a.txt'


f1 <- download.file("https://raw.githubusercontent.com/PaulESantos/flux_data/main/almont%20peak%20day/06122020_almont_day_1a.txt",
                    destfile = dest)

usethis::use_data(f1,
                  internal = TRUE,
                  overwrite = TRUE)


# ---------------------------------------------------------------
