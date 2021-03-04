build_address_arrowds <- function(zipfile, outdir) {

  unlink(outdir)

  td0 <- tempfile()
  adm_files <- unzip(zipfile, exdir = td0)

  read_adresy_jednaobec <- function(path) {
    # print(path)
    dt <- read_delim(path, delim = ";",
                     locale = locale(encoding = "Windows-1250", decimal_mark = ".", grouping_mark = " "),
                     col_types = list(`Číslo orientační` = "-",
                                      `Kód ADM` = "c",
                                      `Kód MOP` = "c",
                                      `Kód obce` = "c",
                                      `Název obce` = col_skip(),
                                      `Kód ulice` = "c",
                                      `Kód MOMC` = "c",
                                      `Název MOMC` = col_skip(),
                                      `Číslo domovní` = col_skip(),
                                      `Souřadnice X` = "d",
                                      `Souřadnice Y` = "d",
                                      `Název ulice` = col_skip(),
                                      `Typ SO` = col_skip(),
                                      `Název části obce` = col_skip(),
                                      `Platí Od` = col_skip(),
                                      `PSČ` = col_skip(),
                                      `Kód části obce` = "c",
                                      `Znak čísla orientačního` = col_skip())) %>%
      select(adm_kod = `Kód ADM`,
             obec_kod = `Kód obce`,
             momc_kod = `Kód MOMC`,
             sour_x = `Souřadnice X`,
             sour_y = `Souřadnice Y`)
    if(nrow(dt) == 0) {
      dt <- tibble(adm_kod = NA_character_,
                   obec_kod = "999999",
                   momc_kod = NA_character_,
                   sour_x = NA_real_,
                   sour_y = NA_real_)
    }
    return(dt)
  }

  rradm <- purrrow::marrow_dir(adm_files, read_adresy_jednaobec,
                               .path = outdir, .partitioning = "obec_kod")
  unlink(td0)
  return(rradm)
}

# build_address_arrowds("data-input/ruian_adm_all.zip", "data-processed/tmparrow")


build_hierarchie <- function(zipfile, outdir) {
  td1 <- tempfile()
  hier <- unzip(zipfile, exdir = td1)
  adm_vazby <- read_csv2(x)

  arrow::write_parquet(adm_vazby, "data-processed/adm_vazby.parquet")
  unlink(td0)
  return(path)
}
