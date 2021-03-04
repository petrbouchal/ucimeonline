msmt_vykaz <- read_delim("data-input/ICT_2018_19.csv", delim = ";",
                         locale = locale(encoding = "windows-1250"),
                         col_types = list(RED_IZO = col_character(),
                                          KOD_RUIAN = "c")) %>%
  clean_names() %>%
  rename(redizo = red_izo,
         adm_kod = kod_ruian)

msmt_vykaz_key <- read_delim("data-input/ICT_R13_metadata_2018_19.csv",
                             delim = ";", locale = locale(encoding = "windows-1250"))

msmt_it <- read_excel("data-input/Seznamy škol-ICT technika a digi kompetence.xlsx",
                      sheet = 2, skip = 2) %>%
  clean_names()
