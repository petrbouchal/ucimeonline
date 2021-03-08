load_dns_gsheet <- function(sheet_id) {
  dns <- googlesheets4::read_sheet(sheet_id,
                                   col_types = "c---------cciicccc") %>%
    rename(red_izo = RED_IZO) %>%
    clean_names()
}
