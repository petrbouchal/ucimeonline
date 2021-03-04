load_mpo_pokryti <- function(variables) {
  read_excel(mpo_xlsx, skip = 1,
                    col_types = "text") %>%
    rename(red_izo = RED_IZO, ico = IČO) %>%
    clean_names() %>%
    select(-c_obce, -c_p, -c_or, -psc, -misto, -nazev_adm, -kraj, -uzemi,
           -spravni_urad, -orp, -ulice) %>%
    mutate(ico = str_trim(ico),
           across(matches("^pocet|^aktivni"), as.numeric))
}

