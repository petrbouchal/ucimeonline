build_demog_all <- function(var_name = "Střední stav obyvatel", stats_rok = 2019) {
  czso_get_table("130141r20") %>%
    select(vuzemi_txt, vuk_text, vuzemi_kod, rok, vuzemi_cis, hodnota) %>%
    filter(rok == stats_rok, vuk_text %in% var_name)
}

build_demog_obce <- function(stat_obyv, breaks) {
  stat_obyv %>%
    filter(vuzemi_cis == "43") %>%
    rename(obec_kod = vuzemi_kod,
           obec_pocobyv = hodnota,
           obec_nazev = vuzemi_txt) %>%
    select(-vuzemi_cis, -vuk_text) %>%
    mutate(obec_pocobyv_kat = kiru(obec_pocobyv,
                                   breaks = obce_pocob_breaks,
                                   labels = lbl_dash(fmt = label_number_cz(1))))

}

build_demog_regionsum <- function(pop_data, uzemi_cis) {
  vazba_kod <- paste0("cis", uzemi_cis, "vaz", 43)

  cis_vazba <- czso_get_codelist(vazba_kod)
  cis_uzemi <- czso_get_codelist(uzemi_cis)

  pop_data_sum <- pop_data %>%
    filter(vuzemi_cis == 43) %>%
    left_join(cis_vazba, by = c(vuzemi_kod = "CHODNOTA2")) %>%
    left_join(cis_uzemi, by = c(CHODNOTA1 = "CHODNOTA")) %>%
    group_by(vuk_text, CZNUTS, TEXT1) %>%
    summarise(hodnota = sum(hodnota), .groups = "drop") %>%
    ungroup()

  return(pop_data_sum)
}

# build_demog_regionsum(pop_data, 101)
