build_school_coords <- function(adb_arrow_path, sch_adr_sch, izo_slct) {
  adm <- open_dataset(adb_arrow_path) %>%
    select(adm_kod, sour_x, sour_y, obec_kod, momc_kod) %>%
    # head() %>%
    collect()

  # print(adm)

  # print(r_skoly %>% head())
  skoly_coords <- sch_adr_sch %>%
    filter(izo %in% izo_slct) %>%
    left_join(adm, by = c(kod_ruian = "adm_kod")) %>%
    mutate(geometry = map2(-sour_y, -sour_x,
                           ~st_point(x = c(.x, .y), dim = "XY"))) %>%
    st_as_sf() %>%
    st_set_crs(5514) %>%
    st_transform(4326) %>%
    mutate(coord_lon = map_dbl(geometry, `[[`, 1),
           coord_lat = map_dbl(geometry, `[[`, 2),
           point_wkt = st_as_text(geometry),
           obec_kod = as.character(obec_kod)) %>%
    select(red_izo, izo, ico, obec_kod, adm_kod = kod_ruian, momc_kod,
           starts_with("sour_"),
           starts_with("coord_"), point_wkt) %>%
    mutate(zuj_kod = if_else(is.na(momc_kod), obec_kod, momc_kod),
           zuj_typ = if_else(is.na(momc_kod), "obec", "momc"))
  return(skoly_coords)
}
