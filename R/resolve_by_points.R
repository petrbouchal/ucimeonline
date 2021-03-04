resolve_by_points <- function(at_with_geo, mdf) {

  mdff <- mdf %>% st_drop_geometry()

  at_unnested <- at_with_geo %>%
    select(id, adresa_lower, starts_with("geo_")) %>%
    unnest(geo_cuzk) %>%
    rename(pt_cuzk = geometry, add_cuzk = address) %>%
    unnest(geo_osm) %>%
    replace_na(list(long = 0, lat = 0)) %>%
    mutate(pt_osm = map2(long, lat, ~st_point(c(.x, .y), dim = "XY")) %>%
             st_as_sfc()) %>%
    st_set_geometry("pt_cuzk") %>% st_set_crs(4326) %>%
    st_set_geometry("pt_osm") %>% st_set_crs(4326)

  ids_nomatch <- at_with_geo %>%
    mutate(izoornazev = m_red_izo | m_nazev_exp) %>%
    filter(!izoornazev) %>%
    pull(id)

  geo_reg <- mdf %>%
    select(geometry)

  geo_reg_nosf <- mdf %>%
    select(geometry, red_izo, izo) %>%
    mutate(index = row_number(),
           geom_txt = st_as_text(geometry)) %>%
    st_drop_geometry()

  at_locate <- at_unnested %>%
    filter(id %in% ids_nomatch) %>%
    select(id, starts_with("pt_")) %>%
    mutate(nearest_adr_cuzk = st_nearest_feature(pt_cuzk, geo_reg$geometry),
           nearest_adr_osm = st_nearest_feature(pt_osm, geo_reg$geometry)) %>%
    left_join(geo_reg_nosf %>%
                rename(geom_nearest_cuzk = geom_txt,
                       red_izo_cuzk = red_izo, izo_cuzk = izo),
              by = c(nearest_adr_cuzk = "index")) %>%
    left_join(geo_reg_nosf %>%
                rename(geom_nearest_osm = geom_txt,
                       red_izo_osm = red_izo, izo_osm = izo),
              by = c(nearest_adr_osm = "index")) %>%
    replace_na(list(geom_nearest_cuzk = "POINT (0 0)")) %>%
    mutate(geom_nearest_osm = st_as_sfc(geom_nearest_osm),
           geom_nearest_cuzk = st_as_sfc(geom_nearest_cuzk)) %>%
    st_set_geometry("geom_nearest_cuzk") %>% st_set_crs(4326) %>%
    st_set_geometry("geom_nearest_osm") %>% st_set_crs(4326) %>%
    mutate(dist_cuzk = st_distance(pt_cuzk, geom_nearest_cuzk, by_element = T) %>% units::drop_units(),
           dist_osm = st_distance(pt_osm, geom_nearest_osm, by_element = T) %>% units::drop_units()) %>%
    replace_na(list(dist_cuzk = 1e10, dist_osm = 1e10)) %>%
    mutate(cuzk_better = dist_cuzk < dist_osm,
           dist_better = if_else(cuzk_better, dist_cuzk, dist_osm),
           same_redizo = red_izo_cuzk == red_izo_osm,
           dist_geoms = st_distance(geom_nearest_osm, geom_nearest_cuzk, by_element = T) %>% units::drop_units())
  # print("A")
  at_georesolved <- at_locate %>%
    group_by(id) %>%
    filter(dist_better == min(dist_better)) %>%
    ungroup() %>%
    mutate(red_izo = if_else(cuzk_better, red_izo_cuzk, red_izo_osm),
           geometry = ifelse(cuzk_better, st_as_text(geom_nearest_cuzk),
                             st_as_text(geom_nearest_osm)),
           geometry = st_as_sfc(geometry)) %>%
    filter(dist_better < 20) %>%
    st_set_geometry("geometry") %>%
    st_set_crs(4326) %>%
    select(id, red_izo, dist_better) %>%
    left_join(mdff %>% select(red_izo, plny_nazev, obec_nazev)) %>%
    left_join(at_with_geo %>% select(id, nazev_skoly, nazev_skoly_expanded,
                                     adresa_lower, skola_druh_kod)) %>%
    mutate(stringdist = stringdist::stringdist(plny_nazev, nazev_skoly_expanded))

  # print("B")
  at_georesolved_fin <- at_georesolved %>%
    mutate(reg_match_type = "geo") %>%
    select(id, red_izo, reg_match_type, skola_druh_kod)

  # print("C")
  at_nameresolved <- at_with_geo %>%
    filter(!m_red_izo, !(id %in% at_georesolved$id)) %>%
    select(id, key = nazev_skoly_expanded, skola_druh_kod) %>%
    mutate(key = str_squish(key) %>% tolower()) %>%
    left_join(mdff %>%
                select(red_izo, key = plny_nazev) %>%
                mutate(key = tolower(key),
                       mtchd = TRUE)) %>%
    drop_na(mtchd) %>%
    mutate(reg_match_type = "school name") %>%
    select(id, red_izo, reg_match_type, skola_druh_kod)

  # print("D")
  at_redizoresolved <- at_with_geo %>%
    select(id, nazev_skoly, red_izo, skola_druh_kod) %>%
    mutate(red_izo = str_trim(red_izo)) %>%
    left_join(mdff %>% select(plny_nazev, red_izo) %>% mutate(mtchd = T)) %>%
    drop_na(mtchd) %>%
    mutate(reg_match_type = "red_izo") %>%
    select(id, red_izo, reg_match_type, skola_druh_kod)

  # print("E")
  resolved <- bind_rows(at_redizoresolved, at_nameresolved, at_georesolved_fin) %>%
    distinct(id, red_izo, reg_match_type, skola_druh_kod, reg_match_type)

  return(resolved)
}

merge_at_with_ids <- function(at_dc, at_ids, mdff) {
  at_resolved <- at_dc %>%
    select(-red_izo) %>%
    left_join(distinct(at_ids))

  at_with_izo <- at_resolved %>%
    select(-starts_with("m_")) %>%
    left_join(mdff %>% select(red_izo, izo, skola_druh_kod))
}
