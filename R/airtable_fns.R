at_load <- function(airtable_base_id, airtable_table_id) {

  # https://github.com/bergant/airtabler

  # https://www.basegenius.com/airpower/help/how-to-find-airtable-base-id/
  # https://airtable.com/api
  # https://airtable.com/appE3bg248O9wK2aM/api/docs#curl/table:tech.%20pomoc%20skoly

  # needs AIRTABLE_API_KEY env var
  airtable_uo <- airtable(
    base = airtable_base_id,
    tables = airtable_table_id
  )

  at_uo_dt <- airtable_uo$`tech. pomoc skoly`$select_all() %>%
    janitor::clean_names() %>%
    rename(red_izo = red_izo_skoly) %>%
    mutate(web_base = stringr::str_remove(webova_adresa_skoly,
                                          "^(https?\\:\\/\\/)?www\\.") %>%
             str_trim(),
           web_domain = urltools::domain(webova_adresa_skoly) %>% str_trim(),
           web_domain_bare = str_remove(web_domain, "^www\\."),
           datum_pozadavku = map_chr(datum_pozadavku_dd_mm_rrrr, as.character) %>%
             lubridate::ymd(),
           timestamp = lubridate::parse_date_time(timestamp, orders = "d/m/y H:M:S", tz = "CET")) %>%
    select(-datum_pozadavku_dd_mm_rrrr)

  return(at_uo_dt)
}

at_process <- function(data, sch_reg_org, sch_reg_sch, sch_adr_org, at_manual_ids) {

  manual_redizo <- at_manual_ids %>%
    distinct(id, red_izo)

  data %>%
    # filter(nazev_skoly != "Testovací škola") %>%
    # filter(nazev_skoly != "Czech & Slovak School Bristol") %>%
    rows_patch(manual_redizo) %>%
    mutate(red_izo = str_trim(red_izo),
           nazev_skoly_expanded = str_replace(nazev_skoly, "ZŠ", "Základní škola") %>%
             str_replace("MŠ", "Mateřská škola") %>%
             str_replace("[Zz][Uu][Šš]", "Základní umělecká škola") %>%
             str_replace("p\\.\\s?\\o.", "příspěvková organizace") %>%
             str_trim() %>%
             str_squish(),
           adresa_lower = tolower(if_else(str_detect(ulice_a_c_p_skoly, mesto_skoly), ulice_a_c_p_skoly, paste(ulice_a_c_p_skoly, mesto_skoly)) %>%
                                    str_replace("(\\b\\d{3})\\s(\\d{2}\\b)", "\\1\\2") %>%
                                    str_replace_all("\\\n|\\\t", " ")),
           m_red_izo = red_izo %in% sch_reg_org$red_izo,
           m_izo = red_izo %in% sch_reg_sch$izo,
           m_ico = red_izo %in% sch_reg_org$ico,
           m_nazev_exp = tolower(nazev_skoly_expanded) %in%
             tolower(sch_reg_org$red_plny_nazev),
           m_any = m_red_izo | m_ico | m_nazev_exp) %>%
    rename(stupen = pro_jaky_stupen_hledate_pomoc) %>%
    mutate(skola_druh_kod = case_when(str_detect(nazev_skoly_expanded, "[Dd][ůŮ][Mm]") ~ "G11",
                                      str_detect(nazev_skoly_expanded, "ákladní umělecká škola") ~ "F10",
                                      str_detect(nazev_skoly_expanded, "^[Vv]yšší odb") ~ "E00",
                                      stupen == "Střední škola" ~ "C00",
                                      str_detect(stupen, "základní") ~ "B00"))
}

geocode_one_cuzk <- function(address) {
  gc <- RCzechia::geocode(address)
  # print(address)

  if(is.data.frame(gc)) {
    gc <- filter(gc, typ == "AdresniMisto")

    # if address ends in a house number, take rows with no street names
    if(str_detect(address, "\\b\\d{1,3}$") & nrow(gc) > 1) {
      gc <- filter(gc, str_detect(address, "^č\\.p\\."))
    }
  }
  return(gc)
}


geocode_cuzk <- function(data, variable) {
  data %>%
    # filter(!m_any) %>%
    filter(!is.na({{variable}})) %>%
    select(id, {{variable}}) %>%
    # sample_n(10) %>%
    mutate(geo_cuzk = map({{variable}}, geocode_one_cuzk))
}

geocode_one_osm <- function(address) {
  add <- paste(address, ", Czech Republic")
  # print(add)
  tidygeocoder::geo_osm(add, min_time = 1)
}

geocode_osm <- function(data, variable) {
  data %>%
    select(id, {{variable}}) %>%
    mutate(geo_osm = map({{variable}}, geocode_one_osm))
}

identify_unmatched_airtable_rows <- function(at_with_ids, at_manual_ids) {
  at_with_ids %>%
    filter(is.na(red_izo) | !is.na(red_izo) & (is.na(izo) | is.na(skola_druh_kod))) %>%
    filter(!id %in% at_manual_ids$id[is.na(at_manual_ids$red_izo)]) %>%
    select(id, red_izo, izo, skola_druh_kod, nazev_skoly_expanded, nazev_skoly, adresa_lower,
           reg_match_type) %>%
    as_tibble()
}
