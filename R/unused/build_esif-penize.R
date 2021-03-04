tf <- tempfile(fileext = "xls")
download.file("https://dotaceeu.cz/getmedia/a6c01899-a81f-4856-a97f-f29fc45098ec/2020_10_Seznam-operaci_List-of-operations.xls.aspx?ext=.xls",
              destfile = tf)

esif <- read_excel(tf, skip = 2, col_types = "text") %>%
  slice(-1) %>%
  clean_names() %>%
  mutate(ico = str_pad(ic, width = 8, pad = "0"),
         across(starts_with("celkove"), as.numeric),
         across(starts_with("financni"), as.numeric))

esif_slct <- esif %>%
  select(celkove_zpusobile_vydaje_pridelene_na_operaci_czk, ico) %>%
  filter(ico %in% skoly_coords$ico) %>%
  group_by(ico) %>%
  summarise(pocet_projektu = n(),
            penize_vse_smlouva = sum(celkove_zpusobile_vydaje_pridelene_na_operaci_czk))

esif_per_cap <- skoly_coords %>%
  left_join(esif_slct) %>%
  left_join(r_skoly %>% select(redizo, izo, kapacita)) %>%
  mutate(kapacita = as.numeric(kapacita),
         esif_per_cap = penize_vse_smlouva/kapacita) %>%
  filter(kapacita > 0) %>%
  st_join(aglo_large %>%
            select(URAU_FULL = URAU_NAME)) %>%
  st_join(aglo_core %>%
            select(URAU_CORE = URAU_NAME)) %>%
  mutate(place_type = case_when(!is.na(URAU_CORE) ~ "metropolis",
                                !is.na(URAU_FULL) ~ "metro area",
                                TRUE ~ "elsewhere"))

esif_per_cap %>%
  group_by(place_type) %>%
  summarise(mn = median(esif_per_cap, na.rm = T))

esif_per_cap_obce <- esif_per_cap %>%
  st_set_geometry(NULL) %>%
  group_by(kod_obce) %>%
  summarise(esif_per_cap = mean(esif_per_cap, na.rm = T)) %>%
  filter(esif_per_cap > 0) %>%

obce %>%
  left_join(esif_per_cap_obce) %>%
  ggplot() +
  geom_sf(aes(fill = esif_per_cap), colour = NA) +
  scale_fill_viridis_c()
