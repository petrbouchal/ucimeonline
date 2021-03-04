
# Aglomerace --------------------------------------------------------------

ua <- giscoR::gisco_get_urban_audit()
ua_cz <- filter()

aglo_large <- ua_cz %>%
  filter(URAU_CATG == "F")

aglo_core <- ua_cz %>%
  filter(URAU_CATG == "C")

write_rds(aglo_core, "urau_aglo_core.rds")
write_rds(aglo_large, "urau_aglo_large.rds")

skoly_coords %>%
  select(redizo) %>%
  st_join(aglo_large %>%
            select(URAU_FULL = URAU_NAME)) %>%
  st_join(aglo_core %>%
            select(URAU_CORE = URAU_NAME))

# Nezamestnanost ----------------------------------------------------------

nezam_obce <- czso::czso_get_table("250169r20") %>%
  filter(vuk_text == "Podíl nezaměstnaných osob") %>%
  rename(kod_obce = uzemi_kod)

pocobyv_obce <- czso::czso_get_table("130141r20") %>%
  filter(vuk_text == "Střední stav obyvatel" & vuzemi_cis == "43") %>%
  rename(kod_obce = vuzemi_kod)

nezam_delta <- nezam_obce %>%
  mutate(mesic_int = as.integer(mesic)) %>%
  group_by(kod_obce) %>%
  arrange(rok, mesic_int) %>%
  mutate(rozdil = last(hodnota) - first(hodnota)) %>%
  filter(mesic == "01") %>%
  arrange(desc(rozdil))

obce <- giscoR::gisco_get_communes() %>%
  filter(CNTR_CODE == "CZ") %>%
  select(kod_obce = NSI_CODE, nuts = NUTS_CODE)

obce %>%
  left_join(nezam_delta) %>%
  ggplot() +
  geom_sf(aes(fill = rozdil), colour = NA) +
  scale_fill_gradient2(high = "red", mid = "white", low = "green")
