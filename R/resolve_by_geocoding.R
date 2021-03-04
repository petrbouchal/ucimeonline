targets::tar_load(at_with_geo)
targets::tar_load(mdff)

resolved <- resolve_by_points(at_with_geo, mdf)

at_still_unresolved <- at_with_geo %>%
  filter(!id %in% resolved$id) %>%
  select(red_izo, nazev_skoly, adresa_lower, id)

# Match to school (not just org) ------------------------------------------


