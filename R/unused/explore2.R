library(tidyverse)
library(arrow)
library(janitor)
library(sf)
library(statnipokladna)

r_adresar <- read_parquet("stistko/Adresar.parquet")
r_skoly <- read_parquet("stistko/Skoly.parquet")
r_mista <- read_parquet("stistko/SkolyAMista.parquet")

adm <- read_parquet("temp/ab.parquet")

names(r_skoly)

r_skoly %>%
  count(druh_typ, sort = T) %>%
  pull()

plot(skoly_coords, max.plot = 1)

mapview::mapview(skoly_coords)

budget <- statnipokladna::sp_get_table("budget-local", 2019, 12)
table(unique(budget$ico) %in% skoly_geom$ico)

vykzz <- statnipokladna::sp_get_table("profit-and-loss", 2018, 12)
table(unique(vykzz$ico) %in% skoly_geom$ico)

vykzz_coded <- vykzz %>%
  filter(ico %in% skoly_geom$ico) %>%
  sp_add_codelist("polvyk")

rozvaha <- statnipokladna::sp_get_table("balance-sheet", 2018, 12, ico = skoly_geom$ico)
table(unique(vykzz$ico) %in% skoly_geom$ico)

rozvaha_coded <- rozvaha %>%
  filter(ico %in% skoly_geom$ico) %>%
  sp_add_codelist("polvyk")

orgs <- sp_get_codelist("ucjed")

sp_get_codelist("polvyk")

r_skoly %>%
  filter(!(`Kód RÚIAN` %in% adm$kod_adm)) %>%
  View()
