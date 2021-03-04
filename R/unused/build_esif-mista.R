dt <- open_dataset("~/github/mmr-esif-clean/data-output/dtl-all-py/")
names(dt)

dt_prijemci <- read_parquet("~/github/mmr-esif-clean/data-processed/misto_fix-02-gnames.parquet") %>%
  distinct(prj_id, p_ico)

dt %>%
  select(rozpad_duvod, prj_id) %>%
  collect() %>%
  distinct() %>%
  group_by(rozpad_duvod) %>%
  count()

obce_svl <- dt %>%
  filter(rozpad_duvod == "Výzva pro SVL") %>%
  select(prj_id, level, value) %>%
  collect() %>%
  distinct(value, level) %>%
  left_join(czso::czso_get_codelist("51") %>%
              select(value = CHODNOTA, nazev = TEXT)) %>%
  drop_na(nazev)
