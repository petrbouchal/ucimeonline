library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)


# Dotaznik distancni vyuka ------------------------------------------------

prioritni_tech <- read_parquet("csi_prioritni-skoly.parquet")
prioritni_tech_long <- read_parquet("csi_prioritni-skoly_long.parquet")
prioritni_tech_long_recoded <- read_parquet("csi_prioritni-skoly_long-recoded.parquet")

# kde chybi data
prioritni_tech_long %>%
  group_by(typskoly, priorita, redizo) %>%
  summarise(has_response = !all(is.na(value))) %>%
  group_by(priorita, typskoly) %>%
  summarise(has_response = mean(has_response)) %>%
  ggplot(aes(colour = typskoly, x = priorita, y = has_response,
             group = typskoly)) +
  geom_line()


prioritni_tech_long_recoded %>%
  count(priorita, typskoly, text_recoded, sort = T) %>%
  spread(text_recoded, n)

prioritni_tech_long_recoded %>%
  count(priorita, typskoly, text_recoded, sort = T) %>%
  ggplot(aes(fill = text_recoded,
             group = typskoly,
             y = text_recoded, x = n)) +
  geom_col() +
  facet_grid(typskoly~priorita)

prioritni_tech_long_recoded %>%
  count(freetext, text_detail, sort = T)

prioritni_tech_long_recoded %>%
  count(priorita, text_detail, sort = T)

prioritni_tech_long_recoded %>%
  mutate(text_recoded = fct_infreq(text_recoded) %>% fct_rev()) %>%
  group_by(typskoly, priorita) %>%
  add_count() %>%
  group_by(text_recoded, .add = T) %>%
  summarise(podil = n()/n) %>%
  ggplot(aes(y = text_recoded, x = priorita)) +
  geom_tile(aes(fill = podil)) +
  geom_label(label.size = 0, aes(label = ptrr::label_percent_cz(1)(podil))) +
  facet_wrap(~ typskoly)

# DNS data ----------------------------------------------------------------

dns <- read_parquet("dns.parquet")

# Pristup k internetu -----------------------------------------------------

mpo <- read_parquet("mpo-internet.parquet")

# DNS vs. pruzkum ---------------------------------------------------------

prioritni_tech_googlesoft <-
  prioritni_tech_long_recoded %>%
  filter(text_recoded %in% c("G Hangout/Meet/Classroom", "Microsoft Teams")) %>%
  mutate(pruzkum_grouped = case_when(text_recoded == "G Hangout/Meet/Classroom" ~ "Google",
                                  text_recoded == "Microsoft Teams" ~ "MS",
                                  TRUE ~ "jiné/žádné"),
         g_in_survey = text_recoded == "G Hangout/Meet/Classroom",
         ms_in_survey = text_recoded == "Microsoft Teams") %>%
  select(redizo, ends_with("in_survey"), pruzkum_grouped, text_recoded,
         text_detail)

dns_a_pruzkum <- dns %>%
  mutate(dns_odhad_grouped = fct_lump_n(odhadovana_sluzba, 5,
                                        other_level = "Ostatní")) %>%
  left_join(prioritni_tech_googlesoft) %>%
  replace_na(list(pruzkum_grouped = "bez odpovědi"))

dns_a_pruzkum %>%
  count(dns_odhad_grouped, pruzkum_grouped) %>%
  spread(pruzkum_grouped, n)

dns %>%
  left_join(prioritni_tech_googlesoft) %>%
  mutate(shoda_google = g_suite_nalezen & g_in_survey,
         shoda_ms = o365_nalezen & ms_in_survey) %>%
  count(shoda_google)
