library(targets)
library(tarchetypes)


# Targets setup -----------------------------------------------------------

options(czso.dest_dir = "data-input/czso")

# Set target-specific options such as packages.


tar_option_set(packages = c("dplyr", "tidyr", "stringr", "lubridate",
                            "purrrow", "vsezved", "sf", "ggplot2", "arrow",
                            "tibble", "googlesheets4", "bigrquery", "janitor",
                            "readr", "readxl", "curl", "purrr", "targets",
                            "tarchetypes", "forcats", "ptrr", "czso", "santoku",
                            "pointblank", "airtabler", "googledrive"),
               imports = c("vsezved"))

# Config ------------------------------------------------------------------

cnf <- config::get(config = "default")

tech_sheet_id = cnf$tech_sheet_id
bq_project = cnf$bq_project
bq_billing = cnf$bq_billing
bq_dataset = cnf$bq_dataset
bq_table_dns = cnf$bq_table_dns

source("R/utils.R")
source("R/functions.R")

options(scipen = 10)

# Auth and connect --------------------------------------------------------

auth_google(cnf$secret_path_gsheets, cnf$secret_name_gsheets,
            googlesheets4::gs4_auth, cnf$email_gsheets)
auth_google(cnf$secret_path_bigquery, cnf$secret_name_bigquery,
            bigrquery::bq_auth, cnf$email_bigquery)
auth_google(cnf$secret_path_gsheets, cnf$secret_name_gsheets,
            googledrive::drive_auth, cnf$email_gsheets)

t_bquery_conn <- tar_target(bq_conn, dbConnect(
  bigrquery::bigquery(),
  project = bq_project,
  dataset = bq_dataset,
  billing = bq_billing))

# School metadata ---------------------------------------------------------

sch_typy <- cnf$sch_typy
names(sch_typy) <- cnf$sch_typy_nazvy

kapacita_breaks <- cnf$kapacita_breaks

reg_is_old <- file_older_than_days("data-input/registr.xml", cnf$sch_reg_update_days)
adr_is_old <- file_older_than_days("data-input/Adresar.xls", cnf$sch_adr_update_days)

t_skoly_metadata <- list(

  # Adresář - štístkot

  tar_target(sch_adr_tables, c("addresses", "schools")),

  tar_target(test_gdrive, {drive_get(id = tech_sheet_id)}),

  tar_force(sch_adr_resps,
            # run if last run is older than N days set in config
            force = adr_is_old,
            vz_get_directory_responses(tables = sch_adr_tables)),
  tar_file(sch_adr_files,
           vz_write_directory_quasixls(sch_adr_resps[[1]],
                                       write_file = T,
                                       dest_dir = "data-input"),
           pattern = map(sch_adr_resps)),
  tar_target(sch_adr_org, vz_load_directory(sch_adr_files[[1]])),
  tar_target(sch_adr_sch, vz_load_directory(sch_adr_files[[2]])),

  # Official school register

  tar_target(sch_reg_tables, c("organisations", "schools")),
  # tar_url(sch_reg_url, vz_get_ckan_url()), # use this to update on every upstream file change
  tar_force(sch_reg_url, vz_get_ckan_url(),
            # run if last run is older than N days set in config
            force = reg_is_old),
  tar_file(sch_reg_file, vz_get_register_xml(sch_reg_url, write_file = T,
                                             dest_dir = "data-input")),
  tar_target(sch_reg_dfs,
             vz_load_register(sch_reg_file, tables = sch_reg_tables)),
  tar_target(sch_reg_org, sch_reg_dfs[[1]]),
  tar_target(sch_reg_sch, sch_reg_dfs[[2]]),

  # IDs of schools we care about

  tar_target(sch_slct_redizo, sch_get_filtered_ids(sch_reg_sch, skola_druh_typ, sch_typy, red_izo)),
  tar_target(sch_slct_izo, sch_get_filtered_ids(sch_reg_sch, skola_druh_typ, sch_typy, izo)),
  tar_target(sch_slct_ico, sch_get_filtered_ids(sch_reg_org, red_izo, sch_slct_redizo, ico))
)

# Geodata -----------------------------------------------------------------

cuzk_date <- cnf$cuzk_date

t_geodata <- list(
  tar_url(g_adresy_url, str_glue("https://vdp.cuzk.cz/vymenny_format/csv/{cuzk_date}_OB_ADR_csv.zip")),
  # tar_url(g_struktura_url, str_glue("https://vdp.cuzk.cz/vymenny_format/csv/{cuzk_date}_strukt_ADR.csv.zip")),
  tar_file(g_adresy_zip, curl_download(g_adresy_url, here::here("data-input/ruian_adm_all.zip"))),
  # tar_file(g_struktura_zip, curl_download(g_struktura_url, here::here("data-input/ruian_hierarchie.zip"))),
  tar_file(g_adresy_arrowds, build_address_arrowds(g_adresy_zip, "data-processed/adresni-mista")),
  tar_target(g_school_coords, build_school_coords(g_adresy_arrowds, sch_adr_sch, sch_slct_izo)),
  tar_target(g_poly_kraje, giscoR::gisco_get_nuts(nuts_level = 3, country = "CZE")),
  tar_target(g_poly_okresy, RCzechia::okresy(resolution = "low") %>%
               rename(geometry = GeneralizovaneHranice) %>%
               st_set_geometry("geometry"))
)


# Data o územích -----------------------------------------------------------

stats_rok <- cnf$stats_rok
obce_pocob_breaks <- unlist(cnf$obce_pocob_breaks)

t_uzemistats <- list(
  tar_target(stats_obyv, build_demog_all(stats_rok = stats_rok)),
  tar_target(stat_obyv_obce, build_demog_obce(stats_obyv,
                                              breaks = obce_pocob_breaks)),
  tar_target(stat_obyv_kraj, build_demog_regionsum(stats_obyv, 100) %>%
               select(kod_kraj = CZNUTS, stat_pocobyv_kraj = hodnota,
                      kraj_nazev = TEXT1)),
  tar_target(stat_obyv_okres, build_demog_regionsum(stats_obyv, 101) %>%
               select(kod_okres = CZNUTS, stat_pocobyv_okres = hodnota,
                      okres_nazev = TEXT1))
)

# MŠMT/ČSI school survey/report data -----------------------------------------------

csi_excel_zs <- cnf$csi_excel_zs
csi_excel_ss <- cnf$csi_excel_ss

t_csi <- list(
  tar_target(csi, csi_priority_load(csi_excel_zs, csi_excel_ss)),
  tar_target(csi_long, csi_priority_lengthen(csi_excel_zs, csi)),
  tar_target(csi_recoded, csi_priority_recode(csi_long)),
  tar_target(csi_summary, csi_summarise(csi, csi_long, csi_recoded))
)

# MPO connectivity data ---------------------------------------------------

mpo_xlsx <- cnf$mpo_xlsx
t_mpo <- list(
  tar_target(mpo, load_mpo_pokryti(mpo_xlsx))
)

# DNS technical scan data -------------------------------------------------

t_dns <- list(
  tar_force(dns_df_date, gdrive_modified(tech_sheet_id), force = TRUE),
  tar_target(dns_df, load_dns_gsheet(tech_sheet_id, dns_df_date)),
  tar_file(dns_pq, write_parquet_pth(dns_df, "data-processed/dns.parquet")),
  tar_target(dns_gqt, {try(bigrquery::bq_table_delete(bq_table(bq_project, bq_dataset, "dns")))
    bigrquery::bq_table_upload(bq_table(bq_project, bq_dataset, bq_table_dns),
                               values = dns_df)})
)


# Airtable data from UO ---------------------------------------------------

airtable_base_id <- cnf$airtable_base_id
airtable_table_id <- cnf$airtable_table_id

manualni_id_sheet_id <- cnf$manualni_id_sheet_id
manualni_id_tabname_manualids <-  cnf$manualni_id_tabname_manualids

airtable_update_days <- cnf$airtable_update_days

t_airtable <- list(
  tar_force(at_dt, at_load(airtable_base_id, airtable_table_id),
            # only run if last run was not today
            force = timestamp_outdated(here::here("data-input/airtable_timestamp.csv"),
                                       airtable_update_days)),
  tar_force(at_manual_sheetmoddate, gdrive_modified(manualni_id_sheet_id), TRUE),
  tar_target(at_manual_ids,
             read_sheet_ifold(manualni_id_sheet_id, manualni_id_tabname_manualids,
                              at_manual_sheetmoddate)),
  tar_target(at_dc, at_process(at_dt, sch_reg_org, sch_reg_sch, sch_adr_org,
                               at_manual_ids)),
  tar_target(at_dc_for_geocoding, at_dc %>% filter(!(m_red_izo | m_nazev_exp))),
  tar_target(at_geocoded_cuzk, geocode_cuzk(at_dc_for_geocoding, adresa_lower)),
  tar_target(at_geocoded_osm, geocode_osm(at_dc_for_geocoding, adresa_lower)),
  tar_target(at_with_geo, at_dc %>%
               left_join(at_geocoded_cuzk) %>%
               left_join(at_geocoded_osm)),
  tar_target(at_ids, resolve_by_points(at_with_geo, mdf)),
  tar_target(at_with_ids, merge_at_with_ids(at_dc, at_ids, mdff)),
  tar_target(at_with_mdf, left_join(at_with_ids, mdf) %>%
               rename(airtable_id = id) %>%
               st_as_sf()),
  tar_target(at_unmatched, identify_unmatched_airtable_rows(at_with_ids))
)

# Merge data --------------------------------------------------------------

t_merge <- list(
  tar_target(mdf, merge_data(dns_df, csi_summary, mpo, g_school_coords,
                             sch_adr_org, sch_adr_sch, sch_reg_org, sch_reg_sch,
                             stat_obyv_obce)),
  tar_target(mdff, mdf %>% st_drop_geometry()),
  tar_target(at_with_mdf_flat, flatten_data(mdf, text_recoded)),
  tar_target(mdf_with_at_mark, mdf %>% left_join(at_with_ids %>%
                                                   filter(!id %in% at_unmatched$id) %>%
                                                   select(red_izo, izo) %>%
                                                   mutate(help_requested = TRUE) %>%
                                                   replace_na(list(help_requested = FALSE)))),
  tar_target(mdf_with_at_mark_flat, flatten_data(mdf_with_at_mark, text_recoded))
)


# Export ------------------------------------------------------------------

output_sheet_id <- cnf$output_sheet_id
output_sheet_tabname_base <- cnf$output_sheet_tabname_base
output_sheet_tabname_compiled <- cnf$output_sheet_tabname_compiled

manualni_id_tabname_missingids <-  cnf$manualni_id_tabname_missingids

t_export <- list(
  tar_target(sheet_base, sheet_write(mdf_with_at_mark_flat, output_sheet_id, sheet = output_sheet_tabname_base)),
  tar_target(sheet_at_with_base, sheet_write(at_with_mdf_flat, output_sheet_id, sheet = output_sheet_tabname_compiled)),
  tar_target(sheet_missing_ids, sheet_write(at_unmatched, manualni_id_sheet_id, sheet = manualni_id_tabname_missingids))
)

# Reports -----------------------------------------------------------------

source("R/targets_reports.R") # creates the t_reports list

# Collate targets----------------------------------------------------------

list(t_bquery_conn, t_dns, t_skoly_metadata, t_geodata, t_csi, t_mpo,
     t_merge, t_reports, t_uzemistats, t_export, t_airtable)
