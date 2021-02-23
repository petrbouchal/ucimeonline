library(targets)
library(tarchetypes)

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "tidyr", "stringr", "lubridate",
                            "purrrow", "vsezved", "sf", "ggplot2", "arrow",
                            "tibble", "googlesheets4", "bigrquery", "janitor"))

cnf <- config::get(config = "default")

tech_sheet_id = cnf$tech_sheet_id
bq_project = cnf$bq_project
bq_billing = cnf$bq_billing
bq_dataset = cnf$bq_dataset
bq_table_dns = cnf$bq_table_dns

source("R/utils.R")
source("R/functions.R")

auth_google(cnf$secret_path_gsheets, cnf$secret_name_gsheets, googlesheets4::gs4_auth, cnf$email_gsheets)
auth_google(cnf$secret_path_bigquery, cnf$secret_name_bigquery, bigrquery::bq_auth, cnf$email_bigquery)

t_bquery_conn <- tar_target(bq_conn, dbConnect(
  bigrquery::bigquery(),
  project = bq_project,
  dataset = bq_dataset,
  billing = bq_billing))

sch_adr_tables <- c("locations", "schools", "addresses")
sch_reg_tables <- c("locations", "schools", "organisations")

t_skoly <- list(
  tar_target(sch_adr_resps, vsezved:::vz_get_directory_responses(tables = sch_adr_tables)),
  tar_file(sch_adr_files, vz_responses_to_quasixls(sch_adr_resps),
           pattern = map(sch_adr_resps)),
  tar_file(sch_adr_pq, stistko_quasixls_to_parquet(sch_adr_files, "data-processed"),
           pattern = map(sch_adr_files)),
  tar_target(sch_reg_url,
             vsezved:::vz_get_ckan_url("rejstrik-skol-a-skolskych-zarizeni-cela-cr",
                                       base_url = vsezved:::msmt_ckan_base_url),
             format = "url"),
  tar_file(sch_reg_file, curl::curl_download(sch_reg_url, "data-input/sch_register.xml")),
  tar_target(sch_reg_dfs, vsezved::vz_load_register(sch_reg_file, tables = sch_reg_tables)),
  tar_file(sch_reg_pq,
           purrr::map2_chr(sch_reg_dfs,
                           file.path("data-processed",
                                     paste0("sch-reg_", sch_reg_tables, ".parquet")),
                           write_parquet_pth))
)

t_geodata <- list(
  # adresy <- load_adresy(),
)

t_techmapa <- list(
  tar_target(techmapa_df, load_dns_gsheet(tech_sheet_id)),
  tar_file(techmapa_pq, write_parquet_pth(techmapa_df, "data-processed/techmapa.parquet")),
  tar_target(techmapa_gqt, {try(bigrquery::bq_table_delete(bq_table(bq_project, bq_dataset, "dns")))
    bigrquery::bq_table_upload(bq_table(bq_project, bq_dataset, bq_table_dns),
                               values = tar_read(techmapa_df))})
)

list(t_bquery_conn, t_techmapa, t_skoly)
