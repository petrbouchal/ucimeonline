library(gargle)
library(sodium)

file_older_than_days <- function(path, days) {
  dplyr::if_else(file.exists(path),
                 file.mtime(path) < Sys.time() - days * 24 * 3600,
                 FALSE)
}

auth_google <- function(key_json_path, pw_var_name, google_fn, email) {
  raw <- readBin(key_json_path, "raw", file.size(key_json_path))
  pth <- sodium::data_decrypt(bin = raw,
                              key = sodium::sha256(charToRaw(Sys.getenv(pw_var_name))),
                              nonce = gargle:::secret_nonce())
  google_fn(email = email, path = rawToChar(pth))
}

# https://gargle.r-lib.org/articles/get-api-credentials.html
# https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html
# https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html

auth_encrypt <- function(input, output, pw_var_name) {
  input <- readBin(input, "raw",
                   file.size(input))
  enc <- sodium::data_encrypt(msg = input,
                              key = sodium::sha256(charToRaw(Sys.getenv(pw_var_name))),
                              nonce = gargle:::secret_nonce())
  attr(enc, "nonce") <- NULL
  writeBin(enc, output)
}

write_parquet_pth <- function(dt, path) {
  arrow::write_parquet(dt, path)
  return(path)
}

read_tarquet <- function(target_name, branch = 1, n = 1) {
  name <- deparse(substitute(target_name))
  t <- targets::tar_read_raw(name, branches = branch)
  arrow::read_parquet(t[[n]])
}

open_tarrow <- function(target_name, branch = 1, n = 1) {
  name <- deparse(substitute(target_name))
  t <- targets::tar_read_raw(name, branches = branch)
  print(t[n])
  arrow::open_dataset(t[n])
}

flatten_data <- function(df, column) {

  if("sf" %in% class(df)) df <- st_drop_geometry(df)

  df %>%
    mutate(text_recoded = map_chr({{column}},
                                  ~ifelse(!is.null(.x),
                                          paste0(.x, collapse = "; "),
                                          NA)))
}

g_resource_attribute <- function(id, attribute_name = "modifiedTime") {
  g_thing_metadata <- drive_get(id = as_id(id))
  g_thing_metadata[['drive_resource']][[1]][[attribute_name]]
}

gdrive_modified <- function(id) {
  g_resource_attribute(id)
}

timestamp_outdated <- function(path, days_old) {
  if(file.exists(path)) {
    timestamp <- read_csv(path, col_types = cols(x = col_datetime()))[[1,"x"]]
    timestamp < (Sys.time() - days_old * 24 * 3600)
  } else {
    TRUE
  }
}

timestamp_every_n_days <- function(days) {
  if(days < 1) {
    return(Sys.time())
  } else {
    as.Date.POSIXct(floor(as.numeric(Sys.time())/(3600*24*days))*(3600*24*days))
  }

}

render_nosite <- function(input, output, output_dir, output_yaml) {
  fs::file_move("_site.yml", new_path = "xsite.yml")

  on.exit(fs::file_move(here::here("xsite.yml"), "_site.yml"))
  rmarkdown::render(input, output_file = output, output_dir = output_dir,
                    output_yaml = output_yaml)
}

theme_uo <- function(gridlines = c("y", "x", "both", "scatter", "none"),
                     base_size = 11, family = "Roboto Condensed", title_family = "Roboto",
                     multiplot = FALSE, tonecol = ptclr_l, margin_side = 6, margin_bottom = 6,
                     plot.title.position = "plot", axis_titles = FALSE, richtext = FALSE,
                     map = FALSE, ...) {
  theme_ptrr(gridlines = gridlines,
             base_size = base_size, family = family, title_family = title_family,
             multiplot = multiplot, tonecol = tonecol, margin_side = margin_side, margin_bottom = margin_bottom,
             plot.title.position = plot.title.position, axis_titles = axis_titles, richtext = richtext,
             map = map, ...)

}

