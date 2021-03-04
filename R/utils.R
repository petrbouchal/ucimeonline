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
