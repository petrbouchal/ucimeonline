library(gargle)
library(sodium)

auth_google <- function(key_json_path, pw_var_name, google_fn, email) {
  raw <- readBin(key_json_path, "raw", file.size(key_json_path))
  pth <- sodium::data_decrypt(bin = raw,
                              key = sodium::sha256(charToRaw(Sys.getenv(pw_var_name))),
                              nonce = gargle:::secret_nonce())
  google_fn(email = email, path = rawToChar(pth))
}

auth_encrypt <- function(input, output, pw_var_name) {
  input <- readBin(input, "raw",
                   file.size(input))
  enc <- sodium::data_encrypt(msg = input,
                              key = sodium::sha256(charToRaw(Sys.getenv(pw_var_name))),
                              nonce = gargle:::secret_nonce())
  attr(enc, "nonce") <- NULL
  writeBin(enc, output)
}
