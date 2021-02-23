##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Petr Bouchal
##' @export
load_adresar <- function() {

  vsezved::vz_get_directory(tables = c("locations", "schools", "addresses"),
                            return_tibbles = F, write_files = T)

}

vz_responses_to_quasixls <- function(responses) {
  purrr::map_chr(responses, vsezved:::response_to_quasixls, FALSE)
}

stistko_quasixls_to_parquet <- function(file, dir) {

  file_base <- tools::file_path_sans_ext(file)
  path <- file.path(dir, paste0("sch-adr_", file_base, ".parquet"))

  vsezved:::vz_load_stistko(file) %>%
    write_parquet_pth(path)
}

