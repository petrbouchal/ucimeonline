# https://rud.is/b/2019/06/28/quick-hit-dig-ging-into-dns-records-with-processx/
dig <- function(..., cat = TRUE) {

  processx::run(
    command = unname(Sys.which("dig")),
    args = unlist(list(...)),
  ) -> out

  # if (cat) message(out$stdout)

  return(out)

}

dig_result <- function(dig_output, n = 14) {
  sss <- stri_split_lines(dig_output$stdout, omit_empty = TRUE) %>% # split the response in stdout into lines
    flatten_chr()

  if (!str_detect(dig_output$stdout, "status\\: NOERROR")) {
    NA
  }  else {
    sd <- tibble(x = sss) %>%
      filter(!str_detect(x, "^\\;")) %>%
      separate(x, into = letters[1:20], sep = "\\s", fill = "right") %>%
      select(-a) %>%
      gather("col", "val") %>%
      mutate(val = na_if(val, "")) %>%
      drop_na(val) %>%
      filter(!str_detect(val, "^MX$|^TXT$|^IN$|^[0-9]+$|ip4\\:|^[0-9]{3}\\.[0-9]{3}")) %>%
      select(-col)

    if(nrow(sd) == 0) {
      "none"
    }  else {
    sd %>% tidyr::chop(val) %>%
      mutate(h = flatten_chr(val) %>% paste(collapse = "; ") %>% tolower()) %>%
      pull(h)
    }
  }
}

# dig_result(dig("gml.cz", "TXT"), 8)
# dig_result(dig("prosrdce.cz", "TXT"), 8)
# dig_result(dig("petrbouchal.xyz", "MX"), 8)
# dig_result(dig("petrbouchal.xyzdsfsd", "MX"), 8)
# dig_result(dig("petrbouchal.xyzdsfsd", "TXT"), 8)
#
# dig_result(dig("gml.cz", "TXT"), 14)
# dig_result(dig("gml.cz", "MX"), 14)
# dig_result(dig("prosrdce.cz", "MX"), 14)
# dig_result(dig("prosrdce.cz", "TXT"), 14)


domain_from_www <- function(url) {
  urltools::domain(url) %>% str_remove("^www\\.")
}


dig_full <- function(url, type = c("MX", "TXT")) {

  # print(url)

  if(is.na(url)) {NA} else {
    n_cols <- if_else(type == "MX", 8, 14)

    domain <- domain_from_www(url)

    if (type == "MX") {
      dg <- dig(domain, "MX")
    }  else {
      dg <- dig(domain, "TXT")
    }
    # print(dg)
    dig_result(dg, n = n_cols)

  }

}

# dig_full("gml.cz", type = "MX")
# dig_full("www.gml.cz", type = "MX")
# dig_full("gml.cz", type = "TXT")
# dig_full("fdsjkfd", type = "TXT")
#
# dig("gml.cz", "MX")
# dig("gml.cz", "TXT")
#
# tibble(url = c("www.ihned.cz", "petrbouchal.xyz", "denikn.cz", "gml.cz",
#                "svinosice.cz", "www.prosrdce.cz", "iprpraha.cz", "praha.eu",
#                "ipr.praha.eu", "skolalhota.cz", "zsrablhota.cz", "webskoly.cz",
#                "gvp.cz", "ceskepriority.cz", "paliativnicentrum.cz")) %>%
#   mutate(mx =  map_chr(url, dig_full, "MX"),
#          txt = map_chr(url, dig_full, "TXT")) %>% View()
#
# dig_full("petrbouchal.xyz", "<X")

scan_dns <- function(data, type, n = 1000) {
  df <- data %>%
    select(red_izo, izo, domena_skoly)

  if(!is.na(n)) df <- sample_n(df, n)

  df %>%
    mutate(dig_data =  future_map_chr(domena_skoly, dig_full, type))
}

resolve_dns <- function(dns_rescan) {
  mutate(dns_rescan,
         odhadovana_sluzba_new =
           case_when(str_detect(dig_mx, "outlook") & str_detect(dig_mx, "google") ~ "G Suite i O365",
                     str_detect(dig_mx, "outlook") ~ "MS Outlook (O365)",
                     str_detect(dig_mx, "google") ~ "G Suite",
                     str_detect(dig_mx, "antee") ~ "Antee",
                     str_detect(dig_mx, "pipni") ~ "Pipni.cz",
                     str_detect(dig_mx, "web4u") ~ "Web4u",
                     str_detect(dig_mx, "wedos") ~ "Wedos Mail",
                     str_detect(dig_mx, "forpsi\\.com") ~ "Forpsi Email",
                     str_detect(dig_mx, "active24\\.com") ~ "Active24",
                     str_detect(dig_mx, "estranky\\.cz") ~ "Estranky",
                     str_detect(dig_mx, "thinline\\.cz") ~ "Český Hosting",
                     str_detect(dig_mx, "zoner\\.com") ~ "Zoner Email",
                     str_detect(dig_mx, "onebit\\.eu") ~ "OneBit Hosting",
                     str_detect(dig_mx, "blueboard\\.cz") ~ "BlueBoard.cz",
                     str_detect(dig_mx, "savana\\.cz") ~ "Savana Webhosting",
                     str_detect(dig_mx, "stable\\.cz") ~ "Stable.cz",
                     str_detect(dig_mx, "web4ce\\.cz") ~ "Web4CE",
                     str_detect(dig_mx, "aerohosting\\.cz") ~ "Aerohosting.cz",
                     str_detect(dig_mx, "fortion\\.") ~ "Fortion Networks",
                     str_detect(dig_mx, "hosting90\\.") ~ "Hosting90",
                     str_detect(dig_mx, "svethostingu\\.") ~ "SvětHostingu",
                     is.na(domena_skoly_new) ~ "Bez URL",
                     is.na(dig_mx) ~ "Bez MX záznamu",
                     TRUE ~ "Ostatní/Neznámé/Vlastní"),
           g_suite_nalezen_new = odhadovana_sluzba_new == "G Suite",
           nalezen_o365_new = odhadovana_sluzba_new == "MS Outlook (O365)",
         )
}
