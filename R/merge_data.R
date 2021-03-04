##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dns_df
##' @param
##' @return
##' @author Petr Bouchal
##' @export
merge_data <- function(dns_df, csi_summary, mpo,
                       g_school_coords,
                       sch_adr_org,
                       sch_adr_sch,
                       sch_reg_org,
                       sch_reg_sch,
                       stat_obyv_obce) {

  adr_sch <- sch_adr_sch %>%
    select(red_izo, izo, kapacita, okres_kod = uzemi,
           skola_druh_nazev = druh_typ)

  adr_org <- sch_adr_org %>%
    select(red_izo, plny_nazev, zkraceny_nazev,
           kraj_nazev = kraj, orp_kod = orp)

  reg_org <- sch_reg_org %>%
    select(red_izo, zriz_ico, zriz_druh_kod = druh_zrizovatele, pravni_forma)

  reg_sch <- sch_reg_sch %>%
    select(red_izo, izo, skola_druh_kod = skola_druh_typ)

  mdfr <- g_school_coords %>%
    left_join(reg_org) %>%
    left_join(reg_sch) %>%
    left_join(adr_sch) %>%
    left_join(adr_org) %>%
    left_join(stat_obyv_obce) %>%
    left_join(csi_summary %>% select(-izo)) %>%
    left_join(mpo %>% select(-kod_adm, -ico, -zrizovatel)) %>%
    left_join(dns_df) %>%
    rename(csi_has_google = has_google,
           csi_has_ms = has_ms,
           csi_only_whatsapp = only_whatsapp,
           csi_priorita = priorita,
           dns_gsuite = g_suite_nalezen,
           dns_o365 = nalezen_o365,
           dns_odhad_sluzba = odhadovana_sluzba
    ) %>%
    mutate(dns_gsuite = as.logical(dns_gsuite),
           dns_o365 = as.logical(dns_o365),
           kapacita = as.numeric(kapacita),
           shoda_ms = (dns_o365 == csi_has_ms) & csi_has_ms,
           shoda_google = (dns_gsuite == csi_has_google) & csi_has_google,
           shoda = shoda_ms | shoda_google,
           src_csi = red_izo %in% csi_summary$red_izo,
           src_dns = red_izo %in% dns_df$red_izo,
           src_mpo = red_izo %in% mpo$red_izo,
           kraj_kod = substr(okres_kod, 1, 5),
           kapacita_kat = santoku::chop(kapacita, breaks = kapacita_breaks))
  mdfr
}

