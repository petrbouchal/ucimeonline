targets::tar_load(mdf_with_at_mark_flat)

make_tp_data_posun_kat <- function(mdf_with_at_mark_flat) {

  mdf_with_at_mark_flat %>%
    filter(skola_druh_kod %in% c("B00", "C00")) %>%
    mutate(gms_orig = dns_gsuite | dns_o365,
           gms_new = dns_new_gsuite | dns_new_o365,
           gms = case_when(gms_orig & gms_new ~ "kept",
                           gms_orig & !gms_new ~ "dropped",
                           !gms_orig & gms_new ~ "adopted",
                           !gms_new & !gms_orig ~ "still not"),
           skola_typ = fct_recode(skola_druh_kod,
                                  `Střední školy` = "C00",
                                  `Základní školy` = "B00") %>% fct_rev()) %>%
    drop_na(gms) %>%
    count(help_requested, skola_typ, gms, gms_new, gms_orig)
}

make_tp_plt_shift <- function(tp_posun_dt_kat) {

  ptrr::set_ptrr_ggplot_fonts("Roboto Condensed", lineheight = .9, fontface = "bold")

  tech_predapo <- tp_posun_dt_kat %>%
    ungroup() %>%
    filter(!gms_orig) %>%
    add_count(skola_typ, help_requested, wt = n, name = "nn") %>%
    mutate(share = n/nn) %>%
    filter(gms == "adopted")

  tech_predapo_wide = tech_predapo %>%
    select(help_requested, skola_typ, share) %>%
    spread(help_requested, share) %>%
    rename(uo = `TRUE`, other = `FALSE`)

  ggplot(tech_predapo, aes(colour = help_requested, x = share, y = fct_rev(skola_typ))) +
    geom_linerange(aes(xmin = uo, xmax = other, x = NULL), colour = "grey70",
                   data = tech_predapo_wide, size = 1) +
    geom_point(size = 3.5) +
    scale_x_percent_cz(expand = expansion(c(0, .2)), limits = c(0, NA)) +
    scale_y_discrete(expand = expansion(mult = .5)) +
    scale_color_manual(values = c("grey40", "blue"), guide = "none") +
    geom_text(aes(label = scales::percent(share, accuracy = 1,
                                          prefix = "   ", suffix = " %   ")),
              hjust = "outward") +
    theme_uo("x", multiplot = T, panel.spacing.x = unit(.5, "cm"),
             richtext = T, axis.text.y = ggtext::element_markdown(size = 11)) +
    labs(title = "Podíl škol, které od jara 2020 zavedly Office 365 nebo Google Suite",
         subtitle = "Mezi školami <span style =  'color: blue;'>**s podporou Učíme online**</span> byla o víc než polovinu vyšší adopce než u <span style = 'color: #666666;'>**ostatních**</span><br />100 % = všechny školy, které na jaře 2020 neměly Office 365 nebo G Suite",
         caption = "Zdroj: síťový sken, školský rejstřík, data Česko.digital; jaro 2020 a březen 2021")

}


make_tp_data_posun_diffindiff <- function(mdf_with_at_mark_flat) {
  mdf_with_at_mark_flat %>%
    filter(skola_druh_kod %in% c("B00", "C00")) %>%
    mutate(skola_typ = fct_recode(skola_druh_kod,
                                  `Střední školy` = "C00",
                                  `Základní školy` = "B00")) %>%
    group_by(help_requested, skola_typ) %>%
    summarise(gms_orig = mean(dns_gsuite | dns_o365, na.rm = T),
              gms_new = mean(dns_new_gsuite | dns_new_o365, na.rm = T),
              .groups = "drop") %>%
    pivot_longer(cols = starts_with("gms_"), names_to = "time", values_to = "share") %>%
    mutate(time = recode(time, `gms_orig` = "Jaro\n2020",
                         `gms_new`  = "Jaro\n2021"))

}

make_tp_plt_diffindiff <- function(tp_posun_dt_diffindiff) {

  ptrr::set_ptrr_ggplot_fonts("Roboto Condensed", fontface = "bold", lineheight = .9)

  lbls <- tibble(skola_typ = c("Základní školy", "Základní školy") %>% fct_rev(),
                 label = c("s podporou\nUčíme online", "ostatní"),
                 help_requested = c(TRUE, FALSE),
                 time = c(1.5, 1.7),
                 share = c(.53, .4))

  ggplot(tp_posun_dt_diffindiff,
         aes(time, share, colour = help_requested, group = help_requested)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = scales::percent(share, accuracy = 1,
                                          prefix = "  ", suffix = " %  ")),
              hjust = "outward") +
    geom_text(data = lbls, aes(label = label), hjust = "outward") +
    facet_wrap(~skola_typ) +
    scale_y_percent_cz(expand = expansion(add = .05)) +
    scale_color_manual(values = c("grey40", "blue"), guide = "none") +
    scale_x_discrete(expand = expansion(add = .3)) +
    theme_uo(multiplot = T, strip.text = element_text(size = 10, face = "bold"),
             richtext = T) +
    labs(title = "Přibylo škol, které využívají Google Suite nebo Office 365",
         subtitle = "o něco rychleji mezi školami, které <span style = 'color: blue;'>**se přihlásily o podporu Učíme online**</span>",
         caption = "Zdroj: síťový sken, školský rejstřík, data Česko.digital; jaro 2020 a březen 2021")
}

