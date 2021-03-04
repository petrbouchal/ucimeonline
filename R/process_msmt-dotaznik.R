# Dotaznik distancni vyuka ------------------------------------------------

make_survey_key <- function(excel) {
  msmt_survey_key_base <- read_excel(excel,
                                     n_max = 5, col_names = T)
  msmt_survey_key <- msmt_survey_key_base %>%
    select(starts_with("Q")) %>%
    slice(1) %>%
    gather(key = "column_orig", value = "text") %>%
    separate(column_orig, into = c("question_answer", "stub"), sep = "\\.\\.\\.", remove = F) %>%
    separate(question_answer, into = c("question", "answer"), sep = "_", remove = F) %>%
    group_by(question, answer) %>%
    mutate(rows_in_qa = n(), rownum_in_qa = row_number())
  return(msmt_survey_key)
}

csi_priority_load <- function(path_excel_zs, path_excel_ss) {

  zs_sheets <- readxl::excel_sheets(path_excel_zs)
  names(zs_sheets) <- zs_sheets
  zs <- map_dfr(zs_sheets, ~read_excel("data-input/ZŠ podle priority_data.xlsx", .x) %>%
                  rename(redizo = REDIZO, ico = `IČ`), .id = "sheet") %>%
    mutate(typskoly = "zs") %>%
    relocate(sheet, .after = last_col()) %>%
    mutate(priorita = str_extract(sheet, "(?<=\\().*(?=\\s)"))

  ss_sheets <- readxl::excel_sheets(path_excel_ss)
  names(ss_sheets) <- ss_sheets
  ss <- map_dfr(ss_sheets, ~read_excel(path_excel_ss, .x) %>%
                  rename(redizo = REDIZO, ico = `IČ`), .id = "sheet") %>%
    mutate(typskoly = "ss") %>%
    relocate(sheet, .after = last_col()) %>%
    mutate(priorita = str_extract(sheet, "(?<=\\().*(?=\\s)"))

  msmt_survey_key <- make_survey_key(path_excel_zs)

  colnames_relevant <- msmt_survey_key %>%
    filter(question == "Q233117") %>%
    pull(column_orig)

  prioritni_tech <- bind_rows(list(zs, ss)) %>%
    filter(redizo != "REDIZO") %>%
    select(red_izo = redizo, izo = IZO, ico,
           all_of(colnames_relevant), typskoly, priorita)  %>%
    mutate(typskoly_txt = case_when(typskoly == "zs" ~ "Základní škola",
                                    typskoly == "ss" ~ "Střední škola") %>%
             fct_relevel("Střední škola", after = Inf),
           priorita = fct_relevel(priorita, "Nejvyšší", after = Inf),
           typskoly = fct_relevel(typskoly, "ss", after = Inf))

  return(prioritni_tech)
}

csi_priority_lengthen <- function(excel, df) {

  msmt_survey_key <- make_survey_key(excel)

  df %>%
    gather(key = "column_orig", value = "value", starts_with("Q")) %>%
    left_join(msmt_survey_key) %>%
    select(-stub, -rows_in_qa, -column_orig, -question_answer, -answer)
}

csi_priority_recode <- function(df) {
  df %>%
    group_by(red_izo) %>%
    mutate(id = paste(red_izo, row_number(), sep = "_")) %>%
    ungroup() %>%
    spread(rownum_in_qa, value) %>%
    rename(checked = `1`, comment = `2`) %>%
    filter(checked == 1 | !is.na(comment)) %>%
    mutate(text_detail = if_else(text == "jiné – uveďte jaké - Komentář",
                                 comment, text),
           freetext = text == "jiné – uveďte jaké - Komentář") %>%
    filter(!text_detail %in% c("0", "jiné – uveďte jaké")) %>%
    mutate(text_detail_lower = tolower(text_detail),
           text_recoded = case_when(!freetext ~ text_detail,
                                    str_detect(text_detail_lower, "bakal") ~ "Bakaláři",
                                    str_detect(text_detail_lower, "messenger|[Ff]acebook") ~ "Facebook/Messenger",
                                    str_detect(text_detail_lower, "google [m]") ~ "G Hangout/Meet/Classroom",
                                    str_detect(text_detail_lower, "google [c]l") ~ "G Hangout/Meet/Classroom",
                                    str_detect(text_detail_lower, "edoo") ~ "Edookit",
                                    str_detect(text_detail_lower, "365") ~ "Office 365",
                                    str_detect(text_detail_lower, "teams") ~ "Microsoft Teams",
                                    str_detect(text_detail_lower, "wh?ats") ~ "WhatsApp",
                                    TRUE ~ "jiné/žádné"
           ) %>%
             recode(`Hangaut meet` = "G Hangout/Meet/Classroom")) %>%
    mutate(typskoly_txt = case_when(typskoly == "zs" ~ "Základní škola",
                                    typskoly == "ss" ~ "Střední škola") %>%
             fct_relevel("Střední škola", after = Inf))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param csi_recoded
##' @return
##' @author Petr Bouchal
##' @export
csi_summarise <- function(csi, csi_long, csi_recoded) {

  base <- csi %>%
    select(red_izo, izo, ico, typskoly, priorita) %>%
    mutate(typskoly_txt = case_when(typskoly == "zs" ~ "Základní škola",
                                    typskoly == "ss" ~ "Střední škola") %>%
             fct_relevel("Střední škola", after = Inf))

  has_response_tech <- csi_long %>%
    group_by(red_izo) %>%
    summarise(has_tech_selected = !all(is.na(value)))

  recoded_selection <- csi_recoded %>%
    select(red_izo, izo, ico, text_recoded, typskoly_txt) %>%
    group_by(red_izo, izo, ico) %>%
    chop(text_recoded) %>%
    mutate(has_google = map_lgl(text_recoded, ~"G Hangout/Meet/Classroom" %in% .),
           only_whatsapp = map_lgl(text_recoded, ~"WhatsApp" %in% .) &
             map_lgl(text_recoded, ~length(.) == 1),
           has_ms = map_lgl(text_recoded, ~any(. %in% c("Office 365", "Microsoft Teams"))))

  collated <- base %>%
    left_join(has_response_tech) %>%
    left_join(recoded_selection)

  return(collated)
}

