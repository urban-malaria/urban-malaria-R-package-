# ==========================================================================================================================================
## Script Name: Data Cleaning
# Author: Grace Legris, Research Data Analyst
# Date: 04/10/25
# Purpose: Cleans ward and LGA names for states that have duplicate ward names.
# ==========================================================================================================================================

## =========================================================================================================================================
### Function to clean up dataframes after merging
## =========================================================================================================================================

clean_merge <- function(df) {
  # clean dataset before returning it
  extracted_data_plus <- df %>%
    dplyr::select(!matches("\\.y$")) %>%  # remove columns ending in .y (assuming .x and .y are duplicates)
    rename_with(~ gsub("\\.x$", "", .))  # remove .x suffix from column names
}

## =========================================================================================================================================
### Function to remove observations that merged incorrectly (this was only a problem for Katsina and Niger)
## =========================================================================================================================================

clean_extracted_plus <- function(state_name, extracted_data_plus) {
  if (state_name %in% c("Katsina", "katsina")) {
    extracted_data_plus <- extracted_data_plus %>%
      filter(
        !(WardName == "Sabon Gari (Funtua LGA)" & LGA == "Rimi") &
          !(WardName == "Sabon Gari (Rimi LGA)" & LGA == "Funtua") &
          !(WardName == "Safana (Charanchi LGA)" & LGA == "Safana") &
          !(WardName == "Safana (Safana LGA)" & LGA == "Charanchi") &
          !(WardName == "Zango (Kankara LGA)" & LGA == "Zango") &
          !(WardName == "Zango (Zango LGA)" & LGA == "Kankara")
      ) %>%
      mutate(
        LGA = ifelse(WardName == "Sabon Gari (Daura LGA)", "Daura", LGA),
        LGA = ifelse(WardName == "Baure (Baure LGA)", "Baure", LGA),
        LGA = ifelse(WardName == "Gurbi (Jibia LGA)", "Jibia", LGA),
        LGA = ifelse(WardName == "Kandawa (Ingawa LGA)", "Ingawa", LGA),
        LGA = ifelse(WardName == "Machika (Mani LGA)", "Mani", LGA),
        LGA = ifelse(WardName == "Makera (Funtua LGA)", "Funtua", LGA),
        LGA = ifelse(WardName == "Mazoji A (Matazu LGA)", "Matazu", LGA),
        LGA = ifelse(WardName == "Mazoji B (Daura LGA)", "Daura", LGA),
        LGA = ifelse(WardName == "Mazoji B (Matazu LGA)", "Matazu", LGA),
      )
  }
  if (state_name %in% c("Niger", "niger")) {
    extracted_data_plus <- extracted_data_plus %>%
      filter(
        !(WardName == "Magajiya (Kontagora LGA)" & LGA == "Suleja") &
          !(WardName == "Magajiya (Suleja LGA)" & LGA == "Kontagora") &
          !(WardName == "Sabon Gari (Chanchaga LGA)" & LGA == "Wushishi") &
          !(WardName == "Sabon Gari (Chanchaga LGA)" & LGA == "Rafi") &
          !(WardName == "Sabon Gari (Wushishi LGA)" & LGA == "Chanchaga") &
          !(WardName == "Sabon Gari (Wushishi LGA)" & LGA == "Rafi") &
          !(WardName == "Sabon Gari (Rafi LGA)" & LGA == "Wushishi") &
          !(WardName == "Sabon Gari (Rafi LGA)" & LGA == "Chanchaga") &
          !(WardName == "Kodo (Bosso LGA)" & LGA == "Wushishi") &
          !(WardName == "Kodo (Wushishi LGA)" & LGA == "Bosso") &
          !(WardName == "Kudu (Kontagora LGA)" & LGA == "Mokwa") &
          !(WardName == "Kudu (Mokwa LGA)" & LGA == "Kontagora") &
          !(WardName == "Kawo (Kontagora LGA)" & LGA == "Magama") &
          !(WardName == "Kawo (Magama LGA)" & LGA == "Kontagora")
      )
  }
  return(extracted_data_plus)
}

clean_extracted_plus <- function(extracted_data_plus) {
  extracted_data_plus %>%
    mutate(
      # extract the LGA name in parentheses from WardName
      lga_in_ward = str_match(WardName, "\\(([^\\)]+)\\)")[,2],
      # normalize for consistent comparison
      lga_in_ward_clean = str_to_lower(str_trim(lga_in_ward)),
      lga_col_clean = str_to_lower(str_trim(LGA))
    ) %>%
    # filter out rows where LGA in parentheses exists and doesn't match actual LGA
    filter(is.na(lga_in_ward_clean) | lga_in_ward_clean == lga_col_clean) %>%
    select(-lga_in_ward, -lga_in_ward_clean, -lga_col_clean)
}
