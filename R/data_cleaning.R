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

add_urban_var <- function(urban_data) {

  if (!"Urban" %in% colnames(urban_data)) {
    urban_data$Urban <- ifelse(urban_data$urbanPercentage > 30, "Yes", "No")
  }

  return(urban_data)

}

clean_extracted_plus <- function(extracted_data_plus) {
  extracted_data_plus <- extracted_data_plus %>%
    mutate(
      lga_in_ward = stringr::str_match(WardName, "\\(([^\\)]+) LGA\\)")[,2]
    ) %>%
    mutate(
      lga_in_ward_clean = stringr::str_to_lower(stringr::str_trim(lga_in_ward)),
      lga_col_clean = stringr::str_to_lower(stringr::str_trim(LGAName))
    ) %>%
    filter(is.na(lga_in_ward_clean) | lga_in_ward_clean == lga_col_clean) %>%
    select(-lga_in_ward, -lga_in_ward_clean, -lga_col_clean)

  return(extracted_data_plus)
}
