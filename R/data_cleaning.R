# ==========================================================================================================================================
## Script Name: Data Cleaning
# Author: Grace Legris, Research Data Analyst
# Date: 04/10/25
# Purpose: Cleans ward and LGA names for states that have duplicate ward names.
# ==========================================================================================================================================

## =========================================================================================================================================
### Function to clean up dataframes after merging
## =========================================================================================================================================

clean_extracted_data <- function(df) {
  # clean dataset before returning it
  extracted_data_plus <- df %>%
    dplyr::select(!matches("\\.y$")) %>%  # remove columns ending in .y (assuming .x and .y are duplicates)
    rename_with(~ gsub("\\.x$", "", .))  # remove .x suffix from column names
}

## =========================================================================================================================================
### Function to add LGA name to ward name in the extracted data
## =========================================================================================================================================

clean_extracted <- function(state_name, extracted_data, state_shapefile) {

  if (state_name %in% c("Niger", "niger")) {
    extracted_data <- extracted_data %>%
      mutate(WardName = case_when(
        WardName == "Magajiya" & LGACode == "27011" ~ "Magajiya (Kontagora LGA)",
        WardName == "Magajiya" & LGACode == "27023"  ~ "Magajiya (Suleja LGA)",
        WardName == "Sabon Gari" & LGACode == "27020"  ~ "Sabon Gari (Rafi LGA)",
        WardName == "Sabon Gari" & LGACode == "27006"  ~ "Sabon Gari (Chanchaga LGA)",
        WardName == "Sabon Gari" & LGACode == "27025"  ~ "Sabon Gari (Wushishi LGA)",
        WardName == "Kodo" & LGACode == "27005" ~ "Kodo (Bosso LGA)",
        WardName == "Kodo" & LGACode == "27025"  ~ "Kodo (Wushishi LGA)",
        WardName == "Kudu" & LGACode == "27011" ~ "Kudu (Kontagora LGA)",
        WardName == "Kudu" & LGACode == "27017"  ~ "Kudu (Mokwa LGA)",
        WardName == "Kawo" & LGACode == "27011" ~ "Kawo (Kontagora LGA)",
        WardName == "Kawo" & LGACode == "27014"  ~ "Kawo (Magama LGA)",
        TRUE ~ WardName
      ))
  }

  if (state_name %in% c("Kaduna", "kaduna")) {
    extracted_data <- extracted_data %>%
      mutate(WardName = case_when(
        WardName == "Kaura" & LGACode == "19013" ~ "Kaura (Kaura LGA)",
        WardName == "Kaura" & LGACode == "19023"  ~ "Kaura (Zaria LGA)",
        WardName == "Tudun Wada" & LGACode == "19018" ~ "Tudun Wada (Makarfi LGA)",
        WardName == "Tudun Wada" & LGACode == "19023"  ~ "Tudun Wada (Zaria LGA)",
        WardName == "Fada" & LGACode == "19006" ~ "Fada (Jaba LGA)",
        WardName == "Fada" & LGACode == "19013"  ~ "Fada (Kaura LGA)",
        WardName == "Kakangi" & LGACode == "19001" ~ "Kakangi (Birnin Gwari LGA)",
        WardName == "Kakangi" & LGACode == "19003"  ~ "Kakangi (Giwa LGA)",
        WardName == "Sabon Birnin" & LGACode == "19004" ~ "Sabon Birnin (Igabi LGA)",
        WardName == "Sabon Birnin" & LGACode == "19017"  ~ "Sabon Birnin (Lere LGA)",
        WardName == "Zabi" & LGACode == "19015" ~ "Zabi (Kubau LGA)",
        WardName == "Zabi" & LGACode == "19016"  ~ "Zabi (Kudan LGA)",
        WardName == "Zabi" & LGACode == "19019"  ~ "Zabi (Sabon Gari LGA)",
        TRUE ~ WardName
      )) %>%
      dplyr::filter(!(WardName == "Garu" & LGACode == 19021))
  }

  if (state_name %in% c("Katsina", "katsina")) {
    extracted_data <- extracted_data %>%
      mutate(WardName = case_when(
        WardName == "Sabon Gari" & WardCode == "41008" ~ "Sabon Gari (Daura LGA)",
        WardName == "Sabon Gari" & WardCode == "41410"  ~ "Sabon Gari (Funtua LGA)",
        WardName == "Sabon Gari" & WardCode == "43010" ~ "Sabon Gari (Rimi LGA)",
        WardName == "Baure" & WardCode == "40402" ~ "Baure (Baure LGA)",
        WardName == "Baure" & WardCode == "40501" ~ "Baure (Bindawa LGA)",
        WardName == "Gurbi" & WardCode == "41605" ~ "Gurbi (Jibia LGA)",
        WardName == "Gurbi" & WardCode == "41904" ~ "Gurbi (Kankara LGA)",
        WardName == "Kandawa" & WardCode == "40305" ~ "Kandawa (Batsari LGA)",
        WardName == "Kandawa" & WardCode == "41507" ~ "Kandawa (Ingawa LGA)",
        WardName == "Machika" & WardCode == "42607" ~ "Machika (Mani LGA)",
        WardName == "Machika" & WardCode == "43105" ~ "Machika (Sabuwa LGA)",
        WardName == "Makera" & WardCode == "41210" ~ "Makera (Dutsin-Ma LGA)",
        WardName == "Makera" & WardCode == "41407" ~ "Makera (Funtua LGA)",
        WardName == "Mazoji A" & WardCode == "41004" ~ "Mazoji A (Daura LGA)",
        WardName == "Mazoji A" & WardCode == "42807" ~ "Mazoji A (Matazu LGA)",
        WardName == "Mazoji B" & WardCode == "41005" ~ "Mazoji B (Daura LGA)",
        WardName == "Mazoji B" & WardCode == "42808" ~ "Mazoji B (Matazu LGA)",
        WardName == "Safana" & WardCode == "40609" ~ "Safana (Charanchi LGA)",
        WardName == "Safana" & WardCode == "43207" ~ "Safana (Safana LGA)",
        WardName == "Zango" & WardCode == "41911" ~ "Zango (Kankara LGA)",
        WardName == "Zango" & WardCode == "43410" ~ "Zango (Zango LGA)",
        TRUE ~ WardName
      )) %>%
      dplyr::filter(!(WardName == "Garu" & LGACode == 19021))
  }

  if (state_name %in% c("Taraba", "taraba")) {
    extracted_data <- extracted_data %>%
      mutate(WardName = case_when(
        WardName == "Suntai" & LGACode == 35002 ~ "Suntai (Bali LGA)",
        WardName == "Suntai" & LGACode == 35003 ~ "Suntai (Donga LGA)",
        TRUE ~ WardName
      )) %>%
      dplyr::filter(!(WardName == "Garu" & LGACode == 19021))
  }

  if (state_name %in% c("Yobe", "yobe")) {
    extracted_data <- extracted_data %>%
      mutate(WardName = case_when(
        WardName == "Hausari" & Urban == "Yes" ~ "Hausari (Nguru LGA)",
        WardName == "Hausari" & Urban == "No"  ~ "Hausari (Geidam LGA)",
        TRUE ~ WardName
      ))
  }

  return(extracted_data)
}

## =========================================================================================================================================
### Function to add the LGA name to the ward name in the shapefiles
## =========================================================================================================================================

clean_shapefile <- function(state_name, state_shapefile) {
  if (state_name %in% c("Niger", "niger")) {
    state_shapefile <- state_shapefile %>%
      mutate(WardName = case_when(
        WardName == "Magajiya" & LGACode == "27011" ~ "Magajiya (Kontagora LGA)",
        WardName == "Magajiya" & LGACode == "27023"  ~ "Magajiya (Suleja LGA)",
        WardName == "Sabon Gari" & LGACode == "27020"  ~ "Sabon Gari (Rafi LGA)",
        WardName == "Sabon Gari" & LGACode == "27006"  ~ "Sabon Gari (Chanchaga LGA)",
        WardName == "Sabon Gari" & LGACode == "27025"  ~ "Sabon Gari (Wushishi LGA)",
        WardName == "Kodo" & LGACode == "27005" ~ "Kodo (Bosso LGA)",
        WardName == "Kodo" & LGACode == "27025"  ~ "Kodo (Wushishi LGA)",
        WardName == "Kudu" & LGACode == "27011" ~ "Kudu (Kontagora LGA)",
        WardName == "Kudu" & LGACode == "27017"  ~ "Kudu (Mokwa LGA)",
        WardName == "Kawo" & LGACode == "27011" ~ "Kawo (Kontagora LGA)",
        WardName == "Kawo" & LGACode == "27014"  ~ "Kawo (Magama LGA)",
        TRUE ~ WardName
      ))
  }

  if (state_name %in% c("Kaduna", "kaduna")) {
    state_shapefile <- state_shapefile %>%
      mutate(WardName = case_when(
        WardName == "Kaura" & LGACode == "19013" ~ "Kaura (Kaura LGA)",
        WardName == "Kaura" & LGACode == "19023"  ~ "Kaura (Zaria LGA)",
        WardName == "Tudun Wada" & LGACode == "19018" ~ "Tudun Wada (Makarfi LGA)",
        WardName == "Tudun Wada" & LGACode == "19023"  ~ "Tudun Wada (Zaria LGA)",
        WardName == "Fada" & LGACode == "19006" ~ "Fada (Jaba LGA)",
        WardName == "Fada" & LGACode == "19013"  ~ "Fada (Kaura LGA)",
        WardName == "Kakangi" & LGACode == "19001" ~ "Kakangi (Birnin Gwari LGA)",
        WardName == "Kakangi" & LGACode == "19003"  ~ "Kakangi (Giwa LGA)",
        WardName == "Sabon Birnin" & LGACode == "19004" ~ "Sabon Birnin (Igabi LGA)",
        WardName == "Sabon Birnin" & LGACode == "19017"  ~ "Sabon Birnin (Lere LGA)",
        WardName == "Zabi" & LGACode == "19015" ~ "Zabi (Kubau LGA)",
        WardName == "Zabi" & LGACode == "19016"  ~ "Zabi (Kudan LGA)",
        WardName == "Zabi" & LGACode == "19019"  ~ "Zabi (Sabon Gari LGA)",
        TRUE ~ WardName
      ))
  }

  if (state_name %in% c("Katsina", "katsina")) {
    state_shapefile <- state_shapefile %>%
      mutate(WardName = case_when(
        WardName == "Sabon Gari" & WardCode == "41008" ~ "Sabon Gari (Daura LGA)",
        WardName == "Sabon Gari" & WardCode == "41410"  ~ "Sabon Gari (Funtua LGA)",
        WardName == "Sabon Gari" & WardCode == "43010" ~ "Sabon Gari (Rimi LGA)",
        WardName == "Baure" & WardCode == "40402" ~ "Baure (Baure LGA)",
        WardName == "Baure" & WardCode == "40501" ~ "Baure (Bindawa LGA)",
        WardName == "Gurbi" & WardCode == "41605" ~ "Gurbi (Jibia LGA)",
        WardName == "Gurbi" & WardCode == "41904" ~ "Gurbi (Kankara LGA)",
        WardName == "Kandawa" & WardCode == "40305" ~ "Kandawa (Batsari LGA)",
        WardName == "Kandawa" & WardCode == "41507" ~ "Kandawa (Ingawa LGA)",
        WardName == "Machika" & WardCode == "42607" ~ "Machika (Mani LGA)",
        WardName == "Machika" & WardCode == "43105" ~ "Machika (Sabuwa LGA)",
        WardName == "Makera" & WardCode == "41210" ~ "Makera (Dutsin-Ma LGA)",
        WardName == "Makera" & WardCode == "41407" ~ "Makera (Funtua LGA)",
        WardName == "Mazoji A" & WardCode == "41004" ~ "Mazoji A (Daura LGA)",
        WardName == "Mazoji A" & WardCode == "42807" ~ "Mazoji A (Matazu LGA)",
        WardName == "Mazoji B" & WardCode == "41005" ~ "Mazoji B (Daura LGA)",
        WardName == "Mazoji B" & WardCode == "42808" ~ "Mazoji B (Matazu LGA)",
        WardName == "Safana" & WardCode == "40609" ~ "Safana (Charanchi LGA)",
        WardName == "Safana" & WardCode == "43207" ~ "Safana (Safana LGA)",
        WardName == "Zango" & WardCode == "41911" ~ "Zango (Kankara LGA)",
        WardName == "Zango" & WardCode == "43410" ~ "Zango (Zango LGA)",
        TRUE ~ WardName
      ))
  }

  if (state_name %in% c("Taraba", "taraba")) {
    state_shapefile <- state_shapefile %>%
      mutate(WardName = case_when(
        WardName == "Suntai" & LGACode == 35002 ~ "Suntai (Bali LGA)",
        WardName == "Suntai" & LGACode == 35003 ~ "Suntai (Donga LGA)",
        TRUE ~ WardName
      )) %>%
      dplyr::filter(!(WardName == "Garu" & LGACode == 19021))
  }

  if (state_name %in% c("Yobe", "yobe")) {
    state_shapefile <- state_shapefile %>%
      mutate(WardName = case_when(
        WardName == "Hausari" & Urban == "Yes" ~ "Hausari (Nguru LGA)",
        WardName == "Hausari" & Urban == "No"  ~ "Hausari (Geidam LGA)",
        TRUE ~ WardName
      ))
  }

  return(state_shapefile)
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

## =========================================================================================================================================
### Function to add LGA to ward name in the ITN datasets
### My ITN cleaning script standardized the spelling/formatting of the ward names, and this code adds the LGA name to wards that appear
### more than once. We could move this to the ITN cleaning script to simplify the package.
## =========================================================================================================================================

clean_itn_data <- function(state_name, state_itn_data) {

  # if niger state, add LGA labels to the duplicate wards
  if (state_name %in% c("Niger", "niger")) {
    state_itn_data <- state_itn_data %>%
      mutate(Ward = case_when(
        Ward == "Magajiya" & LGA == "Suleja" ~ "Magajiya (Suleja LGA)",
        Ward == "Magajiya" & LGA == "Kontagora"  ~ "Magajiya (Kontagora LGA)",
        Ward == "Sabon Gari" & LGA == "Rafi" ~ "Sabon Gari (Rafi LGA)",
        Ward == "Sabon Gari" & LGA == "Chanchaga"  ~ "Sabon Gari (Chanchaga LGA)",
        Ward == "Sabon Gari" & LGA == "Wushishi"  ~ "Sabon Gari (Wushishi LGA)",
        Ward == "Kodo" & LGA == "Bosso" ~ "Kodo (Bosso LGA)",
        Ward == "Kodo" & LGA == "Wushishi"  ~ "Kodo (Wushishi LGA)",
        Ward == "Kudu" & LGA == "Kontagora" ~ "Kudu (Kontagora LGA)",
        Ward == "Kudu" & LGA == "Mokwa"  ~ "Kudu (Mokwa LGA)",
        Ward == "Kawo" & LGA == "Kontagora" ~ "Kawo (Kontagora LGA)",
        Ward == "Kawo" & LGA == "Magama"  ~ "Kawo (Magama LGA)",
        TRUE ~ Ward
      ))
  }

  # if kaduna state, add LGA labels to the duplicate wards
  if (state_name %in% c("Kaduna", "kaduna")) {
    state_itn_data <- state_itn_data %>%
      mutate(Ward = case_when(
        Ward == "Kaura" & LGA == "Kaura" ~ "Kaura (Kaura LGA)",
        Ward == "Kaura" & LGA == "Zaria"  ~ "Kaura (Zaria LGA)",
        Ward == "Tudun Wada" & LGA == "Makarfi" ~ "Tudun Wada (Makarfi LGA)",
        Ward == "Tudun Wada" & LGA == "Zaria"  ~ "Tudun Wada (Zaria LGA)",
        Ward == "Fada" & LGA == "Jaba" ~ "Fada (Jaba LGA)",
        Ward == "Fada" & LGA == "Kaura"  ~ "Fada (Kaura LGA)",
        Ward == "Kakangi" & LGA == "Birnin Gwari" ~ "Kakangi (Birnin Gwari LGA)",
        Ward == "Kakangi" & LGA == "Giwa"  ~ "Kakangi (Giwa LGA)",
        Ward == "Sabon Birnin" & LGA == "Igabi"  ~ "Sabon Birnin (Igabi LGA)",
        Ward == "Zabi" & LGA == "Kubau" ~ "Zabi (Kubau LGA)",
        Ward == "Zabi" & LGA == "Kudan"  ~ "Zabi (Kudan LGA)",
        Ward == "Zabi" & LGA == "Sabon Gari"  ~ "Zabi (Sabon Gari LGA)",
        TRUE ~ Ward
      )) %>%
      dplyr::filter(!(Ward == "Doka" & LGA == "Kachia"))
  }

  # if katsina state, add LGA labels to the duplicate wards
  if (state_name %in% c("Katsina", "katsina")) {
    state_itn_data <- state_itn_data %>%
      mutate(Ward = case_when(
        Ward == "Sabon Gari" & LGA == "Daura" ~ "Sabon Gari (Daura LGA)",
        Ward == "Sabon Gari" & LGA == "Funtua"  ~ "Sabon Gari (Funtua LGA)",
        TRUE ~ Ward
      ))
  }

  # if yobe state, add LGA labels to the duplicate wards
  if (state_name %in% c("Yobe", "yobe")) {
    state_itn_data <- state_itn_data %>%
      mutate(Ward = case_when(
        Ward == "Hausari" & LGA == "Geidam" ~ "Hausari (Geidam LGA)",
        Ward == "Hausari" & LGA == "Nguru"  ~ "Hausari (Nguru LGA)",
        TRUE ~ Ward
      ))
  }

  return(state_itn_data)
}
