
#' Run reprioritization analysis for a given state
#'
#' This function extracts raster data, merges TPR data, calculates composite malaria risk scores,
#' ranks wards, and generates reprioritization maps.
#'
#' @param state_name Name of the state (e.g., "Kano")
#' @param shapefile_path Path to the state shapefile
#' @param tpr_data_path Path to the TPR data CSV file
#' @param itn_dir Path to the ITN distribution data
#' @param raster_paths Named list of paths to raster data for extraction
#' @param risk_factors Vector of covariate names to use in risk score calculation
#' @export

reprioritize <- function(state_name, shapefile_path, tpr_data_path, itn_dir,
                         raster_paths, urban_data_path, settlement_block_path, map_output_dir,
                         include_settlement_type, include_u5_tpr_data, scenarios) {

  # read in shapefile once
  message("Reading in shapefile...")
  state_shapefile <- tryCatch({
      sf::st_make_valid(st_read(shapefile_path))
  }, error = function(e) stop("Failed to load shapefile: ", e$message))

  message("Extracting raster data...")
  extracted_data <- extract_raster_data(
    state_name = state_name,
    shapefile = state_shapefile,
    raster_paths = raster_paths
  )

  # check for urban percentage data and prompt user to download it if they haven't already
  urban_data <- get_urban_percentage(urban_data_path)

  message("Merging extracted data with urban percentage data...")
  extracted_data <- base::merge(extracted_data, urban_data, by = "WardCode", all.x = TRUE)
  extracted_data <- clean_extracted_data(extracted_data)

  # add LGA name to wards that have duplicate names
  extracted_data <- clean_extracted(state_name, extracted_data)
  state_shapefile <- clean_shapefile(state_name, state_shapefile)

  if(include_u5_tpr_data == "Yes" || include_u5_tpr_data == "yes") {
    message("Merging TPR data with extracted data...")
    # delete any duplicate columns
    extracted_data <- extracted_data[!duplicated(extracted_data), ]

    extracted_data_plus <- tpr_merge(
      tpr_data_path = tpr_data_path,
      extracted_data = extracted_data,
      state_name = state_name
    )
    extracted_data_plus <- clean_extracted_data(extracted_data_plus)
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

  # clean duplicates from merge
  extracted_data_plus <- clean_extracted_plus(state_name, extracted_data_plus)

  # check for settlement blocks data and prompt user to download it if they haven't already
  if(include_settlement_type == "Yes" || include_settlement_type == "yes") {
    message("Getting settlement blocks data...")
    settlement_block_shp <- get_settlement_blocks(settlement_block_path)
    extracted_data_plus <- settlement_type_merge(settlement_block_shp, extracted_data_plus, state_name)
    extracted_data_plus <- clean_extracted_data(extracted_data_plus)
  }

  # delete any duplicate columns
  extracted_data_plus <- extracted_data_plus[!duplicated(extracted_data_plus), ]

  message("Calculating composite malaria risk scores...")
  malaria_risk_scores <- calculate_malaria_risk_scores(
    extracted_data = extracted_data_plus,
    raster_paths = raster_paths,
    include_settlement_type = include_settlement_type,
    include_u5_tpr_data = include_u5_tpr_data
  )

  message("Ranking wards by risk score...")
  ranked_wards <- rank(malaria_risk_scores)

  message("Reprioritizing and creating maps...")
  maps <- create_reprioritization_map(
    state_name = state_name,
    shapefile = state_shapefile,
    itn_dir = itn_dir,
    extracted_data = extracted_data_plus,
    ranked_wards = ranked_wards,
    map_output_dir = map_output_dir,
    include_settlement_type = include_settlement_type,
    include_u5_tpr_data = include_u5_tpr_data,
    scenarios = scenarios
  )

  message("Reprioritization process for ", state_name, " completed.")
  return(maps)
}
