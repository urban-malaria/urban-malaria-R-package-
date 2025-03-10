
#' Run reprioritization analysis for a given state
#'
#' This function extracts raster data, merges TPR data, calculates composite malaria risk scores,
#' ranks wards, and generates reprioritization maps.
#'
#' @param state_name Name of the state (e.g., "Kano")
#' @param shapefile_path Path to the state shapefile
#' @param tpr_data_path Path to the TPR data CSV file
#' @param output_dir Path to save extracted and processed data
#' @param itn_dir Path to the ITN distribution data
#' @param raster_paths Named list of paths to raster data for extraction
#' @param extracted_data_dir Path to the extracted data
#' @param risk_factors Vector of covariate names to use in risk score calculation
#' @export

reprioritize <- function(state_name, shapefile_path, tpr_data_path, itn_dir,
                         extracted_data_dir, raster_paths, risk_factors, urban_data_path, map_output_dir,
                         include_settlement_type, include_u5_tpr_data, scenarios) {
  message("Extracting raster data...")
  extracted_data <- extract_raster_data(
    state_name = state_name,
    shapefile_path = shapefile_path,
    raster_paths = raster_paths
  )

  urban_data <- get_urban_percentage(urban_data_path)

  message("Merging extracted data with urban percentage data...")
  extracted_data <- base::merge(extracted_data, urban_data, by = "WardName", all.x = TRUE)

  message("Merging TPR data...")
  extracted_data_plus <- tpr_merge(
    tpr_data_path = tpr_data_path,
    extracted_data = extracted_data
  )

  message("Calculating composite malaria risk scores...")
  malaria_risk_scores <- calculate_malaria_risk_scores(
    extracted_data = extracted_data_plus,
    include_settlement_type = include_settlement_type,
    include_u5_tpr_data = include_u5_tpr_data
  )

  message("Ranking wards by risk score...")
  ranked_wards <- rank(malaria_risk_scores)

  message("Reprioritizing and creating maps...")
  maps <- create_reprioritization_map(
    state_name = state_name,
    shp_dir = shapefile_path,
    itn_dir = itn_dir,
    extracted_data = extracted_data_plus,
    ranked_wards = ranked_wards,
    map_output_dir = MapOutputDir,
    include_settlement_type = include_settlement_type,
    include_u5_tpr_data = include_u5_tpr_data,
    scenarios = scenarios,
  )

  message("Reprioritization process for ", state_name, " completed.")
  return(maps)
}
