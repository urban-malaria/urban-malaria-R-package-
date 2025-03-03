
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

reprioritize <- function(state_name, shapefile_path, tpr_data_path, output_dir, itn_dir,
                         extracted_data_dir, raster_paths, risk_factors, urban_data_path) {
  message("Extracting raster data...")
  extracted_data <- extract_raster_data(
    state_name = state_name,
    shapefile_path = shapefile_path,
    raster_paths = raster_paths
  )

  urban_data <- get_urban_percentage(urban_data_path)

  message("Merging extracted data with urban percentage data...")
  extracted_data <- merge(extracted_data, urban_data, by = "WardName", all.x = TRUE)

  message("Merging TPR data...")
  extracted_data_plus <- merge(
    tpr_data_path = tpr_data_path,
    extracted_data = extracted_data
  )

  message("Calculating composite malaria risk scores...")
  malaria_risk_scores <- calculate_malaria_risk_scores(
    extracted_data = extracted_data_plus,
    risk_factors
  )

  message("Ranking wards by risk score...")
  ranked_wards <- rank(malaria_risk_scores)

  message("Reprioritizing and creating maps...")
  maps <- create_reprioritization_map(
    state_name = state_name,
    shp_dir = shapefile_path,
    output_dir = output_dir,
    itn_dir = itn_dir,
    extracted_data_dir = extracted_data_dir,
    ranked_wards = ranked_wards
  )

  message("Reprioritization process for ", state_name, " completed.")
  return(maps)
}

# final function call
results <- reprioritize(
  state_name = "Kano",
  shapefile_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State.shp",
  tpr_data_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/kanotpr3.csv",
  output_dir = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/extractions",
  itn_dir = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/pbi_distribution_Kano.csv",
  raster_paths = list(
    h2o_distance_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/distance_to_water_bodies/distance_to_water.tif",
    elevation_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/Elevation/ELE.tif",
    output_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/extractions")
  ),
  extracted_data_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/Final Extractions/kano_plus.csv"),
  risk_factors = c("distance_to_water", "elevation", "u5_tpr_rdt"),
  urban_data_path = file.path("file/path")
)



reprioritize <- function() {
  covariate_paths <- list(
    #evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    # ndvi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_NDVI_MOD13A1"),
    # rainfall_path = file.path(RastersDir, "monthly rainfall 2023-24"),
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies", "distance_to_water.tif"),
    elevation_path = file.path(RastersDir, "Elevation", "ELE.tif"),
    # rh_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2023.grib"),
    # rh_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "relative_humidity_2024.grib"),
    # temp_2023 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2023.grib"),
    # temp_2024 = file.path(RastersDir, "temp_humidity_2023_grib", "temperature_2024.grib"),
    # housing_quality_path = file.path(RastersDir, "housing",
    #                                  "2019_Nature_Africa_Housing_2015_NGA.tiff"),
    #ndwi_path = file.path(RastersDir, "global_surface_water",
    #"Nigeria_NDWI_2023.tif"),
    #ndmi_path = file.path(RastersDir, "global_surface_water",
    #"NDMI_Nigeria_2023.tif"),
    # pfpr_path = file.path(PfDir, "pf_parasite_rate",
    #                       "202406_Global_Pf_Parasite_Rate_NGA_2022.tiff"),
    # lights_path = file.path(RastersDir, "night_time_light_2023"),
    # surface_soil_wetness_path = file.path(RastersDir, "surface_soil_wetness"),
    # flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    # #surface_h20_path = file.path(RastersDir, "global_surface_water"),
    # settlement_block = file.path(RastersDir, "nigeria_settlement_classification", "blocks_V1.1",
    #                              "Nigeria_Blocks_V1.shp"),
    output_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/extractions")
  )

  # extract covariates
  message("Extracting raster data...")
  kano_extractions <- extract_raster_data(
    state_name = "Kano",
    shapefile_path = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State.shp"),
    raster_paths = covariate_paths
  )

  # merge tpr data
  message("Merging TPR data...")
  extracted_data_plus <- merge(
    tpr_data_path = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/kanotpr3.csv"),
    extracted_data = kano_extractions
  )

  # calculate composite scores
  message("Calculating composite risk scores...")
  malaria_risk_scores <- calculate_malaria_risk_scores(extracted_data = extracted_data_plus, c("distance_to_water", "settlement_type", "u5_tpr_rdt"))

  # rank
  message("Ranking wards by risk score...")
  ranked_wards <- rank(malaria_risk_scores)

  # create maps
  message("Reprioritizing and creating maps...")
  maps <- create_reprioritization_map(
    state_name = "Kano",
    shp_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State.shp"),
    output_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/outputs"),
    itn_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/ITN_distribution/pbi_distribution_Kano.csv"),
    extracted_data_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/Final Extractions/kano_plus.csv"),
    ranked_wards = ranked_wards
  )

}
