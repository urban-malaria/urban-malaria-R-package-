
# Directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")

DataDir <- file.path(DriveDir, "data")
StateShpDir <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES")
ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")
RastersDir <- file.path(DriveDir, "data/nigeria/Raster_files")
MapOutputDir <- file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs")

# Example final function call
results <- reprioritize(
  state_name = "Delta",
  shapefile_path = file.path(StateShpDir, "Delta/Delta_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/Deltatpr.csv"),
  output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions"),
  itn_dir = file.path(ITNDir, "pbi_distribution_Delta.xlsx"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    #elevation_path = file.path(RastersDir, "Elevation/ELE.tif"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  risk_factors = c("distance_to_water", "mean_EVI", "flood", "u5_tpr_rdt"),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/delta_urban_percentage.geojson"),
  map_output_dir = file.path(MapOutputDir),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 75),
)
