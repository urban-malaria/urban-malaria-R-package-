
# directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")

DataDir <- file.path(DriveDir, "data")
StateShpDir <- file.path(DriveDir, "data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES")
ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")
RastersDir <- file.path(DriveDir, "data/nigeria/Raster_files")

# final function call for each state:

# YOBE
results <- reprioritize(
  state_name = "Yobe",
  shapefile_path = file.path(StateShpDir, "Yobe/Yobe_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/yobetpr_updated.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Yobe_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/yobe_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)

# KATSINA
results <- reprioritize(
  state_name = "Katsina",
  shapefile_path = file.path(StateShpDir, "Katsina/Katsina_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/katsinatpr_updated.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Katsina_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/katsina_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)

# DELTA
results <- reprioritize(
  state_name = "Delta",
  shapefile_path = file.path(StateShpDir, "Delta/Delta_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/deltatpr.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Delta_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/delta_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)

# NIGER
results <- reprioritize(
  state_name = "Niger",
  shapefile_path = file.path(StateShpDir, "Niger/Niger_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/nigertpr_updated.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Niger_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/niger_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)

# TARABA
results <- reprioritize(
  state_name = "Taraba",
  shapefile_path = file.path(StateShpDir, "Taraba/Taraba_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/tarabatpr.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Taraba_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/taraba_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)

# KADUNA
results <- reprioritize(
  state_name = "Kaduna",
  shapefile_path = file.path(StateShpDir, "Kaduna/Kaduna_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/kadunatpr_updated.csv"),
  itn_dir = file.path(ITNDir, "cleaned", "pbi_distribution_Kaduna_clean.csv"),
  raster_paths = list(
    h2o_distance_path = file.path(RastersDir, "distance_to_water_bodies/distance_to_water.tif"),
    evi_path = file.path(RastersDir, "Updated_Covariates", "2023-24_EVI_MOD13A1"),
    flood_path = file.path(RastersDir, "Flooding", "flooding_2023"),
    output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/extractions")
  ),
  urban_data_path = file.path(DataDir, "nigeria/urban_percentage/kaduna_urban_percentage.geojson"),
  settlement_block_path = file.path(DataDir, "nigeria/building_footprints/nigeria_footprints/nigeria blocks/Nigeria_Blocks_V1.shp"),
  map_output_dir = file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs"),
  include_settlement_type = "Yes",
  include_u5_tpr_data = "Yes",
  scenarios = c(20, 30, 50, 75)
)
