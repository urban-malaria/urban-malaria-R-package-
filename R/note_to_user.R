
#' @noRd
.onAttach <- function(libname, pkgname) {
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0(
      "Welcome to the Insecticide-Treated Nets Reprioritization Package!\n",
      "------------------------------------------------\n",
      "To get started, try running:\n",
      "  `results <- reprioritize(\n",
      '    state_name = "state_name",\n',
      '    shapefile_path = "path/to/state_name.shp",\n',
      '    tpr_data_path = "path/to/tpr_data_for_state_name",\n',
      '    output_dir = "path/to/output_to_save_extracted_raster_data",\n',
      '    itn_dir = "path/to/state_name_itn_data.csv",\n',
      '    raster_paths = list(
              h2o_distance_path = "path/to/h20_distance_raster.tif"),
              elevation_path = "path/to/elevation_raster.tif"),
              output_dir = "path/to/same_output_dir"),)
          )",\n',
      '    extracted_data_dir = "path/to/extracted_data.csv",\n',
      '    risk_factors = c("risk_factor1_variable_name", "risk_factor2_variable_name", "etc"),\n',
      '    urban_data_path = "path/to/state_name_urban_percentage.geojson",\n',
      '    map_output_dir = "path/to/folder_where_you_want_to_save_reprioritization_maps",\n',
      "  )`\n",
      "Remember to replace the file paths with your own data.\n",
      "Files you must provide:\n",
      "  1️⃣  State-level shapefile with a ward column called WardName and a ward code column called WardCode.\n",
      "  2️⃣  A .csv file containing under-5 malaria TPR data by ward with column names WardName and u5_tpr_rdt.\n",
      "  3️⃣  An .xlsx file containing ITN distribution data by ward with column names WardName and num_ITN.\n",
      "  4️⃣  A list of raster .tif files containing environmental raster data you would like to include in the composite score calculation.\n",
      "  5️⃣  A .geojson file containing the state's urban percentage data. If you do not have this, leave it blank and run the reprioritize()\n",
      "      function and instructions for how to get it using Google Earth Engine will appear. Then, re-run reprioritize().\n",
      "OUTPUT: This reprioritize() function will save a risk map and four reprioritization scenario maps to map_output_dir.\n",
      "For more information, see the package documentation."
    )
  )
}
