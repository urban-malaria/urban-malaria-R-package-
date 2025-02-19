library(testthat)
library(NetSmartR)
library(sf)

test_that("extract_raster_data returns expected structure", {

  # mock input data
  raster_paths <- list(
    h2o_distance_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/distance_to_water_bodies/distance_to_water.tif",
    elevation_path = "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/Elevation/ELE.tif",
    output_dir = file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/NetSmartR/extractions")
  )

  extracted_data <- extract_raster_data("Kano", "/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/nigeria_shapefiles/shapefiles/ShinyApp_shapefiles/all_reprioritization_nmep_states/STATES/Kano/Kano_State.shp", raster_paths)

  # check output is a data frame
  expect_s3_class(extracted_data, "data.frame")

  # check necessary columns exist
  expect_true(all(c("WardName", "distance_to_water", "elevation") %in% colnames(extracted_data)))

  # check there are no missing values in key columns
  expect_false(any(is.na(extracted_data$distance_to_water)))
})
