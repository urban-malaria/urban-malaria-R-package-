⸻

NetSmartR

NetSmartR: Insecticide-Treated Net Reprioritization in Nigeria

Overview

NetSmartR is an R package designed to support the reprioritization of wards in Nigerian states for malaria intervention planning. It streamlines the integration of multiple datasets—including environmental covariates, malaria test positivity data, ITN distribution data, and urban land cover estimates—to calculate composite malaria risk scores and generate prioritization maps.

Installation

Install NetSmartR from GitHub using:

install.packages("devtools")
devtools::install_github("urban-malaria/urban-malaria-R-package-")

Data Requirements

To run the analysis, you’ll need the following:
	1.	State shapefile
	      •	Must include WardName and WardCode columns.
	2.	Malaria test positivity rate (TPR) data
	      •	A .csv file with WardName and u5_tpr_rdt columns.
	3.	ITN distribution data
	      •	An .xlsx file with WardName and Population.
	4.	Raster data
	      •	.tif files of environmental covariates (e.g., rainfall, elevation, vegetation).
	5.	Urban percentage data
	      •	A .geojson file with ward-level urban land cover percentages.
	6.	Settlement block data (if including settlement type in analysis)
	      •	A shapefile (.shp) of Nigeria building footprints or blocks.

⸻

Getting Urban Percentage Data from Google Earth Engine

If your urban percentage .geojson file is missing, the package will prompt you to retrieve it. Here’s how:
	1.	Open this Google Earth Engine script:
	      🔗 https://code.earthengine.google.com/10e57f311f3cc2b81a0d247593ed4666
	2.	Upload your state shapefile under the Assets tab.
	3.	Edit the script:
      	•	Replace your_username with your Earth Engine username.
      	•	Replace your_shapefile with the name of the uploaded state shapefile:

        var shapefile = ee.FeatureCollection('projects/ee_yourname/assets/Delta_State');

	4.	Click Run to execute the script.
	5.	Export the table:
      	•	Under Tasks, click Run on the unsubmitted export task.
      	•	Save the file as GeoJSON and rename it to include the state name (e.g., delta_urban_percentage).
	6.	Download the file from your Google Drive and save it locally.
	7.	Use the full path to the .geojson file as the urban_data_path.

⸻

Getting Settlement Block Data

If you choose to include settlement type in the composite score calculation and the settlement block shapefile is missing, follow these steps:
	1.	Download the zipped shapefile from this Google Drive folder:
	      🔗 https://drive.google.com/drive/folders/1afjdVOASmx_AspHONK1Pp-IwiY5UfkIJ?usp=sharing
	2.	Save the .zip file locally.
	3.	Unzip the file.
	4.	Locate the .shp file (e.g., Nigeria_Blocks_V1.shp).
	5.	Provide the full file path to the shapefile (not the zip) in the settlement_block_path argument.
	6.	Rerun the function after saving and unzipping.

⸻

Running the Reprioritization Analysis

Once all inputs are ready, use the function below with your state name and file paths:

results <- reprioritize(
  state_name = "Niger",
  shapefile_path = file.path(StateShpDir, "Niger/Niger_State.shp"),
  tpr_data_path = file.path(DataDir, "nigeria/nigeria_hmis/TPR data for selected NMEP prioritized states/nigertpr_updated.csv"),
  itn_dir = file.path(ITNDir, "cleaned/pbi_distribution_Niger_clean.xlsx"),
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



⸻

Output

Running reprioritize() will produce:
	•	✅ Composite malaria risk scores for all wards
	•	📊 Ranked prioritization tables
	•	🗺️ Malaria risk map
	•	📍 Reprioritization maps for selected urban thresholds (e.g., 20%, 30%, 50%, 75%)

⸻

Notes
	•	Replace file paths with your own directory structure.
	•	You can toggle whether to include settlement type or under-5 TPR data.
	•	Urban threshold options may be any or all of: 20, 30, 50, 75.

⸻

License

This package is released under the MIT License.

⸻
