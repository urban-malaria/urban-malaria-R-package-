# ==========================================================================================================================================
# Script Name: Reprioritization
# Purpose: Function to create reprioritization plots for 50% and 75% urban scenarios for each Nigerian state included in the analysis.
# LGA boundaries are outlined in color. Reprioritized wards are labeled on each map.
# ==========================================================================================================================================

create_state_reprioritization_maps <- function(state_name, state_shapefile, shapefile_path, scenarios, map_output_dir) {

  # extract LGA boundaries from the state shapefile (to add orange boundaries on map)
  lga_boundaries <- state_shapefile %>%
    group_by(LGACode) %>%  # group by LGA code to merge geometries for each LGA
    summarize(geometry = sf::st_union(geometry)) %>%  # merge geometries to create LGA boundaries
    ungroup()

  # define the state-specific folder path
  state_folder <- file.path(map_output_dir, state_name)

  # read in ward prioritization data for all scenarios
  # ensure WardCode is character for all datasets
  # merge shapefile with reprioritized wards by WardCode
  # create df with just the reprioritized wards for labeling on the maps
  if (20 %in% scenarios) {
    ward_20 <- read.csv(file.path(state_folder, paste0(state_name, "_prioritized_20.csv")))
    ward_20$WardCode <- as.character(ward_20$WardCode)
    prioritized_20 <- state_shapefile %>%
      left_join(ward_20, by = c("WardCode")) %>%
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    labels_20 <- prioritized_20 %>%
      dplyr::filter(status == "Reprioritized") %>%
      st_centroid() %>%
      mutate(coords = st_coordinates(.))
  }
  if (30 %in% scenarios) {
    ward_30 <- read.csv(file.path(state_folder, paste0(state_name, "_prioritized_30.csv")))
    ward_30$WardCode <- as.character(ward_30$WardCode)
    prioritized_30 <- state_shapefile %>%
      left_join(ward_30, by = c("WardCode")) %>%
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    labels_30 <- prioritized_30 %>%
      dplyr::filter(status == "Reprioritized") %>%
      st_centroid() %>%
      mutate(coords = st_coordinates(.))
  }
  if (50 %in% scenarios) {
    ward_50 <- read.csv(file.path(state_folder, paste0(state_name, "_prioritized_50.csv")))
    ward_50$WardCode <- as.character(ward_50$WardCode)
    prioritized_50 <- state_shapefile %>%
      left_join(ward_50, by = c("WardCode")) %>%
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    labels_50 <- prioritized_50 %>%
      dplyr::filter(status == "Reprioritized") %>%
      st_centroid() %>%
      mutate(coords = st_coordinates(.))
  }
  if (75 %in% scenarios) {
    ward_75 <- read.csv(file.path(state_folder, paste0(state_name, "_prioritized_75.csv")))
    ward_75$WardCode <- as.character(ward_75$WardCode)
    prioritized_75 <- state_shapefile %>%
      left_join(ward_75, by = c("WardCode")) %>%
      mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
    labels_75 <- prioritized_75 %>%
      dplyr::filter(status == "Reprioritized") %>%
      st_centroid() %>%
      mutate(coords = st_coordinates(.))
  }

  state_shapefile$WardCode <- as.character(state_shapefile$WardCode)


  # ------------------------------------------------------------------------------------------------------------------------------#
  # ADD LGA NAMES TO DATA

  lga_shp <- tryCatch({
    shapefile <- sf::st_read(
      dsn = file.path(shapefile_path, "LGAs/NGA_LGAs.shp"),
      quiet = TRUE
    )
    sf::st_make_valid(shapefile)
  }, error = function(e) {
    stop("Failed to load shapefile: ", e$message)
  })

  # check and match crs
  if (st_crs(state_shapefile) != st_crs(lga_shp)) {
    lga_shp <- st_transform(lga_shp, st_crs(state_shapefile))
  }

  # make geometries valid
  lga_shp <- st_make_valid(lga_shp)
  state_shapefile <- st_make_valid(state_shapefile)

  # spatial join to attach the state name to each lga
  lga_with_state <- st_join(lga_shp, state_shapefile, left = FALSE)  # left = FALSE keeps only LGAs that intersect a state

  # re-filter to only include state of interest, filter to only ward name and LGA name
  lga_wards_names <- lga_with_state %>%
    dplyr::filter(State == state_name) %>%
    dplyr::select(State, LGA, WardName)

  # Remove the geometry column and convert to a regular data frame
  lga_wards_names_df <- lga_wards_names %>%
    st_drop_geometry() %>%
    dplyr::select(State, LGA, WardName)

  lga_color <- "orange"

  # add LGA names to dfs of reprioritized wards
  # create plots
  if (20 %in% scenarios) {
    labels_20 <- labels_20 %>%
      left_join(lga_wards_names_df, by = c("WardName"))
    # remove unnecessary columns
    labels_20 <- labels_20 %>%
      select(
        StateCode, WardCode, WardName, LGACode, Urban, Source, Timestamp, GlobalID,
        AMAPCODE, LGA.x, SelectedWards, WardPopulation, WardPercentage,
        CumulativePopulation, CumulativePercentage, status, coords, State, geometry
      )
    # remove exact duplicate rows
    labels_20 <- labels_20 %>% distinct()
    labels_20$WardName <- gsub("\\s*\\([^\\)]+\\)", "", labels_20$WardName)

    plot_20 <- ggplot(data = prioritized_20) +
      geom_sf(aes(geometry = geometry, fill = status)) +
      geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
      geom_text_repel(
        data = labels_20,  # Now contains only the reprioritized wards
        aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA.x, ")")),
        color = "black",
        size = 3,  # Adjust text size
        box.padding = 0.5,  # Space around labels
        segment.color = "black",  # Black line from label to ward
        segment.size = 0.5,
        max.overlaps = Inf,  # Ensure all labels appear
        force = 10  # Helps spread labels further
      ) +
      scale_fill_manual(
        values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
        guide = guide_legend(title = NULL)
      ) +
      labs(
        title = "Reprioritization Scenario 1:",
        subtitle = paste0("at least 20% of ward is urban\n", nrow(labels_20), " wards reprioritized")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      map_theme()
  }
  if (30 %in% scenarios) {
    labels_30 <- labels_30 %>%
      left_join(lga_wards_names_df, by = c("WardName"))
    # remove unnecessary columns
    labels_30 <- labels_30 %>%
      select(
        StateCode, WardCode, WardName, LGACode, Urban, Source, Timestamp, GlobalID,
        AMAPCODE, LGA.x, SelectedWards, WardPopulation, WardPercentage,
        CumulativePopulation, CumulativePercentage, status, coords, State, geometry
      )
    # remove exact duplicate rows
    labels_30 <- labels_30 %>% distinct()
    labels_30$WardName <- gsub("\\s*\\([^\\)]+\\)", "", labels_30$WardName)

    plot_30 <- ggplot(data = prioritized_30) +
      geom_sf(aes(geometry = geometry, fill = status)) +
      geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
      geom_text_repel(
        data = labels_30,  # Now contains only the reprioritized wards
        aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA.x, ")")),
        color = "black",
        size = 3,  # Adjust text size
        box.padding = 0.5,  # Space around labels
        segment.color = "black",  # Black line from label to ward
        segment.size = 0.5,
        max.overlaps = Inf,  # Ensure all labels appear
        force = 10  # Helps spread labels further
      ) +
      scale_fill_manual(
        values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
        guide = guide_legend(title = NULL)
      ) +
      labs(
        title = "Reprioritization Scenario 2:",
        subtitle = paste0("at least 30% of ward is urban\n", nrow(labels_30), " wards reprioritized")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      map_theme()
  }
  if (50 %in% scenarios) {
    labels_50 <- labels_50 %>%
      left_join(lga_wards_names_df, by = c("WardName"))
    # remove unnecessary columns
    labels_50 <- labels_50 %>%
      select(
        StateCode, WardCode, WardName, LGACode, Urban, Source, Timestamp, GlobalID,
        AMAPCODE, LGA.x, SelectedWards, WardPopulation, WardPercentage,
        CumulativePopulation, CumulativePercentage, status, coords, State, geometry
      )
    # remove exact duplicate rows
    labels_50 <- labels_50 %>% distinct()
    labels_50$WardName <- gsub("\\s*\\([^\\)]+\\)", "", labels_50$WardName)

    plot_50 <- ggplot(data = prioritized_50) +
      geom_sf(aes(geometry = geometry, fill = status)) +
      geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
      geom_text_repel(
        data = labels_50,  # Now contains only the reprioritized wards
        aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA.x, ")")),
        color = "black",
        size = 3,  # Adjust text size
        box.padding = 0.5,  # Space around labels
        segment.color = "black",  # Black line from label to ward
        segment.size = 0.5,
        max.overlaps = Inf,  # Ensure all labels appear
        force = 10  # Helps spread labels further
      ) +
      scale_fill_manual(
        values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
        guide = guide_legend(title = NULL)
      ) +
      labs(
        title = "Reprioritization Scenario 3:",
        subtitle = paste0("at least 50% of ward is urban\n", nrow(labels_50), " wards reprioritized")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      map_theme()
  }
  if (75 %in% scenarios) {
    labels_75 <- labels_75 %>%
      left_join(lga_wards_names_df, by = c("WardName"))
    # remove unnecessary columns
    labels_75 <- labels_75 %>%
      select(
        StateCode, WardCode, WardName, LGACode, Urban, Source, Timestamp, GlobalID,
        AMAPCODE, LGA.x, SelectedWards, WardPopulation, WardPercentage,
        CumulativePopulation, CumulativePercentage, status, coords, State, geometry
      )
    # remove exact duplicate rows
    labels_75 <- labels_75 %>% distinct()
    labels_75$WardName <- gsub("\\s*\\([^\\)]+\\)", "", labels_75$WardName)

    plot_75 <- ggplot(data = prioritized_75) +
      geom_sf(aes(geometry = geometry, fill = status)) +
      geom_sf(data = lga_boundaries, aes(geometry = geometry), color = lga_color, fill = NA, linewidth = 0.6) +
      geom_text_repel(
        data = labels_75,  # Now contains only the reprioritized wards
        aes(x = coords[,1], y = coords[,2], label = paste0(WardName, " (", LGA.x, ")")),
        color = "black",
        size = 3,  # Adjust text size
        box.padding = 0.5,  # Space around labels
        segment.color = "black",  # Black line from label to ward
        segment.size = 0.5,
        max.overlaps = Inf,  # Ensure all labels appear
        force = 10  # Helps spread labels further
      ) +
      scale_fill_manual(
        values = c("Not Reprioritized" = "#F5F6F7", "Reprioritized" = "#22AAE2"),
        guide = guide_legend(title = NULL)
      ) +
      labs(
        title = "Reprioritization Scenario 4:",
        subtitle = paste0("at least 75% of ward is urban\n", nrow(labels_75), " wards reprioritized")
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      map_theme()
  }

  # ------------------------------------------------------------------------------------------------------------------------------#

  # save each reprioritization scenario plot individually if it exists

  # make sure output folder exists
  if (!dir.exists(state_folder)) dir.create(state_folder, recursive = TRUE)

  # save plot_20 if it exists
  if (exists("plot_20")) {
    ggsave(
      filename = file.path(state_folder, paste0(Sys.Date(), "_", state_name, "_reprioritization_plot_20.pdf")),
      plot = plot_20,
      width = 12, height = 12
    )
  }

  # save plot_30 if it exists
  if (exists("plot_30")) {
    ggsave(
      filename = file.path(state_folder, paste0(Sys.Date(), "_", state_name, "_reprioritization_plot_30.pdf")),
      plot = plot_30,
      width = 12, height = 12
    )
  }

  # save plot_50 if it exists
  if (exists("plot_50")) {
    ggsave(
      filename = file.path(state_folder, paste0(Sys.Date(), "_", state_name, "_reprioritization_plot_50.pdf")),
      plot = plot_50,
      width = 12, height = 12
    )
  }

  # save plot_75 if it exists
  if (exists("plot_75")) {
    ggsave(
      filename = file.path(state_folder, paste0(Sys.Date(), "_", state_name, "_reprioritization_plot_75.pdf")),
      plot = plot_75,
      width = 12, height = 12
    )
  }

}
