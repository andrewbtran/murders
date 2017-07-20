library(tigris)


US <- core_based_statistical_areas(cb = FALSE, resolution = "500k", year = NULL)

US_fort <- fortify(US, region="GEOID")

counties_map <- counties(cb = FALSE, resolution = "500k", year = NULL)

cm_fort <- fortify(counties_map, region="GEOID")
