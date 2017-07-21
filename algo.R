library(foreign)
library(tidyverse)
#library(maps)
library(tigris)
library(leaflet)

#library(stringr)

data <- read.spss("data/SHR1976_2015.sav", to.data.frame=TRUE)
data2 <- read.spss("data/SHR1976_2015.sav", to.data.frame=TRUE, use.value.labels=F)

data(fips_codes)
county_names <- fips_codes
county_names$fips <- as.numeric(paste0(county_names$state_code, county_names$county_code))
county_names$county_name <- gsub(" County.*", "", county_names$county)
county_names$county_name <- paste0(county_names$county_name, ", ", county_names$state)
county_names <- select(county_names, fips, county_name)

#write.csv(data, "data/raw_data1.csv")
#write.csv(data2, "data/raw_data2.csv")

#data <- read_csv("data/raw_data1.csv")
#data2 <- read_csv("data/raw_data2.csv")

data <- select(data,
               ID, CNTYFIPS, Ori, State, Agency, AGENCY_A, 
               Agentype_label=Agentype,
               Source_label=Source,
               Solved_label=Solved,
               Year,
               Month_label=Month,
               Incident, ActionType,
               Homicide_label=Homicide,
               Situation_label=Situation,
               VicAge,
               VicSex_label=VicSex,
               VicRace_label=VicRace,
               VicEthnic, OffAge,
               OffSex_label=OffSex,
               OffRace_label=OffRace,
               OffEthnic,
               Weapon_label=Weapon,
               Relationship_label=Relationship,
               Circumstance_label=Circumstance,
               Subcircum, VicCount, OffCount, FileDate, 
               fstate_label=fstate,
               MSA_label=MSA,
               StateName2)

data2 <- select(data2,
               Agentype_value=Agentype,
               Source_value=Source,
               Solved_value=Solved,
               Month_value=Month,
               Homicide_value=Homicide,
               Situation_value=Situation,
               VicSex_value=VicSex,
               VicRace_value=VicRace,
               OffSex_value=OffSex,
               OffRace_value=OffRace,
               Weapon_value=Weapon,
               Relationship_value=Relationship,
               Circumstance_value=Circumstance,
               fstate_value=fstate,
               MSA_value=MSA)

data <- cbind(data, data2)

data <-  select(data,
                ID, CNTYFIPS, Ori, State, Agency, AGENCY_A, 
                Agentype_label, Agentype_value,
                Source_label, Source_value,
                Solved_label, Solved_value,
                Year,
                Month_label, Month_value,
                Incident, ActionType,
                Homicide_label,Homicide_value,
                Situation_label,Situation_value,
                VicAge,
                VicSex_label,VicSex_value,
                VicRace_label,VicRace_value,
                VicEthnic, OffAge,
                OffSex_label,OffSex_value,
                OffRace_label,OffRace_value,
                OffEthnic,
                Weapon_label,Weapon_value,
                Relationship_label,Relationship_value,
                Circumstance_label,Circumstance_value,
                Subcircum, VicCount, OffCount, FileDate, 
                fstate_label,fstate_value,
                MSA_label,MSA_value,
                StateName2)

rm(data2)

data$CNTYFIPS <- as.character(data$CNTYFIPS)
data$CNTYFIPS <- ifelse(data$CNTYFIPS=="51560", "51005", data$CNTYFIPS)
data$CNTYFIPS <- ifelse(data$CNTYFIPS=="02232", "02105", data$CNTYFIPS)
data$CNTYFIPS <- ifelse(data$CNTYFIPS=="02280", "02195", data$CNTYFIPS)
data$CNTYFIPS <- ifelse(data$CNTYFIPS=="02201", "02198", data$CNTYFIPS)
data$CNTYFIPS <- ifelse(data$CNTYFIPS=="02201", "02198", data$CNTYFIPS)


data$fips <- as.numeric(data$CNTYFIPS)
data <- left_join(data, county_names)

data <- mutate(data, 
               solved_num = ifelse(Solved_label=="Yes", 1, 0), 
               agegroup_label = ifelse(VicAge %in% 0:14, "0-14", 
                                      ifelse(VicAge %in% 15:19, "15-19",
                                      ifelse(VicAge %in% 20:50, "20-50",
                                      ifelse(VicAge %in% 51:99, "Over 50", "Unknown")))),
               agegroup_value = ifelse(VicAge %in% 0:14, 1, 
                               ifelse(VicAge %in% 15:19, 2,
                                      ifelse(VicAge %in% 20:50, 3,
                                             ifelse(VicAge %in% 51:99,  4, 9)))),
               sex = ifelse(VicSex_label=="Male", 1,
                          ifelse(VicSex_label=="Female", 2, 9)),
               murdgrp_msa=(MSA_value*10000)+(sex*1000)+(agegroup_value*100)+Weapon_value,
               murdgrp_cnty=(as.numeric(as.character(CNTYFIPS))*10000)+(sex*1000)+(agegroup_value*100)+Weapon_value)
               
murdergroup_msa <- data %>%
  group_by(murdgrp_msa, agegroup_label, VicSex_label, MSA_label, Weapon_label) %>%
  summarize(total=n(), solved=sum(Solved_value)) %>%
  mutate(percent=round(solved/total*100,2))

murdergroup_msa2 <- data %>%
  group_by(murdgrp_msa, MSA_value, agegroup_label, VicSex_label, MSA_label, Weapon_label) %>%
  summarize(total=n(), solved=sum(Solved_value)) %>%
  mutate(percent=round(solved/total*100,2))

murdergroup_cnty <- data %>%
  group_by(murdgrp_cnty, agegroup_label, VicSex_label, county_name, Weapon_label) %>%
  summarize(total=n(), solved=sum(Solved_value)) %>%
  mutate(percent=round(solved/total*100,2))

spots_cnty <- murdergroup_cnty %>% 
  filter(VicSex_label=="Female", murdgrp_cnty>0, percent <=33) %>% 
  mutate(unsolved=total-solved) %>% 
  arrange(desc(unsolved))
  
spots_msa <- murdergroup_msa %>% 
  filter(VicSex_label=="Female", murdgrp_msa>0, percent <=33) %>% 
  mutate(unsolved=total-solved) %>% 
  arrange(desc(unsolved))

spots_cnty <- murdergroup_cnty %>% 
  filter(VicSex_label=="Female", murdgrp_cnty>0, percent <=33) %>% 
  mutate(unsolved=total-solved) %>% 
  arrange(desc(unsolved))

spots_cnty_2006 <- data %>%
  group_by(murdgrp_cnty, agegroup_label, VicSex_label, county_name, Weapon_label, CNTYFIPS) %>%
  filter(Year>=2006) %>% 
  summarize(total=n(), solved=sum(Solved_value)) %>%
  mutate(percent=round(solved/total*100,2)) %>%
  filter(VicSex_label=="Female", murdgrp_cnty>0, percent <=33) %>% 
  mutate(unsolved=total-solved) %>% 
  filter(unsolved!=1) %>% 
  arrange(desc(unsolved))


#counties_map <- counties(cb = FALSE, resolution = "500k", year = NULL)
counties_map <- counties(cb = T,  year = NULL)

counties_map <- subset(counties_map, STATEFP!="78" & STATEFP!="72" & STATEFP!="66" & STATEFP!="60" & STATEFP!="69" & STATEFP!="02" & STATEFP!="15")
#states_map <- states(cb=F, resolution="500k", year=NULL)
states_map <- states(cb=T, year=NULL)

#cm_fort <- fortify(counties_map, region="GEOID")

#colnames(spots_cnty_2006)[colnames(spots_cnty_2006) == 'CNTYFIPS'] <- 'id'
#spots_cnty_2006$id <- str_trim(spots_cnty_2006$id)
#cm_fort_spots <- left_join(cm_fort, spots_cnty_2006)
#cm_fort_spots <- filter(cm_fort_spots, !is.na(unsolved))

#cm_fort_spots_2050 <- filter(cm_fort_spots, agegroup_label=="20-50")
#cm_fort_spots_2050_s <- filter(cm_fort_spots_2050, Weapon_label=="Strangulation - hanging")

age_groups <- unique(cm_fort_spots$agegroup_label)
weapons <- unique(cm_fort_spots$Weapon_label)

cm_sub <- filter(spots_cnty_2006, agegroup_label==age_groups[i], Weapon_label==weapons[x])

counties_merged <- geo_join(counties_map, cm_sub, "GEOID", "id")
counties_merged <- subset(counties_merged, !is.na(unsolved))

pal_sb <- colorNumeric("Greens", domain=counties_merged$unsolved)

popup_sb <- paste0("Unsolved: ", as.character(counties_merged$unsolved))


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = counties_map,
              color ="#444444",
              fillColor = "transparent", 
              fillOpacity = 0.9, 
              weight = 0.2, 
              smoothFactor = 0.5) %>% 
  addPolygons(data = states_map,
              color ="#444444",
              fillColor = "transparent", 
              fillOpacity = 0.9, 
              weight = 0.5, 
              smoothFactor = 0.5) %>% 
  addPolygons(data = counties_merged, 
              fillColor = ~pal_sb(counties_merged$unsolved), 
              fillOpacity = 0.9, 
              weight = 0.4, 
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label=popup_sb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_sb, 
            values = counties_merged$unsolved, 
            position = "bottomright", 
            title = "Unsolved")

counties_agg <- data %>%
  group_by(murdgrp_cnty, agegroup_label, VicSex_label, county_name, Weapon_label, CNTYFIPS) %>%
  filter(Year>=2006) %>% 
  summarize(total=n(), solved=sum(Solved_value)) %>%
  mutate(percent=round(solved/total*100,2)) %>%
  filter(VicSex_label=="Female", murdgrp_cnty>0, percent <=33) %>% 
  mutate(unsolved=total-solved) %>% 
  filter(unsolved!=1) %>% 
  arrange(desc(unsolved)) %>% 
  group_by(county_name, CNTYFIPS) %>% 
  summarize(serial=n()) %>%
  arrange(desc(as.character(CNTYFIPS)))

#unique_2006 <- unique(spots_cnty_2006$id)
unique_2006 <- unique(spots_cnty_2006$CNTYFIPS)

for (i in 1:length(unique_2006)) {
  #filtered <- filter(spots_cnty_2006, id==unique_2006[i])
  filtered <- filter(spots_cnty_2006, CNTYFIPS==unique_2006[i])
  
  for (x in 1:nrow(filtered)) {
    if (x==1) {
      popuptxt <- paste0("<strong>", filtered$county_name[x], "</strong><br />", filtered$agegroup_label[x]," | ", filtered$Weapon_label[x], ": ", filtered$unsolved[x])
    } else {
      popuptxt <- paste0(popuptxt, "<br />",  filtered$agegroup_label[x]," | ", filtered$Weapon_label[x], ": ", filtered$unsolved[x])
    }
  }
  the_join <- data.frame(id=unique_2006[i], popuptxt)
  if (i == 1) {
    for_join <- the_join
  } else {
    for_join <- rbind(for_join, the_join)
  }
}

for_join$id <- as.character(for_join$id)
for_join <- arrange(for_join, desc(id))
counties_agg2 <- data.frame(counties_agg)
counties_agg2 <- left_join(counties_agg2, for_join, by=c("CNTYFIPS"="id"))

counties_agg2$CNTYFIPS <- str_trim(counties_agg2$CNTYFIPS)

counties_merged_map <- geo_join(counties_map, counties_agg2, "GEOID", "CNTYFIPS")

counties_merged_map <- subset(counties_merged_map, !is.na(serial))

pal_sb <- colorNumeric("PuRd", domain=counties_merged_map$serial)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  # addPolygons(data = counties_map,
  #             color ="#444444",
  #             fillColor = "transparent", 
  #             fillOpacity = 0.9, 
  #             weight = 0.2, 
  #             smoothFactor = 0.5) %>% 
  addPolygons(data = states_map,
              color ="#444444",
              fillColor = "transparent", 
              fillOpacity = 0.9, 
              weight = 0.5, 
              smoothFactor = 0.5) %>% 
  addPolygons(data = counties_merged_map, 
              fillColor = ~pal_sb(counties_merged_map$serial), 
              fillOpacity = 0.9, 
              weight = 1, 
              smoothFactor = 0.2,
              popup=~popuptxt,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  # addPolygons(data = counties_merged_map, 
  #             fillColor = ~pal_sb(counties_merged_map$serial), 
  #             fillOpacity = 0.9, 
  #             weight = 1, 
  #             smoothFactor = 0.2,
  #             highlight = highlightOptions(
  #               weight = 5,
  #               color = "#666",
  #               dashArray = "",
  #               fillOpacity = 0.7,
  #               bringToFront = TRUE),
  #             label=counties_merged_map$popuptxt,
  #             labelOptions = labelOptions(
  #               style = list("font-weight" = "normal", padding = "3px 8px"),
  #               textsize = "15px",
  #               direction = "auto")) %>%
  addLegend(pal = pal_sb, 
            values = counties_merged_map$serial, 
            position = "bottomright", 
            title = "Unsolved clusters")

###

file_size <- file.info("data/data.csv")

