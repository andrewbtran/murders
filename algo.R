li %>% %>% brary(foreign)
library(tidyverse)
#library(maps)
library(tigris)

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
