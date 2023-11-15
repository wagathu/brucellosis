#set working directory

setwd("~/Library/CloudStorage/Dropbox/phd/Revised_descriptive_7.11.23/Revised_descriptive_brucellosis")

#Load required packages
pacman::p_load(tidyverse,lubridate, ggplot, tidyr)

## cleaning the human health data county
human_zoonoses <- read_csv("Zoonosis_2014_2021_county.csv") %>%
  janitor::clean_names() %>%
  mutate(organisationunitname = str_remove(organisationunitname, "County")) %>%
  mutate(organisationunitname = trimws(organisationunitname)) %>%
  rename(county = organisationunitname) %>%
  pivot_longer(
    cols = c(
      "brucellosis",
      "moh_705a_rev_2020_rift_valley_fever",
      "moh_705a_rev_2020_suspected_anthrax",
      "moh_706_brucella"
    ),
    names_to = "diseases",
    values_to = "disease_values"
  ) %>%
  mutate(
    diseases = recode(
      diseases,
      "moh_705a_rev_2020_rift_valley_fever" = "Rvf",
      "moh_705a_rev_2020_suspected_anthrax" = "Anthrax",
      "moh_706_brucella" = "Brucella"
    )
  ) %>%
  mutate(diagnosis = ifelse(
    diseases %in% "Brucella",
    "Lab confirmed",
    "Clinically confirmed"
  )) %>%
  dplyr::select(periodname, county, diseases, disease_values, diagnosis) %>%
  mutate(date = as.Date(paste0("01", "-", periodname), "%d-%b-%y")) %>%
  filter(!date < "2014-06-01") %>%
  filter(!is.na(disease_values)) %>%
  dplyr::select(date, county, diseases, disease_values, diagnosis) %>%
  mutate(type = "Human")

##human rabies data
human_rabies <- read_csv("human_rabies 2014-2021.csv")%>%
  janitor::clean_names()%>%
  mutate(organisationunitname=str_remove(organisationunitname,"County"))%>%
  mutate(organisationunitname=trimws(organisationunitname))%>%
  rename(county=organisationunitname,disease_values=rabies)%>%
  mutate(diagnosis="Clinically confirmed")%>%
  mutate(diseases="Rabies")%>%
  dplyr::select(periodname, county, diseases, disease_values, diagnosis)%>%
  mutate(date=as.Date(paste0("01","-",periodname), "%d-%b-%y"))%>%
  filter(!date<"2014-06-01")%>%
  filter(!is.na(disease_values))%>%
  dplyr::select(date, county, diseases, disease_values, diagnosis)%>%
  mutate(type="Human")

##combine human zoonoses data and human rabies data
human_zoonoses <- human_zoonoses%>%
  rbind(human_rabies)%>%
 rename(disease_value=disease_values, species=type)%>%
 dplyr::select(date, county, diseases, diagnosis, species, disease_value)

# cleaning the animal health data
Animaldata<- read_csv("animal_data1.csv")%>%
  janitor::clean_names()%>%
  rename(year_1=year_10)%>%
  dplyr::select(county, sub_county, year_1, month_2, disease_condition, nature_of_diagnosis_clinical_lab_pm, species, no_sick)

# Adding missing animal data
Animaldata_21<- read_csv("july-dec_animal.xlsx.csv")%>%
  janitor::clean_names()%>%
  rename(nature_of_diagnosis_clinical_lab_pm=nature_of_diagnosis,species=species_affected,year_1= year_10)%>%
  dplyr::select(county, sub_county, year_1, month_2,disease_condition, nature_of_diagnosis_clinical_lab_pm, species, no_sick)

animal_zoonoses_comb <- Animaldata%>%
  rbind(Animaldata_21)

animal_zoonoses <- animal_zoonoses_comb%>%
  mutate(disease_condition=trimws(str_to_title(disease_condition)))%>%
  mutate(disease_condition=ifelse(grepl("Rabies|Rbies", disease_condition), "Rabies", 
                                  ifelse(grepl("Brucell", disease_condition), "Brucellosis", 
                                         ifelse(grepl("Rvf", disease_condition), "Rvf", disease_condition))))%>%
  mutate(county=str_to_title(county))%>%
  #mutate(county=trimws(county))%>%
  mutate(county=recode(county, "Elgeiyo Marakwet"="Elgeyo Marakwet", "Elgeyo/Marakwet"="Elgeyo Marakwet", "Kaimbu"="Kiambu",
                       "Karatina"="Nyeri", "Murang?"="Murang'a", "Mwingi"="Kitui", "Sotik"="Bomet", "Tharaka-Nithi"="Tharaka Nithi",
                       "Trans-Nzoia"="Trans Nzoia", "Pokot"="West Pokot"))%>%
  mutate(county=ifelse(county%in%"", NA, county))%>%
  mutate(diagnosis=str_to_title(nature_of_diagnosis_clinical_lab_pm))%>%
  mutate(year_1=ifelse(year_1%in%c("ND", "","201"), year,  year_1))%>%
  mutate(date=as.Date(paste0("01","-", month_2, "-",year_1),"%d-%B-%Y" ))%>%
  mutate(diagnosis=ifelse(diagnosis%in%c("Clinical Signs/Lab", "Lab", "Lab Diagnosis", "Laboratory", "Pm &Lb"),"Lab confirmed", 
                          ifelse(diagnosis%in%c("Pm", "Postmortem", "Post Mortem"), "Post Mortem", "Clinically confirmed") ))%>%
  rename("diseases"=disease_condition, "disease_values"= no_sick)%>%
  mutate(species=str_to_title(trimws(species)))%>%
  mutate(species=ifelse(species%in%c("Bobine"), "Bovine",
                        ifelse(species%in%"Bufallo","Buffalo",
                               ifelse(species%in%c("Cani", "Canine(Jackal)", "Cannine", "Dog","Dogs"), "Canine", 
                                      ifelse(species%in%c("Goat"), "Goats", ifelse(species%in%c("", "Nd"), NA, species))))))%>%
  mutate(species=recode(species, "Bovine & Canine"="Canine", "Caprine/Ovine"="Caprine",
                        "Caprine+Donkey"="Caprine", "Dogs/Donkey"="Dogs", "Canine, Feline"="Dogs", "Dogs, Cats"="Dogs"))%>%
  mutate(species=trimws(species))%>%
  mutate(species=recode(species, "Camelidae"="Camel", "Goats"="Caprine", "Cats"="Feline", "Dogs"="Canine", 
                        "Sheep"="Ovine", "Pig"="Porcine", "Donkey"="Equine", "Cattle"="Bovine", "Pigs"="Porcine",
                        "White Tailed Mongoose"="Mongoose"))%>%
  dplyr::select(date, county, diseases,disease_values,diagnosis,species)%>%
  distinct()%>%
  group_by(date, county, diseases, diagnosis, species) %>%
  summarize(disease_value = sum(disease_values)) %>%
  ungroup()

# combine human and animal zoonoses data
all_zoonoses<- rbind(human_zoonoses, animal_zoonoses)%>%
  mutate(diseases=ifelse(grepl("Brucella|brucellosis", diseases), "Brucellosis", diseases))%>%
  mutate(county=ifelse(grepl("Murang\xe1|Muranga", county), "Murang'a", county))%>%
  mutate(species=recode(species, "Camel"="Camels", "Bovine"="cattle", "Equine"="Donkeys", "Caprine"="Goats","Porcine"="Pigs","Ovine"="Sheep"))

write_csv(all_zoonoses, "all_zoonoses_long.csv")
############################################################################

# Filtering out brucellosis
all_bruc<- all_zoonoses%>%
  filter( diseases %in% "Brucellosis")%>%
  filter(species %in% c("Camels", "cattle", "Goats","Human" , "Sheep"))%>%
  pivot_wider(names_from = species, values_from = disease_value)
  
# Import animal population data
anipop <- read_csv("animal_pop_2019.csv")%>%
  janitor::clean_names()%>%
  filter(species %in% c("Camels", "cattle", "Goats", "Sheep"))

# Merge with zoonoses data
all_bruc1 <- full_join(all_bruc, anipop, by = "county")%>%
  rename(hum_cases=Human, catt_cases=cattle,cam_cases=Camels, goat_cases=Goats, shp_cases=Sheep)%>%
  pivot_wider(names_from = species, values_from = species_num)%>%
  janitor::clean_names()%>%
  rename(sheep_pop=sheep, goat_pop=goats, cam_pop=camels, catt_pop=cattle)%>%
  mutate(y = year(date))
 
write_csv(all_bruc1, "all_bruc_pop.csv") 
########################################################################
# Import human population data
hum_pop <- read_csv("hum_pop_14.21.csv")

#merging the data with human population data
all_bruc2 <- all_bruc1%>%
  left_join(hum_pop, by = c("county", "y"))

##compute incidence for humans
all_bruc_inc <- all_bruc2 %>%
  mutate(
    hum_incidence = hum_cases / hum_pop * 1000,
    catt_incidence = catt_cases / catt_pop * 1000000,
    cam_incidence = cam_cases / cam_pop * 1000000,
    goat_incidence = goat_cases / goat_pop * 1000000,
    shp_incidence = shp_cases / sheep_pop * 1000000
  )

all_bruc_inci <- all_bruc_inc %>%
  mutate_if(is.numeric, list(~round(., 2)))

write_csv(all_bruc_inci, "all_bruc_incidence.csv")
#############################################################
total_bruc<- all_bruc2%>%
  group_by(county) %>%
  summarize(
    total_hum_cases = sum(hum_cases, na.rm = TRUE),
    total_catt_cases = sum(catt_cases, na.rm = TRUE),
    total_cam_cases = sum(cam_cases, na.rm = TRUE),
    total_goat_cases = sum(goat_cases, na.rm = TRUE),
    total_shp_cases = sum(shp_cases, na.rm = TRUE),
    total_sheep_pop = sum(sheep_pop, na.rm = TRUE),
    total_goat_pop = sum(goat_pop, na.rm = TRUE)
  )

############################################################################
# selecting only relevant data
bruc_inc_data <- all_bruc_inci%>%
  dplyr::select(date, y, county, diagnosis,hum_incidence, catt_incidence, cam_incidence, goat_incidence, shp_incidence)%>%
  pivot_longer(
    cols = starts_with (c("hum_incidence", "catt_incidence", "cam_incidence", "goat_incidence", "shp_incidence"))  # Replace with your actual species columns
    names_to = "species",
    values_to = "incidence"
  )

library(ggplot2)

# Specify the species you want to create line plots for
species_to_plot <- c("hum_incidence", "catt_incidence", "cam_incidence", "goat_incidence", "shp_incidence")

# Filter the data for the selected species
#filtered_data <- bruc_inc_data %>%
 # select(date, year, county, diagnosis, all_of(species_to_plot))

# Melt the data to long format for plotting
bruc_inc_data_long <- bruc_inc_data%>%
  pivot_longer(filtered_data, cols = c("hum_incidence", "catt_incidence", "cam_incidence", "goat_incidence", "shp_incidence"), names_to = "species", values_to = "incidence")

# Create line plots for each species
line_plots <- ggplot(melted_data, aes(x = date, y = incidence, color = species)) +
  geom_line() +
  labs(
    title = "Incidence Over Time by Species",
    x = "Date",
    y = "Incidence"
  )

# Print the line plots
print(line_plots)



#Time series
pacman::p_load(tidyverse,tsibble, fable)

ts_spp_bruc <- as_tsibble(all_bruc_inci, validate=T, index = date, key=hum_incidence)


hum_ani_plot <-ggplot(brucellosis_data, aes(x=date, y=incidence_human
))+geom_line()+
  geom_line(aes(y=incidence_animals), color="red")

hum_ani_plot

ggsave("hum_ani_plot.png", width=16, height=9)

#Plotting
human_inc_plot <- ggplot(all_bruc_inci, aes(x=y, y=hum_incidence)) +
  geom_col(fill = "#74add1") +
  theme(text=element_text(size=18))+
  labs(x="Livelihood Zones", y= "Human brucellosis incidence (annual Avg)")

print(human_inc_plot)
ggsave("humanincidences_ecozone_plot.png", human_inc_plot, width = 16, height = 10)

# Line plots
human_inc_trend <- ggplot(all_bruc_inci, aes(x=y, y=hum_incidence)) +
  geom_smooth() +
  geom_point()+
  theme(text=element_text(size=18))+
  labs(x="Livelihood Zones", y= "Human brucellosis incidence (annual Avg)")

print(human_inc_trend)
ggsave("humanincidences_ecozone_plot.png", human_inc_plot, width = 16, height = 10)

# List of species columns to plot
species_columns <- c("hum_incidence", "catt_incidence", "cam_incidence", "goat_incidence", "shp_incidence")

# Initialize an empty list to store plots
bruc_inc_plots <- list()

# Iterate through each species column and create a plot
for (species_col in species_columns) {
  plot <- ggplot(all_bruc_inci, aes(x = date, y = .data[[species_col]], color = species)) +
    geom_line() +
    labs(
      title = paste("Incidence for", species_col),
      x = "y",
      y = "Incidence g"
    )

hum_inc_plot<- ggplot(all_bruc_inci, aes(x = date, y = hum_incidence, color = species)) +
  geom_line() +
  labs(
    title = "Incidence by Species",
    x = "Date",
    y = "Incidence"
  )
hum_inc_plot

ggplot(all_bruc_inci, aes(x=date, y=all, fill=diseases))+geom_col()+facet_grid(type~diagnosis, scales = "free_y")+theme_bw()+
  scale_fill_brewer(palette = "Set1")+labs(x="Period",y="Frequency", fill="Disease")+theme(text=element_text(size=18))+
  scale_x_date(date_breaks = "24 months", date_labels = "%Y")

ggplot(all_bruc_inci, aes(x = date, y = hum_incidence, color = species)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Smooth Line Graph of Incidence per Species", x = "Date", y = "Incidence") +
  theme_minimal()

ggplot(all_bruc_inci, aes(x = date, y = catt_incidence)) +
  geom_smooth() +
  labs(title = "Smooth Line Graph of Incidence per Species", x = "Date", y = "Incidence") +
  theme_minimal()

###############################################

################################################
#ggsave("zoonotic_disease.png", width=16)
##########################

library(sf) ##read shapefiles

## read Kenya shapefile
county<-st_read("County.shp")%>%
  mutate(COUNTY=recode(COUNTY, "Keiyo-Marakwet"="Elgeyo Marakwet", "Tharaka"="Tharaka Nithi"))

##create anthrax population files for mapping (animals and human)
county_anthrax1<- full_join(county, all_zoonoses1b[all_zoonoses1b$diseases%in%"Anthrax" & all_zoonoses1b$type%in%"Human",], by=c("COUNTY"="county"))
county_anthrax2<- full_join(county, all_zoonoses1a[all_zoonoses1a$diseases%in%"Anthrax" & all_zoonoses1a$type%in%"Animal",], by=c("COUNTY"="county"))
county_anthrax1$type<- ifelse(is.na(county_anthrax1$type), "Human", county_anthrax1$type)
county_anthrax2$type<- ifelse(is.na(county_anthrax2$type), "Animal", county_anthrax2$type)

#county_anthrax<- rbind(county_anthrax1, county_anthrax2)

##mapping human anthrax
county_anthrax1$diseases<-"Anthrax"
anthrax_h<-ggplot(county_anthrax1, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(~diseases)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 people")+theme(text=element_text(size=16))

## Brucellosis

##join with shapefile data for mapping
county_brucellosis1<- full_join(county, all_zoonoses1b[all_zoonoses1b$diseases%in%"Brucellosis" & 
                                                         all_zoonoses1b$type%in%"Human",], by=c("COUNTY"="county"))

county_brucellosis1$diseases<-"Brucellosis"
brucellosis_h<-ggplot(county_brucellosis1, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(~diseases)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 people")+theme(text=element_text(size=16))


## Animal cases

###clean animal data, compute total cases and mean cases
animal_cases<-animal_zoonoses_comb%>%
  janitor::clean_names()%>%
  mutate(disease_condition=trimws(str_to_title(disease_condition)))%>%
  mutate(disease_condition=ifelse(grepl("Rabies|Rbies", disease_condition), "Rabies", 
                                  ifelse(grepl("Brucell", disease_condition), "Brucellosis", 
                                         ifelse(grepl("Rvf", disease_condition), "Rvf", disease_condition))))%>%
  mutate(county=str_to_title(county))%>%
  #mutate(county=trimws(county))%>%
  mutate(county=recode(county, "Elgeiyo Marakwet"="Elgeyo Marakwet", "Elgeyo/Marakwet"="Elgeyo Marakwet", "Kaimbu"="Kiambu",
                       "Karatina"="Nyeri", "Murang?"="Murang'a", "Mwingi"="Kitui", "Sotik"="Bomet", "Tharaka-Nithi"="Tharaka Nithi",
                       "Trans-Nzoia"="Trans Nzoia", "Pokot"="West Pokot"))%>%
  mutate(county=ifelse(county%in%"", NA, county))%>%
  mutate(diagnosis=str_to_title(nature_of_diagnosis_clinical_lab_pm))%>%
  mutate(year_1=ifelse(year_1%in%c("ND", "","201"), year,  year_1))%>%
  mutate(date=as.Date(paste0("01","-", month_2, "-",year_1),"%d-%B-%Y" ))%>%
  mutate(diagnosis=ifelse(diagnosis%in%c("Clinical Signs/Lab", "Lab", "Lab Diagnosis", "Laboratory", "Pm &Lb"),"Lab confirmed", 
                          ifelse(diagnosis%in%c("Pm", "Postmortem", "Post Mortem"), "Post Mortem", "Clinically confirmed") ))%>%
  rename("diseases"=disease_condition)%>%
  mutate(species=str_to_title(trimws(species)))%>%
  mutate(species=ifelse(species%in%c("Bobine"), "Bovine",
                        ifelse(species%in%"Bufallo","Buffalo",
                               ifelse(species%in%c("Cani", "Canine(Jackal)", "Cannine", "Dog","Dogs"), "Canine", 
                                      ifelse(species%in%c("Goat"), "Goats", ifelse(species%in%c("", "Nd"), NA, species))))))%>%
  mutate(species=recode(species, "Bovine & Canine"="Bovine , Canine", "Caprine/Ovine"="Caprine,Ovine",
                        "Caprine+Donkey"="Caprine,Donkey", "Dogs/Donkey"="Dogs,Donkey"))%>%
  separate(species, c("species1", "species2"), ",")%>%
  mutate(id=seq(1,2177))%>%
  pivot_longer(cols=c("species1", "species2"), names_to="species1", values_to="species")%>%
  mutate(species=trimws(species))%>%
  mutate(species=recode(species, "Camelidae"="Camel", "Goats"="Caprine", "Cats"="Feline", "Dogs"="Canine", 
                        "Sheep"="Ovine", "Pig"="Porcine", "Donkey"="Equine", "Cattle"="Bovine", "Pigs"="Porcine",
                        "White Tailed Mongoose"="Mongoose"))%>%
  filter(!is.na(species))%>%
  dplyr::select(date, county, diseases,species, id  )%>%
  distinct()%>%
  group_by(date, county, diseases, species)%>%
  count()%>%
  ungroup()%>%
  rename("disease_values"=n)%>%
  group_by(county, diseases, species)%>%
  mutate(disease_values=round(mean(disease_values)))%>%
  dplyr::select(county, diseases, disease_values, species)%>%
  filter(!is.na(species))


##animal population data  
animal_pop<- V4_T2.24%>%
  dplyr::select(County, ExoticCattle_Dairy,ExoticCattle_Beef,     
                IndigenousCattle,Sheep ,Goats,                 
                Camels  ,Donkeys,Pigs   )%>%
  group_by(County)%>%
  mutate_at(vars(Sheep ,Goats,                 
                 Camels  ,Donkeys,Pigs ), funs(sum(., na.rm=T)))%>%
  mutate(cattle=sum(ExoticCattle_Dairy,ExoticCattle_Beef,IndigenousCattle, na.rm=T))%>%
  ungroup()%>%
  dplyr::select(-ExoticCattle_Dairy,-ExoticCattle_Beef,-IndigenousCattle)%>%
  distinct()%>%
  filter(County!="xxx")%>%
  mutate(County=str_to_title(County))%>%
  mutate(County=recode(County, "Taita/Taveta"="Taita Taveta", 
                       "Tharaka-Nithi"="Tharaka Nithi", 
                       "Elgeyo/Marakwet"="Elgeyo Marakwet", "Nairobi City"="Nairobi"))%>%
  pivot_longer(c("Sheep", "Goats", "Camels", "Donkeys", "Pigs", "cattle"), 
               names_to ="species", values_to="species_num" )


animal_cases<- animal_cases%>%
  mutate(species=recode(species, "Bovine"="cattle", "Camel"="Camels", "Caprine"="Goats",
                        "Equine"="Donkeys", "Ovine"="Sheep", "Porcine"="Pigs"))
##combine animal average cases data and pop data and compute incidence
animal_data<- left_join(animal_cases, animal_pop, by=c("county"="County", "species"="species"))%>%
  mutate(inc=ifelse(!is.na(species_num), disease_values/species_num*100000,disease_values))


## anthrax

anthrax_buffalo<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Buffalo",], by=c("COUNTY"="county"))

anthrax_camels<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Camels",], by=c("COUNTY"="county"))
anthrax_canine<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Canine",], by=c("COUNTY"="county"))
anthrax_cattle<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"cattle",], by=c("COUNTY"="county"))
anthrax_goats<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Goats",], by=c("COUNTY"="county"))
anthrax_pigs<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Pigs",], by=c("COUNTY"="county"))
anthrax_rhino<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Rhino",], by=c("COUNTY"="county"))
anthrax_sheep<- full_join(county, animal_data[animal_data$diseases%in%"Anthrax" & animal_data$species%in%"Sheep",], by=c("COUNTY"="county"))

anthrax_buffalo$species<- "Buffalo"
anthrax_buf<-ggplot(anthrax_buffalo, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")


anthrax_camels$species<- "Camels"
anthrax_cml<-ggplot(anthrax_camels, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

anthrax_canine$species<- "Canine"
anthrax_cnine<-ggplot(anthrax_canine, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")

anthrax_cattle$species<- "Cattle"
anthrax_cttl<-ggplot(anthrax_cattle, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

anthrax_goats$species<- "Goats"
anthrax_gts<-ggplot(anthrax_goats, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

anthrax_pigs$species<- "Pigs"
anthrax_pgs<-ggplot(anthrax_pigs, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

anthrax_rhino$species<- "Rhino"
anthrax_rhno<-ggplot(anthrax_rhino, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")

anthrax_sheep$species<- "Sheep"
anthrax_shp<-ggplot(anthrax_sheep, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

plot2<- gridExtra::grid.arrange(anthrax_cnine, anthrax_cttl, anthrax_cml, anthrax_gts,anthrax_pgs,
                                anthrax_shp)

ggsave("animal_anthrax.png", plot2, width=16, height=9)
ggsave("anthrax_human.png", anthrax_h, width = 7, height=7)

## Brucellosis

#brucellosis_buffalo<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Buffalo",], by=c("COUNTY"="county"))

brucellosis_camels<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Camels",], by=c("COUNTY"="county"))
brucellosis_canine<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Canine",], by=c("COUNTY"="county"))
brucellosis_cattle<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"cattle",], by=c("COUNTY"="county"))
brucellosis_goats<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Goats",], by=c("COUNTY"="county"))
brucellosis_donkey<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Donkeys",], by=c("COUNTY"="county"))
#brucellosis_rhino<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Rhino",], by=c("COUNTY"="county"))
brucellosis_sheep<- full_join(county, animal_data[animal_data$diseases%in%"Brucellosis" & animal_data$species%in%"Sheep",], by=c("COUNTY"="county"))

#brucellosis_buffalo$species<- "Buffalo"
#brucellosis_buf<-ggplot(brucellosis_buffalo, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
#  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
#  labs(fill="Average cases")


brucellosis_camels$species<- "Camels"
brucellosis_cml<-ggplot(brucellosis_camels, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

brucellosis_canine$species<- "Canine"
brucellosis_cnine<-ggplot(brucellosis_canine, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")

brucellosis_cattle$species<- "Cattle"
brucellosis_cttl<-ggplot(brucellosis_cattle, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

brucellosis_goats$species<- "Goats"
brucellosis_gts<-ggplot(brucellosis_goats, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

brucellosis_donkey$species<- "Donkeys"
brucellosis_pgs<-ggplot(brucellosis_donkey, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

#brucellosis_rhino$species<- "Rhino"
#brucellosis_rhno<-ggplot(brucellosis_rhino, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
#  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
#  labs(fill="Average cases")

brucellosis_sheep$species<- "Sheep"
brucellosis_shp<-ggplot(brucellosis_sheep, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

plot2<- gridExtra::grid.arrange(brucellosis_cnine, brucellosis_cttl, brucellosis_cml, brucellosis_gts,brucellosis_pgs,
                                brucellosis_shp)


ggsave("brucellosis_animals.png", plot2, width=16, height=9)
ggsave("brucellosis_humans.png",brucellosis_h,width = 7, height = 7)

## RVF

#rvf_buffalo<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Buffalo",], by=c("COUNTY"="county"))

rvf_camels<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Camels",], by=c("COUNTY"="county"))
#rvf_canine<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Canine",], by=c("COUNTY"="county"))
rvf_cattle<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"cattle",], by=c("COUNTY"="county"))
rvf_goats<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Goats",], by=c("COUNTY"="county"))
#rvf_donkey<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Donkeys",], by=c("COUNTY"="county"))
#rvf_rhino<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Rhino",], by=c("COUNTY"="county"))
rvf_sheep<- full_join(county, animal_data[animal_data$diseases%in%"Rvf" & animal_data$species%in%"Sheep",], by=c("COUNTY"="county"))

#rvf_buffalo$species<- "Buffalo"
#rvf_buf<-ggplot(rvf_buffalo, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
#  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
#  labs(fill="Average cases")


rvf_camels$species<- "Camels"
rvf_cml<-ggplot(rvf_camels, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")


rvf_cattle$species<- "Cattle"
rvf_cttl<-ggplot(rvf_cattle, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

rvf_goats$species<- "Goats"
rvf_gts<-ggplot(rvf_goats, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")


rvf_sheep$species<- "Sheep"
rvf_shp<-ggplot(rvf_sheep, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases \n per 100,000 animals")

plot2<- gridExtra::grid.arrange( rvf_cttl, rvf_cml, rvf_gts,
                                 rvf_shp)


ggsave("rvf_animals.png", plot2, width=16, height=9)
ggsave("rvf_human.png", rvf_h, width = 7, height=7)

##Rabies
#rabies_buffalo<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Buffalo",], by=c("COUNTY"="county"))

rabies_camels<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Camels",], by=c("COUNTY"="county"))
rabies_feline<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Feline",], by=c("COUNTY"="county"))
rabies_canine<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Canine",], by=c("COUNTY"="county"))
rabies_cattle<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"cattle",], by=c("COUNTY"="county"))
rabies_goats<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Goats",], by=c("COUNTY"="county"))
rabies_donkey<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Donkeys",], by=c("COUNTY"="county"))
#rabies_rhino<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Rhino",], by=c("COUNTY"="county"))
rabies_sheep<- full_join(county, animal_data[animal_data$diseases%in%"Rabies" & animal_data$species%in%"Sheep",], by=c("COUNTY"="county"))

#rabies_buffalo$species<- "Buffalo"
#rabies_buf<-ggplot(rvf_buffalo, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
#  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
#  labs(fill="Average cases")

rabies_feline$species<- "Feline"
rabies_feline<-ggplot(rabies_feline, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")

rabies_canine$species<- "Canine"
rabies_canine<-ggplot(rabies_canine, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average cases")

rabies_camels$species<- "Camels"
rabies_cml<-ggplot(rabies_camels, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average \n100,000 animals")


rabies_cattle$species<- "Cattle"
rabies_cttl<-ggplot(rabies_cattle, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average \n100,000 animals")

rabies_goats$species<- "Goats"
rabies_gts<-ggplot(rabies_goats, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average \n100,000 animals")


rabies_sheep$species<- "Sheep"
rabies_shp<-ggplot(rabies_sheep, aes(fill=inc))+geom_sf()+theme_bw()+facet_grid(.~species)+
  scale_fill_gradient(low="#ffffb2", high="#b10026", na.value = "white")+
  labs(fill="Average \n100,000 animals")

plot2<- gridExtra::grid.arrange(rabies_canine, rabies_feline, rabies_cttl, rabies_cml, rabies_gts,
                                rabies_shp)


ggsave("rabies_animals.png", plot2, width=18, height=9)
ggsave("rabies_human.png", rabies_h, width = 7, height=7)

##brucellosis trends

all_zoonoses1b<- rbind(human_zoonoses, animal_zoonoses)%>%
  mutate(diseases=recode(diseases, "Brucella"="Brucellosis", "brucellosis"="Brucellosis"))%>%
  group_by(date,diseases, type)%>%
  mutate(total=sum(disease_values,na.rm=T))%>%
  ungroup()%>%
  dplyr::select(date, diseases, type,total)%>%
  distinct()%>%
  mutate(total1=ifelse(diseases%in%"Brucellosis" & type%in%"Animal", total*8486.25, total))



ggplot(all_zoonoses1b[all_zoonoses1b$diseases%in%"Brucellosis" ,], aes(x=date, y=total1, group=type, color=type))+
  geom_smooth()+theme_bw()+scale_x_date(date_labels = "%b-%y", date_breaks="6 months")+
  labs(x="Period", y="Frequency", color="")+theme(text=element_text(size=16))

ggsave("Brucellosis_trend.png", width=16, height=8)

##rvf trends
all_zoonoses1b<- rbind(human_zoonoses, animal_zoonoses)%>%
  group_by(date,diseases, type)%>%
  mutate(total=round(sum(disease_values,na.rm=T)))%>%
  ungroup()%>%
  dplyr::select(date, diseases, type,total)%>%
  distinct()%>%
  mutate(total1=ifelse(diseases%in%"Rvf" & type%in%"Animal", total*23.167, total))



ggplot(all_zoonoses1b[all_zoonoses1b$diseases%in%"Rvf" ,], aes(x=date, y=total1, group=type, color=type))+
  geom_smooth()+theme_bw()+scale_x_date(date_labels = "%b-%y", date_breaks="6 months")+
  labs(x="Period", y="Frequency", color="")+theme(text=element_text(size=16))

ggsave("Rvf_trend.png", width=16, height=8)

##anthrax trends
all_zoonoses1b<- rbind(human_zoonoses, animal_zoonoses)%>%
  group_by(date,diseases, type)%>%
  mutate(total=sum(disease_values,na.rm=T))%>%
  ungroup()%>%
  dplyr::select(date, diseases, type,total)%>%
  distinct()%>%
  mutate(total1=ifelse(diseases%in%"Anthrax" & type%in%"Animal", total*93.667, total))



ggplot(all_zoonoses1b[all_zoonoses1b$diseases%in%"Anthrax" ,], aes(x=date, y=total1, group=type, color=type))+
  geom_smooth()+theme_bw()+scale_x_date(date_labels = "%b-%y", date_breaks="6 months")+
  labs(x="Period", y="Frequency", color="")+theme(text=element_text(size=16))

ggsave("Anthrax_trend.png", width=16, height=8)

##Rabies trends
all_zoonoses1b<- rbind(human_zoonoses, animal_zoonoses)%>%
  group_by(date,diseases, type)%>%
  mutate(total=sum(disease_values,na.rm=T))%>%
  ungroup()%>%
  dplyr::select(date, diseases, type,total)%>%
  distinct()%>%
  mutate(total1=ifelse(diseases%in%"Rabies" & type%in%"Animal", total*26.529, total))



ggplot(all_zoonoses1b[all_zoonoses1b$diseases%in%"Rabies" ,], aes(x=date, y=total1, group=type, color=type))+
  geom_smooth()+theme_bw()+scale_x_date(date_labels = "%b-%y", date_breaks="6 months")+
  labs(x="Period", y="Frequency", color="")+theme(text=element_text(size=16))

ggplot(all_zoonoses1b[all_zoonoses1b$diseases%in%"Rabies" ,]) +
  aes(x = date, y = total1,group=type, color=type) +
  geom_smooth()+theme_bw()+labs(x="Period", y="Frequency", color="")+theme(text=element_text(size=16))

ggsave("Rabies_trend.png", width=16, height=8)