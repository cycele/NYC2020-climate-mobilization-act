library(tidyverse)
library(janitor)

##import csv file
y2020_data <- read_csv("Energy_and_Water_Data_Disclosure_for_Local_Law_84_2021__Data_for_Calendar_Year_2020_.csv")

##clean column name
y2020clean_name <- clean_names(y2020_data)

y2020_selected <- select(y2020clean_name, property_id, property_name, primary_property_type_self_selected, national_median_reference_property_type,nyc_borough_block_and_lot_bbl,address_1,
                         total_ghg_emissions_metric_tons_co2e, total_ghg_emissions_intensity_kg_co2e_ft2, net_emissions_metric_tons_co2e)

# how to delete data which is "Not available" rather than "na"?
#I tried the following code but it doesn't work
#y2020_selected['total_ghg_emissions_metric_tons_co2e'%in% "Not Available", 'total_ghg_emissions_metric_tons_co2e'= NA ]
#na.omit(y2020_filter)

y2020_filter <- filter(y2020_selected, total_ghg_emissions_metric_tons_co2e>=0, total_ghg_emissions_intensity_kg_co2e_ft2>=0, net_emissions_metric_tons_co2e>=0)
summary(y2020_filter)
#the mean is incorrect because the data is in the form of chr, need converting to numbers

y2020_filter$Prop_type <- y2020_filter$primary_property_type_self_selected

## add the property type codes 
#might need some help here
if (y2020_filter$primary_property_type_self_selected %in% c("Office", "Bank/Financial Institution", "Service (Vehicle Repair/Service, Postal Service)", "Bank Branch", 
                          "Medical Office" , "Health Care: Outpatient", "Clinic/Other Outpatient Health", "Financial Office",
                          "Senior Care Facility", "Automobile Dealership", "Urgent Care/Clinic/Other Outpatient", "Laboratory",
                          "Repair Services (Vehicle, Shoe, Locksmith, etc.)", 
                          "Outpatient Rehabilitation/Physical Therapy", "Mailing Center/Post Office", "Ambulatory Surgical Center"))
    y2020_filter[]
lgbd_sub[Prop_type %in% c("Supermarket/Grocery", "Retail", "Mall (Strip Mall and Enclosed)", "Retail Store")
                          "Food Sales", "Retail (Misc)", "Strip Mall", "Other - Mall"), Prop_type := "M"]
lgbd_sub[Prop_type %in% c("Library", "K-12 School", "Education", "College/University (Campus-Level)", "Other - Education", "College/University", "Pre-school/Daycare", 
                          "Supermarket/Grocery Store", "Adult Education"
), Prop_type := "E"]
lgbd_sub[Prop_type %in% c("Hotel", "Multifamily Housing", "Residence Hall/Dormitory", "Lodging", "Senior Care Community", "Mixed Use Property", 
                          "Other - Lodging/Residential"
), Prop_type := "R"]
lgbd_sub[Prop_type %in% c("Restaurant/Cafeteria", "House of Worship",  "Entertainment/Culture", "Social/Meeting", "Recreation", "Public Assembly", "Food Service", 
                          "Social/Meeting Hall", "Worship Facility", "Movie Theater", "Enclosed Mall", "Other - Recreation", "Performing Arts", "Other - Entertainment/Public Assembly", 
                          "Restaurant", "Museum", "Courthouse",  "Wholesale Club/Supercenter", "Ice/Curling Rink", 
                          "Fitness Center/Health Club/Gym"
), Prop_type := "A"]
lgbd_sub[Prop_type %in% c("Hospital (General Medical and Surgical)", "Health Care: Inpatient (Specialty Hospitals, Excluding Children's)", "Hospital (General Medical & Surgical)", 
                          "Residential Care Facility", "Other - Specialty Hospital"
), Prop_type := "I"]
lgbd_sub[Prop_type %in% c("Self-Storage", "Self-Storage Facility", "Data Center", "Storage/Shipping/Non-Refrigerated Warehouse", "Warehouse (Unrefrigerated)", "Warehouse (Refrigerated)", 
                          "Non-Refrigerated Warehouse", "Refrigerated Warehouse"
), Prop_type := "S"]
lgbd_sub[Prop_type %in% c("Other", "Other - Public Services", "Other - Technology/Science", "Other - Services") 
         , Prop_type := "Other"]
lgbd_sub[Prop_type %in% "NA"
         , Prop_type := "NA"]