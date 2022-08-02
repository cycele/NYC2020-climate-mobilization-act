# load the data
file_way<-"~/Desktop/R/dtaset"
file_name<-list.files("~/Desktop/R/dtaset")
nn<-list()
for (i in (1:length(file_name))) {
  nn[[i]]<-read.csv(paste(file_way,file_name[i],sep = "/"))
}
# create 11 dataframe
data_1<-data.frame(nn[[1]]$Building.ID,nn[[1]]$Facility.Name,nn[[1]]$Facility.Type,nn[[1]]$Current.Total.GHG.Emissions..MtCO2e.,nn[[1]]$Current.Total.GHG.Emissions..MtCO2e./nn[[1]]$Total.Floor.Space..Sq..Ft..*1000)
data_2<-data.frame(nn[[2]]$Building.ID..ESPM.,nn[[2]]$Facility.Name,nn[[2]]$Facility.Type,nn[[2]]$Current.Total.GHG.Emissions..MtCO2e.,nn[[2]]$Current.Total.GHG.Emissions..MtCO2e./nn[[2]]$Total.Floor.Space..Sq.Ft.*1000)
data_3<-data.frame(nn[[3]]$Property.Id,nn[[3]]$Property.Name,nn[[3]]$Primary.Property.Type...Self.Selected,nn[[3]]$Total.GHG.Emissions..MtCO2e.,as.numeric(nn[[3]]$Total.GHG.Emissions..MtCO2e.)/as.numeric(nn[[3]]$Property.Floor.Area..Buildings.and.Parking...ft..)*1000)
data_4<-data.frame(nn[[4]]$Property.Id,nn[[4]]$Property.Name,nn[[4]]$Primary.Property.Type...Self.Selected,nn[[4]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[4]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[4]]$Property.Floor.Area..Building.s....ft..)*1000)
data_5<-data.frame(nn[[5]]$Property.Id,nn[[5]]$Property.Name,nn[[5]]$Primary.Property.Type...Self.Selected,nn[[5]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[5]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[5]]$Property.Floor.Area..Building.s....ft..)*1000)
data_6<-data.frame(nn[[6]]$Property.Id,nn[[6]]$Property.Name,nn[[6]]$Primary.Property.Type...Self.Selected,nn[[6]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[6]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[6]]$Property.GFA...EPA.Calculated..Buildings.and.Parking...ft..)*1000)
data_7<-data.frame(nn[[7]]$Property.Id,nn[[7]]$Property.Name,nn[[7]]$Primary.Property.Type...Self.Selected,nn[[7]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[7]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[7]]$Property.GFA...Calculated..Buildings.and.Parking...ft..)*1000)
data_8<-data.frame(nn[[8]]$Property.Id,nn[[8]]$Property.Name,nn[[8]]$Primary.Property.Type...Self.Selected,nn[[8]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[8]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[8]]$Property.GFA...Calculated..Buildings.and.Parking...ft..)*1000)
data_9<-data.frame(nn[[9]]$Property.Id,nn[[9]]$Property.Name,nn[[9]]$Primary.Property.Type...Self.Selected,nn[[9]]$Total.GHG.Emissions..Metric.Tons.CO2e.,as.numeric(nn[[9]]$Total.GHG.Emissions..Metric.Tons.CO2e.)/as.numeric(nn[[9]]$Property.GFA...Calculated..Buildings.and.Parking...ft..)*1000)
data_10<-data.frame(nn[[10]]$Property.Id,nn[[10]]$Property.Name,nn[[10]]$Primary.Property.Type...Self.Selected,nn[[10]]$Total.GHG.Emissions..Metric.Tons.CO2e.,nn[[10]]$Total.GHG.Emissions.Intensity..kgCO2e.ft..)
data_11<-data.frame(nn[[11]]$Property.Id,nn[[11]]$Property.Name,nn[[11]]$Primary.Property.Type...Self.Selected,nn[[11]]$Total.GHG.Emissions..Metric.Tons.CO2e.,nn[[11]]$Total.GHG.Emissions.Intensity..kgCO2e.ft..)
a<-list()
a[[1]]<-data_1
a[[2]]<-data_2
a[[3]]<-data_3
a[[4]]<-data_4
a[[5]]<-data_5
a[[6]]<-data_6
a[[7]]<-data_7
a[[8]]<-data_8
a[[9]]<-data_9
a[[10]]<-data_10
a[[11]]<-data_11
b<-c("property id","property name","primary property type-self selected","total GHG emissions(metric tons co2)","total GHG emissions intensity kgco2/ft2")
for (i in 1:length(file_name))
  {
  colnames(a[[i]])<-b
}
# As numeric(total GHG emissionsmetric tons co2, total GHG emissions intensity kgco2/ft2)
for (i in 1:length(file_name)) {
  a[[i]]$`total GHG emissions(metric tons co2)`<-as.numeric(a[[i]]$`total GHG emissions(metric tons co2)`)
  a[[i]]$`total GHG emissions intensity kgco2/ft2`<-as.numeric(a[[i]]$`total GHG emissions intensity kgco2/ft2`)
}
# Append dataframe
for (i in 1:length(file_name)) {
  a[[i]]$calendar_year<-2009+i
}
data<-a[[1]]
for (i in 2:length(file_name)) {
  data<-rbind(data,a[[i]])
}
# plot total GHG emissionsmetric tons co2, total GHG emissions intensity kgco2/ft2
data<-data[which(data$`total GHG emissions intensity kgco2/ft2`!= Inf),]
cal_year<-unique(data$calendar_year)
GHG<-rep(1,1,length(cal_year))
for (i in 1:length(cal_year)) {
  GHG[i]<-sum(data[which(data$calendar_year==cal_year[i]),]$`total GHG emissions(metric tons co2)`,na.rm = T)
}
GHG_intensity<-rep(1,1,length(cal_year))
for (i in 1:length(cal_year)) {
  GHG_intensity[i]<-sum(data[which(data$calendar_year==cal_year[i]),]$`total GHG emissions intensity kgco2/ft2`,na.rm = T)
}
plot(cal_year,GHG,main="total GHG emissions(metric tons co2)",type="l")
plot(cal_year,GHG_intensity,main="total GHG emissions intensity kgco2/ft2",type="l")
# define prop_type
data$prop_type<-factor(data$`primary property type-self selected`,levels = c("Office", 
                                                                             "Bank/Financial Institution", 
                                                                             "Service (Vehicle Repair/Service, Postal Service)", 
                                                                             "Bank Branch", 
                                                                             "Medical Office",
                                                                             "Health Care: Outpatient", 
                                                                             "Clinic/Other Outpatient Health", 
                                                                             "Financial Office", 
                                                                             "Senior Care Facility", 
                                                                             "Automobile Dealership", 
                                                                             "Urgent Care/Clinic/Other Outpatient", 
                                                                             "Laboratory", 
                                                                             "Repair Services (Vehicle, Shoe, Locksmith, etc.)",
                                                                             "Outpatient Rehabilitation/Physical Therapy", 
                                                                             "Mailing Center/Post Office", 
                                                                             "Ambulatory Surgical Center",
                                                                             "Supermarket/Grocery", 
                                                                             "Retail", 
                                                                             "Mall (Strip Mall and Enclosed)",
                                                                             "Retail Store",
                                                                             "Food Sales", 
                                                                             "Retail (Misc) ", 
                                                                             "Strip Mall", 
                                                                             "Other Mall",
                                                                             "Library",
                                                                             "K-12 School",
                                                                             "Education", 
                                                                             "College/University (Campus-Level)", 
                                                                             "Other Education", 
                                                                             "College/University", 
                                                                             "Pre-school/Daycare", 
                                                                             "Adult Education",
                                                                             "Hotel", 
                                                                             "Multifamily Housing", 
                                                                             "Residence Hall/Dormitory",
                                                                             "Lodging", "Senior Care Community",
                                                                             "Mixed Use Property", 
                                                                             "Other Lodging/Residential",
                                                                             "Restaurant/Cafeteria", 
                                                                             "House of Worship",
                                                                             "Entertainment/Culture", 
                                                                             "Social/Meeting", 
                                                                             "Recreation", 
                                                                             "Public Assembly", 
                                                                             "Food Service",
                                                                             "Social/Meeting Hall", 
                                                                             "Worship Facility", 
                                                                             "Movie Theater", 
                                                                             "Enclosed Mall", 
                                                                             "Other Recreation", 
                                                                             "Performing Arts", 
                                                                             "Other Entertainment/Public Assembly",
                                                                             "Restaurant", 
                                                                             "Museum",
                                                                             "Courthouse", 
                                                                             "Wholesale Club/Supercenter", 
                                                                             "Ice/Curling Rink", 
                                                                             "Fitness Center/Health Club/Gym",
                                                                             "Hospital (General Medical and Surgical) ", 
                                                                             "Health Care: Inpatient (Specialty Hospitals, Excluding Children's) ", 
                                                                             "Hospital (General Medical Surgical)", 
                                                                             "Residential Care Facility", 
                                                                             "Other Specialty Hospital",
                                                                             "Self-Storage", 
                                                                             "Self-Storage Facility",
                                                                             "Data Center", 
                                                                             "Storage/Shipping/Non-Refrigerated Warehouse", 
                                                                             "Warehouse (Unrefrigerated)", 
                                                                             "Warehouse (Refrigerated)", 
                                                                             "Non-Refrigerated Warehouse", 
                                                                             "Refrigerated Warehouse",
                                                                             "Other", 
                                                                             "Other Public Services", 
                                                                             "Other Technology/Science",
                                                                             "Other Services",
                                                                             "NA"),labels = c(rep("B",16),rep("M",8),rep("E",8),rep("R",7),rep("A",20),rep("I",5),rep("S",8),rep("Other",4),"NA"))
# plot prop_type
#install.packages("lessR")
library(lessR)
for (i in 1:length(cal_year)) {
  k<-data[which(data$calendar_year==cal_year[i]),]
  m<-aggregate(k$`total GHG emissions(metric tons co2)`, by=list(type=k$prop_type),sum)
  n<-m$x
  name<-m$type
  col<-c("#4398D0","#B28B2A","#5FA140","#D57388","#9A84D6","#00A898","#C97E5B","#909711","#00A3BA" )
  piepercent = paste(round(100*n/sum(n)), "%")
  pie(n,labels =piepercent,col = col,main = paste("total GHG emissions metric tons co2",cal_year[i],sep = " "))
  legend("right", paste(as.character(name),piepercent,sep = ":"), cex = 0.8, fill = col)
}
for (i in 1:length(cal_year)) {
  k<-data[which(data$calendar_year==cal_year[i]),]
  m<-aggregate(k$`total GHG emissions intensity kgco2/ft2`, by=list(type=k$prop_type),sum)
  n<-m$x
  name<-m$type
  col<-c("#4398D0","#B28B2A","#5FA140","#D57388","#9A84D6","#00A898","#C97E5B","#909711","#00A3BA" )
  piepercent = paste(round(100*n/sum(n)), "%")
  pie(n,labels =piepercent,col = col,main = paste("total GHG emissions intensity kgco2/ft2",cal_year[i],sep = " "))
  legend("right", paste(as.character(name),piepercent,sep = ":"), cex = 0.8, fill = col)
}
# line for prop_type 
data<-na.omit(data)
type<-unique(data$prop_type)
for (i in 1:length(type)) {
  f<-data[which(data$prop_type==type[i]),]
  g<-aggregate(f$`total GHG emissions intensity kgco2/ft2`,by=list(type=f$calendar_year),sum )
  l<-aggregate(f$`total GHG emissions(metric tons co2)`,by=list(type=f$calendar_year),sum )
  plot(l$type,l$x,type="l",col="blue",main=paste("type",type[i],sep =":" ))
  lines(g$type,g$x,col="red")
  legend("right",c("total GHG emissions(metric tons co2)","total GHG emissions intensity kgco2/ft2"),lty=1,col = c("red","blue"),cex=0.5)
}


