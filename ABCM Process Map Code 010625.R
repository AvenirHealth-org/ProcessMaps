##########################################################################################################
# ABCM Process Maps

# The Analytics for Advancing the Financial Sustainability of the HIV/AIDS Response (AFS) contract is funded 
# by the United States Agency for International Development (USAID) under Contract No. 7200AA23C00043, March 2023-March 2027. 
# This contract is made possible by the generous support of the American people through support from USAID and the 
# United States President’s Emergency Plan for AIDS Relief (PEPFAR). The contents were prepared by staff of the AFS contract 
# and do not necessarily reflect the views of USAID or the United States Government.

#######################################################################################################
# # # # # # # 
# Section 1 #
# # # # # # #

# If you do not have following packages installed, install before running code (in RStudio) by clicking Packages > Install > Type Name > Install

library(tidyverse)
library(stringi)
library(stringr)

library(igraph)
library(showtext)
library(ggfittext)
library(ggforce)
library(cowplot)
library(magick)
library(png)
library(ggpubr)

# ggchicklet is a package that is hosted through github, uncomment one of the following to install
#install.packages("ggchicklet", repos = "https://cinc.rud.is") 
#remotes::install_git("https://git.rud.is/hrbrmstr/ggchicklet.git")
#https://albert-rapp.de/posts/ggplot2-tips/11_rounded_rectangles/11_rounded_rectangles.html
#remotes::install_gitlab("hrbrmstr/ggchicklet")
library(ggchicklet)
############################################################


############################################################
# # # # # # # 
# Section 2 #
# # # # # # #

# Set working directory to where your data is stored on your computer
setwd("C:/Users/KristinBietsch/files/ABCM/Process Maps 110424")

# Make sure in your working directory you create a folder called "Loop Results"


# Here we are storing the date information, which we will use when exporting graphics
date <- format(Sys.Date(), "%m%d%Y")

# Legends
# These legends were created using file "ABCM Process Map Legends 112624.R"

periodic_norm <-  readPNG( "Legend Normative square6.png")
norm_LEG_height <- 15.444
nrom_LEG_width <- 11
  
periodic_normpat <- readPNG( "Legend Patient Normative Combo Long.png")
normpat_LEG_height <- 22.044
normpat_LEG_width <- 11
############################################################

############################################################
# # # # # # # 
# Section 3 #
# # # # # # #

# Edit the facility name for your facility
facility <- "Anonymous Facility"
############################################################


############################################################
# # # # # # # 
# Section 4 #
# # # # # # #

# Read in Data
input_clean <- read.csv("Anonymized Clean Example 110424 w Branding.csv")

# The data read in the line above includes information about the project in the last row
# To remove anything that does not contain "Patient" in the first column, run the next set of code
input_clean <- input_clean %>% filter(substr(Patient, 1, 7) == "Patient")

# Next we want a full list of all interventions and descriptions of steps
list_intervention_description <- input_clean  %>%
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) %>%
  gather(Variable, Value, Step_1_Description:Step_17_Staff3Other) %>%
  mutate(Variable=substr(Variable, 6, str_length(Variable))) %>%
  separate(Variable, c("Step", "Indicator")) %>%
  filter(Indicator=="Description") %>%
  filter(Value!= "") %>% rename(Description=Value) %>%
  mutate(n=1) %>%
  group_by(Intervention_Full, Description) %>%
  summarise(N=sum(n)) %>% select(-N) %>%
  arrange(Intervention_Full, Description)

# Check the dataset above to see if there are any descriptions that should be the same but are not because of spacing, spelling, capitalization, etc.  For example "Meds Dispensed" versus "Meds dispensed"

# You now have a list of all the intervention names and step descriptions in your dataset.  You can export this information as a csv by uncommenting the line at the bottom of this section.
# Now, you want to match up each Intervention_Full and Description  from your input data with the names used in the Normative order data set "Normative Process Map 111523 Clean.csv"
# You can copy and paste each line into columns G and H of "Normative Process Map 111523 Clean.csv"
# It is important that the strings match exactly, including spaces and capitalization  
# Columns G and H have data which matches the anonymized data used in this code, but you can remove that information and insert the information from your dataset.

#write.csv(list_intervention_description, "Input Data Intervention and Description.csv", row.names = F, na="")

############################################################

############################################################
# # # # # # # 
# Section 5 #
# # # # # # #

# Normative Order

# In the CSV, "Normative Process Map 111523 Clean.csv", the first 6 columns represent each normative process.  
# Column A is the name of the intervention 
# Column B is the step number
# Column C is the name of each step
# Column D is the time it is expected to take to complete each step
# Column E is the location in the health facility the step is expected to take place
# Column F is the staff member who is expected to conduct each step

# Columns G and H should be edited using the information from the section above


order <- read.csv("Normative Process Map 111523 Clean.csv")
intervention_coding <- order %>% select(Intervention_New, Intervention_Full) %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_new_full <- order %>% select(Intervention_New, Intervention_Full) %>% filter(Intervention_Full!="") %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_description <- order %>% select(Intervention_New, Description_new, Description) %>% filter(Description!="")
order_normatative <- order %>% rename(NormativeLocation=Location, NormativeStaff=Staff) %>% select(-Intervention_Full, -Description)
############################################################

############################################################
# # # # # # # 
# Section 6 #
# # # # # # #

# This piece of code counts how many patients are recorded for each type of service
total_patients <- input_clean  %>%
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) %>%
  mutate(Patient_N=1) %>% group_by(Intervention_Full) %>% summarise(Patient_N=sum(Patient_N)) %>%
  left_join(intervention_coding, by="Intervention_Full") %>% filter(!is.na(Intervention_New)) %>%
  select(-Intervention_Full)

#################################################################################################

#################################################################################################
# # # # # # # 
# Section 7 #
# # # # # # #

# We will now begin a series of data cleaning and data checks

# First, we reshape the data from wide to long.  Before, each patient had their own row, but now each row represents a step for a patient, so patients have multiple rows
# We are also standardizing the time variables


input_long0 <- input_clean %>% gather(Variable, Value, Step_1_Description:Step_17_Staff3Other) %>%
  mutate(Patient=as.numeric(as.character(substr(Patient, 8, str_length(Patient))))) %>%
  mutate(Variable=substr(Variable, 6, str_length(Variable))) %>%
  separate(Variable, c("Step", "Indicator")) %>%
  mutate(Step=as.numeric(as.character(Step))) %>%
  spread(Indicator, Value) %>%
  relocate(StartTime, .before=EndTime) %>%
  arrange(Patient, Step) %>%
  filter(Description!= "") %>%
  mutate(EndTime=as.POSIXct(strptime(EndTime, "%H:%M:%S")),
         StartTime=as.POSIXct(strptime(StartTime, "%H:%M:%S"))) %>%
  mutate(time_spent = difftime(EndTime, StartTime, units = "mins"),
         time_spent_0 = difftime(EndTime, EndTime, units = "mins")) %>%
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) 
#########################################################################################################

#########################################################################################################
# # # # # # # 
# Section 8 #
# # # # # # #

# Cleaning for Normative Maps

# Waiting checks:
# Check to make sure waiting is never last step
# Check to make sure there are not two waitings next to one another

# Sometimes weight and vital are recorded as two events, but we want to combine them for the normative maps
# The first chunk of code below identifies if vital and weight are recorded separately
# The second chunk combines the information on vitals and weight
# The third removes the individual steps and adds in the combined step

input_long1 <- input_long0 %>%
  group_by(Patient) %>%
  mutate(Waiting=case_when(Description=="Waiting" ~ 1),
         Waiting_lag = lag(Waiting),
         event_after_waiting=case_when(lag(Waiting_lag) ==1 & lag(Waiting) ==1 ~ 2,
                                       Waiting_lag==1 & is.na(Waiting) ~ 1),
         Waiting_Time=case_when(event_after_waiting==2 ~ lag(time_spent) + lag(time_spent, 2),
                                event_after_waiting==1 ~ lag(time_spent),
                                is.na(event_after_waiting) ~ time_spent_0)) %>%
  filter(Description!="Waiting") %>%
  mutate(Weight_Vital=case_when(Description== "Vitals taken"  | Description=="Weight taken"  ~ 1,
                                TRUE ~ 0)) %>%
  mutate(Weight_Vital_Total = sum(Weight_Vital)) %>%
  mutate(Description=case_when(Weight_Vital==1 & Weight_Vital_Total==1 ~ "Vitals/Weight taken",
                               TRUE ~ Description))

weight_vital <- input_long1 %>% filter(Weight_Vital_Total>1 & Weight_Vital==1) %>%
  group_by(Patient) %>%
  summarise(Questionnaire=first(Questionnaire),
            Facility=first(Facility),
            Region=first(Region),
            District=first(District),
            FacilityType=first(FacilityType),
            Arrival_Time=first(Arrival_Time),
            Departure_Time=first(Departure_Time),
            Intervention=first(Intervention),
            HIV_Classification=first(HIV_Classification),
            Type_of_Service=first(Type_of_Service),
            Patient_Classification=first(Patient_Classification),
            Step=first(Step),
            StartTime=first(StartTime),
            EndTime=last(EndTime),
            Location=first(Location),
            OtherLocation=first(OtherLocation),
            Staff1=first(Staff1),
            Staff1Other=first(Staff1Other),
            Staff2=first(Staff2),
            Staff2Other=first(Staff2Other),
            Staff3=first(Staff3),
            Staff3Other=first(Staff3Other),
            time_spent=sum(time_spent),
            Intervention_Full=first(Intervention_Full),
            Waiting_Time=sum(Waiting_Time), 
            Weight_Vital=first(Weight_Vital),
            Weight_Vital_Total=first(Weight_Vital_Total)) %>%
  mutate(Description="Vitals/Weight taken")

input_long2 <- input_long1 %>% filter(Description!= "Vitals taken" ) %>%
  filter(Description!= "Weight taken" ) %>%
  bind_rows(weight_vital) %>% select(-Weight_Vital, -Weight_Vital_Total) %>%
  arrange(Patient, Step) 


# Next we check if there are two events of the same name for a patient
# If there are duplicates, we want to create one event
duplicates <- input_long2 %>%
  mutate(n=1) %>%
  group_by(Patient, Description) %>%
  mutate(n=sum(n)) %>% filter(n>1) %>%
  group_by(Patient, Description) %>%
  summarise(Questionnaire=first(Questionnaire),
            Facility=first(Facility),
            Region=first(Region),
            District=first(District),
            FacilityType=first(FacilityType),
            Arrival_Time=first(Arrival_Time),
            Departure_Time=first(Departure_Time),
            Intervention=first(Intervention),
            HIV_Classification=first(HIV_Classification),
            Type_of_Service=first(Type_of_Service),
            Patient_Classification=first(Patient_Classification),
            Step=first(Step),
            StartTime=first(StartTime),
            EndTime=last(EndTime),
            Location=first(Location),
            OtherLocation=first(OtherLocation),
            Staff1=first(Staff1),
            Staff1Other=first(Staff1Other),
            Staff2=first(Staff2),
            Staff2Other=first(Staff2Other),
            Staff3=first(Staff3),
            Staff3Other=first(Staff3Other),
            time_spent=sum(time_spent),
            Intervention_Full=first(Intervention_Full),
            Waiting_Time=sum(Waiting_Time))

# In this chunk of code we remove the repeated events and add in the duplicate combined event we created above
input_long3 <- input_long2  %>%
  mutate(n=1) %>%
  group_by(Patient, Description) %>%
  mutate(n=sum(n))  %>% filter(n==1) %>% select(-n) %>%
  bind_rows(duplicates) %>%
  arrange(Patient, Step) %>%
  mutate(one=1) 

# Next we want to clean any staff descriptions or step descriptions
# For a list of all staff in the dataset, run:
levels(as.factor(input_long3$Staff1))
# To check the descriptions, run:
levels(as.factor(input_long3$Description))

# For example, we see that one description is recorded as "Weight/Vitals Taken" while another is "Vitals/Weight taken", we change one so that they match

# Our staff list include: 
#  "Nurse", "Community Health Worker", "Data Clerk", "Lab Technician",
#  "Medical Attendant", "Clinical Officer", "Receptionist", "Counselor",
#  "Doctor", "Pharmacist", "Social Worker", "Assistant Medical Officer", "Pharmacy Technician", 
#  "Cadre x", "Cadre y", "Cadre z", "Patient Arrives", "Patient Departs" 

input_long4 <- input_long3 %>%
  mutate(Staff1_USAID=case_when(Staff1==  "Enrolled Nurse" | Staff1== "Registered Nurse" ~ "Nurse",
                                Staff1== "Community Health worker" ~ "CHW",
                                Staff1== "Data Clerk"    ~ "Data Clerk",
                                Staff1==  "Receptionist"   ~ "Receptionist",
                                Staff1== "Counsellor"      ~ "Counselor",
                                Staff1== "Pharmacist"   ~ "Pharmacist",
                                Staff1=="Health Assistant"    |  Staff1=="TCE Field Worker"  ~ "Assistant Medical Officer",
                                Staff1== "Pharmacist Assistant"   ~ "Pharmacy Technician",
                                Staff1== "Student Nurse"  ~ "Cadre x")) %>%
  mutate(Description=case_when(Description=="Weight/Vitals Taken" ~ "Vitals/Weight taken",
                               TRUE ~ Description))

# The data we are given does not include a step for arrival or departure, for our graphics, we would like to show these steps
# Creating the arrival and departure events
arrive_depart <- input_long4 %>% group_by(Intervention_Full) %>% summarise(One=mean(one)) %>% select(-One) %>%
  mutate(Arrive="Patient Arrives", Depart="Patient Departs") %>%
  gather(Variable, Description_new, Arrive: Depart) %>% select(-Variable) %>%
  left_join(intervention_coding, by="Intervention_Full") %>% filter(!is.na(Intervention_New))


# The next chunks of code creates summary statistics for each normative process
# They also add in the location on our plots where each piece of information will be shown

# Here, we are calculating which type of staff performed a step. 
# In the normative information file, we have assigned a staff member who is expected to preform a step, 
# but this data will show us which staff are actually preforming the step in our dataset
process_analytics_staff <- input_long4 %>%
  group_by(Intervention_Full, Description) %>%
  mutate(Description_Count=sum(one)) %>%
  ungroup() %>%
  group_by(Intervention_Full, Description, Staff1_USAID) %>%
  summarise(Staff_n = sum(one), Description_Count=mean(Description_Count)) %>%
  mutate(StaffDistribution=Staff_n/Description_Count) %>%
  full_join(order, by=c("Intervention_Full", "Description")) %>%
  mutate(NormativeOrder=NormativeOrder + 1) %>%
  arrange(Intervention_New, NormativeOrder) %>%
  group_by(Intervention_New) %>%
  mutate(LastStep=max(NormativeOrder, na.rm=T) + 1) %>%
  mutate(y=1, x=NormativeOrder) %>%
  mutate(xmin = x - 0.22,
         xmax = x + 0.35,
         ymin = case_when(Description_new=="Patient Arrives" ~ y - 0.1,
                          Description_new=="Patient Departs" ~ y - 0.1,
                          TRUE ~ y - 0.25),
         ymax = case_when(Description_new=="Patient Arrives" ~ y + 0.1,
                          Description_new=="Patient Departs" ~ y + 0.1,
                          TRUE ~ y + 0.25)) %>%
  arrange(Intervention_New, NormativeOrder, StaffDistribution) %>%
  group_by(Intervention_New, NormativeOrder) %>%
  mutate(StaffOrder= row_number(),
         CumSum=cumsum(StaffDistribution)) %>%
  mutate(Start=case_when(!is.na(StaffDistribution) & StaffOrder==1 ~ 0,
                         !is.na(StaffDistribution)~ lag(StaffDistribution)),
         End=CumSum) %>%
  mutate(Length=(xmax - xmin) * 0.8,
         Start_x = xmin + (Length * Start),
         End_x = xmin + (Length * End),
         Start_y=case_when(!is.na(StaffDistribution) ~ ymin -0.15)  ,
         End_y= case_when(!is.na(StaffDistribution) ~ ymin -0.05) ) %>%
  select(-Staff) %>%
  rename(Staff=Staff1_USAID) %>% 
  mutate(Patient="Normative") %>%
  mutate(Full_ID=paste(Intervention_New, ": ", Patient, sep="")) %>%
  select(Full_ID, Patient, Intervention_New,  Description_new,  Staff, 
         NormativeOrder, Start_x, End_x, Start_y , End_y )


# The piece of code below is calculating all other summary measures for our normative process map.  
# It calculates the number of people who received a step, the average time on a step, the average wait time
# Finally, it calculates all the information needed for plotting the graph
process_analytics <- input_long4 %>%
  group_by(Intervention_Full, Description)  %>%
  summarise(n_received=sum(one),
            ave_time=mean(time_spent),
            ave_time_waiting=mean(Waiting_Time))  %>%
  full_join(order, by=c("Intervention_Full", "Description")) %>%
  mutate(Location=gsub(" ", "\n", Location)) %>%
  filter(!is.na(Intervention_New)) %>%
  arrange(Intervention_New, NormativeOrder) %>%
  mutate(n_received=case_when(!is.na(n_received) ~ n_received,
                              is.na(n_received) ~ 0)) %>%
  full_join(total_patients, by=c("Intervention_New") )  %>%
  mutate(Percent_Received=n_received/Patient_N) %>%
  bind_rows(arrive_depart) %>%
  group_by(Intervention_New) %>%
  mutate(NormativeOrder=NormativeOrder + 1) %>%
  mutate(NSteps=max(NormativeOrder, na.rm=T)) %>%
  mutate(NormativeOrder=case_when(Description_new=="Patient Arrives" ~ 1,
                                  Description_new=="Patient Departs" ~ NSteps + 1,
                                  TRUE ~ NormativeOrder)) %>%
  arrange(Intervention_New, NormativeOrder) %>%
  group_by(Intervention_New) %>% mutate(LastStep=max(NormativeOrder, na.rm=T)) %>% ungroup() %>%
  mutate(y=1, x=NormativeOrder) %>%
  mutate(xmin = x - 0.35,
         xmax = x + 0.35,
         ymin = case_when(Description_new=="Patient Arrives" ~ y - 0.1,
                          Description_new=="Patient Departs" ~ y - 0.1,
                          TRUE ~ y - 0.25),
         ymax = case_when(Description_new=="Patient Arrives" ~ y + 0.1,
                          Description_new=="Patient Departs" ~ y + 0.1,
                          TRUE ~ y + 0.25)) %>%
  mutate(cirle_x=case_when(NormativeOrder==1 ~ NA ,
                           NormativeOrder==LastStep ~ NA,
                           TRUE ~ xmax),
         circle_y=case_when(NormativeOrder==1 ~ NA ,
                            NormativeOrder==LastStep ~ NA,
                            TRUE ~ ymin)) %>%
  mutate(labcircle_x=case_when(NormativeOrder==1 ~ NA ,
                               NormativeOrder==LastStep ~ NA,
                               TRUE ~ xmin),
         labcircle_y=case_when(NormativeOrder==1 ~ NA ,
                               NormativeOrder==LastStep ~ NA,
                               TRUE ~ ymin)) %>%
  mutate(normcirle_x=case_when(!is.na(NormativeTime) ~  xmax,
                               is.na(NormativeTime) ~ NA),
         normcircle_y=case_when(!is.na(NormativeTime) ~  ymax,
                                is.na(NormativeTime) ~ NA)) %>%
  mutate(arrow_start= case_when(NormativeOrder!=LastStep ~ xmax+0.01) ,
         arrow_end = case_when(NormativeOrder!=LastStep ~ xmax+ .28),
         arrow_start_y=case_when(NormativeOrder!=LastStep ~ 1),
         arrow_end_y=case_when(NormativeOrder!=LastStep ~ 1)) %>%
  mutate(arrow_x_textloc=(arrow_end + arrow_start)/2,
         arrow_y_textloc=1.08)  %>%
  mutate(arrow_x_time=(arrow_end + arrow_start)/2,
         arrow_y_time=.92)  %>%
  group_by(Intervention_New)  %>%
  mutate(Patient_Received_Lead=case_when(NormativeOrder==LastStep ~ "",
                                         NormativeOrder==LastStep-1 ~ "100%",
                                         TRUE ~ paste(round(lead(Percent_Received)*100),"%", sep="")),
         Waiting_Lead=case_when(NormativeOrder==LastStep ~ "",
                                NormativeOrder==LastStep-1 ~ "",
                                TRUE ~ paste(round(lead(ave_time_waiting)),"\n min", sep="")),
         Waiting_Lead=case_when(Waiting_Lead== "NA\n min"  ~ "",
                                TRUE ~ Waiting_Lead),
         Waiting_Lead=str_remove_all(Waiting_Lead, "min"),
         Waiting_Lead=str_remove_all(Waiting_Lead, "[\r\n]"),
         Waiting_Lead=str_trim(Waiting_Lead)) %>%
  mutate(Staff=case_when(Description_new=="Patient Arrives" ~ "Patient Arrives",
                         Description_new=="Patient Departs" ~ "Patient Departs",
                         TRUE ~ Staff)) %>%
  mutate(Step_Local1 = case_when(NormativeOrder!=1 & NormativeOrder!=LastStep ~ (paste("Step ", NormativeOrder-1, sep=""))),
         Step_Local2=case_when(NormativeOrder!=1 & NormativeOrder!=LastStep ~ Location)) %>%
  mutate(Step_Local1_y = y + .5,
         Step_Local2_y =y +  .35) %>% 
  mutate(ave_time =case_when(Description_new=="Patient Arrives" ~ "",
                             Description_new=="Patient Departs" ~ "",
                             round(ave_time)==0 ~ "0 min",
                             round(ave_time)==1 ~ "1 min",
                             ave_time>1 ~ paste(round(ave_time), "min", sep=" ")))  %>%
  mutate(ave_time=str_remove_all(ave_time, "min"),
         ave_time=str_remove_all(ave_time, "[\r\n]"),
         ave_time=str_trim(ave_time),
         ave_time=as.numeric(as.character(ave_time))) %>% 
  mutate(Patient="Normative") %>%
  mutate(Full_ID=paste(Intervention_New, ": ", Patient, sep="")) %>%
  select(Full_ID, Patient, Intervention_New,  Description_new,  Staff, 
         NormativeOrder, NormativeTime, ave_time, Patient_Received_Lead,  Waiting_Lead,
         x, xmin , xmax, ymin, ymax,
         arrow_start,  arrow_end,  arrow_start_y, arrow_end_y, arrow_x_time, arrow_y_time,
         arrow_x_textloc,  arrow_y_textloc,
         cirle_x, circle_y, labcircle_x, labcircle_y, normcirle_x, normcircle_y,
         Step_Local1, Step_Local2, Step_Local1_y , Step_Local2_y )

##################################################################################################################################

##################################################################################################################################
# # # # # # # 
# Section 9 #
# # # # # # #

# We would also like to create visualizations for individual patients, in order to visualize how their process departs from the norm

# For each patient, we create a step for arrival and departure, including times
patient_arrive_depart <- input_long0 %>% select(Patient:Patient_Classification, Intervention_Full) %>%
  group_by(Patient) %>%
  filter(row_number()==1) %>%
  mutate(Arrive=Arrival_Time,
         Depart=Departure_Time) %>% 
  gather(Description_new, Value, Arrive:Depart) %>%
  mutate(StartTime=as.POSIXct(strptime(Value, "%H:%M:%S")), EndTime=as.POSIXct(strptime(Value, "%H:%M:%S"))) %>%
  mutate(time_spent = difftime(EndTime, StartTime, units = "mins")) %>%
  select(-Value) %>% arrange(Patient) %>%
  left_join(intervention_new_full, by=c("Intervention_Full"))   %>%
  filter(!is.na(Intervention_New))  %>%
  select(Patient, Facility, Arrival_Time, Departure_Time, Intervention_New, Description_new, time_spent, StartTime, EndTime) %>%
  mutate(Description_new=case_when(Description_new=="Arrive" ~ "Patient Arrives", Description_new=="Depart" ~ "Patient Departs")) %>%
  mutate(Staff1_USAID=Description_new)

# Next we want to clean any staff descriptions, as we did above
# For a list of all staff in the dataset, run:
levels(as.factor(input_long0$Staff1))

# We bring in the steps for arrival and departure
# Finally, it calculates all the information needed for plotting the graph

patients_long <- input_long0 %>%
  mutate(Staff1_USAID=case_when(Staff1==  "Enrolled Nurse" | Staff1== "Registered Nurse" ~ "Nurse",
                                Staff1== "Community Health worker" ~ "CHW",
                                Staff1== "Data Clerk"    ~ "Data Clerk",
                                Staff1==  "Receptionist"   ~ "Receptionist",
                                Staff1== "Counsellor"      ~ "Counselor",
                                Staff1== "Pharmacist"   ~ "Pharmacist",
                                Staff1=="Health Assistant"    |  Staff1=="TCE Field Worker"  ~ "Assistant Medical Officer",
                                Staff1== "Pharmacist Assistant"   ~ "Pharmacy Technician",
                                Staff1== "Student Nurse"  ~ "Cadre x")) %>%
  left_join(intervention_new_full, by=c("Intervention_Full")) %>%
  filter(!is.na(Intervention_New)) %>%
  left_join(intervention_description, by=c("Intervention_New", "Description")) %>% # if I have a clean new name, using that, if not, using original name
  mutate(Description_new=case_when(!is.na(Description_new) ~ Description_new,
                                   is.na(Description_new) ~ Description)) %>%
  left_join(order_normatative, by=c("Intervention_New", "Description_new")) %>%
  select(Patient, Facility, Arrival_Time, Departure_Time,  Intervention_New, Description_new, Staff1_USAID, 
         Step, Location, time_spent, time_spent_0, StartTime, EndTime, NormativeOrder, NormativeTime, NormativeLocation, NormativeStaff) %>%
  bind_rows(patient_arrive_depart)  %>%
  group_by(Patient) %>% arrange(Patient, Step) %>% mutate(NSteps=max(Step, na.rm=T)) %>%
  mutate(Step=case_when(Description_new=="Patient Arrives" ~ 0,
                        Description_new=="Patient Departs" ~ NSteps + 1,
                        TRUE ~ Step)) %>%
  mutate(Step= Step + 1) %>%
  arrange(Patient, Step) %>%
  mutate(Step_Local1 = case_when( !is.na(NormativeOrder) ~ (paste("Normative Step ", NormativeOrder, sep=""))),
         Step_Local2 = Location) %>%
  mutate(NextStep_Start=lead(StartTime)) %>% ungroup() %>%
  mutate(time_to_next_step =round(difftime(NextStep_Start, EndTime, units = "mins")) ) %>%
  mutate(Waiting_Lead=paste(round(time_to_next_step),"\n min", sep=""),
         Waiting_Lead=case_when(Waiting_Lead== "NA\n min"  ~ "",
                                TRUE ~ Waiting_Lead),
         Waiting_Lead=str_remove_all(Waiting_Lead, "min"),
         Waiting_Lead=str_remove_all(Waiting_Lead, "[\r\n]"),
         Waiting_Lead=str_trim(Waiting_Lead)) %>% 
  mutate(ave_time =case_when(Description_new=="Patient Arrives" ~ "",
                             Description_new=="Patient Departs" ~ "",
                             round(time_spent)==0 ~ "0 min",
                             round(time_spent)==1 ~ "1 min",
                             time_spent>1 ~ paste(round(time_spent), "min", sep=" ")))  %>%
  mutate(ave_time=str_remove_all(ave_time, "min"),
         ave_time=str_remove_all(ave_time, "[\r\n]"),
         ave_time=str_trim(ave_time),
         ave_time=as.numeric(as.character(ave_time))) %>%
  rename(Staff=Staff1_USAID) %>%
  mutate(y=1, x=Step) %>%
  mutate(xmin = x - 0.35,
         xmax = x + 0.35,
         ymin = case_when(Description_new=="Patient Arrives" ~ y - 0.1,
                          Description_new=="Patient Departs" ~ y - 0.1,
                          TRUE ~ y - 0.25),
         ymax = case_when(Description_new=="Patient Arrives" ~ y + 0.1,
                          Description_new=="Patient Departs" ~ y + 0.1,
                          TRUE ~ y + 0.25)) %>%
  mutate(Step_Local1_y = y + .5,
         Step_Local2_y =y +  .35) %>%
  mutate(arrow_start= case_when(Description_new!="Patient Departs"  ~ xmax+0.01) ,
         arrow_end = case_when(Description_new!="Patient Departs"  ~ xmax+ .28),
         arrow_start_y=case_when(Description_new!="Patient Departs"  ~ 1),
         arrow_end_y=case_when(Description_new!="Patient Departs"  ~ 1)) %>%
  mutate(arrow_x_textloc=(arrow_end + arrow_start)/2,
         arrow_y_textloc=1.08)  %>%
  mutate(arrow_x_time=(arrow_end + arrow_start)/2,
         arrow_y_time=.92)  %>%
  mutate(cirle_x=case_when(Description_new=="Patient Arrives"  ~ NA ,
                           Description_new=="Patient Departs"  ~ NA,
                           TRUE ~ xmax),
         circle_y=case_when(Description_new=="Patient Arrives"  ~ NA ,
                            Description_new=="Patient Departs" ~ NA,
                            TRUE ~ ymin)) %>%
  mutate(labcircle_x=case_when(Description_new=="Patient Arrives"  ~ NA ,
                               Description_new=="Patient Departs" ~ NA,
                               TRUE ~ xmin),
         labcircle_y=case_when(Description_new=="Patient Arrives"  ~ NA ,
                               Description_new=="Patient Departs" ~ NA,
                               TRUE ~ ymin)) %>%
  mutate(normcirle_x=case_when(!is.na(NormativeTime) ~  xmax,
                               is.na(NormativeTime) ~ NA),
         normcircle_y=case_when(!is.na(NormativeTime) ~  ymax,
                                is.na(NormativeTime) ~ NA)) %>%
  mutate(Patient_Received_Lead=NA)  %>%
  mutate(Patient=paste("Patient ", Patient, sep="")) %>%
  mutate(Full_ID=paste(Intervention_New, ": ", Patient, sep="")) %>%
  select(Full_ID, Patient, Intervention_New,  Description_new,  Staff, 
         NormativeOrder, NormativeTime, ave_time, Patient_Received_Lead,  Waiting_Lead,
         x, xmin , xmax, ymin, ymax,
         arrow_start,  arrow_end,  arrow_start_y, arrow_end_y, arrow_x_time, arrow_y_time,
         arrow_x_textloc,  arrow_y_textloc,
         cirle_x, circle_y, labcircle_x, labcircle_y, normcirle_x, normcircle_y,
         Step_Local1, Step_Local2, Step_Local1_y , Step_Local2_y ) %>%
  mutate(Step_Local2=gsub(" ", "\n", Step_Local2))

# We create a dataset which lists all of our patients and their interventions, which we will use later in this code
patient_staff_na <- patients_long %>% group_by(Patient) %>%
  filter(row_number()==1) %>%
  select(Full_ID, Patient, Intervention_New) %>%
  mutate(Description_new=NA,  Staff="Nurse", 
         NormativeOrder=NA, Start_x=1, End_x=1, Start_y=1 , End_y=1 )

##################################################################################################################################

##################################################################################################################################
# # # # # #  # 
# Section 10 #
# # # # # #  #

# Combo of Normative and Patients
# We want to plot normative graphics with individual patients.  The code creates a dataset with information for both types of graphics below

# It gives each map (normative or patient) a unique ID.  The code is going to graph the normative map first and the patients underneath
combo_ids <- bind_rows(process_analytics, patients_long)  %>% group_by(Intervention_New, Patient)  %>%
  filter(row_number()==1) %>%
  select(Patient, Intervention_New) %>%
  ungroup() %>%
  group_by(Intervention_New) %>%
  mutate(id = (row_number() - 1)* -1) 

# Next the code adds labels which will be in front of each process map
combo_labels <- combo_ids %>%
  mutate(xmin= -0.1,
         xmax=0.50, 
         ymin= 0.75 +  id,
         ymax=1.25 +  id,
         Staff="",
         Description_new=Patient) %>%
  mutate(Description_new=case_when(Description_new=="Normative" ~ "National Norms and Facility Averages",
                                   TRUE ~ Description_new)) %>%
  mutate(ymin=case_when(Patient=="Normative" ~ ymin,
                        TRUE ~ ymin - 0.5),
         ymax=case_when(Patient=="Normative" ~ ymax,
                        TRUE ~ ymax - 0.5))

# Finally, it calculates all the information needed for plotting the graph
combo_data <- process_analytics %>% 
  mutate(Step_Local1=case_when(!is.na(Step_Local1) ~ paste("Normative ", Step_Local1, sep=""),
                               TRUE ~ Step_Local1)) %>%
  bind_rows( patients_long) %>%
  left_join(combo_ids, by=c("Intervention_New", "Patient")) %>%
  mutate(ymin= ymin + id,
         ymax= ymax + id,
         arrow_start_y = arrow_start_y + id,
         arrow_end_y = arrow_end_y + id,
         arrow_y_textloc  = arrow_y_textloc + id,
         circle_y = circle_y + id,
         labcircle_y= labcircle_y +  id,
         normcircle_y = normcircle_y + id,
         Step_Local1_y= Step_Local1_y + id,
         Step_Local2_y= Step_Local2_y + id,
         arrow_y_time = arrow_y_time + id) %>%
  mutate(ymin=case_when(Patient=="Normative" ~ ymin,
                        TRUE ~ ymin - 0.5),
         ymax=case_when(Patient=="Normative" ~ ymax,
                        TRUE ~ ymax - 0.5),
         arrow_start_y=case_when(Patient=="Normative" ~ arrow_start_y,
                                 TRUE ~ arrow_start_y - 0.5),
         arrow_end_y=case_when(Patient=="Normative" ~ arrow_end_y,
                               TRUE ~ arrow_end_y - 0.5),
         arrow_y_textloc=case_when(Patient=="Normative" ~ arrow_y_textloc,
                                   TRUE ~ arrow_y_textloc - 0.5),
         circle_y=case_when(Patient=="Normative" ~ circle_y,
                            TRUE ~ circle_y - 0.5),
         labcircle_y=case_when(Patient=="Normative" ~ labcircle_y,
                               TRUE ~ labcircle_y - 0.5),
         normcircle_y=case_when(Patient=="Normative" ~ normcircle_y,
                                TRUE ~ normcircle_y - 0.5),
         Step_Local1_y=case_when(Patient=="Normative" ~ Step_Local1_y,
                                 TRUE ~ Step_Local1_y - 0.5),
         Step_Local2_y=case_when(Patient=="Normative" ~ Step_Local2_y,
                                 TRUE ~ Step_Local2_y - 0.5),
         arrow_y_time=case_when(Patient=="Normative" ~ arrow_y_time,
                                TRUE ~ arrow_y_time - 0.5))

# The code combines the information on staff from the normative and patient datesets
combo_data_staff <- bind_rows(process_analytics_staff, patient_staff_na) %>%
  left_join(combo_ids, by=c("Intervention_New", "Patient"))  %>%
  mutate(Start_y= Start_y + id,
         End_y= End_y + id)
##################################################################################################################################

##################################################################################################################################
# # # # # #  # 
# Section 11 #
# # # # # #  #

# Making multiple rows that are 5 boxes wide

# Patients may experience many steps when receiving a service.  
# For ease of visualization, we want to control how many steps are in each row of our graphics.  
# Here the default is set to 5, but it can be edited in the line below

# You can change the width here
break_n <- 5

# This code changes the plotting locations of steps in the process map so the steps appear on multiple rows
process_analytics_multirows <- process_analytics %>%
  group_by(Intervention_New) %>%
  mutate(arrow_start=lag(arrow_start),
         arrow_end=lag(arrow_end),
         arrow_start_y=lag(arrow_start_y),
         arrow_end_y=lag(arrow_end_y),
         arrow_x_textloc=lag(arrow_x_textloc),
         arrow_y_textloc=lag(arrow_y_textloc),
         Patient_Received_Lead=lag(Patient_Received_Lead),
         Waiting_Lead=lag(Waiting_Lead),
         arrow_x_time=lag(arrow_x_time),
         arrow_y_time=lag(arrow_y_time)) %>% # lagging all arrow data so it will go in front of event instead of behind event
  mutate(Rowsnumber=(NormativeOrder-1) %/%  break_n ,
         Colsnumber=(NormativeOrder-1) %%   break_n) %>%
  mutate(row= Rowsnumber * -1) %>%
  mutate(x= x + (row * break_n),
         xmin= xmin + (row * break_n),
         xmax= xmax + (row * break_n),
         arrow_start= arrow_start + (row * break_n),
         arrow_end= arrow_end + (row * break_n),
         arrow_x_textloc= arrow_x_textloc + (row * break_n),
         cirle_x= cirle_x + (row * break_n),
         normcirle_x= normcirle_x + (row * break_n),
         labcircle_x= labcircle_x + (row * break_n),
         arrow_x_time = arrow_x_time + (row * break_n),
         ymin= ymin + row,
         ymax= ymax + row,
         arrow_start_y = arrow_start_y + row,
         arrow_end_y = arrow_end_y + row,
         arrow_y_textloc  = arrow_y_textloc + row,
         circle_y = circle_y + row,
         labcircle_y= labcircle_y +  row,
         normcircle_y = normcircle_y + row,
         Step_Local1_y= Step_Local1_y + row,
         Step_Local2_y= Step_Local2_y + row,
         arrow_y_time = arrow_y_time + row)

process_analytics_staff_multirows <- process_analytics_staff %>%
  mutate(Rowsnumber=(NormativeOrder-1) %/%  break_n ,
         Colsnumber=(NormativeOrder-1) %%   break_n) %>%
  mutate(row= Rowsnumber * -1) %>%
  mutate(Start_x= Start_x + (row * break_n),
         End_x= End_x + (row * break_n),
         Start_y= Start_y + row,
         End_y= End_y + row)
##################################################################################################################################


##################################################################################################################################
# # # # # #  # 
# Section 12 #
# # # # # #  #

# For our legends, we set a color for each staff member (and a coordinating border color)

staff_color_fill <- c("Nurse"="#c7e8ac", "Community Health Worker"="#b391b5", "Data Clerk"="#3aa6dd", "Lab Technician"="#fcc438",
                      "Medical Attendant"="#ef8d22", "Clinical Officer"="#f5b5c8", "Receptionist"="#c1e4f7", "Counselor"="#ffbbb1",
                      "Doctor"="#7ab648", "Pharmacist"="#de5f85", "Social Worker"="#83bbe5", "Assistant Medical Officer"="#99d5ca", "Pharmacy Technician"="#CE93D8", 
                      "Cadre x"="#E6E89C", "Cadre y"="#A9F9C9", "Cadre z"="#BBA4F6", "Patient Arrives"="#ced4db", "Patient Departs" ="#ced4db")

staff_color_border <- c("Nurse"="#7ab648",  "Community Health Worker"="#834187", "Data Clerk"="#0c7cba", "Lab Technician"="#ef8d22", "Medical Attendant"="#fcc438",
                        "Clinical Officer"="#de5f85", "Receptionist"="#3aa6dd",   "Counselor"="#c92d39", "Doctor"="#19967d",
                        "Pharmacist"="#c92d39", "Social Worker"="#0c4cba", "Assistant Medical Officer"="#19967d",  "Pharmacy Technician"="#80338D",
                        "Cadre x"="#A8AB27", "Cadre y"="#0DBF55", "Cadre z"="#4513C3", "Patient Arrives"="#6f7681", "Patient Departs" ="#6f7681")
##################################################################################################################################

##################################################################################################################################
# # # # # #  # 
# Section 13 #
# # # # # #  #

# Graphic details can be edited below

arrow_size <- .1
arrow_type <- "open"
circle_size <- .1
##################################################################################################################################

##################################################################################################################################
# # # # # #  # 
# Section 14 #
# # # # # #  #

# We are now editing our legends.  
# The code below defines their size and adds a dotted verticle line which will visually separate them from the process maps

legend_norm <-   ggplot() +
  annotation_raster(periodic_norm, xmin = 0 , xmax = nrom_LEG_width , ymin = 0, ymax = norm_LEG_height) +
  geom_vline(xintercept=0, linetype="dashed") +
  ylim(0, norm_LEG_height) +
  xlim(0, nrom_LEG_width ) +
  theme_void() +
  theme(legend.position = "none")

legend_norm


legend_norm_pat <- ggplot() +
  annotation_raster(periodic_normpat, xmin = 0 , xmax = normpat_LEG_width, ymin = 0, ymax = normpat_LEG_height) +
  geom_vline(xintercept=0, linetype="dashed") +
  ylim(0, normpat_LEG_height) +
  xlim(0, normpat_LEG_width) +
  theme_void() +
  theme(legend.position = "none")

legend_norm_pat
#############################################################################################

#############################################################################################
# # # # # #  # 
# Section 15 #
# # # # # #  #

# Lists to loop through

# Make sure in your working directory you create a folder called "Loop Results"

# This piece of code creates a dataset with a row per intervention.  It records how many patients receive each intervention and the maximum number of steps in an intervention
patients_per_intervention <- patients_long %>% group_by(Patient) %>%
  summarise(Patient=first(Patient), Intervention_New=first(Intervention_New), Boxes=max(x)) %>% 
  mutate(n=1) %>% ungroup() %>% group_by(Intervention_New) %>%
  summarise(NPatients=sum(n), NPatientBoxes=max(Boxes))

# This piece of code calculates the needed dimensions and the filename of each graph  that will be produced in the loop below
normative_list  <- process_analytics %>% group_by(Intervention_New) %>%
  summarise(Full_ID=first(Full_ID), Intervention_New=first(Intervention_New),
            Boxes=max(NormativeOrder)) %>%
  full_join(patients_per_intervention, by="Intervention_New") %>% 
  mutate(NPatientBoxes=case_when(NPatientBoxes>Boxes ~ NPatientBoxes,
                                 Boxes > NPatientBoxes ~ Boxes,
                                 Boxes == NPatientBoxes ~ NPatientBoxes)) %>%
  mutate(SingleWidth=Boxes * 2) %>% 
  mutate(MultiRows=ceiling(Boxes/break_n),
         MultiCols= break_n,
         MultiHeight=MultiRows * 3,
         MultiWidth=MultiCols * 2,
         NorPatHeight=(NPatients + 1) * 3,
         NorPatWidth=NPatientBoxes * 2) %>%
  mutate(MultiFile=paste("Loop Results/", Intervention_New, " ", facility, " Multi Row ", date, ".pdf", sep=""),
         NPFile=paste("Loop Results/", Intervention_New, " ", facility, " NormPatient Row ", date, ".pdf", sep=""))

#############################################################################################

#############################################################################################
# # # # # #  # 
# Section 16 #
# # # # # #  #

# This loop will produce all your normative graphics

# You must highlight and run the entire loop at the same time- from "for (row in 1:nrow(normative_list)) {" until "}"
for (row in 1:nrow(normative_list)) {
  
  # First, we store each piece of information we need from the normative list file as a value
  intervention_n <- normative_list[row, "Intervention_New"]
  intervention_value <- intervention_n$Intervention_New[1]
  
  # # # # # # # # # # # # # # # # # # # # # # # # 
  multi_file <- normative_list[row, "MultiFile"]
  multi_file_name <- multi_file$MultiFile[1]
  
  multi_width <- normative_list[row, "MultiWidth"]
  multi_width_value <- multi_width$MultiWidth[1]
  
  multi_height <- normative_list[row, "MultiHeight"]
  multi_height_value <- multi_height$MultiHeight[1]
  
  # # # # # # # # # # # # # # # # # # # # # # # # 
  
  normpat_file <- normative_list[row, "NPFile"]
  normpat_file_name <- normpat_file$NPFile[1]
  
  normpat_width <- normative_list[row, "NorPatWidth"]
  normpat_width_value <- normpat_width$NorPatWidth[1]
  
  normpat_height <- normative_list[row, "NorPatHeight"]
  normpat_height_value <- normpat_height$NorPatHeight[1]
  
  # # # # # # # # # # # # # # # # # # # # # # # # 

  # Next, we filter our larger datasets to the information we want in each graphic
  
  intervention_multi_data <- process_analytics_multirows %>% filter(Intervention_New==intervention_value )
  intervention_multi_staff <- process_analytics_staff_multirows  %>% filter(Intervention_New==intervention_value) 
  
  intervention_normpat_data <- bind_rows(combo_data, combo_labels) %>% filter(Intervention_New==intervention_value)
  intervention_normpat_staff <- combo_data_staff  %>% filter(Intervention_New==intervention_value) 
  

  # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Here we plot our normative graphic for each intervention
  
  multirows <- ggplot(data = intervention_multi_data,
                      mapping = aes(xmin = xmin, ymin = ymin, 
                                    xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
    geom_rect(data=subset(intervention_multi_data, Description_new!="Patient Arrives" & Description_new!="Patient Departs")) +
    ggchicklet:::geom_rrect(data=subset(intervention_multi_data, Description_new=="Patient Arrives" | Description_new=="Patient Departs")) +
    scale_fill_manual(values=staff_color_fill,  na.value ="white") +
    scale_color_manual(values=staff_color_border, guide = "none") +
    geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
    geom_segment(data = intervention_multi_data,
                 mapping = aes(x = arrow_start, y = arrow_start_y, 
                               xend = arrow_end, yend = arrow_end_y),
                 arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
    geom_text(data=intervention_multi_data, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333") +
    geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
    geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
    geom_text(data = intervention_multi_data, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F) +
    geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
    geom_text(data = intervention_multi_data, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F) +
    geom_text(data=intervention_multi_data, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold") +
    geom_text(data=intervention_multi_data, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=3) +
    geom_rect(data=intervention_multi_staff, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
    geom_text(data=intervention_multi_data, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=4) +
    labs(title= "Process Map - National Norms and Facility Averages" , subtitle=paste(facility, " - ", intervention_value, sep="" )) +
    theme_void() +
    theme(legend.position = "none")
  
  multirows
  
  # Now, we combine the graphic with the legend (and adjust the sizes)
  output_1 <- ggdraw() +
    draw_plot(multirows,  x = 0, y = 0, width = (multi_width_value/(multi_width_value + (multi_height_value * (nrom_LEG_width /norm_LEG_height)))), height = 1) +
    draw_plot(legend_norm, x=(multi_width_value/(multi_width_value + (multi_height_value * (nrom_LEG_width /norm_LEG_height)))), y=0, width=1-(multi_width_value/(multi_width_value + (multi_height_value * (nrom_LEG_width /norm_LEG_height)))), height=1)

  
    multi.page <- ggarrange(output_1 ,  nrow=1,ncol=1) 
  
  # Finally, we export the graphic to our results folder
  ggexport(multi.page, filename=multi_file_name, width=(multi_width_value + (multi_height_value * (nrom_LEG_width /norm_LEG_height))), height=multi_height_value)
  
  # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # Now we are creating are normative plus patients maps
  normpatient <- 
    ggplot(data = intervention_normpat_data,
           mapping = aes(xmin = xmin, ymin = ymin, 
                         xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
    geom_rect(data=subset(intervention_normpat_data, Description_new!="Patient Arrives" & Description_new!="Patient Departs")) +
    ggchicklet:::geom_rrect(data=subset(intervention_normpat_data, Description_new=="Patient Arrives" | Description_new=="Patient Departs")) +
    scale_fill_manual(values=staff_color_fill,  na.value ="white") +
    scale_color_manual(values=staff_color_border, guide = "none") +
    geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
    geom_segment(data = intervention_normpat_data,
                 mapping = aes(x = arrow_start, y = arrow_start_y, 
                               xend = arrow_end, yend = arrow_end_y),
                 arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
    geom_text(data=intervention_normpat_data, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333") +
    geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
    geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
    geom_text(data = intervention_normpat_data, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F) +
    geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
    geom_text(data = intervention_normpat_data, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F) +
    geom_text(data=intervention_normpat_data, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold", size=3) +
    geom_text(data=intervention_normpat_data, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=3) +
    geom_rect(data=intervention_normpat_staff, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
    geom_text(data=intervention_normpat_data, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=4) +
    geom_hline(yintercept= 0.45) +
    labs(title= paste("Process Map - National Norms, Facility Averages, and Patient Observations\n", facility, " - ", intervention_value, sep="") ) +
    theme_void() +
    theme(legend.position = "none")
  
  normpatient

  # Now, we combine the graphic with the legend (and adjust the sizes)
  output_1 <- ggdraw() +
    draw_plot(normpatient,  x = 0, y = 0, width =(normpat_width_value/(normpat_width_value + (normpat_height_value * (normpat_LEG_width/normpat_LEG_height)))) , height =  1) +
    draw_plot(legend_norm_pat, x=(normpat_width_value/(normpat_width_value + (normpat_height_value * (normpat_LEG_width/normpat_LEG_height)))), y=0, width=(1-((normpat_width_value/(normpat_width_value + (normpat_height_value * (normpat_LEG_width/normpat_LEG_height)))))), height=1)
  
  multi.page <- ggarrange(output_1 ,  nrow=1,ncol=1) 
  
  # Finally, we export the graphic to our results folder
  ggexport(multi.page, filename=normpat_file_name, width= (normpat_width_value + (normpat_height_value * (normpat_LEG_width/normpat_LEG_height))), height=normpat_height_value)
  
  # # # # # # # # # # # # # # # # # # # # # # # # 
  
  
}
#############################################################################################

