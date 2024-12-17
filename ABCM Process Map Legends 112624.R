##########################################################################################################
# ABCM Process Maps

# Creating Legend Graphics

# Anonymous Data
# November 21, 2024

# Kristin Bietsch, PhD
# Avenir Health
#######################################################################################################

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

# Set working directory to where your data is stored on your computer
setwd("C:/Users/KristinBietsch/files/ABCM/Process Maps 110424")

############################################################

date <- format(Sys.Date(), "%m%d%Y")

############################################################

############################################################
# Edit the facility name for your facility
facility <- "Anonymous Facility"
############################################################
# For our legends, we set a color for each staff member (and a coordinating border color)

staff_color_fill <- c("Nurse"="#c7e8ac", "Community Health Worker"="#b391b5", "Data Clerk"="#3aa6dd", "Lab Technician"="#fcc438",
                      "Medical Attendant"="#ef8d22", "Clinical Officer"="#f5b5c8", "Receptionist"="#c1e4f7", "Counselor"="#ffbbb1",
                      "Doctor"="#7ab648", "Pharmacist"="#de5f85", "Social Worker"="#83bbe5", "Assistant Medical Officer"="#99d5ca", "Pharmacy Technician"="#CE93D8", 
                      "Cadre x"="#E6E89C", "Cadre y"="#A9F9C9", "Cadre z"="#BBA4F6", "Patient Arrives"="#ced4db", "Patient Departs" ="#ced4db")

staff_color_border <- c("Nurse"="#7ab648",  "Community Health Worker"="#834187", "Data Clerk"="#0c7cba", "Lab Technician"="#ef8d22", "Medical Attendant"="#fcc438",
                        "Clinical Officer"="#de5f85", "Receptionist"="#3aa6dd",   "Counselor"="#c92d39", "Doctor"="#19967d",
                        "Pharmacist"="#c92d39", "Social Worker"="#0c4cba", "Assistant Medical Officer"="#19967d",  "Pharmacy Technician"="#80338D",
                        "Cadre x"="#A8AB27", "Cadre y"="#0DBF55", "Cadre z"="#4513C3", "Patient Arrives"="#6f7681", "Patient Departs" ="#6f7681")
##################################################################################################################################33

# Graphic details can be edited below

arrow_size <- .1
arrow_type <- "open"
circle_size <- .1
##################################################################################################################################33
# We are now creating our legend.  There are several options

giant_legend <- data.frame(staff_color_fill, staff_color_border) %>%   rownames_to_column("Provider") %>%
  filter(Provider!="Patient Arrives") %>%
  filter(Provider!="Patient Departs") %>%
  arrange(Provider) %>%
  mutate(x=1, y=row_number()) %>%
  mutate(y=17-y)

cadre <- c( "Cadre x", "Cadre y", "Cadre z")
giant_legend_cadre <- giant_legend %>% filter(!Provider %in% cadre) %>% select(-y) %>%
  mutate( y=row_number()) %>%
  mutate(y=14-y)

giant_legend_cadre_2 <- giant_legend_cadre %>% 
  arrange(-y) %>%
  mutate(newy=row_number()) %>%
  mutate(col=case_when(y>=7 ~ 1, TRUE ~ 2),
         row=case_when(newy<=7 ~ newy, TRUE ~ newy-7)) %>%
  mutate(xmin = case_when(col==1 ~ 7.75,
                          col==2 ~ 9),
         xmax=case_when(col==1 ~ 9,
                        col==2 ~ 10.25),
         ymax=0.4 - (row * (1.5/7)),
         ymin= ymax - (1.5/7) ) %>% mutate(Description_new="", Staff="")

ggplot() +
  geom_rect(data=giant_legend_cadre_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Provider, color=Provider)) +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text(data=giant_legend_cadre_2, aes(x=(xmax+xmin)/2, y=(ymax+ymin)/2, label=Provider), color = "#333333", reflow = TRUE, show.legend = F) 


giant_legend_cadre_patnorm <- giant_legend_cadre_2 %>% mutate(ymin=ymin-1.8, ymax=ymax-1.8 )
############################################################
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

# You now have a list of all the intervention names and step descriptions in your dataset.  You can export this information as a csv by uncommenting the line below.
# Now, you want to match up each Intervention_Full and Description  from your input data with the names used in the Normative order data set "Normative Process Map 111523 Clean.csv"
# You can copy and paste each line into columns G and H of "Normative Process Map 111523 Clean.csv"
# It is important that the strings match exactly, including spaces and capitalization  
# Columns G and H have data which matches the anonymized data used in this code, but you can remove that information and insert the information from your dataset.

#write.csv(list_intervention_description, "Input Data Intervention and Description.csv", row.names = F, na="")

############################################################


############################################################
# Normative Order

# In the CSV, "Normative Process Map 111523 Clean.csv", the first 6 columns represent each normative process.  
# Column A is the name of the intervention, 
# Column B is the step number
# Column C is the name of each step
# Column D is the time it is expected to take to complete each step
# Column E is the location in the health faciity the step is expected to take place
# Column F is the staff member who is expected to coduct each step

# Columns G and H should be edited using the information from the section above


order <- read.csv("Normative Process Map 111523 Clean.csv")
intervention_coding <- order %>% select(Intervention_New, Intervention_Full) %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_new_full <- order %>% select(Intervention_New, Intervention_Full) %>% filter(Intervention_Full!="") %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_description <- order %>% select(Intervention_New, Description_new, Description) %>% filter(Description!="")
order_normatative <- order %>% rename(NormativeLocation=Location, NormativeStaff=Staff) %>% select(-Intervention_Full, -Description)
############################################################


# This piece of code counts how many patients are recorded for each type of service
total_patients <- input_clean  %>%
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) %>%
  mutate(Patient_N=1) %>% group_by(Intervention_Full) %>% summarise(Patient_N=sum(Patient_N)) %>%
  left_join(intervention_coding, by="Intervention_Full") %>% filter(!is.na(Intervention_New)) %>%
  select(-Intervention_Full)

#################################################################################################
# We will now begin a series of data cleaning and data checks

# Frist, we reshape the data from wide to long.  Before, each patient had their own row, but now each row represents a step for a patient, so parients have multiple rows
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

# For example, we see that one description is recorded as "Weight/Vitals Taken" while anoter is "Vitals/Weight taken", we change one so that they match

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


# This next chunks of code creates summary statistics for each normative process
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
#################################################################################################


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

################################################################################################
# We just want one box for the normative legend and one for the patient
# For the patient data, we are changing the box's location so we can plot normative and patient on the same graph
################################################################################################
patient_single <- patients_long %>% filter(Patient=="Patient 21") %>% 
  filter(NormativeOrder==9 | NormativeOrder==10) %>%
  mutate(xmin=xmin+2,
         xmax=xmax+2,
         arrow_start=arrow_start+2,
         arrow_end=arrow_end+2,
         arrow_x_textloc=arrow_x_textloc+2,
         labcircle_x=labcircle_x+2,
         cirle_x=cirle_x+2,
         normcirle_x=normcirle_x+2,
         x=x+2,
         arrow_x_time=arrow_x_time+2) %>%
  mutate(ymin=ymin-1.8,
         ymax=ymax-1.8,
         arrow_start_y=arrow_start_y-1.8,
         arrow_end_y=arrow_end_y-1.8,
         arrow_y_textloc=arrow_y_textloc-1.8,
         labcircle_y=labcircle_y-1.8,
         circle_y=circle_y-1.8,
         normcircle_y=normcircle_y-1.8,
         Step_Local1_y=Step_Local1_y-1.8,
         Step_Local2_y=Step_Local2_y-1.8,
         arrow_y_time=arrow_y_time-1.8)


patient_box <- patient_single %>% filter( NormativeOrder==10)
patient_arrow <-patient_single %>% filter(NormativeOrder==9)


################################################################################################

process_analytics_single <- process_analytics %>% filter(Step_Local1=="Step 7" | Step_Local1=="Step 8") %>% filter(Staff=="Assistant Medical Officer")
process_box <- process_analytics_single %>% filter( Step_Local1=="Step 8")
process_arrow <-process_analytics_single %>% filter( Step_Local1=="Step 7")

staff_single <- process_analytics_staff %>% filter(Intervention_New=="HIV Testing") %>% filter(NormativeOrder==9)

staff_color_fill <- c("Nurse"="#c7e8ac", "Community Health Worker"="#b391b5", "Data Clerk"="#3aa6dd", "Lab Technician"="#fcc438",
                      "Medical Attendant"="#ef8d22", "Clinical Officer"="#f5b5c8", "Receptionist"="#c1e4f7", "Counselor"="#ffbbb1",
                      "Doctor"="#7ab648", "Pharmacist"="#de5f85", "Social Worker"="#83bbe5", "Assistant Medical Officer"="#99d5ca", "Pharmacy Technician"="#CE93D8", 
                      "Cadre x"="#E6E89C", "Cadre y"="#A9F9C9", "Cadre z"="#BBA4F6", "Patient Arrives"="#ced4db", "Patient Departs" ="#ced4db")

staff_color_border <- c("Nurse"="#7ab648",  "Community Health Worker"="#834187", "Data Clerk"="#0c7cba", "Lab Technician"="#ef8d22", "Medical Attendant"="#fcc438",
                        "Clinical Officer"="#de5f85", "Receptionist"="#3aa6dd",   "Counselor"="#c92d39", "Doctor"="#19967d",
                        "Pharmacist"="#c92d39", "Social Worker"="#0c4cba", "Assistant Medical Officer"="#19967d",  "Pharmacy Technician"="#80338D",
                        "Cadre x"="#A8AB27", "Cadre y"="#0DBF55", "Cadre z"="#4513C3", "Patient Arrives"="#6f7681", "Patient Departs" ="#6f7681")
arrow_size <- .1
arrow_type <- "open"
circle_size <- .1



#####################################################################################

# This code creates the legend for the normative process maps
ggplot(data = process_box,
       mapping = aes(xmin = xmin, ymin = ymin, 
                     xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
  geom_rect(data=subset(process_box, Description_new!="Patient Arrives" & Description_new!="Patient Departs"))  +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F, size=15 ) +
  geom_segment(data = process_arrow,
               mapping = aes(x = arrow_start, y = arrow_start_y, 
                             xend = arrow_end, yend = arrow_end_y),
               arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
  geom_text(data=process_arrow, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333", size=5) +
  geom_text(data=process_arrow, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="#333333", size=5) +
  geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
  geom_text(aes(x=labcircle_x, y=labcircle_y), label="15", color="#333333", size=6) +
  geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
  geom_text(data = process_box, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F , size=6) +
  geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
  geom_text(data = process_box, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F , size=6) +
  geom_text(data=process_box, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold", size=5) +
  geom_text(data=process_box, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=5)  +
  geom_rect(data=staff_single, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
  annotate("text", x=9 , y=1.68, label="Step Number", size=5) +
  annotate("text", x=8.25 , y=1.5, label="Normative\nLocation", hjust=1, size=5, vjust=1) +
  annotate("text", x=8.25 , y=1.2 , label="Percentage of\nPatients Observed\nReceving the Step", hjust=1, vjust=1, size=5) +
  annotate("text", x=8.25 , y=0.9, label="Observed Patient\nWait Time Between\nSteps", hjust=1, vjust=1, size=5) +
  annotate("text", x=8.25 , y=0.6, label="Observed Duration\nof Lab Specimen\nProcessing (in\nminutes)", hjust=1, vjust=1, size=5) +
  annotate("text", x=9 , y=.37, label="Distribution of\nObserved Step\nProviders (by color)", size=5) +
  annotate("text", x=9.75 , y=1.5, label="Normative Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=1.2 , label="Normative Provider\nof the Step (by\nColor)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=0.9, label="Step Name", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=0.6, label="Observed Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("segment", x = 8.28, y = 1.4, xend = 8.8, yend = 1.35,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = 1.1, xend = 8.4, yend = 1.08,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = .87, xend = 8.4, yend = .92,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = .6, xend = 8.5, yend = .72,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = .6, xend = 9.5, yend = .72,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 0.90, xend = 9.2, yend = 1,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 1.15, xend = 9.1, yend = 1.1,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 1.4, xend = 9.47, yend = 1.30,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9, y = 1.64, xend = 9, yend = 1.53,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9, y = .49, xend = 9, yend = .59,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  geom_rect(data=giant_legend_cadre_2, aes(xmin=xmin, xmax=xmax, ymin=ymin-.3, ymax=ymax-.3, fill=Provider, color=Provider)) +
  geom_text(data=giant_legend_cadre_2, aes(x=(xmax+xmin)/2, y=((ymax+ymin)/2)-.3, label=Provider), color = "#333333", size=6, show.legend = F) +
  annotate("text",  x=9 , y=1.8, label="Legend for Normative Process Maps with Client Average", size=7, fontface="bold") +
  annotate("text", x=9 , y= 0, label="Provider Colors and Names", size=7, fontface="bold") +
  ylim(-1.7, 1.81) +
  xlim(7.75 , 10.25) +
  theme_void()  +
  theme(legend.position = "none")

ggsave( "C:/Users/KristinBietsch/files/ABCM/Process Maps 110424/Legend Normative square6.png", height=15.444, width=11, units="in")

#################################################################################

#######################################################################################
# This code creates the legend for the normative process maps plus patient process maps

ggplot(data = process_box,
       mapping = aes(xmin = xmin, ymin = ymin, 
                     xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
  geom_rect(data=subset(process_box, Description_new!="Patient Arrives" & Description_new!="Patient Departs"))  +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F, size=15 ) +
  geom_segment(data = process_arrow,
               mapping = aes(x = arrow_start, y = arrow_start_y, 
                             xend = arrow_end, yend = arrow_end_y),
               arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
  geom_text(data=process_arrow, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333", size=5) +
  geom_text(data=process_arrow, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="#333333", size=5) +
  geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
  geom_text(aes(x=labcircle_x, y=labcircle_y), label="15", color="#333333", size=6) +
  geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
  geom_text(data = process_box, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F , size=6) +
  geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
  geom_text(data = process_box, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F , size=6) +
  geom_text(data=process_box, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold", size=5) +
  geom_text(data=process_box, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=5)  +
  geom_rect(data=staff_single, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
  annotate("text", x=9 , y=1.68, label="Step Number", size=5) +
  annotate("text", x=8.25 , y=1.5, label="Normative\nLocation", hjust=1, size=5, vjust=1) +
  annotate("text", x=8.25 , y=1.2 , label="Percentage of\nPatients Observed\nReceving the Step", hjust=1, vjust=1, size=5) +
  annotate("text", x=8.25 , y=0.9, label="Observed Patient\nWait Time Between\nSteps", hjust=1, vjust=1, size=5) +
  annotate("text", x=8.25 , y=0.6, label="Observed Duration\nof Lab Specimen\nProcessing (in\nminutes)", hjust=1, vjust=1, size=5) +
  annotate("text", x=9 , y=.37, label="Distribution of\nObserved Step\nProviders (by color)", size=5) +
  annotate("text", x=9.75 , y=1.5, label="Normative Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=1.2 , label="Normative Provider\nof the Step (by\nColor)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=0.9, label="Step Name", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=0.6, label="Observed Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("segment", x = 8.28, y = 1.4, xend = 8.8, yend = 1.35,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = 1.1, xend = 8.4, yend = 1.08,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = .87, xend = 8.4, yend = .92,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 8.28, y = .6, xend = 8.5, yend = .72,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = .6, xend = 9.5, yend = .72,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 0.90, xend = 9.2, yend = 1,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 1.15, xend = 9.1, yend = 1.1,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9.72, y = 1.4, xend = 9.47, yend = 1.30,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9, y = 1.64, xend = 9, yend = 1.53,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("segment", x = 9, y = .49, xend = 9, yend = .59,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  geom_rect(data = patient_box,
            mapping = aes(xmin = xmin, ymin = ymin, 
                          xmax = xmax, ymax = ymax,  fill=Staff, color=Staff)) +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text(data = patient_box, aes(xmin = xmin, ymin = ymin, 
                                        xmax = xmax, ymax = ymax, label = Description_new), color = "#333333", reflow = TRUE, show.legend = F, size=15 )+
  geom_segment(data = patient_arrow,
               mapping = aes(x = arrow_start, y = arrow_start_y, 
                             xend = arrow_end, yend = arrow_end_y),
               arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
  geom_text(data=patient_arrow, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333" , size=5) +
  geom_circle(data = patient_box,aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
  geom_circle(data = patient_box,aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e" ) +
  geom_text(data = patient_box, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333" , size=6, reflow = TRUE, show.legend = F) +
  geom_circle(data = patient_box,aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
  geom_text(data = patient_box, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", size=6 , reflow = TRUE, show.legend = F) +
  geom_text(data = patient_box, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold", size=5) +
  geom_text(data = patient_box, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=5) +
  geom_text(data=patient_arrow, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=5) +
  geom_text(data = patient_box,aes(x=labcircle_x, y=labcircle_y), label="5", color="#333333", size=6) +
  annotate("text", x=9 , y=(1.65-1.8), label="Step Number in the Normative Process", size=5) +
  annotate("text", x=8.25 , y=(1.45-1.8), label="Observed\nLocation", hjust=1, size=5, vjust=1)  +
  annotate("text", x=8.25 , y=(0.95-1.8) , label="Observed Patient\nWait Time between\nSteps (in minutes)", hjust=1, vjust=1, size=5) +
  annotate("text", x=8.25 , y=(0.7-1.8), label="Observed Duration\nof Lab Specimen\nProcessing (in\nminutes)", hjust=1, vjust=1, size=5) +
  annotate("text", x=9.75 , y=(1.45 -1.8), label="Normative Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=(1.2 -1.8), label="Observed Provider\nof the Step (by\nColor)", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=(0.95 -1.8), label="Step Name", hjust=0, vjust=1, size=5) +
  annotate("text", x=9.75 , y=(0.7 -1.8),  label="Observed Duration\nof the Step (in\nminutes)", hjust=0, vjust=1, size=5) +
  annotate("segment", x = 8.28, y =(1.4 -1.8) , xend = 8.8, yend = (1.35-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 8.28, y = (.9-1.8), xend = 8.4, yend = (.92-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 8.28, y = (.6-1.8), xend = 8.5, yend = (.72-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 9.72, y = (.6-1.8), xend = 9.5, yend = (.72-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 9.72, y = (0.94-1.8), xend = 9.2, yend = (1-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 9.72, y = (1.15-1.8), xend = 9.1, yend = (1.1-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 9.72, y = (1.4-1.8), xend = 9.47, yend = (1.30-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 9, y = (1.6-1.8), xend = 9, yend = (1.53-1.8),
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))  +
  geom_rect(data=giant_legend_cadre_patnorm, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Provider, color=Provider)) +
  geom_text(data=giant_legend_cadre_patnorm, aes(x=(xmax+xmin)/2, y=(ymax+ymin)/2, label=Provider), color = "#333333", size=6, show.legend = F) +
  annotate("text",  x=9 , y=1.8, label="Legend for Normative Process Maps with Client Average", size=7, fontface="bold") +
  annotate("text", x=9 , y= -0.03, label="Legend for Individual Client Process Maps", size=7, fontface="bold") +
  annotate("text", x=9 , y= -1.5, label="Provider Colors and Names", size=7, fontface="bold") +
  xlim(7.75 , 10.25) +
  ylim(-3.2, 1.81) +
  theme_void()  +
  theme(legend.position = "none")

ggsave( "C:/Users/KristinBietsch/files/ABCM/Process Maps 110424/Legend Patient Normative Combo Long.png", height= 22.044, width=11, units="in")

