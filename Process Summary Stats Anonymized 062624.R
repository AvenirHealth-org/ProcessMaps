# ABCM Process Maps
# Anonymous Data
# June 26, 2024

# Kristin Bietsch, PhD
# Avenir Health
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

#install.packages("ggchicklet", repos = "https://cinc.rud.is") 
#https://albert-rapp.de/posts/ggplot2-tips/11_rounded_rectangles/11_rounded_rectangles.html
library(ggchicklet)

setwd("C:/Users/KristinBietsch/files/Lori")

date <- format(Sys.Date(), "%m%d%Y")

############################################################
# Legend Information
periodic_norm <-  readPNG( "Normative Legend.png")
periodic_pat <- readPNG( "Patient Legend.png")
facility <- "Anonymous Facility"
############################################################

############################################################
# Normative Order
order <- read.csv("Normative Process Map 111523 Clean.csv")
intervention_coding <- order %>% select(Intervention_New, Intervention_Full) %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_new_full <- order %>% select(Intervention_New, Intervention_Full) %>% filter(Intervention_Full!="") %>% group_by(Intervention_New) %>% summarise(Intervention_Full=first(Intervention_Full))
intervention_description <- order %>% select(Intervention_New, Description_new, Description) %>% filter(Description!="")
order_normatative <- order %>% rename(NormativeLocation=Location, NormativeStaff=Staff) %>% select(-Intervention_Full, -Description)
############################################################

############################################################
# Read in Data
input_clean <- read.csv("Anonymized Clean Example_v2_06.27.2024.csv")
############################################################

total_patients <- input_clean  %>%
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) %>%
  mutate(Patient_N=1) %>% group_by(Intervention_Full) %>% summarise(Patient_N=sum(Patient_N)) %>%
  left_join(intervention_coding, by="Intervention_Full") %>% filter(!is.na(Intervention_New)) %>%
  select(-Intervention_Full)

# waiting checks
# check to make sure waiting is never last step
# check to make sure there are not two waitings next to one another

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
  mutate(Intervention_Full=paste(Intervention, HIV_Classification, Type_of_Service, sep=" ")) %>%
  mutate(Description=case_when(Description== "Meds dispensed"  ~     "Meds Dispensed",
                               TRUE ~ Description)) 
#########################################################################################################
# Cleaning for Normative Maps

# Sometimes weight and vital are recorded as two events, but we want to combine them for the normative maps

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
  arrange(Patient, Step) %>%
  mutate(n=1) %>%
  group_by(Patient, Description) %>%
  mutate(n=sum(n))

# Also checking if there are two events of the same name for a patient
duplicates <- input_long2 %>% filter(n>1) %>%
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

input_long3 <- input_long2 %>% filter(n==1) %>% select(-n) %>%
  bind_rows(duplicates) %>%
  arrange(Patient, Step) %>%
  mutate(one=1) %>%
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

# Creating the arrival and departure events
arrive_depart <- input_long3 %>% group_by(Intervention_Full) %>% summarise(One=mean(one)) %>% select(-One) %>%
  mutate(Arrive="Patient Arrives", Depart="Patient Departs") %>%
  gather(Variable, Description_new, Arrive: Depart) %>% select(-Variable) %>%
  left_join(intervention_coding, by="Intervention_Full") %>% filter(!is.na(Intervention_New))


process_analytics_staff <- input_long3 %>%
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


levels(as.factor(process_analytics_staff$Staff1))

process_analytics <- input_long3 %>%
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

##################################################################################################################################33
# Individuals

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


patient_staff_na <- patients_long %>% group_by(Patient) %>%
  filter(row_number()==1) %>%
  select(Full_ID, Patient, Intervention_New) %>%
  mutate(Description_new=NA,  Staff="Nurse", 
         NormativeOrder=NA, Start_x=1, End_x=1, Start_y=1 , End_y=1 )


##################################################################################################################################33
# Combo of Normative and Patients


combo_ids <- bind_rows(process_analytics, patients_long)  %>% group_by(Intervention_New, Patient)  %>%
  filter(row_number()==1) %>%
  select(Patient, Intervention_New) %>%
  ungroup() %>%
  group_by(Intervention_New) %>%
  mutate(id = (row_number() - 1)* -1) 

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

combo_data_staff <- bind_rows(process_analytics_staff, patient_staff_na) %>%
  left_join(combo_ids, by=c("Intervention_New", "Patient"))  %>%
  mutate(Start_y= Start_y + id,
         End_y= End_y + id)


##################################################################################################################################33
# Making multiple rows that are 5 boxes wide

# You can change the width here
break_n <- 5

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


# # # # #  # # # #  # # # # #  # #  # #
process_analytics_snake <- process_analytics %>%
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
  mutate(RevOrder = case_when(row == -1 | row == -3 | row == -5 ~ 1,
                              TRUE ~ 0))  %>%
  mutate(x= case_when(RevOrder==0 ~ x + (row * break_n),
                      RevOrder==1 ~ (break_n + 1) - (x + (row * break_n))),
         xmin= case_when(RevOrder==0 ~ xmin + (row * break_n),
                         RevOrder==1 ~ (break_n + 1) - (xmin + (row * break_n))),
         xmax= case_when(RevOrder==0 ~ xmax + (row * break_n),
                         RevOrder==1 ~ (break_n + 1) - (xmax + (row * break_n))),
         arrow_start= case_when(RevOrder==0 ~ arrow_start + (row * break_n),
                                RevOrder==1 ~ (break_n + 1) - (arrow_start + (row * break_n))),
         arrow_end= case_when(RevOrder==0 ~ arrow_end + (row * break_n),
                              RevOrder==1 ~ (break_n + 1) - (arrow_end + (row * break_n))),
         arrow_x_textloc=  case_when(RevOrder==0 ~ arrow_x_textloc + (row * break_n),
                                     RevOrder==1 ~ (break_n + 1) - (arrow_x_textloc + (row * break_n))),
         cirle_x=  case_when(RevOrder==0 ~ cirle_x + (row * break_n)  ,
                             RevOrder==1 ~ (break_n + 1) - (cirle_x + (row * break_n)) + abs(xmax-xmin)),
         normcirle_x= case_when(RevOrder==0 ~ normcirle_x + (row * break_n),
                                RevOrder==1 ~ (break_n + 1) - (normcirle_x + (row * break_n))+ abs(xmax-xmin)),
         labcircle_x= case_when(RevOrder==0 ~ labcircle_x + (row * break_n),
                                RevOrder==1 ~ (break_n + 1) - (labcircle_x + (row * break_n)) - abs(xmax-xmin)),
         arrow_x_time = case_when(RevOrder==0 ~ arrow_x_time + (row * break_n),
                                  RevOrder==1 ~ (break_n + 1) - (arrow_x_time + (row * break_n))),
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
         arrow_y_time = arrow_y_time + row) %>% 
  mutate(Special_Arrow=case_when(row!=0 & Colsnumber==0 & row %% 2 ==0 ~ "Left",
                                 row!=0 & Colsnumber==0 & row %% 2 !=0 ~ "Right"),
         arrow_start=case_when(Special_Arrow== "Right" ~  xmin,
                               Special_Arrow== "Left" ~  xmin,
                               TRUE ~ arrow_start),
         arrow_end=case_when(Special_Arrow== "Right" ~  xmin,
                             Special_Arrow== "Left" ~  xmin,
                             TRUE ~ arrow_end),
         arrow_start_y=case_when(Special_Arrow== "Right" ~ ymax + 0.35,
                                 Special_Arrow== "Left" ~ ymax + 0.35,
                                 TRUE ~ arrow_start_y),
         arrow_end_y=case_when(Special_Arrow== "Right" ~ ymax + 0.15,
                               Special_Arrow== "Left" ~ ymax + 0.15,
                               TRUE ~ arrow_end_y),
         xmin_s=xmin,
         xmax_s=xmax,
         xmin=case_when(RevOrder== 1 & Description_new=="Patient Departs" ~ xmax_s,
                        TRUE ~ xmin),
         xmax=case_when(RevOrder== 1 & Description_new=="Patient Departs" ~ xmin_s,
                        TRUE ~ xmax),
         arrow_x_textloc=case_when(Description_new=="Patient Departs" & Special_Arrow== "Right" ~ xmax -.1,
                                   Special_Arrow== "Right" ~ xmin -.1,
                                   Special_Arrow== "Left" ~ xmin -.1,
                                   TRUE ~ arrow_x_textloc),
         arrow_y_textloc=case_when(Special_Arrow== "Right" ~ ymax + 0.25,
                                   Special_Arrow== "Left" ~ ymax + 0.25,
                                   TRUE ~ arrow_y_textloc),
         arrow_x_time=case_when(Special_Arrow== "Right" ~ xmin + .1,
                                Special_Arrow== "Left" ~ xmin + .1,
                                TRUE ~ arrow_x_time),
         arrow_y_time=case_when(Special_Arrow== "Right" ~ ymax + 0.25,
                                Special_Arrow== "Left" ~ ymax + 0.25,
                                TRUE ~ arrow_y_time)) 



process_analytics_staff_snake <- process_analytics_staff %>%
  mutate(Rowsnumber=(NormativeOrder-1) %/%  break_n ,
         Colsnumber=(NormativeOrder-1) %%   break_n) %>%
  mutate(row= Rowsnumber * -1) %>%
  mutate(RevOrder = case_when(row == -1 | row == -3 | row == -5 ~ 1,
                              TRUE ~ 0))  %>%
  mutate(Rowsnumber=(NormativeOrder-1) %/%  break_n ,
         Colsnumber=(NormativeOrder-1) %%   break_n) %>%
  mutate(row= Rowsnumber * -1) %>%
  mutate(Start_x= case_when(RevOrder==0 ~ Start_x + (row * break_n),
                            RevOrder==1 ~ (break_n + 1) - (Start_x + (row * break_n))),
         End_x= case_when(RevOrder==0 ~ End_x + (row * break_n),
                          RevOrder==1 ~ (break_n + 1) - (End_x + (row * break_n))),
         Start_y= Start_y + row,
         End_y= End_y + row)




##################################################################################################################################33

staff_color_fill <- c("Nurse"="#c7e8ac", "Community Health Worker"="#b391b5", "Data Clerk"="#3aa6dd", "Lab Technician"="#fcc438",
                      "Medical Attendant"="#ef8d22", "Clinical Officer"="#f5b5c8", "Receptionist"="#c1e4f7", "Counselor"="#ffbbb1",
                      "Doctor"="#7ab648", "Pharmacist"="#de5f85", "Social Worker"="#83bbe5", "Assistant Medical Officer"="#99d5ca", "Pharmacy Technician"="#CE93D8", 
                      "Cadre x"="#E6E89C", "Cadre y"="#A9F9C9", "Cadre z"="#BBA4F6", "Patient Arrives"="#ced4db", "Patient Departs" ="#ced4db")

staff_color_border <- c("Nurse"="#7ab648",  "Community Health Worker"="#834187", "Data Clerk"="#0c7cba", "Lab Technician"="#ef8d22", "Medical Attendant"="#fcc438",
                        "Clinical Officer"="#de5f85", "Receptionist"="#3aa6dd",   "Counselor"="#c92d39", "Doctor"="#19967d",
                        "Pharmacist"="#c92d39", "Social Worker"="#0c4cba", "Assistant Medical Officer"="#19967d",  "Pharmacy Technician"="#80338D",
                        "Cadre x"="#A8AB27", "Cadre y"="#0DBF55", "Cadre z"="#4513C3", "Patient Arrives"="#6f7681", "Patient Departs" ="#6f7681")
##################################################################################################################################33
# Graphics Details
arrow_size <- .1
arrow_type <- "open"
circle_size <- .1
##################################################################################################################################33
giant_legend <- data.frame(staff_color_fill, staff_color_border) %>%   rownames_to_column("Provider") %>%
  filter(Provider!="Patient Arrives") %>%
  filter(Provider!="Patient Departs") %>%
  arrange(Provider) %>%
  mutate(x=1, y=row_number()) %>%
  mutate(y=17-y)

legend <- ggplot(giant_legend, aes(xmin=x-5, xmax=x+5, ymin=y-.45, ymax=y+.45, fill=Provider, color=Provider, label=Provider)) +
  geom_rect() +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0, "cm"))
legend
#ggsave( paste("Legend", date, ".jpg", sep=""), height=7, width=2.5, units="in")

# using ratio of pictur4e
legend_pic <- 
  ggplot(giant_legend, aes(xmin=x-5, xmax=x+5, ymin=y-.45, ymax=y+.45, fill=Provider, color=Provider, label=Provider)) +
  geom_rect() +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) + 
  annotation_raster(periodic_norm, xmin = -4 , xmax = 6, ymin = 17, ymax = (17 + (1895/3587)*10)) +
  ylim(0, 22.5) +
  theme_void() +
  theme(legend.position = "none")
legend_pic

#ggsave(  paste("Legend with Display", date, ".jpg", sep=""), height=10, width=5, units="in")

legend_pic2 <- 
  ggplot(giant_legend, aes(xmin=x-5, xmax=x+5, ymin=y-.45, ymax=y+.45, fill=Provider, color=Provider, label=Provider)) +
  geom_rect() +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) + 
  annotation_raster(periodic_pat, xmin = -4 , xmax = 6, ymin = 17.5, ymax = (17.5  + (1895/3587)*10)) +
  annotation_raster(periodic_norm, xmin = -4 , xmax = 6, ymin = (17.5 + 1 + (1895/3587)*10), ymax =17.5 + 1 + (1895/3587)*10 +  (1763/3594)*10  ) +
  annotate("text", x = 1, y = 17, label = "Provider Colors and Names") +
  annotate("text", x = 1, y = (18 + (1895/3587)*10), label = "Legend for Individual Client Process Maps") +
  annotate("text", x = 1, y = 17 + 2 + (1895/3587)*10 +  (1763/3594)*10, label = "Legend for Normative Process Maps with Client Averages") +
  ylim(0, 29.5) +
  theme_void() +
  theme(legend.position = "none")
legend_pic2

#ggsave( paste("Legend with Both Display", date, ".jpg", sep=""), height=12, width=5, units="in")


ggplot(giant_legend, aes(xmin=x-4, xmax=x+4, ymin=y-.45, ymax=y+.45, fill=Provider, color=Provider, label=Provider)) +
  geom_rect() +
  scale_fill_manual(values=staff_color_fill,  na.value ="white") +
  scale_color_manual(values=staff_color_border, guide = "none") +
  geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F)  +
  annotation_raster(periodic_pat, xmin = -14 , xmax = -3, ymin = 7- ((1763/3594)*11), ymax = 7) +
  annotation_raster(periodic_norm, xmin = -14 , xmax = -3, ymin = 9 , ymax = 9 +  (1895/3587)*11) +
  annotate("text", x = 1, y = 17, label = "Provider Colors and Names") +
  annotate("text", x = -8.5, y = 8, label = "Legend for Individual Client Process Maps") +
  annotate("text", x = -8.5, y = 10 +  (1895/3587)*11, label = "Legend for Normative Process Maps with Client Averages") +
  ylim(0, 17) +
  xlim(-14.5 , 6.5) +
  theme_void() +
  theme(legend.position = "none")
#ggsave( paste("Legend with Both Display Side by Side", date, ".jpg", sep=""), height=10, width=10, units="in")


#############################################################################################
# Lists to loop through
patients_per_intervention <- patients_long %>% group_by(Patient) %>%
  summarise(Patient=first(Patient), Intervention_New=first(Intervention_New), Boxes=max(x)) %>% 
  mutate(n=1) %>% ungroup() %>% group_by(Intervention_New) %>%
  summarise(NPatients=sum(n), NPatientBoxes=max(Boxes))

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
         NorParHeight=(NPatients + 1) * 3,
         NorParWidth=NPatientBoxes * 2) %>%
  mutate(SingleFile=paste(Intervention_New, " ", facility, " Single Row ", date, ".png", sep=""),
         MultiFile=paste(Intervention_New, " ", facility, " Multi Row ", date, ".png", sep=""),
         SnakeFile=paste(Intervention_New, " ", facility, " Snake Row ", date, ".png", sep=""),
         NPFile=paste(Intervention_New, " ", facility, " NormPatient Row ", date, ".png", sep=""))

patient_list <- patients_long %>% group_by(Patient) %>%
  summarise(Full_ID=first(Full_ID), Intervention_New=first(Intervention_New),
            Boxes=max(x))  %>%
  mutate(SingleWidth=Boxes * 2) %>%
  mutate(SingleFile=paste(Patient, " ", Intervention_New, " ", facility, " Single Row ", date, ".png", sep=""))
#############################################################################################


setwd("C:/Users/KristinBietsch/files/Lori/Loop Results/Anonymous")
#############################################################################################

for (row in 1:nrow(normative_list)) {
  intervention_n <- normative_list[row, "Intervention_New"]
  intervention_value <- intervention_n$Intervention_New[1]
  
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  single_file <- normative_list[row, "SingleFile"]
  single_file_name <- single_file$SingleFile[1]
  
  single_width <- normative_list[row, "SingleWidth"]
  single_width_value <- single_width$SingleWidth[1]
  
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  multi_file <- normative_list[row, "MultiFile"]
  multi_file_name <- multi_file$MultiFile[1]
  
  multi_width <- normative_list[row, "MultiWidth"]
  multi_width_value <- multi_width$MultiWidth[1]
  
  multi_height <- normative_list[row, "MultiHeight"]
  multi_height_value <- multi_height$MultiHeight[1]
  
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  
  snake_file <- normative_list[row, "SnakeFile"]
  snake_file_name <- snake_file$SnakeFile[1]
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  
  normpat_file <- normative_list[row, "NPFile"]
  normpat_file_name <- normpat_file$NPFile[1]
  
  normpat_width <- normative_list[row, "NorParWidth"]
  normpat_width_value <- normpat_width$NorParWidth[1]
  
  normpat_height <- normative_list[row, "NorParHeight"]
  normpat_height_value <- normpat_height$NorParHeight[1]
  
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  #########################################################################################################
  
  intervention_single_data <- process_analytics %>% filter(Intervention_New==intervention_value)
  intervention_single_staff <- process_analytics_staff  %>% filter(Intervention_New==intervention_value      ) 
  
  intervention_multi_data <- process_analytics_multirows %>% filter(Intervention_New==intervention_value )
  intervention_multi_staff <- process_analytics_staff_multirows  %>% filter(Intervention_New==intervention_value) 
  
  intervention_snake_data <- process_analytics_snake %>% filter(Intervention_New==intervention_value)
  intervention_snake_staff <- process_analytics_staff_snake  %>% filter(Intervention_New==intervention_value) 
  
  intervention_normpat_data <- bind_rows(combo_data, combo_labels) %>% filter(Intervention_New==intervention_value)
  intervention_normpat_staff <- combo_data_staff  %>% filter(Intervention_New==intervention_value) 
  
  #########################################################################################################
  
  singlerow <- ggplot(data = intervention_single_data,
                      mapping = aes(xmin = xmin, ymin = ymin, 
                                    xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
    geom_rect(data=subset(intervention_single_data, Description_new!="Patient Arrives" & Description_new!="Patient Departs")) +
    ggchicklet:::geom_rrect(data=subset(intervention_single_data, Description_new=="Patient Arrives" | Description_new=="Patient Departs")) +
    scale_fill_manual(values=staff_color_fill,  na.value ="white") +
    scale_color_manual(values=staff_color_border, guide = "none") +
    geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
    geom_segment(data = intervention_single_data,
                 mapping = aes(x = arrow_start, y = arrow_start_y, 
                               xend = arrow_end, yend = arrow_end_y),
                 arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
    geom_text(data=intervention_single_data, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333") +
    geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
    geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
    geom_text(data = intervention_single_data, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F) +
    geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
    geom_text(data = intervention_single_data, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F) +
    geom_text(data=intervention_single_data, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold") +
    geom_text(data=intervention_single_data, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=3) +
    geom_rect(data=intervention_single_staff, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
    geom_text(data=intervention_single_data, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=4) +
    labs(title= "Process Map - National Norms and Facility Averages" , subtitle=paste(facility, " - ", intervention_value, sep="" )) +
    theme_void() +
    theme(legend.position = "none")
  
  singlerow
  
  ggsave(single_file_name, height=3, width=single_width_value, units="in", bg = "white")
  
  #########################################################################################################
  
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
  
  ggsave(multi_file_name, height=multi_height_value, width=multi_width_value, units="in", bg = "white")
  
  #########################################################################################################
  
  snakerows <- ggplot(data = intervention_snake_data,
                      mapping = aes(xmin = xmin, ymin = ymin, 
                                    xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
    geom_rect(data=subset(intervention_snake_data, Description_new!="Patient Arrives" & Description_new!="Patient Departs")) +
    ggchicklet:::geom_rrect(data=subset(intervention_snake_data, Description_new=="Patient Arrives" | Description_new=="Patient Departs")) +
    scale_fill_manual(values=staff_color_fill,  na.value ="white") +
    scale_color_manual(values=staff_color_border, guide = "none") +
    geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
    geom_rect(data=intervention_snake_staff, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
    geom_segment(data = intervention_snake_data,
                 mapping = aes(x = arrow_start, y = arrow_start_y, 
                               xend = arrow_end, yend = arrow_end_y),
                 arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
    geom_text(data=intervention_snake_data, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333") +
    geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
    geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
    geom_text(data = intervention_snake_data, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F) +
    geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
    geom_text(data = intervention_snake_data, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F) +
    geom_text(data=intervention_snake_data, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold") +
    geom_text(data=intervention_snake_data, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=3) +
    geom_text(data=intervention_snake_data, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=4) +
    labs(title= "Process Map - National Norms and Facility Averages" , subtitle=paste(facility, " - ", intervention_value, sep="" )) +
    theme_void() +
    theme(legend.position = "none")
  
  snakerows
  
  ggsave(snake_file_name, height=multi_height_value, width=multi_width_value, units="in", bg = "white")
  
  ####################################################################################
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
  
  ggsave(normpat_file_name, height=normpat_height_value, width=normpat_width_value, units="in", bg = "white")
  
  ####################################################################################
  
  
}





# Patients Only
#######################################################################################

for (row in 1:nrow(patient_list)) {
  intervention_n <- patient_list[row, "Full_ID"]
  intervention_value <- intervention_n$Full_ID[1]
  
  # # # # # # # #  # # # # # # # # # # # # # # # # 
  single_file <- patient_list[row, "SingleFile"]
  single_file_name <- single_file$SingleFile[1]
  
  single_width <- patient_list[row, "SingleWidth"]
  single_width_value <- single_width$SingleWidth[1]
  
  ####################################################
  
  patient <- patients_long %>% filter(Full_ID==intervention_value    )
  patient_staff <- patient_staff_na  %>% filter(Full_ID==intervention_value)
  ####################################################
  singlerow_p <-   ggplot(data = patient,
                          mapping = aes(xmin = xmin, ymin = ymin, 
                                        xmax = xmax, ymax = ymax, label = Description_new, fill=Staff, color=Staff)) +
    geom_rect(data=subset(patient, Description_new!="Patient Arrives" & Description_new!="Patient Departs")) +
    ggchicklet:::geom_rrect(data=subset(patient, Description_new=="Patient Arrives" | Description_new=="Patient Departs")) +
    scale_fill_manual(values=staff_color_fill,  na.value ="white") +
    scale_color_manual(values=staff_color_border, guide = "none") +
    geom_fit_text( color = "#333333", reflow = TRUE, show.legend = F) +
    geom_segment(data = patient,
                 mapping = aes(x = arrow_start, y = arrow_start_y, 
                               xend = arrow_end, yend = arrow_end_y),
                 arrow = arrow(length = unit(arrow_size, "cm"), type = arrow_type), color="black") +
    geom_text(data=patient, aes(x=arrow_x_textloc, y=arrow_y_textloc, label=Patient_Received_Lead), color="#333333") +
    geom_circle(aes(x0=labcircle_x, y0=labcircle_y, r=circle_size), fill="#fff7a1", color="#5e5e5e") +
    geom_circle(aes(x0=cirle_x, y0=circle_y, r=circle_size), fill="#e5e5e5", color="#5e5e5e") +
    geom_text(data = patient, aes(x=cirle_x, y=circle_y, label=round(ave_time)), color = "#333333", reflow = TRUE, show.legend = F) +
    geom_circle(aes(x0=normcirle_x, y0=normcircle_y, r=circle_size), fill="#333333", color="#333333") +
    geom_text(data = patient, aes(x=normcirle_x, y=normcircle_y, label=round(NormativeTime)), color = "white", reflow = TRUE, show.legend = F) +
    geom_text(data=patient, aes(x=x, y=Step_Local1_y, label=Step_Local1), color="#333333", fontface = "bold") +
    geom_text(data=patient, aes(x=x, y=Step_Local2_y, label=Step_Local2), color="#333333", size=3) +
    geom_rect(data=patient_staff, mapping= aes(xmin=Start_x, xmax=End_x, ymin=Start_y, ymax=End_y, fill=Staff)) +
    geom_text(data=patient, aes(x= arrow_x_time, y=arrow_y_time, label=Waiting_Lead), color="black", size=4) +
    labs(title= "Process Map - Patient Observation" , subtitle=paste(facility, " - ", intervention_value, sep="" )) +
    theme_void() +
    theme(legend.position = "none")
  
  singlerow_p
  
  ggsave(single_file_name, height=3, width=single_width_value, units="in")
  
  
}