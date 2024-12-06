# Instructions for Using R Code to Generate Process Maps that Illustrate Health Service Delivery based on ABC/M Data 


Analytics for Advancing the Financial Sustainability of the HIV/AIDS Response (AFS)
Version 0.1: December 6, 2024

The Analytics for Advancing the Financial Sustainability of the HIV/AIDS Response (AFS) contract is funded by the United States Agency for International Development (USAID) under Contract No. 7200AA23C00043, March 2023-March 2027. This contract is made possible by the generous support of the American people through support from USAID and the United States President’s Emergency Plan for AIDS Relief (PEPFAR). The contents were prepared by staff of the AFS contract and do not necessarily reflect the views of USAID or the United States Government.

## Background
Part of the data analysis of any ABC/M application is analyzing process maps. Process maps illustrate the steps required to complete a health intervention as well as the order, location, and duration of the steps. 
Initial norms-based process maps are developed using existing guidelines related to the delivery of health interventions or services. The normative process is based on written standard operating procedures (SOPs) that describe how an intervention should be delivered, in particular: (a) when and where services are provided, (b) what activities are performed in each step of the process, and (c) the expected duration of each step. Key informant interviews (KIIs) with national experts may also be conducted to either comment on the SOP or, if no SOPs exist, describe the intervention. The SOPs and KIIs should indicate whether norms vary by site level or location. For example, KIIs may note that a client receiving antiretroviral therapy (ART) for HIV at a hospital should receive a viral load test during their consultation whereas a client receiving ART at a clinic may not be expected to receive a viral load test. 

Process maps can also be developed based on data that is collected as part of an ABC/M study. These process maps also illustrate the steps involved in a health intervention, including the observed order, location, and duration of the steps. The R code below will generate process maps for each individual client and for a site. The process maps for the site will show the norms-based process and the observed averages of the clients in the data set who received that intervention at the site. For example, if data collectors gathered data on 10 clients receiving ART at a clinic, then a process map illustrating the norms-based expected process and the averages of the client data for the clinic location will be generated at the top of the page with an individual process map for each of the 10 clients in the data set (which does not include the norm-based steps) displayed underneath. The generation of process maps can be repeated if there is data from multiple facilities.  

Instructions for generating process maps using R are provided in the following pages (R code and sample files are available at: https://github.com/AvenirHealth-org/ProcessMaps/).

## FILES AVAILABLE ON GITHUB
The following sample files are available in the Github repository for process maps: 

o	ABCM Process Map Code 112224.R – Code used to create process maps

o	ABCM Process Map Legends 112624.R - Code to create legends

o	Anonymized Clean Example 110424 w Branding.csv – File containing patient information

o	Normative Process Map 111523 Clean.csv – File containing the normative process for each intervention

o	Legend Normative square6.png – Legend created by ABCM Process Map Legends 112624.R that is used in ABCM Process Map Code 112224.R

o	Legend Patient Normative Combo Long.png – Legend created by ABCM Process Map Legends 112624.R that is used in ABCM Process Map Code 112224.R

## INSTRUCTIONS
o	Step 1: Create a csv file with data formatted as in the example csv file: “Anonymized Clean Example _v2_06.27.2024 w branding”. If you want to include additional information, such as branding language, include it beginning in column A at the bottom of your dataset, like the example CSV file, which includes branding language about the AFS project. 

o	Step 2: Using RStudio (available at https://posit.co/download/rstudio-desktop/), open “ABCM Process Map Code 112224.R”. To run a single line of code in R, click your cursor onto that row, then click ctrl + enter. To run multiple lines at the same time, highlight the multiple lines of code and click ctrl+ enter. Note that any line with a “#” at the beginning is read as text, not code. 

o	Step 3: If this is your first time running the code, make sure you have all the necessary packages loaded - see Section 1 in the R code for the list of packages. To install packages while in RStudio, click on the Packages tab on the bottom right window, click “Install” and search for a package name, and then click the associated “Install” button. One package, “ggchicklet” must be installed in a different manner. Uncomment (remove the “#”) any one of the 4 lines of code above “library(ggchicklet)” in the R code, run the line of code using click ctrl + enter. 

o	Step 4: In Section 2 of the R code, change the working directory to the folder on your computer where your CSV data file, any updated legend graphics (see below), and “Normative Process Map 111523 Clean.csv” files are stored. At the same time, create a folder within the folder now defined as your working directory called "Loop Results"- this is where the created graphics will be stored. Note that, if you have created new legends or changed the existing legends created in "ABCM Process Map Legends 112624.R", you need to change the names of the legend graphic files being read in and the dimensions (in inches) of the legends. 

o	Step 5: In Section 3 of the R code, edit the name of your facility, which is written currently as "Anonymous Facility".  

o	Step 6: Section 4 of the R code reads in the patient-level data and creates a dataset with all interventions and steps. You can export this dataset by uncommenting the line beginning with “write.csv” in the R code. Note that you must confirm that the list of interventions and descriptions are included in “Normative Process Map 111523 Clean.csv.” To do this, open the “Normative Process Map 111523 Clean.csv” file and review the list of interventions (Column A), step number (Column B), step description (Column C), normative time (Column D), normative location (Column E), and normative staff (Column F). Columns G and H have data which matches the anonymized data used in this code, but you can remove that information and insert the information from your dataset if needed.  If you make changes to any of these fields or add new interventions, make sure to make a note, so that you can make the appropriate changes in the R code as well; because the R code matches information from strings, the text in the strings must be identical, including spacing and capitalization. 

o	Step 7: If you replaced data in the "Normative Process Map 111523 Clean.csv" file and saved it under a new name, update the following line in Section 5 of the R code:
order <- read.csv("Normative Process Map 111523 Clean.csv")
with the new file name. Make sure that the file is saved in your working directory (see Step 4).

o	Step 8: Begin running each section of the R code. If you run into an error, check if you have run all of the R code above where the error is occurring. Note that there are notes in the code which explain the purpose of each piece of code. For example, Section 8 cleans the data for the normative process maps, and Section 9 prepares the data for creating patient specific maps. Section 10 combines the data from Section 8 and 9 and is used to create normative and patient process maps on the same graphic.

o	Step 9: Section 11of the R code creates an alternative plotting structure, resulting in normative process maps that are several rows long. The default is set to 5 boxes wide. If you would like to change this number, edit line “break_n <- 5”, replacing 5 with your desired number of rows. 

o	Step 10: Section 12 of the R code assigns the colors used in the output graphics of the process maps. If you want to change these colors, you will also need to edit them in the file "ABCM Process Map Legends 112624.R" so that the legends use the same color scheme. 

o	Step 11: In Section 13 of the R code, you can edit the size and type of arrows and size of circles used in the output graphics by replacing the information on the right-hand side of the “<-“.

o	Step 12: Section 14 of the R code does the final preparation for the legends. If you have edited the legends in "ABCM Process Map Legends 112624.R" and changed the dimensions, confirm that you entered the correct dimensions in Section 2 of the R code. 

o	Step 13: Section 15 of the R code prepares the dataset that we will use in our loop to produce all output graphics. Make sure your working directory is set correctly (see Step 4) and that you have a folder called “Loop Results” in your working directory.

o	Step 14: Section 16 of the R code is the loop producing all output graphics. The entire loop needs to be run at the same time, by highlighting all rows in the loop (starting at "for (row in 1:nrow(normative_list)) {" and ending at "}") and clicking run. If you created a legend with different dimensions than the one provided, confirm that you entered the correct dimensions in Section 2 of the R code. 

Analytics for Advancing the Financial Sustainability of the HIV/AIDS Response. Activity-Based Costing and Management (ABC/M): Data Management and Analysis Plan. Glastonbury, CT: Avenir Health, 2024.
