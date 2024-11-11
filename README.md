Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

# Title: Experimental Electoral Division Profiles (Health)
# Date: 2024/11/11
# Author: Tomás Kelly
# Contact: tomas.kelly@cso.ie

This project originated as a collaboration between the Central Statistics Office and Cork Healthy Cities in Ireland. The goals were to profile every Electoral Division (ED) from a health perspective, taking into account the social determinants of health (economic status, education, housing etc). 

The <a href="https://edprofiles.cso.ie/2022/health/index.html">Homepage example here</a> contains a map of Electoral Divisions in Ireland, from which a user can select the appropriate HTML or PDF profile report (HTML reports contain only charts, whereas the PDFs contain data tables in addition to charts). From the homepage, the user can navigate to a searchable table to filter reports for a specific ED or Administrative County. Finally, the user can navigate from the homepage to the Github repository containing the code required to generate the reports. 

The code creates PDF and HTML reports for every ED in Ireland using R. It takes a variety of data that is publicly available on http://www.cso.ie, adjusts their structure/formatting appropriately and creates a series of tables and plots that are then included in the final reports. It also creates the searchable map, as well as a csv that is used to create the searchable table contained on the website using the Python package “csvtotable”. 

There are three separate geographical units profiled alongside each other in the reports to allow for contextualisation/comparison:
1. An **Electoral Divisions (ED**) is the smallest legally defined administrative areas in the republic of Ireland and their populations vary from 71 to 43,905.
2. An **Administrative County (AC)** is one of 31 geographical units in Ireland through which Local Government is operated. 
3. **State** refers to the Republic of Ireland

The “GUID” of a given geographical unit is an identifier that can refer to geographical units across a variety of scales uniquely, including ED, AC and State. 

Dependencies for this project are R (4.3.1 was used in the creation of these profiles), Miktex (an open-source distribution of the LaTeX typesetting system) and Texnic Centre (an open-source Integrated Development Environment  for Latex). 

To use this code, one first needs to clone the repository to a local location and then open "HealthProfileMain.R" (it is important for this file to be opened first in a new R session for dynamic file locations to work correctly). The process as is should run completely from that point. 

**“HealthProfileMain”** is the main driver file. This file will run all the appropriate subfiles and generate the reports without having the user need to open any other files. Initially, it creates a dataframe with ED names, AC names and GUIDs. It then creates a csv of this information that also contains appropriate formatted links to the reports that will be created. It then loops through a list of ED names and finds the associated EDGUID, AC name and AC GUID, which are used to select the appropriate data from given tables/files. It also creates shortened versions of ED and AC names to be used in tables to ensure consistent spacing. For Every ED, AC combination, it sources a map of the ED (see notes for a description of how these maps were created) and loops through all of the associated subfiles to process data and create plots/tables. It then uses the R package Sweave to build the pdf for the Electoral Division. Sweave is a bridge between R and Latex (a language/technology used to create the actual pdf). Finally, it uses RMarkdown to create the HTML report.

**"ReadAndFormatPXStat.R" reads in the PXStat data for each theme, formats it correctly and restructures where appropriate. 

**"SAPSPercentages.R" calculates percentages for each SAPS variable using configuration settings from “GlossaryEditForPercentages.csv”. It calculates percentages for individual variables based on “TotalColForPerc” in the csv. 

The process is divided thematically in **files 3-16**. Each file here extracts the data for the appropriate ED and AC, as well as State and creates specific tables and plots to  be later included in the outputted report. 

**“17_KeyPointsAndOtherCalcs”** calculates some figures for the Key Points section that are not calculated elsewhere, such as ED Area, ED Population Density, Age Dependency Ratios and aggregations of multiple variables within themes.

**“18_SAPS”** Selects the appropriate SAPS data for ED, AC and State and creates associated tables. It creates the tables used in the "Detailed Tables" section of the report using "GLossaryEditForPercentages.csv" as the driver. It creates a dataframe for every “LatexTableTitle” in the file, with one row per “Label”, one column for “ED”,”AC” and “State” and orders the rows by “Rank”.

**“19_EDSearchTool”** creates the html map of EDs, from which reports can be selected.

**“HealthProfileTemplate.Rnw”** is the file that the R package Sweave uses to create the “.tex” Latex file that Latex uses to build the pdf. It extracts textual/tabular data from R objects via the \Sexpr{} function, which are then included in the “.tex” file (due to the fact that Latex cannot process R code). Note: Latex errors outputted in R are not as descriptive as errors via Texnic Centre. When troubleshooting, it can sometimes be beneficial to run the .tex file generated via Sweave in Texnic Centre.

**“HealthProfileMarkdown.Rmd”** is the RMarkdown template that is used to create the HTML reports.

There are four main types of data sources used in the project:
* **PXStat tables**, which are tables hosted and searchable on data.cso.ie. There are separate PxStat tables for given themes. Data for Electoral Divisions and Administrative Counties are contained within separate PxSTat tables for given themes. 
* The **raw SAPS tables**. These files contain the majority of Census count data from Census 2022. SAPS tables for ED and AC are imported by the process and joined together. These files are used to calculate the percentages contained in the “Detailed Tables” section of the report.
* **Custom CSVs** to drive parts of the process. “EDACLookup.csv” finds the AC corresponding with a given ED. “GlossaryEditForPercentages.csv” is used by the SAPS program to drive the creation of the tables included in the “Detailed Tables” section of the report.
* **JPG maps** of every ED

## Other notes:
* When working on this project, line 88 - 89  in “HealthProfileMain” can be commented back in to only run the process on a sample of files to save time. 
* The loop in “HealthProfileMain.R” includes error tracking functionality. If there was an error for an ED, the process skips the ED,  logs the error, the iteration number and the ED GUID and saves the information to “ErrorList.csv” in the output folder.
* ED maps were created using raw data from OSM and pre-created styles located at https://gitlab.com/champs-libres/public/champs-libres-qgis-osm-style. To create these maps, the raw OSM data was downloaded from geofabrik.de and the shapefiles were copied into the “shp” folder located in of “champs-libres-qgis-osm-style-master”. The champs-libres-qgis-osm-style.qgs file was then opened in QGIS and some minor edits were made. Finally, the QGIS Atlas tool was used to create individual maps for every ED.
* Headers were added to the index.html page and the table search page manually during the process, as well as some other edits. If trying to recreate, using the Notepad++ plugin “compare” to compare the outputted map and HTML table to those on the website would be a good starting point. 
* "TopAndBottomEDs.R" is what is used to create a ranking of the top 5 and bottom 5 EDs across key health variables.
* The outputted reports are contained within the "Reports" folder of the repository