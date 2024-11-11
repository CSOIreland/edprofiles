library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(pxR)
library(dplyr)
library(webshot)
library(htmlwidgets)
library(lubridate)
library(stringi)
library(highcharter)


TodaysDate <- format(Sys.Date(), "%d/%m/%Y")
TodayForJSONLD <- format(Sys.Date(), "%Y/%m/%d/")

#set up working directory, input and output folders. 
RootWD <- getwd()
setwd(RootWD)
InputFilesLoc <- file.path(RootWD, "inputs")
OutputFilesLoc <- file.path(RootWD, "outputs")

# Set seed so that when running on a sample of files,given samples don't change between runs
set.seed(123456)


#Create Lists of EDs
EDList <- unique(as.data.frame(read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1AED/PX/2013/en"))$CSO.Electoral.Divisions.2022)
EDList <- as.data.frame(EDList)
colnames(EDList)[1] <- "ED"

#Create list of ED GUIDs 
GUIDList <- unique(as.data.frame(read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1AED/PX/2013/en"), use.codes = T)$CSO.Electoral.Divisions.2022)
GUIDList <- as.data.frame(GUIDList)
colnames(GUIDList)[1] <- "GUID"

#Create EDs df W GUIDs and remove special chars
EDWGUID <- EDList
EDWGUID$GUID <- GUIDList$GUID 
EDWGUID$ED <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",EDWGUID$ED)
EDWGUID$ED <- gsub("D├║n Laoghaire","Dun Laoghaire", EDWGUID$ED)
EDWGUID$ED <- gsub("'","", EDWGUID$ED)
EDWGUID$ED <- gsub("&","and", EDWGUID$ED)
EDWGUID$ED  <- gsub("M├│r","Mor", EDWGUID$ED)

#List of ACs and associated GUIDs
ACListStart <- as.data.frame(read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1ACTY/PX/2013/en"))
ACGUIDList <- as.data.frame(read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1ACTY/PX/2013/en"),use.codes=T)
ACListStart$GUID <- ACGUIDList$Administrative.Counties.2019
ACList <- ACListStart%>%dplyr::select(Administrative.Counties.2019, GUID)
ACList <- ACList[!duplicated(ACList[,c("Administrative.Counties.2019", "GUID")]),]

# Remove special chars in AC Names
ACList$Administrative.Counties.2019 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",ACList$Administrative.Counties.2019)
ACList$Administrative.Counties.2019 <- gsub("D├║n Laoghaire","Dun Laoghaire", ACList$Administrative.Counties.2019)
ACList$Administrative.Counties.2019 <- gsub("'","", ACList$Administrative.Counties.2019)
ACList$Administrative.Counties.2019 <- gsub("&","and", ACList$Administrative.Counties.2019)
ACList$Administrative.Counties.2019 <- gsub("M├│r","Mor", ACList$Administrative.Counties.2019)

#Lookup table from ED to AC and replacing of special chars
EDACLookup <- as.data.frame(read.csv(paste0(InputFilesLoc,"/EDACLookupFinal.csv")))%>%select(ED_GUID,AC)
EDACLookup$AC <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire", EDACLookup$AC)
EDACLookup$AC <- gsub("D├║n Laoghaire","Dun Laoghaire", EDACLookup$AC)
EDACLookup$AC <- gsub("'","", EDACLookup$AC)
EDACLookup$AC <- gsub("&","and", EDACLookup$AC)
EDACLookup$AC <- gsub("M├│r","Mor", EDACLookup$AC)

# Create combined dataframe of EDs GUIDs and ACs
# also creates lower case names of EDs and ACs with dashes instead of spaces so they can be used for filenames
EDWGUIDAC <- merge(EDWGUID,EDACLookup, by.x = "GUID", by.y = "ED_GUID")
EDWGUIDAC$EDLC <- make.names(tolower(EDWGUIDAC$ED))
EDWGUIDAC$EDLC <- gsub(".","-",EDWGUIDAC$EDLC, fixed = T)
EDWGUIDAC$ACLC <- make.names(tolower(EDWGUIDAC$AC))
EDWGUIDAC$ACLC <- gsub(".","-",EDWGUIDAC$ACLC, fixed = T)
EDWGUIDAC$GUIDLC <- tolower(EDWGUIDAC$GUID)

#Create CSV For HTML Table and EDsearch tool with links to each file
CSVForHTML <- EDWGUIDAC%>%select(ED,GUID,AC,EDLC,ACLC,GUIDLC)
CSVForHTML$ReportNumber <- 1:nrow(CSVForHTML)

#add links for html and pdf reports
CSVForHTML$Report <- paste0("<a href=\"https://edprofiles.cso.ie/2022/health/html/",CSVForHTML$ReportNumber,"-",CSVForHTML$EDLC,"-ac-",CSVForHTML$ACLC,".html","\"", " target=\"_blank\"", " title=\"",CSVForHTML$ED," summary report\"", ">","HTML","</a>", "  ",
                            "<a href=\"https://cdn.jsdelivr.net/gh/CSOIreland/edprofiles@resources/2022/health/pdf/",CSVForHTML$ReportNumber,"-",CSVForHTML$EDLC,"-ac-",CSVForHTML$ACLC,".pdf","\"", " target=\"_blank\"", ", title=\"",CSVForHTML$ED," PDF Report\">","PDF","</a>"  )
CSVForHTML$Report <- gsub("--","-", CSVForHTML$Report) 
CSVForHTMLToExport <- CSVForHTML%>%select(ED,AC,Report, GUID)%>%dplyr::rename("Electoral Division" = "ED", "Administrative County" = "AC")

write.csv(CSVForHTMLToExport, file = paste0(OutputFilesLoc, "/CSVForHTML.csv"), row.names = F)

# #sample dataset if running tests
#Sample <- sample(1:nrow(EDWGUIDAC),1)
#EDWGUIDAC <- EDWGUIDAC[Sample,]

# Read and Format PX Stat Files
source(paste0(getwd(),"/scripts/1_ReadAndFormatPXStat.R"))

# calculate SAPS Percentages
source(paste0(getwd(),"/scripts/2_SAPSPercentages.R"))

# create an empty list for errors, to be filled later
ErrorList <- list()

Indices <- which(EDWGUIDAC$AC == "South Dublin County Council"|EDWGUIDAC$AC == "Fingal County Council")

# Loop through every file in the process for every ED
for (i in Indices)  {
  
  setwd(RootWD)
  
  #skip to next controls whether an iteration should be skipped if there is an error(if SkipToNext == TRUE)
  SkipToNext <- FALSE
  
  tryCatch({
    #Select the appropriate, ED, AC and map link
    ED <- as.character(EDWGUIDAC$ED[i])
    EDGUID <- as.character(EDWGUIDAC$GUID[i])
    EDGUIDLC <- as.character(EDWGUIDAC$GUIDLC[i])
    EDLC <- as.character(EDWGUIDAC$EDLC[i])
    
    AC <- EDWGUIDAC$AC[i]
    ACGUID <- ACList$GUID[ACList$Administrative.Counties.2019 == AC]
    ACLC <- as.character(EDWGUIDAC$ACLC[i])
    
    #Create a truncated version of ED so that tables and plots remain consistent
    EDForTables <- str_trunc(ED, 25)
    EDForTables <- str_replace_all(EDForTables, "[^[:alnum:]]\\,", " ")
    EDName <- make.names(ED)
    EDName <- gsub("\\.","-", EDName)
    
    
    #Create a truncated version of AC so that tables and plots remain consistent
    ACForTables <- str_trunc(AC, 25)
    ACForTables <- str_replace_all(ACForTables, "[^[:alnum:]]\\,", " ")
    ACForTablesShort <- str_trunc(AC, 15)
    ACForTablesShort <- str_replace_all(ACForTablesShort , "[^[:alnum:]]\\,", " ")
    ACName <- make.names(AC)
    ACName <-  gsub("\\.","-", ACName)
    
    # Title for the markdown/html
    
    TitleForRMD <- paste0("Electoral Division Health Profile - ", ED)
    
    # link to map for ED
    EDMapLink <- paste0("../inputs/exportededmaps/",EDGUID,".jpg")
    
    # Run through Subfiles
    source(paste0(getwd(),"/scripts/3_Population.R"))
    source(paste0(getwd(),"/scripts/4_Carers.R"))
    source(paste0(getwd(),"/scripts/5_GeneralHealth.R"))
    source(paste0(getwd(),"/scripts/6_Disability.R"))
    source(paste0(getwd(),"/scripts/7_Smoking.R"))
    source(paste0(getwd(),"/scripts/8_Education.R"))
    source(paste0(getwd(),"/scripts/9_PrincipleEconomicStatus.R"))
    source(paste0(getwd(),"/scripts/10_Families.R"))
    source(paste0(getwd(),"/scripts/11_Birthplace.R"))
    source(paste0(getwd(),"/scripts/12_Volunteering.R"))
    source(paste0(getwd(),"/scripts/13_SocialClass.R"))
    source(paste0(getwd(),"/scripts/14_Households.R"))
    source(paste0(getwd(),"/scripts/15_RenewableEnergy.R"))
    source(paste0(getwd(),"/scripts/16_Travel.R"))
    source(paste0(getwd(),"/scripts/17_KeyPointsAndOtherCalcs.R"))
    source(paste0(getwd(),"/scripts/18_SAPS.R"))
    
    setwd(paste0(getwd(),"/scripts"))
    
    #Create the .RNW file using sweave for compiling
    Sweave("HealthProfileTemplate.Rnw",output=gsub("--","-",paste0(i,"-",EDLC,"-ac-",ACLC,".tex")))
    #Compile the .rnw with Latex
    tools::texi2pdf(gsub("--","-",paste0(i,"-",EDLC,"-ac-",ACLC,".tex")))
    tools::texi2pdf(gsub("--","-",paste0(i,"-",EDLC,"-ac-",ACLC,".tex")))
    #EDProfile pdf Link for RMD
    EDLinkPDF<- paste0("<font size=\"5\"><a href=\"https://cdn.jsdelivr.net/gh/CSOIreland/edprofiles@resources/2022/health/pdf/",i,"-",EDLC,"-ac-",ACLC,".pdf\""," style=\"text-decoration: none\">A more detailed and print friendly pdf profile - with accompanying tables - is available here.</a></font>")
    EDLinkPDF <- gsub("--","-", EDLinkPDF)
    #render R Markdown  
    rmarkdown::render("HealthProfileMarkdown.Rmd", output_file =gsub("--","-",paste0(i,"-",EDLC,"-ac-",ACLC,".html")))
    
    
    # print progress
    print(paste0("Iteration ", i, " of ", nrow(EDWGUIDAC), " complete(",round(i*100/nrow(EDWGUIDAC),1),"%)"))
    
    # log errors
  }, error = function(e) {
    ErrorMessage <- paste0("Error in Iteration _", i ,"_: for ED -",ED,"- ", conditionMessage(e))
    ErrorList <<- c(ErrorList,ErrorMessage)
    SkipToNext <<-TRUE
  }
  )
  #if SkipToNext is TRUE, skip to next iteration
  if(SkipToNext) { next }
}

# Create the ED search map
source(paste0(getwd(),"/19_EDSearchTool.R"))

print(ErrorList)

#write a csv of errors
write.csv(as.data.frame(ErrorList), file = paste0(OutputFilesLoc,"/ErrorList.csv"))

#setWD so if rerunning runs correctly
setwd(RootWD)

###Run was randomly crashing before error list was saved so had to identify runs that didn't complete below

##List files
FileListPDF <- as.data.frame(list.files(paste0("../reports/pdf/")))
FileListHTML <- as.data.frame(list.files(paste0("../reports/html/")))
#List HTML only
colnames(FileListHTML) <- "FileName"
#Extract file numbers from filenames
FileNumberList <- as.numeric(sub("-.*","",FileListHTML$FileName))

#compare numbers completed with entire set that should be completed to identify failed runs
FullSequence <- seq(1, 3420)
MissingHTMLFiles <- setdiff(FullSequence, FileNumberList)

# Do the same for PDFs
colnames(FileListPDF) <- "FileName"
FileNumberListPDF <- as.numeric(sub("-.*","",FileListPDF$FileName))
MissingPDFFiles <- setdiff(FullSequence, FileNumberListPDF)

# join missing pdfs and htmls
UnionMissingFiles <- union(MissingHTMLFiles, MissingPDFFiles)
str(MissingPDFFiles)

#investigate missing
EDWGUIDACMissing <- EDWGUIDAC[UnionMissingFiles,]
