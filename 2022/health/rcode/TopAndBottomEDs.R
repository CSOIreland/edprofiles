library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringi)


#set up working directory, input and output folders
RootWD <- setwd(getwd())
InputFilesLoc <- file.path(RootWD, "inputs")
OutputFilesLoc <- file.path(RootWD, "outputs")

SAPSED <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_CSOED3270923.csv"), header = T)
SAPSPercentages <- readRDS(paste0(OutputFilesLoc,"/SAPSPercentages.Rds"))
ForJoin <- read.csv(paste0(InputFilesLoc, "/EDCountyLink.csv"))
SAPSPercentages <- merge(SAPSPercentages, ForJoin, by.x = "GUID", by.y = "ED_GUID",all.x=T)
colnames(SAPSED) <- paste0("Total_", colnames(SAPSED))
SAPSAll <- merge(SAPSPercentages,SAPSED, by.x = "GUID", by.y = "Total_GUID" )

##############VERY GOOD HEALTH####################
SAPSAll$VeryGoodHealth <- SAPSAll$T12_3_VGT
SAPSAll$VeryGoodHealthTotal <- SAPSAll$Total_T12_3_VGT
SAPSAll$TotalPersons <- SAPSAll$Total_T12_3_TT

VeryGoodHealth <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,VeryGoodHealthTotal, TotalPersons, VeryGoodHealth)%>%arrange(desc(VeryGoodHealth))%>%slice_head(n=10)
VeryGoodHealth$Rank <- 1:10

VeryGoodHealthIreland<- SAPSAll%>%select(GUID, ED_ENGLISH,COUNTY_ENG,VeryGoodHealthTotal, TotalPersons, VeryGoodHealth)%>%filter(GUID == "IE0")%>%select(-GUID)
VeryGoodHealthIreland$Rank <- "-"
VeryGoodHealthIreland$ED_ENGLISH <- "State"
VeryGoodHealthIreland$COUNTY_ENG <- "State"


VeryGoodHealthBottom <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,VeryGoodHealthTotal, TotalPersons, VeryGoodHealth)%>%arrange(desc(VeryGoodHealth))%>%slice_tail(n=10)
VeryGoodHealthBottom$Rank <- 3411:3420

FillerRow <- VeryGoodHealthBottom[-c(1:10),]
FillerRow[1,] <- rep("...", ncol(FillerRow))

VeryGoodHealthFin <- rbind(VeryGoodHealth,FillerRow,VeryGoodHealthIreland,FillerRow, VeryGoodHealthBottom)


write.csv(VeryGoodHealthFin[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","VeryGoodHealthTotal","TotalPersons","VeryGoodHealth")], file = paste0(OutputFilesLoc,"/Top10EDs_VeryGoodHealth.csv"), row.names = F)


###############VERY BAD HEALTH#########################
SAPSAll$VeryBadHealth <- SAPSAll$T12_3_VBT
SAPSAll$VeryBadHealthTotal <- SAPSAll$Total_T12_3_VBT
SAPSAll$TotalPersons <- SAPSAll$Total_T12_3_TT

VeryBadHealth <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,VeryBadHealthTotal, TotalPersons, VeryBadHealth)%>%arrange(desc(VeryBadHealth))%>%slice_head(n=10)
VeryBadHealth$Rank <- 1:10

VeryBadHealthIreland<- SAPSAll%>%select(GUID, ED_ENGLISH,COUNTY_ENG,VeryBadHealthTotal, TotalPersons, VeryBadHealth)%>%filter(GUID == "IE0")%>%select(-GUID)
VeryBadHealthIreland$Rank <- "-"
VeryBadHealthIreland$ED_ENGLISH <- "State"
VeryBadHealthIreland$COUNTY_ENG <- "State"

VeryBadHealthBottom <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,VeryBadHealthTotal, TotalPersons, VeryBadHealth)%>%arrange(desc(VeryBadHealth))%>%slice_tail(n=10)
VeryBadHealthBottom$Rank <- 3411:3420

FillerRow <- VeryBadHealthBottom[-c(1:10),]
FillerRow[1,] <- rep("...", ncol(FillerRow))

VeryBadHealthFin <- rbind(VeryBadHealth,FillerRow,VeryBadHealthIreland,FillerRow, VeryBadHealthBottom)


write.csv(VeryBadHealthFin[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","VeryBadHealthTotal","TotalPersons","VeryBadHealth")], file = paste0(OutputFilesLoc,"/Top10EDs_VeryBadHealth.csv"), row.names = F)


###################Carers################################
SAPSAll$Carers <- SAPSAll$T12_2_T
SAPSAll$CarersTotal <- SAPSAll$Total_T12_2_T
SAPSAll$TotalPersons <- SAPSAll$Total_T1_1AGETT

Carers <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG, CarersTotal,TotalPersons,Carers)%>%arrange(desc(Carers))%>%slice_head(n=10)
Carers$Rank <- 1:10

CarersIreland<- SAPSAll%>%select(GUID,ED_ENGLISH,COUNTY_ENG, CarersTotal,TotalPersons,Carers)%>%filter(GUID == "IE0")%>%select(-GUID)
CarersIreland$Rank <- "-"
CarersIreland$ED_ENGLISH <- "State"
CarersIreland$COUNTY_ENG <- "State"

CarersBottom <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG, CarersTotal,TotalPersons,Carers)%>%arrange(desc(Carers))%>%slice_tail(n=10)
CarersBottom$Rank <- 3411:3420

FillerRow <- CarersBottom[-c(1:10),]
FillerRow[1,] <- rep("...", ncol(FillerRow))

CarersFin <- rbind(Carers,FillerRow,CarersIreland,FillerRow, CarersBottom)

write.csv(CarersFin[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","CarersTotal","TotalPersons","Carers")], file = paste0(OutputFilesLoc,"/Top10EDs_Carers.csv"), row.names = F)


##########DISABILITY###################
SAPSAll$Disability <- SAPSAll$T12_1_T
SAPSAll$DisabilityTotal <- SAPSAll$Total_T12_1_T
SAPSAll$TotalPersons <- SAPSAll$Total_T1_1AGETT

Disability <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,DisabilityTotal, TotalPersons, Disability)%>%arrange(desc(Disability))%>%slice_head(n=10)
Disability$Rank <- 1:10

DisabilityIreland<- SAPSAll%>%select(GUID, ED_ENGLISH,COUNTY_ENG,DisabilityTotal, TotalPersons, Disability)%>%filter(GUID == "IE0")%>%select(-GUID)
DisabilityIreland$Rank <- "-"
DisabilityIreland$ED_ENGLISH <- "State"
DisabilityIreland$COUNTY_ENG <- "State"

DisabilityBottom <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,DisabilityTotal, TotalPersons, Disability)%>%arrange(desc(Disability))%>%slice_tail(n=10)
DisabilityBottom$Rank <- 3411:3420

FillerRow <- DisabilityBottom[-c(1:10),]
FillerRow[1,] <- rep("...", ncol(FillerRow))

DisabilityFin <- rbind(Disability,FillerRow,DisabilityIreland,FillerRow, DisabilityBottom)

write.csv(DisabilityFin[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","DisabilityTotal","TotalPersons","Disability")], file = paste0(OutputFilesLoc,"/Top10EDs_Disability.csv"), row.names = F)

######################SMOKING################
SAPSAll$Smoking <- SAPSAll$T12_4_YES
SAPSAll$SmokingTotal <- SAPSAll$Total_T12_4_YES
SAPSAll$TotalPersons <- SAPSAll$Total_T12_4_T

Smoking <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,SmokingTotal, TotalPersons, Smoking)%>%arrange(desc(Smoking))%>%slice_head(n=10)
Smoking$Rank <- 1:10

SmokingIreland<- SAPSAll%>%select(GUID, ED_ENGLISH,COUNTY_ENG,SmokingTotal, TotalPersons, Smoking)%>%filter(GUID == "IE0")%>%select(-GUID)
SmokingIreland$Rank <- "-"
SmokingIreland$ED_ENGLISH <- "State"
SmokingIreland$COUNTY_ENG <- "State"

SmokingBottom <- SAPSAll%>%select(ED_ENGLISH,COUNTY_ENG,SmokingTotal, TotalPersons, Smoking)%>%arrange(desc(Smoking))%>%slice_tail(n=10)
SmokingBottom$Rank <- 3411:3420

FillerRow <- SmokingBottom[-c(1:10),]
FillerRow[1,] <- rep("...", ncol(FillerRow))

SmokingFin <- rbind(Smoking,FillerRow,SmokingIreland,FillerRow, SmokingBottom)

write.csv(SmokingFin[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","SmokingTotal","TotalPersons","Smoking")], file = paste0(OutputFilesLoc,"/Top10EDs_Smoking.csv"), row.names = F)
