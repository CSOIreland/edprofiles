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


SAPSPercentages <- readRDS(paste0(OutputFilesLoc,"/SAPSPercentages.Rds"))
ForJoin <- read.csv(paste0(InputFilesLoc, "/EDCountyLink.csv"))
SAPSPercentages <- merge(SAPSPercentages, ForJoin, by.x = "GUID", by.y = "ED_GUID",all.x=T)

##############VERY GOOD HEALTH####################
SAPSPercentages$VeryGoodHealth <- SAPSPercentages$T12_3_VGT

VeryGoodHealth <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, VeryGoodHealth)%>%arrange(desc(VeryGoodHealth))%>%slice_head(n=10)
VeryGoodHealth$Rank <- 1:10
VeryGoodHealth$Ireland <- SAPSPercentages$VeryGoodHealth[SAPSPercentages$GUID == "IE0"]

VeryGoodHealthBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, VeryGoodHealth)%>%arrange(desc(VeryGoodHealth))%>%slice_tail(n=10)
VeryGoodHealthBottom$Rank <- 3412:3421


write.csv(VeryGoodHealth[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","VeryGoodHealth", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_VeryGoodHealth.csv"), row.names = F)


###############VERY BAD HEALTH#########################
SAPSPercentages$VeryBadHealth <- SAPSPercentages$T12_3_VBT

VeryBadHealth <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, VeryBadHealth)%>%arrange(desc(VeryBadHealth))%>%slice_head(n=10)
VeryBadHealth$Rank <- 1:10
VeryBadHealth$Ireland <- SAPSPercentages$VeryBadHealth[SAPSPercentages$GUID == "IE0"]

VeryBadHealthBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, VeryBadHealth)%>%arrange(desc(VeryBadHealth))%>%slice_tail(n=10)
VeryBadHealthBottom$Rank <- 3412:3421


write.csv(VeryBadHealth[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","VeryBadHealth", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_VeryBadHealth.csv"), row.names = F)


###################Carers################################
SAPSPercentages$Carers <- SAPSPercentages$T12_2_T
Carers <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Carers)%>%arrange(desc(Carers))%>%slice_head(n=10)
Carers$Rank <- 1:10
Carers$Ireland <- SAPSPercentages$Carers[SAPSPercentages$GUID == "IE0"]

CarersBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Carers)%>%arrange(desc(Carers))%>%slice_tail(n=10)
CarersBottom$Rank <- 3412:3421

write.csv(Carers[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Carers", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Carers.csv"), row.names = F)


##########DISABILITY###################
SAPSPercentages$Disability <- SAPSPercentages$T12_1_T
Disability <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Disability)%>%arrange(desc(Disability))%>%slice_head(n=10)
Disability$Rank <- 1:10
Disability$Ireland <- SAPSPercentages$Disability[SAPSPercentages$GUID == "IE0"]

DisabilityBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Disability)%>%arrange(desc(Disability))%>%slice_tail(n=10)
DisabilityBottom$Rank <- 3412:3421

write.csv(Disability[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Disability", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Disability.csv"), row.names = F)

######################SMOKING################
SAPSPercentages$Smoking <- SAPSPercentages$T12_4_YES
Smoking <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Smoking)%>%arrange(desc(Smoking))%>%slice_head(n=10)
Smoking$Rank <- 1:10
Smoking$Ireland <- SAPSPercentages$Smoking[SAPSPercentages$GUID == "IE0"]

SmokingBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Smoking)%>%arrange(desc(Smoking))%>%slice_tail(n=10)
SmokingBottom$Rank <- 3412:3421

write.csv(Smoking[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Smoking", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Smoking.csv"), row.names = F)
