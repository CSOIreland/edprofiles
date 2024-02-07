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

SAPSPercentages$PopulationAge15PUnemployed <- SAPSPercentages$T8_1_STUT + SAPSPercentages$T8_1_LTUT
PopulationAge15PUnemployed <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG,PopulationAge15PUnemployed)%>%arrange(desc(PopulationAge15PUnemployed))%>%slice_head(n=10)
PopulationAge15PUnemployed$Rank <- 1:10
PopulationAge15PUnemployed$Ireland <- SAPSPercentages$PopulationAge15PUnemployed[SAPSPercentages$GUID == "IE0"]

PopulationAge15PUnemployedBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, PopulationAge15PUnemployed)%>%arrange(desc(PopulationAge15PUnemployed))%>%slice_tail(n=10)
PopulationAge15PUnemployedBottom$Rank <- 3412:3421


write.csv(PopulationAge15PUnemployed[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG","PopulationAge15PUnemployed", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_PopulationAge15PUnemployed.csv"), row.names = F)

   
SAPSPercentages$GoodVeryGoodHealth <- SAPSPercentages$T12_3_VGT + SAPSPercentages$T12_3_GT

GoodVeryGoodHealth <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, GoodVeryGoodHealth)%>%arrange(desc(GoodVeryGoodHealth))%>%slice_head(n=10)
GoodVeryGoodHealth$Rank <- 1:10
GoodVeryGoodHealth$Ireland <- SAPSPercentages$GoodVeryGoodHealth[SAPSPercentages$GUID == "IE0"]

GoodVeryGoodHealthBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, GoodVeryGoodHealth)%>%arrange(desc(GoodVeryGoodHealth))%>%slice_tail(n=10)
GoodVeryGoodHealthBottom$Rank <- 3412:3421


write.csv(GoodVeryGoodHealth[,c("Rank", "ED_ENGLISH", "COUNTY_ENG","GoodVeryGoodHealth", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_GoodVeryGoodHealth.csv"), row.names = F)

  
SAPSPercentages$LoneParents <- SAPSPercentages$T5_1OPFC_H + SAPSPercentages$T5_1OPMC_H
LoneParents <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, LoneParents)%>%arrange(desc(LoneParents))%>%slice_head(n=10)
LoneParents$Rank <- 1:10
LoneParents$Ireland <- SAPSPercentages$LoneParents[SAPSPercentages$GUID == "IE0"]

LoneParentsBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, LoneParents)%>%arrange(desc(LoneParents))%>%slice_tail(n=10)
LoneParentsBottom$Rank <- 3412:3421


write.csv(LoneParents[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG","LoneParents", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_LoneParents.csv"), row.names = F)


  
  
SAPSPercentages$FamiliesWith5OrMoreChildren <- SAPSPercentages$T4_2_GE5CU15
FamiliesWith5OrMoreChildren <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, FamiliesWith5OrMoreChildren)%>%arrange(desc(FamiliesWith5OrMoreChildren))%>%slice_head(n=10)
FamiliesWith5OrMoreChildren$Rank <- 1:10
FamiliesWith5OrMoreChildren$Ireland <- SAPSPercentages$FamiliesWith5OrMoreChildren[SAPSPercentages$GUID == "IE0"]

FamiliesWith5OrMoreChildrenBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, FamiliesWith5OrMoreChildren)%>%arrange(desc(FamiliesWith5OrMoreChildren))%>%slice_tail(n=10)
FamiliesWith5OrMoreChildrenBottom$Rank <- 3412:3421


write.csv(FamiliesWith5OrMoreChildren[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "FamiliesWith5OrMoreChildren", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_FamiliesWith5OrMoreChildren.csv"), row.names = F)

  
SAPSPercentages$Carers <- SAPSPercentages$T12_2_T
Carers <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Carers)%>%arrange(desc(Carers))%>%slice_head(n=10)
Carers$Rank <- 1:10
Carers$Ireland <- SAPSPercentages$Carers[SAPSPercentages$GUID == "IE0"]

CarersBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Carers)%>%arrange(desc(Carers))%>%slice_tail(n=10)
CarersBottom$Rank <- 3412:3421

write.csv(Carers[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Carers", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Carers.csv"), row.names = F)


  
#add count
SAPSPercentages$SixtyFivePlus <- SAPSPercentages$T1_1AGE65_69T+ SAPSPercentages$T1_1AGE70_74T+ SAPSPercentages$T1_1AGE75_79T+ SAPSPercentages$T1_1AGE80_84T +SAPSPercentages$T1_1AGEGE_85T
SixtyFivePlus <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, SixtyFivePlus)%>%arrange(desc(SixtyFivePlus))%>%slice_head(n=10)
SixtyFivePlus$Rank <- 1:10
SixtyFivePlus$Ireland <- SAPSPercentages$SixtyFivePlus[SAPSPercentages$GUID == "IE0"]

SixtyFivePlusBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, SixtyFivePlus)%>%arrange(desc(SixtyFivePlus))%>%slice_tail(n=10)
SixtyFivePlusBottom$Rank <- 3412:3421

write.csv(SixtyFivePlus[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "SixtyFivePlus", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_SixtyFivePlus.csv"), row.names = F)


SAPSPercentages$Disability <- SAPSPercentages$T12_1_T
Disability <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Disability)%>%arrange(desc(Disability))%>%slice_head(n=10)
Disability$Rank <- 1:10
Disability$Ireland <- SAPSPercentages$Disability[SAPSPercentages$GUID == "IE0"]

DisabilityBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Disability)%>%arrange(desc(Disability))%>%slice_tail(n=10)
DisabilityBottom$Rank <- 3412:3421

write.csv(Disability[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Disability", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Disability.csv"), row.names = F)

SAPSPercentages$Smoking <- SAPSPercentages$T12_4_YES
Smoking <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Smoking)%>%arrange(desc(Smoking))%>%slice_head(n=10)
Smoking$Rank <- 1:10
Smoking$Ireland <- SAPSPercentages$Smoking[SAPSPercentages$GUID == "IE0"]

SmokingBottom <- SAPSPercentages%>%select(ED_ENGLISH,COUNTY_ENG, Smoking)%>%arrange(desc(Smoking))%>%slice_tail(n=10)
SmokingBottom$Rank <- 3412:3421

write.csv(Smoking[,c("Rank",  "ED_ENGLISH", "COUNTY_ENG", "Smoking", "Ireland")], file = paste0(OutputFilesLoc,"/Top10EDs_Smoking.csv"), row.names = F)

SAPSED <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_CSOED3270923.csv"), header = T)
EDPop <- SAPSED%>%group_by(GUID)%>%dplyr::summarise(TotalPopulation = sum(T1_1AGETT))

write.csv(EDPop, file = paste0(OutputFilesLoc,"/Top10EDs_EDPop.csv"))
