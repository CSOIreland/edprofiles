#########################################SAPS###################################
# import SAPS files and SAPS Glossary
SAPSED <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_CSOED3270923.csv"), header = T)
SAPSCounty <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_county_270923.csv"), header = T)
SAPSGlossary<- read.csv(paste0(InputFilesLoc,"/SAPS2022/GlossaryEditForPercentages.csv"), header = T)
SAPSTotal <- rbind(SAPSED,SAPSCounty)


for (j in 1:nrow(SAPSGlossary)) {
  # Get the column name and total column name from the csv
  ColumnName <- SAPSGlossary$ColName[j]
  TotalColumnName <- SAPSGlossary$TotalColForPerc[j]
  
  # Create a new column name with "perc" appended
  NewColumnName <- paste0(ColumnName, "_Perc")
  
  # Calculate the new column 
  SAPSTotal <- SAPSTotal %>%
    dplyr::mutate({{NewColumnName }} := get(ColumnName)*100 / get(TotalColumnName))
}

# new_col will be added to main_df for each iteration
SAPSPercentages <- SAPSTotal%>%dplyr::select(matches("_Perc"))
SAPSPercentages$GUID <- SAPSTotal$GUID
colnames(SAPSPercentages) <- gsub("_Perc","", colnames(SAPSPercentages))

# round the percentages to one decimal place
SAPSPercentages <- SAPSPercentages%>%mutate_if(is.numeric, function(x) round(x,1))

# Select the appropriate ED,AC and State from the SAPS percentages file
SAPSPercentages <- SAPSPercentages[!duplicated(SAPSPercentages$GUID),]
SAPSTotal <- SAPSTotal[!duplicated(SAPSTotal$GUID),]

SAPSPercentages$ED <- SAPSTotal$GEOGDESC
saveRDS(SAPSPercentages, file = paste0(OutputFilesLoc,"/SAPSPercentages.Rds"))
