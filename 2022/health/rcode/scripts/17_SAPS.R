SAPSPercentagesEDACState <- SAPSPercentages[SAPSPercentages$GUID == EDGUID | 	SAPSPercentages$GUID == "IE0"|SAPSPercentages$GUID == ACGUID,]
SAPSPercentagesEDACState <- SAPSPercentagesEDACState[!duplicated(SAPSPercentagesEDACState$GUID),]
# Add region var
SAPSPercentagesEDACState$Region[SAPSPercentagesEDACState$GUID == "IE0"] <- "State"
SAPSPercentagesEDACState$Region[SAPSPercentagesEDACState$GUID == EDGUID] <- "ED"
SAPSPercentagesEDACState$Region[SAPSPercentagesEDACState$GUID == ACGUID] <- "AC"

# Transpose the dataframe so tables can be more easily created
SAPSTransposed <- as.data.frame(t(SAPSPercentagesEDACState))
colnames(SAPSTransposed) <- SAPSTransposed[rownames(SAPSTransposed) == "Region",]
SAPSTransposed <- SAPSTransposed[rownames(SAPSTransposed)!="Region" & rownames(SAPSTransposed)!="GUID", ]
SAPSTransposed$Lookup <- rownames(SAPSTransposed)
# SAPSTransposed$AbsDiff <- round(as.numeric(SAPSTransposed$ED) - as.numeric(SAPSTransposed$State),1)
# SAPSTransposed$RelDiff <- round(as.numeric(SAPSTransposed$ED)*100/as.numeric(SAPSTransposed$AC) - 100,1)

# merge with glossary file so appropriate tables can be created using columns from glossary
SAPSWGlossary <- merge(SAPSTransposed, SAPSGlossary, by.x = "Lookup", by.y = "ColName", all.x=T)
SAPSWGlossary[is.na(SAPSWGlossary)] <- "Missing"


# Columns to select for each dataframe
ColsToSelect <- c("Label", "ED", "AC","State", "LatexTableTitle","Rank")
SAPSWGlossary$LatexTableTitle <- factor(SAPSWGlossary$LatexTableTitle,levels = unique(SAPSWGlossary$LatexTableTitle))

# Split the dataframes based on latex table title
SplitDataframes <- SAPSWGlossary %>%
  dplyr::select(all_of(ColsToSelect)) %>%
  group_split(LatexTableTitle, .keep = F)


# Naming the list elements with make.names of the values in  latextabletitle column
names(SplitDataframes) <- make.names(unique(SAPSWGlossary$LatexTableTitle))

# Creating individual dataframes from the list
list2env(SplitDataframes, envir = .GlobalEnv)


#Ordering the tables by the "rank column"
Education..percentage.of.those.aged.15.. <- as.data.frame(Education..percentage.of.those.aged.15..)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Employment..percentage.of.those.aged.15.. <- as.data.frame(Employment..percentage.of.those.aged.15..)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Employment..percentage.of.those.at.work. <- as.data.frame(Employment..percentage.of.those.at.work.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Families..percentage.of.family.units. <- as.data.frame(Families..percentage.of.family.units.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Health..percentage.of.population. <- as.data.frame(Health..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Household.Composition..percentage.of.private.households. <- as.data.frame(Household.Composition..percentage.of.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.households. <- as.data.frame(Housing..percentage.of.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.permanent.dwellings. <- as.data.frame(Housing..percentage.of.permanent.dwellings.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.permanent.private.households. <- as.data.frame(Housing..percentage.of.permanent.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.private.households. <- as.data.frame(Housing..percentage.of.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Marital.Status..percentage.of.population. <- as.data.frame(Marital.Status..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Migration.and.Ethnicity..percentage.of.population. <- as.data.frame(Migration.and.Ethnicity..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Migration.and.Ethnicity..percentage.of.usually.resident.population. <- as.data.frame(Migration.and.Ethnicity..percentage.of.usually.resident.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Occupation..percentage.of.those.at.work.or.unemployed. <- as.data.frame(Occupation..percentage.of.those.at.work.or.unemployed.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Social.Class..percentage.of.population... <- as.data.frame(Social.Class..percentage.of.population...)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Socio.economic.group.of.reference.person..percentage.of.private.households... <- as.data.frame(Socio.economic.group.of.reference.person..percentage.of.private.households...)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Transport..percentage.of.population.aged.5.years.and.over. <- as.data.frame(Transport..percentage.of.population.aged.5.years.and.over.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Other <- as.data.frame(Other)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Age1 <- as.data.frame(Age1)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)


# refactoring the age column of the age table
Age1$AgeNew <- ifelse(Age1$Label == "Age 0 - Total"|Age1$Label=="Age 1 - Total"|Age1$Label=="Age 2 - Total"|Age1$Label=="Age 3 - Total"|Age1$Label=="Age 4 - Total"|Age1$Label == "Age 5 - Total"|Age1$Label=="Age 6 - Total"|Age1$Label=="Age 7 - Total"|Age1$Label=="Age 8 - Total"|Age1$Label=="Age 9 - Total"|Age1$Label == "Age 10 - Total"|Age1$Label=="Age 11 - Total"|Age1$Label=="Age 12 - Total"|Age1$Label=="Age 13 - Total"|Age1$Label=="Age 14 - Total", "0-14",
              ifelse(Age1$Label=="Age 15 - Total"|Age1$Label=="Age 16 - Total"|Age1$Label=="Age 17 - Total"|Age1$Label=="Age 18 - Total"|Age1$Label=="Age 19 - Total"|Age1$Label == "Age 20 - 24 - Total"|Age1$Label=="Age 25 - 29 - Total"|Age1$Label=="Age 30 - 34 - Total"|Age1$Label=="Age 35 - 39 - Total"|Age1$Label=="Age 40 - 44 - Total"|Age1$Label == "Age 45 - 49 - Total"|Age1$Label=="Age 50 - 54 - Total"|Age1$Label=="Age 55 - 59 - Total"|Age1$Label=="Age 60 - 64 - Total", "15-64",
                                   ifelse(Age1$Label == "Age 65 - 69 - Total"|Age1$Label=="Age 70 - 74 - Total"|Age1$Label == "Age 75 - 79 - Total"|Age1$Label=="Age 80 - 84 - Total"|Age1$Label == "Age 85 and over - Total", "65+", "Missing")))
#changing format of Age columns so rank works successfully
Age1$ED <- as.numeric(Age1$ED)
Age1$AC <- as.numeric(Age1$AC)
Age1$State <- as.numeric(Age1$State)
Age1$Rank <- as.character(Age1$Rank)
Age2 <- Age1%>%group_by(AgeNew)%>%dplyr::summarise(across(where(is.numeric), sum), across(where(is.character), ~first(.)))
Age2$Rank <- as.numeric(Age2$Rank)
Age2 <- Age2[order(Age2$Rank),]

# changing the labels of the transport tables
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare on foot", "On Foot", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Bicycle", "By Bicycle", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Bus, minibus or coach", "By Bus, minibus or coach", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Train, DART or LUAS", "By Train, DART or LUAS", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Motorcycle or scooter", "By Motorcycle or scooter", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Car driver", "Car driver", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Car passenger", "Car passenger", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Van", "By Van", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Other (incl. lorry)", "By Other (incl. lorry)", Transport..percentage.of.population.aged.5.years.and.over.$Label, fixed=T)



