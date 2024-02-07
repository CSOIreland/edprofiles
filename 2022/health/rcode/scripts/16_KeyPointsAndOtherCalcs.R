
#key points stats - names used here should be self explanatory
Area.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/F1011/PX/2013/")
Area <- as.data.frame(Area.px)


Area$Electoral.Divisions <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",Area$Electoral.Divisions)
Area$Electoral.Divisions <- gsub("D├║n Laoghaire","Dun Laoghaire", Area$Electoral.Divisions)
Area$Electoral.Divisions<- gsub("'","", Area$Electoral.Divisions)
Area$Electoral.Divisions<- gsub("&","and", Area$Electoral.Divisions)

ED<- as.character(ED)
AreaED <- Area%>%filter(Electoral.Divisions == ED)
EDPopDens <- AreaED$value[AreaED$Statistic == "Population density (persons per sq km)"]
EDArea <- AreaED$value[AreaED$Statistic == "Area (sq km)"]
AreaState <- Area%>%filter(Electoral.Divisions == "Ireland")
StatePopDens <- AreaState$value[AreaState$Statistic == "Population density (persons per sq km)"]
StateArea <- AreaState$value[AreaState$Statistic == "Area (sq km)"]

#families
FamiliesInPrivateHouseholds.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T4T1ED/PX/2013/")
  
FamiliesInPrivateHouseholds <- as.data.frame(FamiliesInPrivateHouseholds.px)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("'","", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("&","and", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)

EDFamiliesInPrivateHouseholds <- FamiliesInPrivateHouseholds$value[FamiliesInPrivateHouseholds$Household.Size == "Total" & FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 == ED & FamiliesInPrivateHouseholds$Statistic == "Number of families in private households"]
StateFamiliesInPrivateHouseholds <- FamiliesInPrivateHouseholds$value[FamiliesInPrivateHouseholds$Household.Size == "Total" & FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 == "State" & FamiliesInPrivateHouseholds$Statistic == "Number of families in private households"]

#age and dependency ratio
EDAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependency <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)
EDYouthDependency <- round((EDAged0to14)*100/EDAged1564,1)
EDOldAgeDependency <- round((EDAged65P)*100/EDAged1564,1)

EDAged0to14Fem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65PFem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564Fem <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Females" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotalFem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependencyFem <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)

EDAged0to14Mal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65PMal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564Mal <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Males" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotalMal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependencyMal <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)

EDAged0to14Perc <- sprintf("%.1f", round(EDAged0to14*100/TotalPopEDBothSexes,1))
EDAged15to64Perc <- sprintf("%.1f", round(EDAged1564*100/TotalPopEDBothSexes,1))
EDAged65PPerc <- sprintf("%.1f", round(EDAged65P*100/TotalPopEDBothSexes,1))

EDAged0to14PercMal <- sprintf("%.1f", round(EDAged0to14Mal*100/TotalPopEDMales,1))
EDAged15to64PercMal <- sprintf("%.1f", round(EDAged1564Mal*100/TotalPopEDMales,1))
EDAged65PPercMal <- sprintf("%.1f", round(EDAged65PMal*100/TotalPopEDMales,1))

EDAged0to14PercFem <- sprintf("%.1f", round(EDAged0to14Fem*100/TotalPopEDFemales,1))
EDAged15to64PercFem <- sprintf("%.1f", round(EDAged1564Fem*100/TotalPopEDFemales,1))
EDAged65PPercFem <- sprintf("%.1f", round(EDAged65PFem*100/TotalPopEDFemales,1))

StateAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == "State" & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
StateAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == "State"  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
StateAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == "State"  & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
StateAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == "State"  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
StateAgeDependency <- round((StateAged0to14+StateAged65P)*100/StateAged1564,1)
StateYouthDependency <- round((StateAged0to14)*100/StateAged1564,1)
StateOldAgeDependency <- round((StateAged65P)*100/StateAged1564,1)

StateAged0to14Perc <- sprintf("%.1f", round(StateAged0to14*100/TotalPopStateBothSexes,1))
StateAged15to64Perc <- sprintf("%.1f", round(StateAged1564*100/TotalPopStateBothSexes,1))
StateAged65PPerc <- sprintf("%.1f", round(StateAged65P*100/TotalPopStateBothSexes,1))

ACAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == AC & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
ACAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
ACAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
ACAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$CSO.Electoral.Divisions.2022 == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
ACAgeDependency <- round((ACAged0to14+ACAged65P)*100/ACAged1564,1)
ACYouthDependency <- round((ACAged0to14)*100/ACAged1564,1)
ACOldAgeDependency <- round((ACAged65P)*100/ACAged1564,1)

ACAged0to14Perc <- sprintf("%.1f", round(ACAged0to14*100/TotalPopACBothSexes,1))
ACAged15to64Perc <- sprintf("%.1f", round(ACAged1564*100/TotalPopACBothSexes,1))
ACAged65PPerc <- sprintf("%.1f", round(ACAged65P*100/TotalPopACBothSexes,1))

#disability
PopDisabledED <-  EDDisWiderAllSexes$`Population with any disability`[EDDisWiderAllSexes$Age.Group == "All ages"]
PopDisabledPercED <- EDDisWiderAllSexes$`Population with a disability as a percentage`[EDDisWiderAllSexes$Age.Group == "All ages"]
PopDisabledState <- StateDisWiderAllSexes$`Population with any disability`[StateDisWiderAllSexes$Age.Group == "All ages"]
PopDisabledPercState <- StateDisWiderAllSexes$`Population with a disability as a percentage`[StateDisWiderAllSexes$Age.Group == "All ages"]

TotalCarersED <- CarersED$value[CarersED$Sex == "Both Sexes"]
PercCarersED <- round(as.numeric(CarersED$PercentageOfPopulation)[CarersED$Sex == "Both Sexes"],1)

TotalCarersState <- CarersState$value[CarersState$Sex == "Both Sexes"]
PercCarersState <- round(as.numeric(CarersState$PercentageOfPopulation)[CarersState$Sex == "Both Sexes"],1)

#health
BadVBadED <- sum(GenLongEDBothSexes$value[GenLongEDBothSexes$General.Health == "Bad" |GenLongEDBothSexes$General.Health == "Very Bad" ])
BadVBadEDPerc <- round(sum(as.numeric(GenLongEDBothSexes$Percentage.Of.Population)[GenLongEDBothSexes$General.Health == "Bad" |GenLongEDBothSexes$General.Health == "Very Bad" ]),1)


BadVBadState <- sum(GenLongStateBothSexes$value[GenLongStateBothSexes$General.Health == "Bad" |GenLongStateBothSexes$General.Health == "Very Bad" ])
BadVBadStatePerc <- round(sum(as.numeric(GenLongStateBothSexes$Percentage.Of.Population)[GenLongStateBothSexes$General.Health == "Bad" |GenLongStateBothSexes$General.Health == "Very Bad" ]),1)

#Education
LowerEduEDPerc <- as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "No formal education"]) +as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "Primary education"]) +as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "Lower secondary"])
LowerEduStatePerc <- as.numeric(EduStateTable$PercentageOfPopulation[EduStateTable$Highest.Level.of.Education.Completed == "No formal education"]) +as.numeric(EduStateTable$PercentageOfPopulation[EduStateTable$Highest.Level.of.Education.Completed == "Primary education"]) +as.numeric(EduStateTable$PercentageOfPopulation[EduStateTable$Highest.Level.of.Education.Completed == "Lower secondary"])

#Birthplace
OIEDPerc <- sum(as.numeric(BirthED$PercentageOfPopulation[BirthED$Location!="Ireland" & BirthED$Location!= "Not stated"& BirthED$Location!= "Total"]))
OIStatePerc <- sum(as.numeric(BirthState$PercentageOfPopulation[BirthState$Location!="Ireland" & BirthState$Location!= "Not stated"& BirthState$Location!= "Total"]))
                
                
#Occupancy
