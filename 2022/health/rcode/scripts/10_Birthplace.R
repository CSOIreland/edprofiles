
#BirthED table and percentages
BirthED <- BirthSource%>%filter(CSO.Electoral.Divisions.2022 == ED)
BirthED$PercentageOfPopulation <- BirthED$value*100/BirthED$value[BirthED$Location == "Total"]
BirthED[is.na(BirthED)] <- 0
BirthED$Location <- gsub("&", "and", BirthED$Location)

#figures for those outside ireland, total pop usually resident pop and percentage
BirthplaceOIED <- sum(BirthED$value[BirthED$Location != "State" &BirthED$Location != "Not stated" &BirthED$Location != "Total"])
BirthplaceTotalED <- BirthED$value[BirthED$Location == "Total"]
BirthplaceOIEDPerc <- sprintf("%.1f", round(BirthplaceOIED*100/BirthplaceTotalED,1))

# state table and percentages
BirthState <- BirthSource%>%filter(CSO.Electoral.Divisions.2022 == "State")
BirthState$PercentageOfPopulation <- BirthState$value*100/BirthState$value[BirthState$Location == "Total"]
BirthState[is.na(BirthState)] <- 0

#figures for those outside ireland, total pop usually resident pop and percentage
BirthState$Location <- gsub("&", "and", BirthState$Location)
BirthplaceOIState <- sum(BirthState$value[BirthState$Location != "State" &BirthState$Location != "Not stated" &BirthState$Location != "Total"])
BirthplaceTotalState <- BirthState$value[BirthState$Location == "Total"]
BirthplaceOIStatePerc <- sprintf("%.1f", round(BirthplaceOIState*100/BirthplaceTotalState,1))

#ac table and percentages
BirthAC <- BirthSource%>%filter(CSO.Electoral.Divisions.2022 == AC)
BirthAC$PercentageOfPopulation <- BirthAC$value*100/BirthAC$value[BirthAC$Location == "Total"]
BirthAC[is.na(BirthAC)] <- 0
#figures for those outside ireland, total pop usually resident pop and percentage
BirthAC$Location <- gsub("&", "and", BirthAC$Location)
BirthplaceOIAC <- sum(BirthAC$value[BirthAC$Location != AC &BirthAC$Location != "Not ACd" &BirthAC$Location != "Total"])
BirthplaceTotalAC <- BirthAC$value[BirthAC$Location == "Total"]
BirthplaceOIACPerc <- sprintf("%.1f", round(BirthplaceOIAC*100/BirthplaceTotalAC,1))

# ed without total for plot, wrap lables and factorise
BirthEDLessT <- BirthED%>%filter(Location!= "Total")
BirthEDLessT$Location2 <- stringr::str_wrap(BirthEDLessT$Location, 25)
BirthEDLessT$Location2 <- factor(BirthEDLessT$Location2, levels = c("Ireland", "United Kingdom","Poland","India", "Other EU272020 (Exec\nIreland and Poland)", "Rest of World","Not stated"))

# state without total for plot, wrap lables and factorise
BirthStateLessT <- BirthState%>%filter(Location!= "Total")
BirthStateLessT$Location2 <- stringr::str_wrap(BirthStateLessT$Location, 25)
BirthStateLessT$Location2 <- factor(BirthStateLessT$Location2, levels = c("Ireland", "United Kingdom","Poland","India", "Other EU272020 (Exec\nIreland and Poland)", "Rest of World","Not stated"))

# ac without total for plot, wrap lables and factorise
BirthACLessT <- BirthAC%>%filter(Location!= "Total")
BirthACLessT$Location2 <- stringr::str_wrap(BirthACLessT$Location, 25)
BirthACLessT$Location2 <- factor(BirthACLessT$Location2, levels = c("Ireland", "United Kingdom","Poland","India", "Other EU272020 (Exec\nIreland and Poland)", "Rest of World","Not stated"))

# bind three tables for plot
BirthLessTAll <- bind_rows(BirthEDLessT,BirthACLessT,BirthStateLessT)
BirthLessTAll$CSO.Electoral.Divisions.2022 <- factor(BirthLessTAll$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
#plot
BirthPlot  <- ggplot(BirthLessTAll , aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x=Location2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Birthplace")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Usually Resident Population")

#export plot for latex
pdf(paste0(getwd(),"/figures/BirthED.pdf"))
print(BirthPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/BirthED.svg"))
print(BirthPlot)
dev.off()

#reformat percentages for table
BirthED$PercentageOfPopulation <- sprintf("%.1f", round(BirthED$PercentageOfPopulation,1))
BirthState$PercentageOfPopulation <- sprintf("%.1f", round(BirthState$PercentageOfPopulation,1))
BirthAC$PercentageOfPopulation <- sprintf("%.1f", round(BirthAC$PercentageOfPopulation,1))
