#REad ED and AC  table from PXSTat
RenewableEnergySourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T10ED/PX/2013/")
RenewableEnergySourceTable <- as.data.frame(RenewableEnergySourceTable.px)

RenewableEnergySourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T10CTY/PX/2013/")
#removing ireland as this is already in the EDtable
RenewableEnergySourceTableAC <- as.data.frame(RenewableEnergySourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")


#EDs as cahracter for correct join
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022<- as.character(RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022[RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
RenewableEnergySourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", RenewableEnergySourceTableAC$CSO.Electoral.Divisions.2022)

RenewableEnergySourceTable <- rbind(RenewableEnergySourceTable,RenewableEnergySourceTableAC)


#remove special chars etc
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)

# ED Table
RenewableEnergyED <- RenewableEnergySourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to RenewableEnergy table so percentage of population can be calculated
RenewableEnergyED$TotalPop <- RenewableEnergyED$value[RenewableEnergyED$Renewable.Energy == "All households"]

# Calculate percentage of population
RenewableEnergyED$PercentageOfPopulation <- RenewableEnergyED$value*100/RenewableEnergyED$TotalPop

#State table
RenewableEnergyState <- RenewableEnergySourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
RenewableEnergyState$TotalPop <- RenewableEnergyState$value[RenewableEnergyState$Renewable.Energy == "All households"]

# Calculate percentage of pop
RenewableEnergyState$PercentageOfPopulation <- RenewableEnergyState$value*100/RenewableEnergyState$TotalPop

# AC table
RenewableEnergyAC <- RenewableEnergySourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated

RenewableEnergyAC$TotalPop <- RenewableEnergyAC$value[RenewableEnergyAC$Renewable.Energy == "All households"]

# Calculate percentage of pop
RenewableEnergyAC$PercentageOfPopulation <- RenewableEnergyAC$value*100/RenewableEnergyAC$TotalPop

# Bind all tables for plot
RenewableEnergyBinded <- bind_rows(RenewableEnergyED,RenewableEnergyAC, RenewableEnergyState)%>%filter(Renewable.Energy!= "All households")
RenewableEnergyBinded$CSO.Electoral.Divisions.2022 <- factor(RenewableEnergyBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))

#wrap labels and factorise
RenewableEnergyBinded$RenewableEnergy2 <- stringr::str_wrap(RenewableEnergyBinded$Renewable.Energy, 25)
RenewableEnergyBinded$RenewableEnergy2 <- factor(RenewableEnergyBinded$RenewableEnergy2, levels = c("No renewable energy\nsources", "Renewable energy source\nnot stated","Has at least one\nrenewable energy source\nof any type","All households"))


# Plot
RenewableEnergyPlot <- ggplot(RenewableEnergyBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x = RenewableEnergy2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Type of Occupancy")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of RenewableEnergy")

#export plot for latex
pdf(paste0(getwd(),"/figures/RenewableEnergyED.pdf"))
print(RenewableEnergyPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/RenewableEnergyED.svg"))
print(RenewableEnergyPlot)
dev.off()


# Reformat percentages so they are correct for tables
RenewableEnergyState$PercentageOfPopulation <- sprintf("%.1f", round(RenewableEnergyState$PercentageOfPopulation,1))
RenewableEnergyED$PercentageOfPopulation <- sprintf("%.1f", round(RenewableEnergyED$PercentageOfPopulation,1))
RenewableEnergyAC$PercentageOfPopulation <- sprintf("%.1f", round(RenewableEnergyAC$PercentageOfPopulation,1))
