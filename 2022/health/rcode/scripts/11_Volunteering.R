#REad ED and AC  table from PXSTat
VolunteersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T7T1ED/PX/2013/")
VolunteersSourceTable <- as.data.frame(VolunteersSourceTable.px)

VolunteersSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T7T1CTY/PX/2013/")
#removing ireland as this is already in the EDtable
VolunteersSourceTableAC <- as.data.frame(VolunteersSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")


#EDs as cahracter for correct join
VolunteersSourceTable$CSO.Electoral.Divisions.2022<- as.character(VolunteersSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
VolunteersSourceTable$CSO.Electoral.Divisions.2022[VolunteersSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
VolunteersSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire |D├║n Laoghaire","Dun Laoghaire", VolunteersSourceTableAC$CSO.Electoral.Divisions.2022)

VolunteersSourceTable <- rbind(VolunteersSourceTable,VolunteersSourceTableAC)


#remove special chars etc
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", VolunteersSourceTable$CSO.Electoral.Divisions.2022)
# ED Table
VolunteersED <- VolunteersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to Volunteers table so percentage of population can be calculated
VolunteersED$TotalPop <- TotalPopEDBothSexes

# Calculate percentage of population
VolunteersED$PercentageOfPopulation <- VolunteersED$value*100/VolunteersED$TotalPop

#State table
VolunteersState <- VolunteersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
VolunteersState$TotalPop <- TotalPopStateBothSexes

# Calculate percentage of pop
VolunteersState$PercentageOfPopulation <- VolunteersState$value*100/VolunteersState$TotalPop

# AC table
VolunteersAC <- VolunteersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated

VolunteersAC$TotalPop <- TotalPopACBothSexes

# Calculate percentage of pop
VolunteersAC$PercentageOfPopulation <- VolunteersAC$value*100/VolunteersAC$TotalPop

# Bind all tables for plot
VolunteersBinded <- bind_rows(VolunteersED,VolunteersAC, VolunteersState)
VolunteersBinded$CSO.Electoral.Divisions.2022 <- factor(VolunteersBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
# Plot
VolunteerPlot <- ggplot(VolunteersBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x = CSO.Electoral.Divisions.2022)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Region")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")

#export plot for latex
pdf(paste0(getwd(),"/figures/VolunteerED.pdf"))
print(VolunteerPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/VolunteerED.svg"))
print(VolunteerPlot)
dev.off()


# Reformat percentages so they are correct for tables
VolunteersState$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersState$PercentageOfPopulation,1))
VolunteersED$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersED$PercentageOfPopulation,1))
VolunteersAC$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersAC$PercentageOfPopulation,1))
