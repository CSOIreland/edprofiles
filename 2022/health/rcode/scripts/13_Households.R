#REad ED and AC  table from PXSTat
HouseholdsSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3ED/PX/2013/")
HouseholdsSourceTable <- as.data.frame(HouseholdsSourceTable.px)%>%filter(Statistic == "Permanent private households")

HouseholdsSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3CTY/PX/2013/")
HouseholdsSourceTableAC<- as.data.frame(HouseholdsSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland" & Statistic == "Permanent private households")

#EDs as cahracter for correct join
HouseholdsSourceTable$CSO.Electoral.Divisions.2022<- as.character(HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
HouseholdsSourceTable$CSO.Electoral.Divisions.2022[HouseholdsSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022)

HouseholdsSourceTable <- rbind(HouseholdsSourceTable,HouseholdsSourceTableAC)


HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)

# ED Table
HouseholdsED <- HouseholdsSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to Households table so percentage of population can be calculated
HouseholdsED$TotalPop <- HouseholdsED$value[HouseholdsED$Type.of.Occupancy == "Total"]

# Calculate percentage of population
HouseholdsED$PercentageOfPopulation <- HouseholdsED$value*100/HouseholdsED$TotalPop

#State table
HouseholdsState <- HouseholdsSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
HouseholdsState$TotalPop <- HouseholdsState$value[HouseholdsState$Type.of.Occupancy == "Total"]

# Calculate percentage of pop
HouseholdsState$PercentageOfPopulation <- HouseholdsState$value*100/HouseholdsState$TotalPop

# AC table
HouseholdsAC <- HouseholdsSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated

HouseholdsAC$TotalPop <- HouseholdsAC$value[HouseholdsAC$Type.of.Occupancy == "Total"]

# Calculate percentage of pop
HouseholdsAC$PercentageOfPopulation <- HouseholdsAC$value*100/HouseholdsAC$TotalPop

# Bind all tables for plot
HouseholdsBinded <- bind_rows(HouseholdsED,HouseholdsAC, HouseholdsState)%>%filter(Type.of.Occupancy!="Total")
HouseholdsBinded$CSO.Electoral.Divisions.2022 <- factor(HouseholdsBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))


#wrap labels and factorise
HouseholdsBinded$Type.of.Occupancy2 <- stringr::str_wrap(HouseholdsBinded$Type.of.Occupancy, 25)
HouseholdsBinded$Type.of.Occupancy2 <- factor(HouseholdsBinded$Type.of.Occupancy2, levels = c("Owned with mortgage or\nloan", "Owned outright","Rented from private\nlandlord","Rented from Local\nAuthority", "Rented from\nvoluntary/co-operative\nhousing body", "Occupied free of rent","Not stated"))


# Plot
HouseholdsPlot <- ggplot(HouseholdsBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x = Type.of.Occupancy2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Type of Occupancy")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Households")

#export plot for latex
pdf(paste0(getwd(),"/figures/HouseholdsED.pdf"))
print(HouseholdsPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/HouseholdsED.svg"))
print(HouseholdsPlot)
dev.off()


# Reformat percentages so they are correct for tables
HouseholdsState$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsState$PercentageOfPopulation,1))
HouseholdsED$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsED$PercentageOfPopulation,1))
HouseholdsAC$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsAC$PercentageOfPopulation,1))
