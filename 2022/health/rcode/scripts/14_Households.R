
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


# Highcharts plot for RMD
HouseholdsBinded$PercentageOfPopulation <- round(HouseholdsBinded$PercentageOfPopulation,1)
HouseholdsBinded$colouract <- 1
HouseholdsBinded$colouract[HouseholdsBinded$CSO.Electoral.Divisions.2022 == ED] <- '#405381'
HouseholdsBinded$colouract[HouseholdsBinded$CSO.Electoral.Divisions.2022 == AC] <- '#FCBE72'
HouseholdsBinded$colouract[HouseholdsBinded$CSO.Electoral.Divisions.2022 == "State"] <- '#13C1A5'

HouseholdsPlot2 <- highchart()|>       
  hc_add_series(HouseholdsBinded, "column", hcaes(x = Type.of.Occupancy2, y = PercentageOfPopulation, color = colouract,  group = CSO.Electoral.Divisions.2022), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Households"))|>
  hc_xAxis(type = "category",
           title = list(text = "Type of Occupancy"))


#export plot for latex
pdf(paste0(getwd(),"/figures/HouseholdsED.pdf"))
print(HouseholdsPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/HouseholdsED.svg"))
# print(HouseholdsPlot)
# dev.off()


# Reformat percentages so they are correct for tables
HouseholdsState$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsState$PercentageOfPopulation,1))
HouseholdsED$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsED$PercentageOfPopulation,1))
HouseholdsAC$PercentageOfPopulation <- sprintf("%.1f", round(HouseholdsAC$PercentageOfPopulation,1))
