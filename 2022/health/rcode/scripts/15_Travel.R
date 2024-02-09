
# ED Table
TravelED <- TravelSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to Travel table so percentage of population can be calculated
TravelED$TotalPop <- TravelED$value[TravelED$Means.of.Travel == "Total"]

# Calculate percentage of population
TravelED$PercentageOfPopulation <- TravelED$value*100/TravelED$TotalPop

#State table
TravelState <- TravelSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
TravelState$TotalPop <- TravelState$value[TravelState$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelState$PercentageOfPopulation <- TravelState$value*100/TravelState$TotalPop

# AC table
TravelAC <- TravelSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated

TravelAC$TotalPop <- TravelAC$value[TravelAC$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelAC$PercentageOfPopulation <- TravelAC$value*100/TravelAC$TotalPop

# Bind all tables for plot
TravelBinded <- bind_rows(TravelED,TravelAC, TravelState)%>%filter(Means.of.Travel!="Total")
TravelBinded$CSO.Electoral.Divisions.2022 <- factor(TravelBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))

#wrap labels and factorise
TravelBinded$Means.of.Travel2 <- stringr::str_wrap(TravelBinded$Means.of.Travel, 25)
TravelBinded$Means.of.Travel2 <- factor(TravelBinded$Means.of.Travel2, levels = c("On Foot", "Bicycle","Bus, minibus or coach","Train, DART or LUAS", "Motorcycle or scooter", "Car Driver","Car passenger","Van","Other (incl. lorry)","Work mainly at or from\nhome","Not stated"))


# Plot
TravelPlot <- ggplot(TravelBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x = Means.of.Travel2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_x_discrete(name = "Means of Travel")+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Usually Resident Population")

#export plot for latex
pdf(paste0(getwd(),"/figures/TravelED.pdf"))
print(TravelPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/TravelED.svg"))
print(TravelPlot)
dev.off()



# Reformat percentages so they are correct for tables
TravelState$PercentageOfPopulation <- sprintf("%.1f", round(TravelState$PercentageOfPopulation,1))
TravelED$PercentageOfPopulation <- sprintf("%.1f", round(TravelED$PercentageOfPopulation,1))
TravelAC$PercentageOfPopulation <- sprintf("%.1f", round(TravelAC$PercentageOfPopulation,1))
