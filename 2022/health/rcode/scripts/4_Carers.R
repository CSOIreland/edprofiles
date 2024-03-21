# ED Table
CarersED <- CarersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to carers table so percentage of population can be calculated
CarersED$TotalPop[CarersED$Sex == "Males"] <- TotalPopEDMales
CarersED$TotalPop[CarersED$Sex == "Females"] <- TotalPopEDFemales
CarersED$TotalPop[CarersED$Sex == "Both Sexes"] <- TotalPopEDBothSexes

# Calculate percentage of population
CarersED$PercentageOfPopulation <- CarersED$value*100/CarersED$TotalPop

#State table
CarersState <- CarersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
CarersState$TotalPop[CarersState$Sex == "Males"] <- TotalPopStateMales
CarersState$TotalPop[CarersState$Sex == "Females"] <- TotalPopStateFemales
CarersState$TotalPop[CarersState$Sex == "Both Sexes"] <- TotalPopStateBothSexes

# Calculate percentage of pop
CarersState$PercentageOfPopulation <- CarersState$value*100/CarersState$TotalPop[CarersState$Sex == "Both Sexes"]

# AC table
CarersAC <- CarersSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated
CarersAC$TotalPop[CarersAC$Sex == "Males"] <- TotalPopACMales
CarersAC$TotalPop[CarersAC$Sex == "Females"] <- TotalPopACFemales
CarersAC$TotalPop[CarersAC$Sex == "Both Sexes"] <- TotalPopACBothSexes

# Calculate percentage of pop
CarersAC$PercentageOfPopulation <- CarersAC$value*100/CarersAC$TotalPop

# Bind all tables for plot
CarersBinded <- bind_rows(CarersED,CarersAC, CarersState)
CarersBinded$CSO.Electoral.Divisions.2022 <- factor(CarersBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
# Plot
CarePlot <- ggplot(CarersBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x=Sex)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.25, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)))  +
  scale_x_discrete(name = "Sex")+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")

# Highcharts plot for RMD
CarersBinded$PercentageOfPopulation <- round(CarersBinded$PercentageOfPopulation,1)
CarersBinded$colouract <- 1
CarersBinded$colouract[CarersBinded$CSO.Electoral.Divisions.2022 == ED] <- '#405381'
CarersBinded$colouract[CarersBinded$CSO.Electoral.Divisions.2022 == AC] <- '#FCBE72'
CarersBinded$colouract[CarersBinded$CSO.Electoral.Divisions.2022 == "State"] <- '#13C1A5'

CarePlot2 <- highchart()|>       
  hc_add_series(CarersBinded, "column", hcaes(x = Sex, y = PercentageOfPopulation, color = colouract,  group = CSO.Electoral.Divisions.2022), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
    title = list(text = "Sex"))

#export plot for latex
pdf(paste0(getwd(),"/figures/CareED.pdf"))
print(CarePlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/CareED.svg"))
# print(CarePlot)
# dev.off()


# Reformat percentages so they are correct for tables
CarersState$PercentageOfPopulation <- sprintf("%.1f", round(CarersState$PercentageOfPopulation,1))
CarersED$PercentageOfPopulation <- sprintf("%.1f", round(CarersED$PercentageOfPopulation,1))
CarersAC$PercentageOfPopulation <- sprintf("%.1f", round(CarersAC$PercentageOfPopulation,1))
