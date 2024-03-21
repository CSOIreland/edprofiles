
#Separate ED, AC and State tables
SmokingED <- SmokingWider%>%filter(CSO.Electoral.Divisions.2022 == ED)
SmokingAC <-  SmokingWider%>%filter(CSO.Electoral.Divisions.2022 == AC)
SmokingState <- SmokingWider%>%filter(CSO.Electoral.Divisions.2022 == "State")

#Bind three tables for plot
SmokingEDACState <- rbind(SmokingED, SmokingAC,SmokingState)%>%dplyr::select(-c(Census.Year, Statistic))
SmokingEDACState$CSO.Electoral.Divisions.2022 <- factor(SmokingEDACState$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
SmokingEDACState$Census.Year <- "2022"

#plot smoking
SmokingPlot <- ggplot(SmokingEDACState , aes(fill=CSO.Electoral.Divisions.2022, y=PercentageWhoSmokeTobaccoProducts, x=Census.Year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "\n Persons who Smoke Tobacco Products (Daily and Occasionally)")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
SmokingEDACState$PercentageWhoSmokeTobaccoProducts <- round(SmokingEDACState$PercentageWhoSmokeTobaccoProducts,1)
SmokingEDACState$colouract <- 1
SmokingEDACState$colouract[SmokingEDACState$CSO.Electoral.Divisions.2022 == ED] <- '#405381'
SmokingEDACState$colouract[SmokingEDACState$CSO.Electoral.Divisions.2022 == AC] <- '#FCBE72'
SmokingEDACState$colouract[SmokingEDACState$CSO.Electoral.Divisions.2022 == "State"] <- '#13C1A5'

SmokingPlot2 <- highchart()|>       
  hc_add_series(SmokingEDACState, "column", hcaes(x = Census.Year, y = PercentageWhoSmokeTobaccoProducts, color = colouract,  group = CSO.Electoral.Divisions.2022), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "Persons who Smoke Tobacco Products (Daily and Occasionally)"))

#
#export plot for latex
pdf(paste0(getwd(),"/figures/SmokingED.pdf"))
print(SmokingPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/SmokingED.svg"))
# print(SmokingPlot)
# dev.off()



#reformat percentages to be correct for table
SmokingED$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingED$PercentageWhoSmokeTobaccoProducts,1))
SmokingState$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingState$PercentageWhoSmokeTobaccoProducts,1))
SmokingAC$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingAC$PercentageWhoSmokeTobaccoProducts,1))
