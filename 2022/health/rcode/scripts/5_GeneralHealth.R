
#ED subtables
GenEDMales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Males")
GenEDFemales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Females")
GenEDBothSexes<- GenWider%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Both Sexes")

#State subtables
GenStateMales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Males")
GenStateFemales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Females")
GenStateBothSexes<- GenWider%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Both Sexes")

#AC subtables
GenACMales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Males")
GenACFemales <- GenWider%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Females")
GenACBothSexes<- GenWider%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Both Sexes")

#long table ed males, including table without total for plot
GenLongEDMales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Males")
GenLongEDMales$Percentage.Of.Population <- GenLongEDMales$value*100/GenLongEDMales$value[GenLongEDMales$General.Health == "Total"]
GenLongEDMales$General.Health <- factor(GenLongEDMales$General.Health, levels = unique(GenLongEDMales$General.Health))
GenLongEDMalesGraph <- GenLongEDMales%>%filter(General.Health!="Total")


#long table ed females, including table without total for plot
GenLongEDFemales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Females")
GenLongEDFemales$Percentage.Of.Population <- GenLongEDFemales$value*100/GenLongEDFemales$value[GenLongEDFemales$General.Health == "Total"]
GenLongEDFemales$General.Health <- factor(GenLongEDFemales$General.Health, levels = unique(GenLongEDFemales$General.Health))
GenLongEDFemalesGraph <- GenLongEDFemales%>%filter(General.Health!="Total")


#long table ed both sexes, including table without total for plot
GenLongEDBothSexes<- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Both Sexes")
GenLongEDBothSexes$Percentage.Of.Population <- GenLongEDBothSexes$value*100/GenLongEDBothSexes$value[GenLongEDBothSexes$General.Health == "Total"]
GenLongEDBothSexes$General.Health <- factor(GenLongEDBothSexes$General.Health, levels = unique(GenLongEDBothSexes$General.Health))
GenLongEDBothSexesGraph <- GenLongEDBothSexes%>%filter(General.Health!="Total")


#long table state males, including table without total for plot
GenLongStateMales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Males")
GenLongStateMales$Percentage.Of.Population <- GenLongStateMales$value*100/GenLongStateMales$value[GenLongStateMales$General.Health == "Total"]
GenLongStateMales$General.Health <- factor(GenLongStateMales$General.Health, levels = unique(GenLongStateMales$General.Health))
GenLongStateMalesGraph <- GenLongStateMales%>%filter(General.Health!="Total")


#long table state females, including table without total for plot
GenLongStateFemales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Females")
GenLongStateFemales$Percentage.Of.Population <- GenLongStateFemales$value*100/GenLongStateFemales$value[GenLongStateFemales$General.Health == "Total"]
GenLongStateFemales$General.Health <- factor(GenLongStateFemales$General.Health, levels = unique(GenLongStateFemales$General.Health))
GenLongStateFemalesGraph <- GenLongStateFemales%>%filter(General.Health!="Total")


#long table state both sexes, including table without total for plot
GenLongStateBothSexes<- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Both Sexes")
GenLongStateBothSexes$Percentage.Of.Population <- GenLongStateBothSexes$value*100/GenLongStateBothSexes$value[GenLongStateBothSexes$General.Health == "Total"]
GenLongStateBothSexes$General.Health <- factor(GenLongStateBothSexes$General.Health, levels = unique(GenLongStateBothSexes$General.Health))
GenLongStateBothSexesGraph <- GenLongStateBothSexes%>%filter(General.Health!="Total")


#long table ac males, including table without total for plot
GenLongACMales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Males")
GenLongACMales$Percentage.Of.Population <- GenLongACMales$value*100/GenLongACMales$value[GenLongACMales$General.Health == "Total"]
GenLongACMales$General.Health <- factor(GenLongACMales$General.Health, levels = unique(GenLongACMales$General.Health))
GenLongACMalesGraph <- GenLongACMales%>%filter(General.Health!="Total")


#long table ac females, including table without total for plot
GenLongACFemales <- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Females")
GenLongACFemales$Percentage.Of.Population <- GenLongACFemales$value*100/GenLongACFemales$value[GenLongACFemales$General.Health == "Total"]
GenLongACFemales$General.Health <- factor(GenLongACFemales$General.Health, levels = unique(GenLongACFemales$General.Health))
GenLongACFemalesGraph <- GenLongACFemales%>%filter(General.Health!="Total")

#long table ac both sexes, including table without total for plot
GenLongACBothSexes<- GenSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Both Sexes")
GenLongACBothSexes$Percentage.Of.Population <- GenLongACBothSexes$value*100/GenLongACBothSexes$value[GenLongACBothSexes$General.Health == "Total"]
GenLongACBothSexes$General.Health <- factor(GenLongACBothSexes$General.Health, levels = unique(GenLongACBothSexes$General.Health))
GenLongACBothSexesGraph <- GenLongACBothSexes%>%filter(General.Health!="Total")

#bind long tables for plot
GenLongBinded <- bind_rows(GenLongEDBothSexesGraph,GenLongStateBothSexesGraph,GenLongACBothSexesGraph)
GenLongBinded$CSO.Electoral.Divisions.2022 <- factor(GenLongBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
#plot general health
GenPlot<- ggplot(GenLongBinded , aes(fill=CSO.Electoral.Divisions.2022, y=Percentage.Of.Population, x=General.Health)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.25, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_x_discrete(name = "General Health")+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
GenLongBinded$Percentage.Of.Population <- round(GenLongBinded$Percentage.Of.Population,1)
GenLongBinded$colouract <- 1
GenLongBinded$colouract[GenLongBinded$CSO.Electoral.Divisions.2022 == ED] <- '#405381'
GenLongBinded$colouract[GenLongBinded$CSO.Electoral.Divisions.2022 == AC] <- '#FCBE72'
GenLongBinded$colouract[GenLongBinded$CSO.Electoral.Divisions.2022 == "State"] <- '#13C1A5'

GenPlot2 <- highchart()|>       
  hc_add_series(GenLongBinded, "column", hcaes(x = General.Health, y = Percentage.Of.Population, color = colouract,  group = CSO.Electoral.Divisions.2022), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "General Health"))


#export plot for latex
pdf(paste0(getwd(),"/figures/GenED.pdf"))
print(GenPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/GenED.svg"))
# print(GenPlot)
# dev.off()


# Reformat percentages to be correct for table
GenLongEDMales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDMales$Percentage.Of.Population,1))
GenLongEDFemales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDFemales$Percentage.Of.Population,1))
GenLongEDBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDBothSexes$Percentage.Of.Population,1))
GenLongStateMales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateMales$Percentage.Of.Population,1))
GenLongStateFemales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateFemales$Percentage.Of.Population,1))
GenLongStateBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateBothSexes$Percentage.Of.Population,1))
GenLongACMales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongACMales$Percentage.Of.Population,1))
GenLongACFemales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongACFemales$Percentage.Of.Population,1))
GenLongACBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(GenLongACBothSexes$Percentage.Of.Population,1))
