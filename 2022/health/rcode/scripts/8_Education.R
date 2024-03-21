
#ED table and calculate percentages
EduEDTable <- EduSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)
EduEDTable$PercentageOfPopulation <- EduEDTable$value*100/EduEDTable$value[EduEDTable$Highest.Level.of.Education.Completed == "Total"]

#Calculate individual education metrics (ED)
EduEDLessThanPrimary <- sum(EduEDTable$value[EduEDTable$Highest.Level.of.Education.Completed == "No formal education" |EduEDTable$Highest.Level.of.Education.Completed == "Primary education"] )
EduEDDegreeHigher <- sum(EduEDTable$value[EduEDTable$Highest.Level.of.Education.Completed == "Ordinary bachelor degree or national diploma" |EduEDTable$Highest.Level.of.Education.Completed == "Honours bachelor degree, professional qualification or both"|EduEDTable$Highest.Level.of.Education.Completed == "Postgraduate diploma or degree"|EduEDTable$Highest.Level.of.Education.Completed == "Doctorate(Ph.D) or higher"] )
EduEDLessThanPrimaryPerc <- sprintf("%.1f", round(EduEDLessThanPrimary*100/EduEDTable$value[EduEDTable$Highest.Level.of.Education.Completed == "Total"]))
EduEDDegreeHigherPerc <-  sprintf("%.1f", round(EduEDDegreeHigher*100/EduEDTable$value[EduEDTable$Highest.Level.of.Education.Completed == "Total"]))

#state table and calculate percentages
EduStateTable <- EduSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")
EduStateTable$PercentageOfPopulation <- EduStateTable$value*100/EduStateTable$value[EduStateTable$Highest.Level.of.Education.Completed == "Total"]

#Calculate individual education metrics (State)
EduStateLessThanPrimary <- sum(EduStateTable$value[EduStateTable$Highest.Level.of.Education.Completed == "No formal education" |EduStateTable$Highest.Level.of.Education.Completed == "Primary education"] )
EduStateDegreeHigher <- sum(EduStateTable$value[EduStateTable$Highest.Level.of.Education.Completed == "Ordinary bachelor degree or national diploma" |EduStateTable$Highest.Level.of.Education.Completed == "Honours bachelor degree, professional qualification or both"|EduStateTable$Highest.Level.of.Education.Completed == "Postgraduate diploma or degree"|EduStateTable$Highest.Level.of.Education.Completed == "Doctorate(Ph.D) or higher"] )
EduStateLessThanPrimaryPerc <- sprintf("%.1f", round(EduStateLessThanPrimary*100/EduStateTable$value[EduStateTable$Highest.Level.of.Education.Completed == "Total"],1))
EduStateDegreeHigherPerc <-  sprintf("%.1f", round(EduStateDegreeHigher*100/EduStateTable$value[EduStateTable$Highest.Level.of.Education.Completed == "Total"],1))

#AC table and percentages
EduACTable <- EduSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)
EduACTable$PercentageOfPopulation <- EduACTable$value*100/EduACTable$value[EduACTable$Highest.Level.of.Education.Completed == "Total"]

#Calculate individual education metrics (AC)
EduACLessThanPrimary <- sum(EduACTable$value[EduACTable$Highest.Level.of.Education.Completed == "No formal education" |EduACTable$Highest.Level.of.Education.Completed == "Primary education"] )
EduACDegreeHigher <- sum(EduACTable$value[EduACTable$Highest.Level.of.Education.Completed == "Ordinary bachelor degree or national diploma" |EduACTable$Highest.Level.of.Education.Completed == "Honours bachelor degree, professional qualification or both"|EduACTable$Highest.Level.of.Education.Completed == "Postgraduate diploma or degree"|EduACTable$Highest.Level.of.Education.Completed == "Doctorate(Ph.D) or higher"] )
EduACLessThanPrimaryPerc <- sprintf("%.1f", round(EduACLessThanPrimary*100/EduACTable$value[EduACTable$Highest.Level.of.Education.Completed == "Total"],1))
EduACDegreeHigherPerc <-  sprintf("%.1f", round(EduACDegreeHigher*100/EduACTable$value[EduACTable$Highest.Level.of.Education.Completed == "Total"],1))

#ED table without total (for plot). multiline labels and factorise
EduEDLessT <- EduEDTable%>%filter(Highest.Level.of.Education.Completed != "Total")
EduEDLessT$Highest.Level.of.Education.Completed2 <- stringr::str_wrap(EduEDLessT$Highest.Level.of.Education.Completed, 25)
EduEDLessT$Highest.Level.of.Education.Completed2 <- factor(EduEDLessT$Highest.Level.of.Education.Completed2, levels = c("No formal education", "Primary education","Lower secondary","Upper secondary", "Technical or vocational\nqualification", "Advanced\ncertificate/Completed\napprenticeship","Higher certificate","Ordinary bachelor degree\nor national diploma","Honours bachelor\ndegree, professional\nqualification or both", "Postgraduate diploma or\ndegree", "Doctorate(Ph.D) or higher" , "Not stated"   ))
#EduEDLessT <- EduEDLessT%>%dplyr::rename(PercentageOfPopulationED = "PercentageOfPopulation", valueED = "value")

#State table without total (for plot). multiline labels and factorise
EduStateLessT <- EduStateTable%>%filter(Highest.Level.of.Education.Completed != "Total")
EduStateLessT$Highest.Level.of.Education.Completed2 <- stringr::str_wrap(EduStateLessT$Highest.Level.of.Education.Completed, 25)
EduStateLessT$Highest.Level.of.Education.Completed2 <- factor(EduStateLessT$Highest.Level.of.Education.Completed2, levels = c("No formal education", "Primary education","Lower secondary","Upper secondary", "Technical or vocational\nqualification", "Advanced\ncertificate/Completed\napprenticeship","Higher certificate","Ordinary bachelor degree\nor national diploma","Honours bachelor\ndegree, professional\nqualification or both", "Postgraduate diploma or\ndegree", "Doctorate(Ph.D) or higher" , "Not stated"   ))
#EduStateLessT <- EduStateLessT%>%dplyr::rename(PercentageOfPopulationState = "PercentageOfPopulation", valueState = "value")

#AC table without total (for plot). multiline labels and factorise
EduACLessT <- EduACTable%>%filter(Highest.Level.of.Education.Completed != "Total")
EduACLessT$Highest.Level.of.Education.Completed2 <- stringr::str_wrap(EduACLessT$Highest.Level.of.Education.Completed, 25)
EduACLessT$Highest.Level.of.Education.Completed2 <- factor(EduACLessT$Highest.Level.of.Education.Completed2, levels = c("No formal education", "Primary education","Lower secondary","Upper secondary", "Technical or vocational\nqualification", "Advanced\ncertificate/Completed\napprenticeship","Higher certificate","Ordinary bachelor degree\nor national diploma","Honours bachelor\ndegree, professional\nqualification or both", "Postgraduate diploma or\ndegree", "Doctorate(Ph.D) or higher" , "Not stated"   ))

#bind all tables without total for plot
EduAllLessT <- bind_rows(EduACLessT,EduEDLessT,EduStateLessT)
EduAllLessT$CSO.Electoral.Divisions.2022 <- factor(EduAllLessT$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
#plot
EduPlot  <- ggplot(EduAllLessT , aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x=Highest.Level.of.Education.Completed2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.6, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Highest Level of Education")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population aged 15+")


# Highcharts plot for RMD
EduAllLessT$PercentageOfPopulation <- round(EduAllLessT$PercentageOfPopulation,1)
EduAllLessT$colouract <- 1
EduAllLessT$colouract[EduAllLessT$CSO.Electoral.Divisions.2022 == ED] <- '#405381'
EduAllLessT$colouract[EduAllLessT$CSO.Electoral.Divisions.2022 == AC] <- '#FCBE72'
EduAllLessT$colouract[EduAllLessT$CSO.Electoral.Divisions.2022 == "State"] <- '#13C1A5'

EduPlot2 <- highchart()|>       
  hc_add_series(EduAllLessT, "column", hcaes(x = Highest.Level.of.Education.Completed2, y = PercentageOfPopulation, color = colouract,  group = CSO.Electoral.Divisions.2022), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population aged 15+"))|>
  hc_xAxis(type = "category",
           title = list(text = "Highest Level of Education"))


# export plot for latex
pdf(paste0(getwd(),"/figures/EduED.pdf"))
print(EduPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/EduED.svg"))
# print(EduPlot)
# dev.off()


#reformat percentages to be correct for table
EduEDTable$PercentageOfPopulation <- sprintf("%.1f", round(EduEDTable$PercentageOfPopulation,1))
EduStateTable$PercentageOfPopulation <- sprintf("%.1f", round(EduStateTable$PercentageOfPopulation,1))
EduACTable$PercentageOfPopulation <- sprintf("%.1f", round(EduACTable$PercentageOfPopulation,1))
