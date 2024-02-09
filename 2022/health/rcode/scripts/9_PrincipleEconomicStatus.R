
#ED table and percentages
PESED <- PESSource%>%filter(CSO.Electoral.Divisions.2022 == ED)
PESED$PercentageOfPopulation <- PESED$value*100/PESED$value[PESED$Principle.Economic.Status == "Total"]

#individual metrics for ED
EDPESTot <- PESED$value[PESED$Sex == "Both Sexes" & PESED$Principle.Economic.Status == "Total"]
EDUnemployed <- sum(PESED$value[PESED$Sex == "Both Sexes" & (PESED$Principle.Economic.Status == "Short term unemployed" |PESED$Principle.Economic.Status == "Long term unemployed")])
EDUnemployedPerc <- sprintf("%.1f", round(EDUnemployed*100/EDPESTot,1))
EDStudent <- PESED$value[PESED$Sex == "Both Sexes" & PESED$Principle.Economic.Status == "Student"]
EDStudentPerc <- sprintf("%.1f", round(EDStudent*100/EDPESTot,1))
EDUnableToWork <- PESED$value[PESED$Sex == "Both Sexes" & PESED$Principle.Economic.Status == "Unable to work due to permanent sickness or disability"]
EDUnableToWorkPerc <- sprintf("%.1f", round(EDUnableToWork*100/EDPESTot,1))

#State table and pcerentages
PESState <- PESSource%>%filter(CSO.Electoral.Divisions.2022 == "State")
PESState$PercentageOfPopulation <- PESState$value*100/PESState$value[PESState$Principle.Economic.Status == "Total"]

#state individual metrics
StatePESTot <- PESState$value[PESState$Sex == "Both Sexes" & PESState$Principle.Economic.Status == "Total"]
StateUnemployed <- sum(PESState$value[PESState$Sex == "Both Sexes" & (PESState$Principle.Economic.Status == "Short term unemployed" |PESState$Principle.Economic.Status == "Long term unemployed")])
StateUnemployedPerc <- sprintf("%.1f", round(StateUnemployed*100/StatePESTot,1))
StateStudent <- PESState$value[PESState$Sex == "Both Sexes" & PESState$Principle.Economic.Status == "Student"]
StateStudentPerc <- sprintf("%.1f", round(StateStudent*100/StatePESTot,1))
StateUnableToWork <- PESState$value[PESState$Sex == "Both Sexes" & PESState$Principle.Economic.Status == "Unable to work due to permanent sickness or disability"]
StateUnableToWorkPerc <- sprintf("%.1f", round(StateUnableToWork*100/StatePESTot,1))

#ac table and percentages
PESAC <- PESSource%>%filter(CSO.Electoral.Divisions.2022 == AC)
PESAC$PercentageOfPopulation <- PESAC$value*100/PESAC$value[PESAC$Principle.Economic.Status == "Total"]

#AC individual metrics
ACPESTot <- PESAC$value[PESAC$Sex == "Both Sexes" & PESAC$Principle.Economic.Status == "Total"]
ACUnemployed <- sum(PESAC$value[PESAC$Sex == "Both Sexes" & (PESAC$Principle.Economic.Status == "Short term unemployed" |PESAC$Principle.Economic.Status == "Long term unemployed")])
ACUnemployedPerc <- sprintf("%.1f", round(ACUnemployed*100/ACPESTot,1))
ACStudent <- PESAC$value[PESAC$Sex == "Both Sexes" & PESAC$Principle.Economic.Status == "Student"]
ACStudentPerc <- sprintf("%.1f", round(ACStudent*100/ACPESTot,1))
ACUnableToWork <- PESAC$value[PESAC$Sex == "Both Sexes" & PESAC$Principle.Economic.Status == "Unable to work due to permanent sickness or disability"]
ACUnableToWorkPerc <- sprintf("%.1f", round(ACUnableToWork*100/ACPESTot,1))


# Ed table without total for plot
PESEDLessT <- PESED%>%filter(Principle.Economic.Status!= "Total")

# wrap labels and factorise
PESEDLessT$Principle.Economic.Status2 <- stringr::str_wrap(PESEDLessT$Principle.Economic.Status, 25)
PESEDLessT$Principle.Economic.Status2 <- factor(PESEDLessT$Principle.Economic.Status2, levels = c("At work", "Looking for first regular\njob","Short term unemployed","Long term unemployed", "Student", "Looking after home/family","Retired","Unable to work due to\npermanent sickness or\ndisability","Other"))

#State table less total for plot
PESStateLessT <- PESState%>%filter(Principle.Economic.Status!= "Total")

#wrap labels and factorise
PESStateLessT$Principle.Economic.Status2 <- stringr::str_wrap(PESStateLessT$Principle.Economic.Status, 25)
PESStateLessT$Principle.Economic.Status2 <- factor(PESStateLessT$Principle.Economic.Status2, levels = c("At work", "Looking for first regular\njob","Short term unemployed","Long term unemployed", "Student", "Looking after home/family","Retired","Unable to work due to\npermanent sickness or\ndisability","Other"))

#AC table less total for plot
PESACLessT <- PESAC%>%filter(Principle.Economic.Status!= "Total")


#wrap labels and factorise
PESACLessT$Principle.Economic.Status2 <- stringr::str_wrap(PESACLessT$Principle.Economic.Status, 25)
PESACLessT$Principle.Economic.Status2 <- factor(PESACLessT$Principle.Economic.Status2, levels = c("At work", "Looking for first regular\njob","Short term unemployed","Long term unemployed", "Student", "Looking after home/family","Retired","Unable to work due to\npermanent sickness or\ndisability","Other"))


# bind all tables ithout total for plot
PESLessTAll <- bind_rows(PESEDLessT,PESACLessT,PESStateLessT)
PESLessTAll$CSO.Electoral.Divisions.2022 <- factor(PESLessTAll$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))

#plot
PESPlot  <- ggplot(PESLessTAll , aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x=Principle.Economic.Status2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Principal Economic Status")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population aged 15+")

#export plot for latex
pdf(paste0(getwd(),"/figures/PESED.pdf"))
print(PESPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/PESED.svg"))
print(PESPlot)
dev.off()


#reformat percentages to be correct for tables
PESED$PercentageOfPopulation <- sprintf("%.1f", round(PESED$PercentageOfPopulation,1))
PESState$PercentageOfPopulation <- sprintf("%.1f", round(PESState$PercentageOfPopulation,1))
PESAC$PercentageOfPopulation <- sprintf("%.1f", round(PESAC$PercentageOfPopulation,1))
