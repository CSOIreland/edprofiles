# ED Table
SocialClassED <- SocialClassSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED)

# Add population variable to SocialClass table so percentage of population can be calculated
SocialClassED$TotalPop <- SocialClassED$value[SocialClassED$Social.Class == "Total"]

# Calculate percentage of population
SocialClassED$PercentageOfPopulation <- SocialClassED$value*100/SocialClassED$TotalPop

#State table
SocialClassState <- SocialClassSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State")

# Total pop added so percentage of pop can be calculated
SocialClassState$TotalPop <- SocialClassState$value[SocialClassState$Social.Class == "Total"]

# Calculate percentage of pop
SocialClassState$PercentageOfPopulation <- SocialClassState$value*100/SocialClassState$TotalPop

# AC table
SocialClassAC <- SocialClassSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC)

#Total pop added so perentage of population can be calculated

SocialClassAC$TotalPop <- SocialClassAC$value[SocialClassAC$Social.Class == "Total"]

# Calculate percentage of pop
SocialClassAC$PercentageOfPopulation <- SocialClassAC$value*100/SocialClassAC$TotalPop

# Bind all tables for plot
SocialClassBinded <- bind_rows(SocialClassED,SocialClassAC, SocialClassState)%>%filter(Social.Class!="Total")
SocialClassBinded$CSO.Electoral.Divisions.2022 <- factor(SocialClassBinded$CSO.Electoral.Divisions.2022, levels = c(ED,AC,"State"))
#wrap labels and factorise
SocialClassBinded$Social.Class2 <- stringr::str_wrap(SocialClassBinded$Social.Class, 25)
SocialClassBinded$Social.Class2 <- factor(SocialClassBinded$Social.Class2, levels = c("Professional workers", "Managerial and technical","Non-manual","Skilled manual", "Semi-skilled", "Unskilled","All others gainfully\noccupied and unknown"))


# Plot
SocialClassPlot <- ggplot(SocialClassBinded, aes(fill=CSO.Electoral.Divisions.2022, y=PercentageOfPopulation, x = Social.Class2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-0.1, 0, -0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Social Class")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")

#export plot for latex
pdf(paste0(getwd(),"/figures/SocialClassED.pdf"))
print(SocialClassPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/SocialClassED.svg"))
print(SocialClassPlot)
dev.off()


# Reformat percentages so they are correct for tables
SocialClassState$PercentageOfPopulation <- sprintf("%.1f", round(SocialClassState$PercentageOfPopulation,1))
SocialClassED$PercentageOfPopulation <- sprintf("%.1f", round(SocialClassED$PercentageOfPopulation,1))
SocialClassAC$PercentageOfPopulation <- sprintf("%.1f", round(SocialClassAC$PercentageOfPopulation,1))
