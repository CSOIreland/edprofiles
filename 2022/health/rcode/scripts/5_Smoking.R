# Read pxstat tables
SmokingSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T4ED/PX/2013/")
SmokingSourceTable <- as.data.frame(SmokingSourceTable.px)

SmokingSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T4CTY/PX/2013/")
#removing ireland as this is already in the EDtable
SmokingSourceTableAC <- as.data.frame(SmokingSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022 !="Ireland")

#eD as character for correct join
SmokingSourceTable$CSO.Electoral.Divisions.2022 <- as.character(SmokingSourceTable$CSO.Electoral.Divisions.2022)

# rename Ireland
SmokingSourceTable$CSO.Electoral.Divisions.2022[SmokingSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#replace special chars in AC table
SmokingSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", SmokingSourceTableAC$CSO.Electoral.Divisions.2022)

#bind ED and Ac tables
SmokingSourceTable <- rbind(SmokingSourceTable, SmokingSourceTableAC)

#remove special chars etc
SmokingSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",SmokingSourceTable$CSO.Electoral.Divisions.2022)
SmokingSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", SmokingSourceTable$CSO.Electoral.Divisions.2022)
SmokingSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", SmokingSourceTable$CSO.Electoral.Divisions.2022)
SmokingSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", SmokingSourceTable$CSO.Electoral.Divisions.2022)

#pivot wider to make table easier to work with
SmokingWider <- SmokingSourceTable %>%
  pivot_wider(
    names_from = Persons.Who.Smoke.Tobacco.Products,
    values_from = value)

#calculate percentages
SmokingWider$PercentageWhoSmokeTobaccoProducts <- SmokingWider$`Persons who smoke tobacco products (Daily and Occasionally)`*100/SmokingWider$`All persons`
SmokingWider$PercentageWhoDontSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingWider$`Persons who donΓÇÖt smoke tobacco products (Never and have given up)`*100/SmokingWider$`All persons`,1))
SmokingWider$PercentageNotStated <- sprintf("%.1f", round(SmokingWider$`Smoking status not stated`*100/SmokingWider$`All persons`,1))
SmokingWider <- SmokingWider%>%dplyr::rename(`Persons who dont smoke tobacco products (Never and have given up)` = `Persons who donΓÇÖt smoke tobacco products (Never and have given up)`)

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
#
#export plot for latex
pdf(paste0(getwd(),"/figures/SmokingED.pdf"))
print(SmokingPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/SmokingED.svg"))
print(SmokingPlot)
dev.off()



#reformat percentages to be correct for table
SmokingED$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingED$PercentageWhoSmokeTobaccoProducts,1))
SmokingState$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingState$PercentageWhoSmokeTobaccoProducts,1))
SmokingAC$PercentageWhoSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingAC$PercentageWhoSmokeTobaccoProducts,1))
