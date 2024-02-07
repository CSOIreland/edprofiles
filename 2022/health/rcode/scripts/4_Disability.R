# REad pxstat tables
DisSourceTable2.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/F4004/PX/2013/")
DisSourceTable2 <- as.data.frame(DisSourceTable2.px)

DisSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/FY084/PX/2013/")
#removing ireland as this is already in the EDtable
DisSourceTableAC <- as.data.frame(DisSourceTableAC.px)%>%dplyr::rename(Electoral.Divisions = "Administrative.Counties")%>%filter(Electoral.Divisions != "Ireland")

#reformat electoral divisions for correct join
DisSourceTable2$Electoral.Divisions<- as.character(DisSourceTable2$Electoral.Divisions)

#rename ireland state
DisSourceTable2$Electoral.Divisions[DisSourceTable2$Electoral.Divisions == "Ireland"] <- "State"

#remove special characters in ac table
DisSourceTableAC$Electoral.Divisions <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", DisSourceTableAC$Electoral.Divisions)

#Bind EC and AC tables
DisSourceTable2 <- rbind(DisSourceTable2, DisSourceTableAC)

#remove special chars etc
DisSourceTable2$Electoral.Divisions <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",DisSourceTable2$Electoral.Divisions)
DisSourceTable2$Electoral.Divisions <- gsub("D├║n Laoghaire","Dun Laoghaire", DisSourceTable2$Electoral.Divisions)
DisSourceTable2$Electoral.Divisions <- gsub("'","", DisSourceTable2$Electoral.Divisions)
DisSourceTable2$Electoral.Divisions <- gsub("&","and", DisSourceTable2$Electoral.Divisions)

#Rename Categories
DisSourceTable2$Statistic <- gsub("Population with a disability to any extent", "Population with any disability",DisSourceTable2$Statistic)
DisSourceTable2$Statistic <- gsub("Population with a disability to any extent as a percentage", "Population with a disability as a percentage",DisSourceTable2$Statistic)
DisSourceTable2$Statistic <- gsub("Population with any disability as a percentage", "Population with a disability as a percentage",DisSourceTable2$Statistic)

# pivot wider for structure that is easier to work with
DisWider <- DisSourceTable2 %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value)

DisWider$Electoral.Divisions <- as.character(DisWider$Electoral.Divisions)

# ED subtables
EDDisWiderAllSexes <- DisWider%>%filter(Electoral.Divisions == ED & Sex == "Both sexes")
EDDisWiderMale <- DisWider%>%filter(Electoral.Divisions == ED & Sex == "Male")
EDDisWiderFemale <- DisWider%>%filter(Electoral.Divisions == ED & Sex == "Female")

#State subtables
StateDisWiderAllSexes <- DisWider%>%filter(Electoral.Divisions == "State" & Sex == "Both sexes")
StateDisWiderMale <- DisWider%>%filter(Electoral.Divisions == "State" & Sex == "Male")
StateDisWiderFemale <- DisWider%>%filter(Electoral.Divisions == "State" & Sex == "Female")

# AC subtabels
ACDisWiderAllSexes <- DisWider%>%filter(Electoral.Divisions == AC & Sex == "Both sexes")
ACDisWiderMale <- DisWider%>%filter(Electoral.Divisions == AC & Sex == "Male")
ACDisWiderFemale <- DisWider%>%filter(Electoral.Divisions == AC & Sex == "Female")

#Recategorise AC age groups because they are different
ACDisWiderAllSexesNewAges <- ACDisWiderAllSexes
ACDisWiderAllSexesNewAges$Age.Group <- ifelse(ACDisWiderAllSexesNewAges$Age.Group == "0 - 4 years"|ACDisWiderAllSexesNewAges$Age.Group == "5 - 9 years"|ACDisWiderAllSexesNewAges$Age.Group =="10 - 14 years", "0 - 14 years" ,
                                              ifelse(ACDisWiderAllSexesNewAges$Age.Group == "15 - 19 years"|ACDisWiderAllSexesNewAges$Age.Group == "20 - 24 years", "15 -24 years" ,
                                                     ifelse(ACDisWiderAllSexesNewAges$Age.Group == "25 - 29 years"|ACDisWiderAllSexesNewAges$Age.Group == "30 - 34 years"|ACDisWiderAllSexesNewAges$Age.Group == "35 - 39 years"|ACDisWiderAllSexesNewAges$Age.Group == "40 - 44 years", "25 - 44 years" ,
                                                            ifelse(ACDisWiderAllSexesNewAges$Age.Group == "45 - 49 years"|ACDisWiderAllSexesNewAges$Age.Group == "50 - 54 years"|ACDisWiderAllSexesNewAges$Age.Group == "55 - 59 years"|ACDisWiderAllSexesNewAges$Age.Group == "60 - 64 years", "45 - 64 years" ,
                                                                   ifelse(ACDisWiderAllSexesNewAges$Age.Group == "65 - 69 years"|ACDisWiderAllSexesNewAges$Age.Group == "70 - 74 years"|ACDisWiderAllSexesNewAges$Age.Group == "75 - 79 years"|ACDisWiderAllSexesNewAges$Age.Group == "80 - 84 years"|ACDisWiderAllSexesNewAges$Age.Group == "85 years and over", "65 years and over" ,
                                                                          ifelse(ACDisWiderAllSexesNewAges$Age.Group == "All ages", "All ages","Missing"))))))


#regroup by new categories and summarise
ACDisWiderAllSexesNewAges <- ACDisWiderAllSexesNewAges%>%group_by(Electoral.Divisions, Age.Group, Sex, CensusYear)%>%dplyr::summarise( `Population with any disability` = sum(`Population with any disability`, na.rm=T),  `Population with a disability to a great extent` = sum(`Population with a disability to a great extent`, na.rm=T), `Population with a disability to some extent` = sum(`Population with a disability to some extent`, na.rm=T), `Population with a disability as a percentage` = sum(`Population with a disability as a percentage`, na.rm=T), `Population with a disability to a great extent as a percentage` = sum(`Population with a disability to a great extent as a percentage`, na.rm=T), `Population with a disability to some extent as a percentage` = sum(`Population with a disability to some extent as a percentage`, na.rm=T))

#add total pop so percentages can ve calculated
ACDisWiderAllSexesNewAges$TotalPop <- PopACBothSexes$value

#calculate percentage
ACDisWiderAllSexesNewAges$`Population with a disability as a percentage` <- ACDisWiderAllSexesNewAges$`Population with any disability`*100/ACDisWiderAllSexesNewAges$TotalPop

# bind ED,  AC and State for plot
DisBinded <- bind_rows(EDDisWiderAllSexes,StateDisWiderAllSexes,ACDisWiderAllSexesNewAges)
DisBinded$Electoral.Divisions <- factor(DisBinded$Electoral.Divisions, levels = c(ED,AC,"State"))
#plot disability
DisPlot<- ggplot(DisBinded , aes(fill=Electoral.Divisions, y=`Population with a disability as a percentage` , x=Age.Group)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.15, 0.9))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Age Group")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")

#export plot for latex
pdf(paste0(getwd(),"/figures/DisED.pdf"))
print(DisPlot)
dev.off()

# Export plot for RMD
svg(paste0(getwd(),"/figures/DisED.svg"))
print(DisPlot)
dev.off()


# reformat percentages so they are correct for table

ACDisWiderAllSexesNewAges$`Population with a disability as a percentage` <- sprintf("%.1f", round(ACDisWiderAllSexesNewAges$`Population with a disability as a percentage`,1))


EDDisWiderAllSexes$`Population with a disability as a percentage` <- sprintf("%.1f", round(EDDisWiderAllSexes$`Population with a disability as a percentage`,1)) 
EDDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
EDDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

EDDisWiderMale$`Population with a disability as a percentage` <- sprintf("%.1f", round(EDDisWiderMale$`Population with a disability as a percentage`,1)) 
EDDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
EDDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


EDDisWiderFemale$`Population with a disability as a percentage` <- sprintf("%.1f", round(EDDisWiderFemale$`Population with a disability as a percentage`,1)) 
EDDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
EDDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 

ACDisWiderAllSexes$`Population with a disability as a percentage` <- sprintf("%.1f", round(ACDisWiderAllSexes$`Population with a disability as a percentage`,1)) 
ACDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
ACDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

ACDisWiderMale$`Population with a disability as a percentage` <- sprintf("%.1f", round(ACDisWiderMale$`Population with a disability as a percentage`,1)) 
ACDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
ACDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


ACDisWiderFemale$`Population with a disability as a percentage` <- sprintf("%.1f", round(ACDisWiderFemale$`Population with a disability as a percentage`,1)) 
ACDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
ACDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 

StateDisWiderAllSexes$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability as a percentage`,1)) 
StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

StateDisWiderMale$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability as a percentage`,1)) 
StateDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
StateDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


StateDisWiderFemale$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability as a percentage`,1)) 
StateDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
StateDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 
