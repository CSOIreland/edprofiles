
# Create Pop Table for ED
PopEDMales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Males")
PopEDFemales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Females")
PopEDBothSexes<- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Both Sexes")

# Males table ED including percentage and total
PopEDMalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Males")
PopEDMalesFull$Percentage <-sprintf("%.1f", round(PopEDMalesFull$value*100/PopEDMalesFull$value[PopEDMalesFull$Age == "Total"],1))

#Males table ED without total (For plots)
PopEDMalesFullLessT <- PopEDMalesFull%>%filter(Age!="Total")
PopEDMalesFullLessT$Gender <- "Male"
PopEDMalesFullLessT$Age <- factor(PopEDMalesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))

#Females table ED including percentage and total
PopEDFemalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Females")
PopEDFemalesFull$Percentage <- sprintf("%.1f", round(PopEDFemalesFull$value*100/PopEDFemalesFull$value[PopEDFemalesFull$Age == "Total"],1))

#Females table ED without total (for plots)
PopEDFemalesFullLessT <- PopEDFemalesFull%>%filter(Age!="Total")
PopEDFemalesFullLessT$Gender = "Female"
PopEDFemalesFullLessT$Age <- factor(PopEDFemalesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))

#Both sexes ED with Percentage and Total
PopEDBothSexesFull<- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == ED & Sex == "Both Sexes")
PopEDBothSexesFull$Percentage <- sprintf("%.1f", round(PopEDBothSexesFull$value*100/PopEDBothSexesFull$value[PopEDBothSexesFull$Age == "Total"],1))

#Both sexess ED without total (for plots)
PopEDBothSexesFullLessT <- PopEDBothSexesFull%>%filter(Age!="Total")
PopEDBothSexesFullLessT$Age <- factor(PopEDBothSexesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))

# Total population of males, females and both sexes in ED
TotalPopEDFemales <- PopEDFemalesFull$value[PopEDFemalesFull$Age == "Total"]
TotalPopEDMales <- PopEDMalesFull$value[PopEDMalesFull$Age == "Total"]
TotalPopEDBothSexes <- PopEDBothSexesFull$value[PopEDBothSexesFull$Age == "Total"]

# Pop tables for AC
PopACMales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Males")
PopACFemales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Females")
PopACBothSexes<- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Both Sexes")

# Males AC with percentage
PopACMalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Males")
PopACMalesFull$Percentage <-PopACMalesFull$value*100/PopACMalesFull$value[PopACMalesFull$Age == "Total"]

#Males Ac without total (for plots)
PopACMalesFullLessT <- PopACMalesFull%>%filter(Age!="Total")
PopACMalesFullLessT$Gender <- "Male"
PopACMalesFullLessT$Age <- factor(PopACMalesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))

#Females AC full table with percentage
PopACFemalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Females")
PopACFemalesFull$Percentage <- PopACFemalesFull$value*100/PopACFemalesFull$value[PopACFemalesFull$Age == "Total"]

#Females AC without total
PopACFemalesFullLessT <- PopACFemalesFull%>%filter(Age!="Total")
PopACFemalesFullLessT$Gender = "Female"
PopACFemalesFullLessT$Age <- factor(PopACFemalesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))


#Both sexes AC with percentage
PopACBothSexesFull<- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == AC & Sex == "Both Sexes")
PopACBothSexesFull$Percentage <- sprintf("%.1f", round(PopACBothSexesFull$value*100/PopACBothSexesFull$value[PopACBothSexesFull$Age == "Total"],1))

#both sexes AC wthout total (for plots)
PopACBothSexesFullLessT <- PopACBothSexesFull%>%filter(Age!="Total")
PopACBothSexesFullLessT$Age <- factor(PopACBothSexesFullLessT$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over"))


# Total population of males, females and both sexes in AC
TotalPopACFemales <- PopACFemalesFull$value[PopACFemalesFull$Age == "Total"]
TotalPopACMales <- PopACMalesFull$value[PopACMalesFull$Age == "Total"]
TotalPopACBothSexes <- PopACBothSexesFull$value[PopACBothSexesFull$Age == "Total"]

# Total Population of State
PopStateMales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == "State" & Sex == "Males")
PopStateFemales <- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == "State"  & Sex == "Females")
PopStateBothSexes<- PopSourceTableRegrouped%>%filter(CSO.Electoral.Divisions.2022 == "State"  & Sex == "Both Sexes")

# Population males state with percentage and total
PopStateMalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State"  & Sex == "Males")
PopStateMalesFull$Percentage <- sprintf("%.1f", round(PopStateMalesFull$value*100/PopStateMalesFull$value[PopStateMalesFull$Age == "Total"],1))

#Population males state without total (for plots)
PopStateMalesFullLessT <- PopStateMalesFull%>%filter(Age!="Total")
PopStateMalesFullLessT$Gender <- "Male"

# Population Females state with percentage and total
PopStateFemalesFull <- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State"  & Sex == "Females")
PopStateFemalesFull$Percentage <- PopStateFemalesFull$value*100/PopStateFemalesFull$value[PopStateFemalesFull$Age == "Total"]

# without total(for plots)
PopStateFemalesFullLessT <- PopStateFemalesFull%>%filter(Age!="Total")
PopStateFemalesFullLessT$Gender = "Female"

# both sexes state with percentage and total
PopStateBothSexesFull<- PopSourceTable%>%filter(CSO.Electoral.Divisions.2022 == "State"  & Sex == "Both Sexes")
PopStateBothSexesFull$Percentage <- sprintf("%.1f", round(PopStateBothSexesFull$value*100/PopStateBothSexesFull$value[PopStateBothSexesFull$Age == "Total"],1))

# without total (for plots)
PopStateBothSexesFullLessT <- PopStateBothSexesFull%>%filter(Age!="Total")


#Total population of males, females and both sexes in the state
TotalPopStateFemales <- PopStateFemalesFull$value[PopStateFemalesFull$Age == "Total"]
TotalPopStateMales <- PopStateMalesFull$value[PopStateMalesFull$Age == "Total"]
TotalPopStateBothSexes <- PopStateBothSexesFull$value[PopStateBothSexesFull$Age == "Total"]



# Change percentage to numeric for pyramid plot
PopEDMalesFullLessT$Percentage <- as.numeric(PopEDMalesFullLessT$Percentage )
PopEDFemalesFullLessT$Percentage <- as.numeric(PopEDFemalesFullLessT$Percentage )

# Change males to negative to be displayed correctly in plot (labels in plot are then changed to positive later)
PopEDMalesFullLessT$value <- PopEDMalesFullLessT$value*-1

PopEDBinded <- rbind(PopEDFemalesFullLessT,PopEDMalesFullLessT)

PyramidLevels <- levels(PopEDMalesFullLessT$Age)

#Create pyramid plot
PyramidPlot <- ggplot() +
  geom_bar(data = PopEDMalesFullLessT, aes(x = 1:length(PyramidLevels), y = value, fill = "red"), width= 0.4,
           position = position_nudge(0.05), stat = "identity") +
  geom_bar(data = PopEDFemalesFullLessT, aes(x =  1:length(PyramidLevels), y = value, fill = "blue"), width= 0.4,
           position = position_nudge(0.05), stat = "identity") +
  scale_x_continuous(name = "Age Group", breaks = 1:length(PyramidLevels),labels = PyramidLevels,
                     sec.axis = sec_axis(~.,
                                         breaks = 1:length(PyramidLevels),
                                         labels = NULL))+
  labs(fill = "Legend") +
  theme_classic()+
  theme(axis.text.x = element_text(margin = margin(t = 0.25, unit = "in")),
        axis.title.x = element_text(margin = margin(0.5, 0.5, 0.5, 0.5)),
        legend.position = "none"
  )+
  coord_flip()+
  scale_fill_discrete(labels=c('Males', 'Females'))+
  scale_y_continuous(name = "Persons",labels = abs, limits = c(-1*max(c(PopEDMalesFullLessT$value*-1,PopEDFemalesFullLessT$value)),max(c(PopEDMalesFullLessT$value*-1,PopEDFemalesFullLessT$value)))) +
  annotate("text", x=length(PyramidLevels),y=max(PopEDFemalesFullLessT$value) - max(PopEDFemalesFullLessT$value)*0.2, hjust=.2,label= "Females", color = "red", size = 5) +
  annotate("text",  x=length(PyramidLevels),y=min(PopEDMalesFullLessT$value) + -1*min(PopEDMalesFullLessT$value)*0.2,hjust=.2, label= "Males", color = "blue", size = 5)

# Highchart for RMD
PyramidPlot2 <- highchart() %>%
  hc_add_series(PopEDBinded, type = "bar", hcaes(x = c(PyramidLevels,PyramidLevels), y = value, group = Gender), color = c("#2f7ed8","#FF0000"), 
                showInLegend = F) %>% 
  hc_plotOptions(bar = list(stacking = "normal")) %>% 
  # format the labels on the x-axis (y-axis per HC)
  hc_yAxis( min = -1*max(c(PopEDMalesFullLessT$value*-1,PopEDFemalesFullLessT$value))-1, max = max(c(PopEDMalesFullLessT$value*-1,PopEDFemalesFullLessT$value))+1,labels = list(formatter = htmlwidgets::JS( 
    "function() {
        return Math.abs(this.value); /* all labels to absolute values */
    }"
  ))) %>% 
  hc_xAxis(categories = PyramidLevels, reversed = F)%>%hc_title(text = "Males", align = "center", x = -115, y = 20, margin = 0,
           style = list(fontSize = "12px", color = "#FF0000")) %>%
  hc_subtitle(text = "Females", align = "center", y = 20, margin = 0,
              x = 250, style = list(fontSize = "12px", color = "#2f7ed8"))%>%
  hc_tooltip(formatter = htmlwidgets::JS("function() { return Math.abs(this.point.y); }"))

# Export pyramid plot for Latex
pdf(paste0(getwd(),"/figures/PyramidPlot.pdf"))
print(PyramidPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/PyramidPlot.svg"))
# print(PyramidPlot)
# dev.off()


#reformat percentages so that they are rounded and  formatted correctly for tables
PopEDMalesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopEDMalesFullLessT$Percentage)*-1,1))
PopEDFemalesFullLessT$Percentage <- sprintf("%.1f", round(PopEDFemalesFullLessT$Percentage,1))
PopEDBothSexesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopEDBothSexesFullLessT$Percentage),1))

PopStateMalesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopStateMalesFullLessT$Percentage)*-1,1))
PopStateFemalesFullLessT$Percentage <- sprintf("%.1f", round(PopStateFemalesFullLessT$Percentage,1))
PopStateBothSexesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopStateBothSexesFullLessT$Percentage),1))

PopACMalesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopACMalesFullLessT$Percentage)*-1,1))
PopACFemalesFullLessT$Percentage <- sprintf("%.1f", round(PopACFemalesFullLessT$Percentage,1))
PopACBothSexesFullLessT$Percentage <- sprintf("%.1f", round(as.numeric(PopACBothSexesFullLessT$Percentage),1))
