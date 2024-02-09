# ################################Population####################################


#read the pxstat file for EDs
PopSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1AED/PX/2013/")
PopSourceTable <- as.data.frame(PopSourceTable.px)


#Read pxStat table for ACs (Removing Ireland as this is in the ED table and change geotitle to ED to be able to bind correctly)
PopSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T1T1ACTY/PX/2013/")
PopSourceTableAC <- as.data.frame(PopSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022 !="Ireland")

#Bind the two tables into one
PopSourceTable <- rbind(PopSourceTable, PopSourceTableAC)

#Change formatting of ED to join correctly
PopSourceTable$CSO.Electoral.Divisions.2022 <- as.character(PopSourceTable$CSO.Electoral.Divisions.2022)

#remove special chars etc
PopSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",PopSourceTable$CSO.Electoral.Divisions.2022)
PopSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", PopSourceTable$CSO.Electoral.Divisions.2022)
PopSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", PopSourceTable$CSO.Electoral.Divisions.2022)
PopSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", PopSourceTable$CSO.Electoral.Divisions.2022)

#Rename Ireland State
PopSourceTable$CSO.Electoral.Divisions.2022[PopSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

# Fix Special characters in Dun Laoghaire AC
PopSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire Rathdown County Council|D├║n Laoghaire Rathdown County Council","Dun Laoghaire Rathdown County Council", PopSourceTable$CSO.Electoral.Divisions.2022)



#factorise age
PopSourceTable$Age <- factor(PopSourceTable$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over","Total"))


#Create a duplicate table where the age groups will be recategorised for tables
PopSourceTable2 <- PopSourceTable

# recategorise
PopSourceTable2$Age <- gsub("Age 0-4|Age 5-9|Age 10-14", "0-14", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 15-19|Age 20-24", "15-24", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 25-29|Age 30-34|Age 35-39|Age 40-44", "25-44", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 45-49|Age 50-54|Age 55-59|Age 60-64", "45-64", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 65-69|Age 70-74|Age 75-79|Age 80-84|Age 85 and over", "65 years and over", PopSourceTable2$Age )

#remove word age from category
#PopSourceTable2$Age <- as.character(gsub("Age ","", PopSourceTable2$Age))

#group by new categories for summary
PopSourceTableRegrouped <- PopSourceTable2%>%group_by(Sex,Age,CSO.Electoral.Divisions.2022)%>%dplyr::summarise(value = sum(value, na.rm=T))
PopSourceTableRegrouped$Age <- as.character(gsub("Age ","", PopSourceTableRegrouped$Age))

#factorise age
PopSourceTableRegrouped$Age <- factor(PopSourceTableRegrouped$Age, levels = c("0-14","15-24","25-44", "45-64","65 years and over","Total"))


#############################################CARERS################################
#REad ED and AC  table from PXSTat
CarersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T2ED/PX/2013/")
CarersSourceTable <- as.data.frame(CarersSourceTable.px)


CarersSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T2CTY/PX/2013/")

#removing ireland as this is already in the EDtable
CarersSourceTableAC <- as.data.frame(CarersSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")


#EDs as cahracter for correct join
CarersSourceTable$CSO.Electoral.Divisions.2022<- as.character(CarersSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
CarersSourceTable$CSO.Electoral.Divisions.2022[CarersSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
CarersSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", CarersSourceTableAC$CSO.Electoral.Divisions.2022)

CarersSourceTable <- rbind(CarersSourceTable,CarersSourceTableAC)

#remove special chars etc
CarersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",CarersSourceTable$CSO.Electoral.Divisions.2022)
CarersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", CarersSourceTable$CSO.Electoral.Divisions.2022)
CarersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", CarersSourceTable$CSO.Electoral.Divisions.2022)
CarersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", CarersSourceTable$CSO.Electoral.Divisions.2022)

############################GENERAL HEALTH###########################################

#import ED and AC tables
GenSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T3ED/PX/2013/")
GenSourceTable <- as.data.frame(GenSourceTable.px)

GenSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T12T3CTY/PX/2013/")
GenSourceTableAC <- as.data.frame(GenSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022 != "Ireland")

#as character for correct join
GenSourceTable$CSO.Electoral.Divisions.2022 <- as.character(GenSourceTable$CSO.Electoral.Divisions.2022)
#removing ireland as this is already in the EDtable
GenSourceTable$CSO.Electoral.Divisions.2022[GenSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#remove special characters in AC
GenSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire Rathdown County Council|D├║n Laoghaire Rathdown County Council","Dun Laoghaire Rathdown County Council", GenSourceTableAC$CSO.Electoral.Divisions.2022 )

# bind two tables
GenSourceTable <- rbind(GenSourceTable,GenSourceTableAC)
#remove special chars etc
GenSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",GenSourceTable$CSO.Electoral.Divisions.2022)
GenSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", GenSourceTable$CSO.Electoral.Divisions.2022)
GenSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", GenSourceTable$CSO.Electoral.Divisions.2022)
GenSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", GenSourceTable$CSO.Electoral.Divisions.2022)
# pivot table to wider for easier prep
GenWider <- GenSourceTable %>%
  pivot_wider(
    names_from = General.Health,
    values_from = value)

# Calculate percentages
GenWider$`Very Good as a Percentage` <- GenWider$`Very Good`*100/GenWider$Total
GenWider$`Good as a Percentage` <- GenWider$Good*100/GenWider$Total
GenWider$`Fair as a Percentage` <- GenWider$Fair*100/GenWider$Total
GenWider$`Bad as a Percentage` <- GenWider$Bad*100/GenWider$Total
GenWider$`Very Bad as a Percentage` <- GenWider$`Very Bad`*100/GenWider$Total
GenWider$`Bad or Very Bad` <- GenWider$Bad + GenWider$`Very Bad`
GenWider$`Bad or Very Bad as a Percentage` <- GenWider$`Bad or Very Bad` *100/GenWider$Total  

#################################DISABILITY#####################################
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

##############################################SMOKING############################################
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

#################################EDUCATION#####################################
# REad pxstat tables
EduSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T10T4ED/PX/2013/")
EduSourceTable <- as.data.frame(EduSourceTable.px)

EduSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T10T4CTY/PX/2013/")
#removing ireland as this is already in the EDtable
EduSourceTableAC <- as.data.frame(EduSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022 !="Ireland")

#ED as character for corrcet join
EduSourceTable$CSO.Electoral.Divisions.2022 <- as.character(EduSourceTable$CSO.Electoral.Divisions.2022)

# rename Ireland State
EduSourceTable$CSO.Electoral.Divisions.2022[EduSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#remove special chars in AC table
EduSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", EduSourceTableAC$CSO.Electoral.Divisions.2022)

#bind both tables
EduSourceTable <- rbind(EduSourceTable,EduSourceTableAC)


#remove special chars etc
EduSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",EduSourceTable$CSO.Electoral.Divisions.2022)
EduSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", EduSourceTable$CSO.Electoral.Divisions.2022)
EduSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", EduSourceTable$CSO.Electoral.Divisions.2022)
EduSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", EduSourceTable$CSO.Electoral.Divisions.2022)

# filter only both sexes as that is all we are interested in here
EduSourceTable <- EduSourceTable%>%filter(Sex == "Both Sexes")

############################PRINCIPLE ECONOMIC STATUS################################
# REad Pxstat tables
PESSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T8T1ED/PX/2013/")
PESSource <- as.data.frame(PESSource.px)

PESSourceAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T8T1CTY/PX/2013/")
PESSourceAC <- as.data.frame(PESSourceAC.px)

# Electoral Division as character for correct join
PESSource$CSO.Electoral.Divisions.2022 <- as.character(PESSource$CSO.Electoral.Divisions.2022)

# Rename Ireland State
PESSource$CSO.Electoral.Divisions.2022[PESSource$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

# Rename AC geogrpahy so can be rbinded. also filter out Ireland as it is in the ED table
PESSourceAC <- PESSourceAC%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022 !="Ireland")

#remove special chars from ac
PESSourceAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", PESSourceAC$CSO.Electoral.Divisions.2022)

#bind both tables and filter for both sexes as that is what we are interested in
PESSource <- rbind(PESSource, PESSourceAC)

#remove special chars etc
PESSource$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",PESSource$CSO.Electoral.Divisions.2022)
PESSource$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", PESSource$CSO.Electoral.Divisions.2022)
PESSource$CSO.Electoral.Divisions.2022 <- gsub("'","", PESSource$CSO.Electoral.Divisions.2022)
PESSource$CSO.Electoral.Divisions.2022 <- gsub("&","and", PESSource$CSO.Electoral.Divisions.2022)

PESSource <- PESSource%>%filter(Sex == "Both Sexes")

##############################FAMILIES#####################################
# read pxstat tables
FamSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T4T3ED/PX/2013/")
FamSource <- as.data.frame(FamSource.px)%>%dplyr::rename(CSO.Electoral.Divisions.2020 = "CSO.Electoral.Divisions.2022")

FamSourceAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T4T3CTY/PX/2013/")
#removing ireland as this is already in the EDtable
FamSourceAC <- as.data.frame(FamSourceAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2020  = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2020!="Ireland")

# EDs as character for correct join
FamSource$CSO.Electoral.Divisions.2020 <- as.character(FamSource$CSO.Electoral.Divisions.2020)

#rename Ireland state
FamSource$CSO.Electoral.Divisions.2020[FamSource$CSO.Electoral.Divisions.2020 == "Ireland"] <- "State"

#Remove special chars in AC table
FamSourceAC$CSO.Electoral.Divisions.2020 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire ", FamSourceAC$CSO.Electoral.Divisions.2020)

#bind both tables
FamSource <- rbind(FamSource,FamSourceAC)


#remove special chars etc
FamSource$CSO.Electoral.Divisions.2020 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",FamSource$CSO.Electoral.Divisions.2020)
FamSource$CSO.Electoral.Divisions.2020 <- gsub("D├║n Laoghaire","Dun Laoghaire", FamSource$CSO.Electoral.Divisions.2020)
FamSource$CSO.Electoral.Divisions.2020 <- gsub("'","", FamSource$CSO.Electoral.Divisions.2020)
FamSource$CSO.Electoral.Divisions.2020 <- gsub("&","and", FamSource$CSO.Electoral.Divisions.2020)

################################BIRTHPLACE########################################
#read pxstat tabls for ac and ED

BirthSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T2T1ED/PX/2013/")
BirthSource <- as.data.frame(BirthSource.px)%>%filter(Statistic == "Usually resident population by birthplace")

BirthSourceAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T2T1CTY/PX/2013/")
#removing ireland as this is already in the EDtable
BirthSourceAC<- as.data.frame(BirthSourceAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")%>%filter(Statistic == "Usually resident population by birthplace")

# ED as character for correct join
BirthSource$CSO.Electoral.Divisions.2022<-as.character(BirthSource$CSO.Electoral.Divisions.2022)

# rename Ireland to state
BirthSource$CSO.Electoral.Divisions.2022[BirthSource$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

# replace special chars in ac
BirthSourceAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", BirthSourceAC$CSO.Electoral.Divisions.2022)

#bind both tables
BirthSource <- rbind(BirthSource,BirthSourceAC)


#remove special chars etc
BirthSource$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",BirthSource$CSO.Electoral.Divisions.2022)
BirthSource$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", BirthSource$CSO.Electoral.Divisions.2022)
BirthSource$CSO.Electoral.Divisions.2022 <- gsub("'","", BirthSource$CSO.Electoral.Divisions.2022)
BirthSource$CSO.Electoral.Divisions.2022 <- gsub("&","and", BirthSource$CSO.Electoral.Divisions.2022)

###############################VOLUNTEERING####################################
#REad ED and AC  table from PXSTat
VolunteersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T7T1ED/PX/2013/")
VolunteersSourceTable <- as.data.frame(VolunteersSourceTable.px)

VolunteersSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T7T1CTY/PX/2013/")
#removing ireland as this is already in the EDtable
VolunteersSourceTableAC <- as.data.frame(VolunteersSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")


#EDs as cahracter for correct join
VolunteersSourceTable$CSO.Electoral.Divisions.2022<- as.character(VolunteersSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
VolunteersSourceTable$CSO.Electoral.Divisions.2022[VolunteersSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
VolunteersSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire |D├║n Laoghaire","Dun Laoghaire", VolunteersSourceTableAC$CSO.Electoral.Divisions.2022)

VolunteersSourceTable <- rbind(VolunteersSourceTable,VolunteersSourceTableAC)


#remove special chars etc
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", VolunteersSourceTable$CSO.Electoral.Divisions.2022)
VolunteersSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", VolunteersSourceTable$CSO.Electoral.Divisions.2022)

#########################################SOCIAL CLASS##########################################
#REad ED and AC  table from PXSTat
SocialClassSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T9T1ED/PX/2013/")
SocialClassSourceTable <- as.data.frame(SocialClassSourceTable.px)%>%filter(Sex == "Both Sexes")

SocialClassSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T9T1CTY/PX/2013/")
#removing ireland as this is already in the EDtable
SocialClassSourceTableAC <- as.data.frame(SocialClassSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022!="Ireland" & Sex == "Both Sexes")

#EDs as cahracter for correct join
SocialClassSourceTable$CSO.Electoral.Divisions.2022<- as.character(SocialClassSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
SocialClassSourceTable$CSO.Electoral.Divisions.2022[SocialClassSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
SocialClassSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", SocialClassSourceTableAC$CSO.Electoral.Divisions.2022)

SocialClassSourceTable <- rbind(SocialClassSourceTable,SocialClassSourceTableAC)


#remove special chars etc
SocialClassSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",SocialClassSourceTable$CSO.Electoral.Divisions.2022)
SocialClassSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", SocialClassSourceTable$CSO.Electoral.Divisions.2022)
SocialClassSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", SocialClassSourceTable$CSO.Electoral.Divisions.2022)
SocialClassSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", SocialClassSourceTable$CSO.Electoral.Divisions.2022)

##################################################HOUSEHOLDS##################################################
#REad ED and AC  table from PXSTat
HouseholdsSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3ED/PX/2013/")
HouseholdsSourceTable <- as.data.frame(HouseholdsSourceTable.px)%>%filter(Statistic == "Permanent private households")

HouseholdsSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3CTY/PX/2013/")
HouseholdsSourceTableAC<- as.data.frame(HouseholdsSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland" & Statistic == "Permanent private households")

#EDs as cahracter for correct join
HouseholdsSourceTable$CSO.Electoral.Divisions.2022<- as.character(HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
HouseholdsSourceTable$CSO.Electoral.Divisions.2022[HouseholdsSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022)

HouseholdsSourceTable <- rbind(HouseholdsSourceTable,HouseholdsSourceTableAC)


HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
#REad ED and AC  table from PXSTat
HouseholdsSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3ED/PX/2013/")
HouseholdsSourceTable <- as.data.frame(HouseholdsSourceTable.px)%>%filter(Statistic == "Permanent private households")

HouseholdsSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T3CTY/PX/2013/")
HouseholdsSourceTableAC<- as.data.frame(HouseholdsSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland" & Statistic == "Permanent private households")

#EDs as cahracter for correct join
HouseholdsSourceTable$CSO.Electoral.Divisions.2022<- as.character(HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
HouseholdsSourceTable$CSO.Electoral.Divisions.2022[HouseholdsSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTableAC$CSO.Electoral.Divisions.2022)

HouseholdsSourceTable <- rbind(HouseholdsSourceTable,HouseholdsSourceTableAC)


HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)
HouseholdsSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", HouseholdsSourceTable$CSO.Electoral.Divisions.2022)

#################################RENEWABLE ENERGY################################
#REad ED and AC  table from PXSTat
RenewableEnergySourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T10ED/PX/2013/")
RenewableEnergySourceTable <- as.data.frame(RenewableEnergySourceTable.px)

RenewableEnergySourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T6T10CTY/PX/2013/")
#removing ireland as this is already in the EDtable
RenewableEnergySourceTableAC <- as.data.frame(RenewableEnergySourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties")%>%filter(CSO.Electoral.Divisions.2022!="Ireland")


#EDs as cahracter for correct join
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022<- as.character(RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022[RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
RenewableEnergySourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire", RenewableEnergySourceTableAC$CSO.Electoral.Divisions.2022)

RenewableEnergySourceTable <- rbind(RenewableEnergySourceTable,RenewableEnergySourceTableAC)


#remove special chars etc
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)
RenewableEnergySourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", RenewableEnergySourceTable$CSO.Electoral.Divisions.2022)

######################################TRAVEL#########################################
#REad ED and AC  table from PXSTat
TravelSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T11T1ED/PX/2013/")
TravelSourceTable <- as.data.frame(TravelSourceTable.px)%>%filter(Statistic == "Usually resident by means of travel to work, school, college or childcare (total)")

TravelSourceTableAC.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T11T1CTY/PX/2013/")
#removing ireland as this is already in the EDtable
TravelSourceTableAC <- as.data.frame(TravelSourceTableAC.px)%>%dplyr::rename(CSO.Electoral.Divisions.2022 = "Administrative.Counties.2019")%>%filter(CSO.Electoral.Divisions.2022!="Ireland" & Statistic == "Usually resident by means of travel to work, school, college or childcare (total)")

#EDs as cahracter for correct join
TravelSourceTable$CSO.Electoral.Divisions.2022<- as.character(TravelSourceTable$CSO.Electoral.Divisions.2022)
#rename Ireland State
TravelSourceTable$CSO.Electoral.Divisions.2022[TravelSourceTable$CSO.Electoral.Divisions.2022 == "Ireland"] <- "State"

#Fix special characters in AC table
TravelSourceTableAC$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire|D├║n Laoghaire","Dun Laoghaire Rathdown County Council", TravelSourceTableAC$CSO.Electoral.Divisions.2022)

TravelSourceTable <- rbind(TravelSourceTable,TravelSourceTableAC)


#remove special chars etc
TravelSourceTable$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",TravelSourceTable$CSO.Electoral.Divisions.2022)
TravelSourceTable$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", TravelSourceTable$CSO.Electoral.Divisions.2022)
TravelSourceTable$CSO.Electoral.Divisions.2022 <- gsub("'","", TravelSourceTable$CSO.Electoral.Divisions.2022)
TravelSourceTable$CSO.Electoral.Divisions.2022 <- gsub("&","and", TravelSourceTable$CSO.Electoral.Divisions.2022)


############################KEY POINTS AND OTHER CALCS#####################################

#key points stats - names used here should be self explanatory
Area.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/F1011/PX/2013/")
Area <- as.data.frame(Area.px)


Area$Electoral.Divisions <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",Area$Electoral.Divisions)
Area$Electoral.Divisions <- gsub("D├║n Laoghaire","Dun Laoghaire", Area$Electoral.Divisions)
Area$Electoral.Divisions<- gsub("'","", Area$Electoral.Divisions)
Area$Electoral.Divisions<- gsub("&","and", Area$Electoral.Divisions)

FamiliesInPrivateHouseholds.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/SAP2022T4T1ED/PX/2013/")

FamiliesInPrivateHouseholds <- as.data.frame(FamiliesInPrivateHouseholds.px)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("Dâ”œâ•‘n Laoghaire","Dun Laoghaire",FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("D├║n Laoghaire","Dun Laoghaire", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("'","", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)
FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022 <- gsub("&","and", FamiliesInPrivateHouseholds$CSO.Electoral.Divisions.2022)

#########################################SAPS###################################
# import SAPS files and SAPS Glossary
SAPSED <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_CSOED3270923.csv"), header = T)
SAPSCounty <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_county_270923.csv"), header = T)
SAPSGlossary<- read.csv(paste0(InputFilesLoc,"/SAPS2022/GlossaryEditForPercentages.csv"), header = T)
SAPSTotal <- rbind(SAPSED,SAPSCounty)


for (j in 1:nrow(SAPSGlossary)) {
  # Get the column name and total column name from the csv
  ColumnName <- SAPSGlossary$ColName[j]
  TotalColumnName <- SAPSGlossary$TotalColForPerc[j]
  
  # Create a new column name with "perc" appended
  NewColumnName <- paste0(ColumnName, "_Perc")
  
  # Calculate the new column 
  SAPSTotal <- SAPSTotal %>%
    dplyr::mutate({{NewColumnName }} := get(ColumnName)*100 / get(TotalColumnName))
}

# new_col will be added to main_df for each iteration
SAPSPercentages <- SAPSTotal%>%dplyr::select(matches("_Perc"))
SAPSPercentages$GUID <- SAPSTotal$GUID
colnames(SAPSPercentages) <- gsub("_Perc","", colnames(SAPSPercentages))

# round the percentages to one decimal place
SAPSPercentages <- SAPSPercentages%>%mutate_if(is.numeric, function(x) round(x,1))

# Select the appropriate ED,AC and State from the SAPS percentages file
SAPSPercentages <- SAPSPercentages[!duplicated(SAPSPercentages$GUID),]
SAPSTotal <- SAPSTotal[!duplicated(SAPSTotal$GUID),]

SAPSPercentages$ED <- SAPSTotal$GEOGDESC
saveRDS(SAPSPercentages, file = paste0(OutputFilesLoc,"/SAPSPercentages.Rds"))
