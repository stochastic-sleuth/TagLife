###Read in start up files

setwd("/Volumes/JRFREY_NOAA/SemiAnnReport/report2/Start_Up_Spec_Sheets")

start<-list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

#Add in tag model and PRI columns to start up dataframe
start$Tag_Model<-ifelse(as.Date(start$Date, format = "%m/%d/%Y")=="2023-02-01" | as.Date(start$Date, format = "%m/%d/%Y")=="2022-12-13" 
                        | as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-01" | as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-15", "SS400", "Error")

start$PRI<-ifelse(as.Date(start$Date, format = "%m/%d/%Y")=="2023-02-01" | as.Date(start$Date, format = "%m/%d/%Y")=="2022-12-13" 
                  | as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-01" | as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-15", 5, "Error")


#Add in warranty life and date
###Tag Model and warranty life dataframe
Tag_Model<-c("SS300","SS400","SS300 bat 392","SS300","SS400","SS300 bat 392","SS300","SS400","SS300 bat 392")
PRI<-c(3,3,3,5,5,5,10,10,10)
warranty_life<-c(23, 48, 79, 37, 71, 128, 68, 111, 238)

warranty<-data.frame(Tag_Model, PRI, warranty_life)

start2<-merge(start, warranty, all.x = TRUE)

start2<-start2[which(start2$TagID!=""),]

start2$warranty_date<-as.Date(start2$Date, format = "%m/%d/%Y") + start2$warranty_life

#Create list of each study, use this to filter the detection data by study

SS_Wk1<-str_c(start2$TagID[which(as.Date(start$Date, format = "%m/%d/%Y")=="2022-12-13")], collapse = "|")
SS_Wk2<-str_c(start2$TagID[which(as.Date(start$Date, format = "%m/%d/%Y")=="2023-02-01")], collapse = "|")
SS_Wk3<-str_c(start2$TagID[which(as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-01")], collapse = "|")
SS_Wk4<-str_c(start2$TagID[which(as.Date(start$Date, format = "%m/%d/%Y")=="2023-03-15")], collapse = "|")


read in PRI5 detections

setwd("/Volumes/JRFREY_NOAA/SemiAnnReport/report2/PRI5")

PRI5<-list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))

rownames(PRI5)<-NULL
#convert the data frame to a tibble and select only unique detections
PRI5<-distinct_all(as_tibble(PRI5))

PRI5$TagCode2<-substr(PRI5$TagCode, 3, 6)

#Add a 0 to the tag codes that start with 0 - they lose it somewhwere
PRI5$TagCode3<-ifelse(substr(PRI5$TagCode2, 1,1)==" ", paste("0", substr(PRI5$TagCode2, 2, 5), sep = ""), PRI5$TagCode2)

#calculate date tag battery died
date_dead<-plyr::ddply(PRI5, ~TagCode3, summarise, date_dead=max(DateTime_PST, na.rm = TRUE))

#Add date dead to the start up file
start3<-merge(start2, date_dead, by.x = "TagID", by.y = "TagCode3", all.x = TRUE)

write.csv(start3,"/Volumes/JRFREY_NOAA/SemiAnnReport/report2/start3.csv" )

# Create Tag Life Tables --------------------------------------------------

###Read in tag inventory data for all studies
###Must be evaluated outside of script

inv<- read.csv("/Volumes/JRFREY_NOAA/SemiAnnReport/report2/tagspecs.csv")

###

##Merge inventory with start up: add start date and PRI
start_inv<-left_join(start3, inv, by= c("TagID" = "HexCode"), keep=TRUE)

###create column for days on (Run one line or the other depending on if it has a timestamp)
start_inv$Date<-as.POSIXct(start_inv$Date, format = "%m/%d/%Y")
start_inv$days_on_a<-difftime(start_inv$date_dead, start_inv$Date, units = "days")
start_inv$days_on<-format(round(start_inv$days_on_a, 1))
start_inv$days<- (as.numeric(str_extract(start_inv$days_on, ".*\\d" )))

#reduce
start_uni<-start_inv[,c("Date","HexCode","Tag_Model","ManufactureDate","PRI","date_dead","days_on", "days")]
#filter all tags by study

#tags started
tagcount<- nrow(start_uni)
#average tag life in days
avgrun<- signif(mean(start_uni$days),3)
#min tag life in days
mi<- signif(min(start_uni$days),3)
#max tag life in days
ma<- signif(max(start_uni$days),3)

#Tag Model SS400 @ PRI=5 has a warranty of 71 days
#number of tags that lasted to warranty or longer
n<- sum(start_uni$days >="71")

#Table format
start_uni<- rename(start_uni, c("Start Date" = "Date", "Hex Code" = "HexCode", 
                                   "Date Manufactured" = "ManufactureDate", "Tag Model" = "Tag_Model",
                                   "Period" = "PRI","Date Dead" = "date_dead", "Days on" = "days_on"))
start_uni<- start_uni[,-8]

#Reverse order of the table so that it matches the order of the plot
start_uni<-start_uni[nrow(start_uni):1,]





# Make Plot by Tagging Study ----------------------------------------------

filter_file<- PRI5 %>% 
  filter(str_detect(TagCode3, SS_Wk1))


startup_filtered<- start3 %>% 
  filter(str_detect(TagID, SS_Wk1))


#repeat the tags the same number of times as there are lines in the "dates" dataframe
tags<-as.data.frame(unique(filter_file$TagCode3))
names(tags)[1]<-"TagCode"


###Plot percent tags remaining by days since start

tags2<-merge(tags,start3[,c("TagID","Date","date_dead")], by.x = "TagCode", by.y = "TagID")
tags2$Date<- as.POSIXct(as.character(tags2$Date, format = "%Y-%m-%d %H:%M:%S"))
tags2$date_dead<- as.POSIXct(as.character(tags2$Date, format = "%Y-%m-%d %H:%M:%S"))
#Create days on column
tags2$Days_On<-as.numeric(difftime(tags2$date_dead, as.Date(tags2$Date, format = "%m/%d/%Y", units = "days")))
tags2$Days_On<- round(tags2$Days_On, digits = 0)

#Create a dataset that is the same length as the study
date_range<-as.data.frame(list(Days_On = seq(0, max(tags2$Days_On))))

tags4<-merge(tags2,date_range, all.y = TRUE)


#Calculate percent tags remaining by day
tags4$dead<-1
tags4$dead[is.na(tags4$TagCode)]<-0
tags4$dead_percent<-(cumsum(tags4$dead/length(unique(filter_file$TagCode3)))*100)
tags4$tags_alive<-(length(unique(filter_file$TagCode3))-cumsum(tags4$dead))
tags4$percent_alive<-100-((cumsum(tags4$dead)/length(unique(filter_file$TagCode3)))*100)

pw1<- ggplot2::ggplot(tags4, aes(Days_On, percent_alive)) +
  geom_line(linewidth=1.2) +
  geom_segment(aes(x = max(startup_filtered$warranty_life), y = 0, xend = max(startup_filtered$warranty_life), yend = 100), linetype = "longdash") +
  scale_x_continuous(name = "Days Since Start of Tags", breaks = seq(0,max(tags4$Days_On)+10, by=20)) +
  scale_y_continuous(name = "Percent Tags Remaining", breaks = seq(0,100, by=5)) +
  labs(title = "Figure 1.", subtitle = "The percent of tags remainaing over the course of the study, the dashed line indicates the warranty \n life (71 days).") +
  theme_bw() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        plot.title = element_text(size = 11, family = "Times", face = "bold", hjust = 0))


