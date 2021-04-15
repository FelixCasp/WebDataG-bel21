#Title: Back to the roots? New significance of traditional tools
#Author: Felix Caspari
#Seminar: Web data collection with R
#Matriculation number: 01/1012736





#### INSTALL AND LOAD PACKAGES ====================================================================================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(tidyverse,car, 
               survey, 
               naniar, 
               gapminder, 
               foreign, 
               srvyr,
               readr,
               haven, 
               mice, 
               mitools, 
               dplyr,
               magrittr,
               purrr,
               lubridate,
               stringr,
               ggplot2,
               tidyverse,
               xml2,
               car,
               lme4,
               tidyr,
               readxl,
               XML,
               flatxml,
               httr,
               rvest,
               robotstxt,
               install = TRUE,
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())




# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))





#Obtain general election result of 2021=============================================================================================================

# assign url ----------------------------------------------------------------------------
ltw_url_ge21 <- "https://de.wikipedia.org/wiki/Landtagswahlkreis_T%C3%BCbingen#cite_note-3"


# parse url -----------------------------------------------------------------------------
xml2::url_parse(ltw_url_ge21)


# open url in browser -------------------------------------------------------------------
browseURL(ltw_url_ge21)


# name of current html ------------------------------------------------------------------
LTW21_ge <- stringr::str_c(basename(ltw_url_ge21), ".html")

# create a folder to store htmls --------------------------------------------------------
path <- file.path("htmls")
if (!dir.exists("htmls")) {
  path %>%
    dir.create(recursive = TRUE)
}


# download the html file ----------------------------------------------------------------
xml2::download_html(url = ltw_url_ge21,
                    file = file.path(path, LTW21_ge))

# adjust user_agent ---------------------------------------------------------------------
str_c("felixcaspari@live.de", 
      "collecting data for study purposes",
      R.version$platform,
      R.version$version.string,
      sep = ", ") %>%
  httr::user_agent() %>%
  httr::set_config()

# assign and parse urls ---------------------------------------------------------------------------
ltw_url21ge_parsed <- xml2::url_parse(ltw_url_ge21)



# ask for permission --------------------------------------------------------------------
ltw_robotstxt <- ltw_url21ge_parsed$server %>%  
  robotstxt()
ltw_url21ge_parsed$path %>%
  
  ltw_robotstxt$check()
ltw_robotstxt$crawl_delay


if(! ltw_url21ge_parsed$path %>%
   ltw_robotstxt$check())
  (
    stop(message="STOPPING, we are not allowed to scrape")) #just if it is the case I included an abort command


# parse html files ----------------------------------------------------------------------
ltw_url21ge_parsed <- path %>%
  file.path(LTW21_ge) %>%
  xml2::read_html()
ltw_url21ge_parsed

# assigning the node with the parsed document
ltw_node_21ge <- ltw_url21ge_parsed %>%
  rvest::html_node(xpath = "//table")



# content of node
ltw_node_21ge %>%
  rvest::html_table(fill = TRUE) %>% 
  View()

#generally retrieving all tables from the document through 
tables_21ge <- ltw_url21ge_parsed %>% html_table(fill = TRUE) #assigning list of tables
tables_21ge <- tables_21ge[[2]]# make general table for results across all the voting district 62 --> this information is found in table number 2 
tables_21ge <- tables_21ge %>% slice(-c(7:16))#delete the minor parties because of better visualization

map(tables_21ge, class)
tables_21ge$Direktkandidat<- as.factor(tables_21ge$Direktkandidat)
tables_21ge$Partei<- as.factor(tables_21ge$Partei)

tables_21ge$`Stimmen in %` <- gsub(",",".",tables_21ge$`Stimmen in %`) 
tables_21ge$`Landtagswahl 2016Stimmen in %` <- gsub(",",".",tables_21ge$`Landtagswahl 2016Stimmen in %`) 

tables_21ge$`Stimmen in %`<- as.numeric(tables_21ge$`Stimmen in %`)
tables_21ge$`Landtagswahl 2016Stimmen in %`<- as.numeric(tables_21ge$`Landtagswahl 2016Stimmen in %`)

tables_21ge$Change_21 <- (tables_21ge$`Stimmen in %` - tables_21ge$`Landtagswahl 2016Stimmen in %`)#calculate difference of vote share result


names(tables_21ge)[names(tables_21ge) == "Stimmen in %"] <- 'result21'
names(tables_21ge)[names(tables_21ge) == 'Landtagswahl 2016Stimmen in %'] <- 'result16'




#Plot for general vote share across parties 2021  (FIGURE 1)

ggplot(data = tables_21ge, mapping = aes(x = Partei, y = result21, fill = Partei)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('2021 state level result Tübingen') + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual("Parties", values = c("CDU" = "black", "AfD" = "blue", "Grüne" = "darkgreen", "SPD" = "Red", "FDP" = "yellow",  "DIE LINKE" =   "darkred")) + labs(x = "Party") + ylab("Votes share")  + geom_text(aes(label= result21), position=position_dodge(width=0.9), vjust=-0.25)


#Plot für vote share change across parties 2021 (FIGURE 2)
ggplot(data = tables_21ge, mapping = aes(x = Partei, y = Change_21, fill= Partei)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('2021 State level vote share change Tübingen') + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual("Parties", values = c("CDU" = "black", "AfD" = "blue", "Grüne" = "darkgreen", "SPD" = "Red", "FDP" = "yellow",  "DIE LINKE" =   "darkred")) + labs(x = "Party") + ylab("Votes share")

#========================================================================================================================================================================
  
  
  
  



##Download 2016 state level data =============================================================================================================

# assign url ----------------------------------------------------------------------------
ltw_url <- "https://www.tuebingen.de/wahl/html/lt2016.html"


# parse url -----------------------------------------------------------------------------
xml2::url_parse(ltw_url)


# open url in browser -------------------------------------------------------------------
browseURL(ltw_url)


# name of current html ------------------------------------------------------------------
LTW16 <- stringr::str_c(basename(ltw_url), ".html")

# create a folder to store htmls --------------------------------------------------------
path <- file.path("htmls")
if (!dir.exists("htmls")) {
  path %>%
    dir.create(recursive = TRUE)
}
###Download of 2016 state level election data

# download the html file ----------------------------------------------------------------
xml2::download_html(url = ltw_url,
                    file = file.path(path, LTW16))

# adjust user_agent ---------------------------------------------------------------------
str_c("felixcaspari@live.de", 
      "collecting data for study purposes",
      R.version$platform,
      R.version$version.string,
      sep = ", ") %>%
  httr::user_agent() %>%
  httr::set_config()

# assign and parse urls ---------------------------------------------------------------------------
ltw_url_parsed <- xml2::url_parse(ltw_url)



# ask for permission --------------------------------------------------------------------
ltw_robotstxt <- ltw_url_parsed$server %>%  
  robotstxt()
ltw_url_parsed$path %>%
  
  ltw_robotstxt$check()
ltw_robotstxt$crawl_delay


if(! ltw_url_parsed$path %>%
   ltw_robotstxt$check())
(
  stop(message="STOPPING, we are not allowed to scrape")) #just if it is the case I included an abort command


# parse html files ----------------------------------------------------------------------
ltw_parsed <- path %>%
  file.path(LTW16) %>%
  xml2::read_html()
ltw_parsed

# assigning the node with the parsed document
ltw_node <- ltw_parsed %>%
  rvest::html_node(xpath = "//table")



# content of node
ltw_node %>%
  rvest::html_table(fill = TRUE) %>% 
  View()


#generally retrieving all tables from the document through 
tables <- ltw_parsed %>% html_table(fill = TRUE) #assigning list of tables into tables list 
fi_ltw_table <- tables[[14]] # Number 14-17 are giving us the information we want(Vote share and turnout . 
                            #First, Second,third and fourth ltw table are created in order to prepare for a join 
se_ltw_table <- tables[[15]]
th_ltw_table <- tables[[16]]
fo_ltw_table <- tables[[17]]



##Generate overview table for the district of Tübingen of 2016. ==========================================================



ge_tables_16 <- tables[[1]]# make general table for results across all the voting district 62 --> this information is found in table number 1 
ge_tables_16 <- ge_tables_16 %>% filter(row_number() %% 2 != 0) ## Delete even-rows because they are uninformative
ge_tables_16 <- ge_tables_16[,-(2:3)] #delete uninformative columns
ge_tables_16 <- ge_tables_16 %>% slice(c(1:5, 11))#delete the minor parties because of better visualization


map(ge_tables_16, class)

ge_tables_16$X4<- gsub(",", ".", ge_tables_16$X4, perl=TRUE)#remove dots 
ge_tables_16$X5<- gsub(",", ".", ge_tables_16$X5, perl=TRUE)#remove dots 
ge_tables_16$X4<- gsub("%", "", ge_tables_16$X4, perl=TRUE)#remove % 
ge_tables_16$X5<- gsub("%", "", ge_tables_16$X5, perl=TRUE)#remove %

oldnames = c("X1","X4","X5", "X6")
newnames = c("Party","Result","Change", "Vote Share") # assign new names to data 

ge_tables_16 <- ge_tables_16 %>% rename_at(vars(oldnames), ~ newnames)

ix <- 2:4
ge_tables_16[ix] <- lapply(ge_tables_16[ix], as.numeric) #use column index to perform as numeric



#Plot for general vote share across parties 2016

ggplot(data = ge_tables_16, mapping = aes(x = Party, y = Result, fill= Party)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('2021 state level result Tübingen') + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual("Parties", values = c("CDU" = "black", "AfD" = "blue", "GRÜNE" = "darkgreen", "SPD" = "Red", "FDP" = "yellow",  "DIE LINKE" =   "darkred")) + labs(x = "Party") + ylab("Votes share")  + geom_text(aes(label=Result), position=position_dodge(width=0.9), vjust=-0.25)


#Plot für vote share change across parties 2016
ggplot(data = ge_tables_16, mapping = aes(x = Party, y = Change, fill= Party)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('2021 State level vote share change Tübingen') + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual("Parties", values = c("CDU" = "black", "AfD" = "blue", "GRÜNE" = "darkgreen", "SPD" = "Red", "FDP" = "yellow",  "DIE LINKE" =   "darkred")) + labs(x = "Party") + ylab("Votes share")





# Continue merging the different subtables into one large dataframe ==========================================================

ltw <- full_join(fi_ltw_table, se_ltw_table)# full join is necessary, since 
ltw <- full_join(ltw, th_ltw_table)
fo_ltw_table$`Wahl-berech-tigte` <- as.double(fo_ltw_table$`Wahl-berech-tigte`)
fo_ltw_table <- fo_ltw_table[-c(14, 15),] #have to change datatypes since Wahlberechtigte is not double in ltw and character in fo_ltw_table. I also have to delete the last two entries, since they contain postal voters, which are not addressed or referenced geographically. Hence they are omitted from the data. 


ltw <- full_join(ltw, fo_ltw_table) 


fo_ltw_table <- fo_ltw_table[-c(14, 15),]


ltw <- full_join(ltw, fo_ltw_table)


#Data Cleaning

ltw_de <- subset(ltw, `Wahl-berech-tigte` > 2)
ltw_de1<-ltw[!(ltw$`Wahl-berech-tigte`>2),]#get subset of the values that do have decimals 


#change to character to easily remove all dots in order to work without decimal points in the upcoming table
ltw_de1$`Wahl-berech-tigte`<- as.character(ltw_de1$`Wahl-berech-tigte`) 

ltw_de1$`Wahl-berech-tigte`<- gsub("\\.", "", ltw_de1$`Wahl-berech-tigte`, perl=TRUE) #remove dots 

ltw_de1$`Wahl-berech-tigte`<- as.numeric(ltw_de1$`Wahl-berech-tigte`)#change back to numeric 

#join the two datables back together 
ltw <- full_join(ltw_de1, ltw_de)

#deletion of data I do not need for the further cleaning process
ltw <- select(ltw, -c(11:15))

#check for more problems in data classes
map(ltw_de1, class)


#regular expression cleaning

ltw_reg_ch <- ltw ##assign new object for regular expression cleaning


ltw_reg_ch$GRÜNE <- gsub("\\%.*","",ltw_reg_ch$GRÜNE) ##remove everything after the %

# extract 18-24th characters in string
ltw_reg_ch$GRÜNE <- substr(ltw_reg_ch$GRÜNE, start = 4, stop=20) #in order to clean around the given number of interest. 20 is a little bit random, but it is big enough to cut enough content. 


#switch "," with "." in order to prepare for conflicts with type conversion

ltw_reg_ch$GRÜNE <- gsub(",",".",ltw_reg_ch$GRÜNE) 


#switch from cleaned character to numeric in order to finally start working 

ltw_reg_ch$GRÜNE<- as.numeric(ltw_reg_ch$GRÜNE)


map(ltw_reg_ch, class)


#first barplot to check for distribution ==========================================================

ggplot(data = ltw_reg_ch, mapping = aes(x = Wahlbezirk, y = GRÜNE))+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Voting Results across districts') + 
  geom_bar(stat = "identity", , fill = "Darkgreen") +
  labs(x = "Voting Districts") + ylab("Vote-Share")
#at first glance it seems there is an outlier for bebenhausen. Furthermore the district Französisches Viertel is a very famous and very green-supportive district. 
ltw_reg_ch[51, 7]<- 35.3 # select rows and columns and manually include value


#calculating turnout from eligible voters in relation to valid votes

ltw_reg_ch$turnout <- (ltw_reg_ch$GültigeStimmen*100 /(ltw_reg_ch$`Wahl-berech-tigte`))

#check for outliers
ggplot(data = ltw_reg_ch, mapping = aes(x = Wahlbezirk, y = turnout))+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('turnout results across districts') + 
  geom_bar(stat = "identity", , fill = "Darkblue") +
  labs(x = "Voting Districts") + ylab("turnout")

#three outliers can be detected graphically in hasenbühl, westbahnhof and denzenberg
#since only three outliers are detected we do not need to search for a systematic error. Therefore manually adapating for those three districts 
#is sufficient. 

ltw_reg_ch[30, 3]<- 1220 # select rows and columns
ltw_reg_ch[25, 3]<- 1550
ltw_reg_ch[24, 3]<- 1160

#calculating turnout from eligible voters in relation to valid votes

ltw_reg_ch$turnout <- (ltw_reg_ch$GültigeStimmen*100 /(ltw_reg_ch$`Wahl-berech-tigte`))

#check for outliers
ggplot(data = ltw_reg_ch, mapping = aes(x = Wahlbezirk, y = turnout)) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('turnout results across districts') + 
  geom_bar(stat = "identity", , fill = "Darkblue") +
  labs(x = "Voting Districts") + ylab("turnout")

###looks good

#####ps score ==========================================================

 ltw_fin <- ltw_reg_ch


 
 ltw_fin$ps_base <- 10+round(10*(2* (ltw_fin$GRÜNE/max(ltw_fin$GRÜNE))+(ltw_fin$turnout/max(ltw_fin$turnout))))#in order to receive a scale, I divided the 
 #individual voteshare with the maximum of the given variables(vote-share, turnout). Hence I use turnout and vote share as indicators for potential. To obtain score, set district share in relation to optimal outcome. Naturally due to the low number of units, I added 10 as a baseline, to avoid having zero potential in a district, hence this would not be true in the real world. 
 
 ltw_fin$ps_1_100<-  round(scales::rescale(-ltw_fin$ps_base, to = c(100, 25))) #due to the fact that the number of units is very small a scale between 1 and 100 starting from 0 would stretch the values to strong leading to missperception in potential evaluation. 
 

 #FIGURE 6
 ggplot(data = ltw_fin, mapping = aes(x = Wahlbezirk, y = ps_1_100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Potential score  across districts') + 
  geom_bar(stat = "identity", , fill = "Darkred") +
  labs(x = "Voting Districts") + ylab("Potential Score")
 
 ###subset specific rows and values: potential score over 60 
 
 ltw_hi_ps <- ltw_fin %>% filter(ps_1_100 > 60)
 
 ###show graphical representation
 
 ggplot(data = ltw_hi_ps, mapping = aes(x = Wahlbezirk, y = ps_1_100))+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   ggtitle('Potential score across high potential districts') + 
   geom_bar(stat = "identity", , fill = "Darkolivegreen") + 
   geom_text(aes(label=ps_1_100), vjust=-0.3, size=3.5)+
   labs(x = "Voting Districts") + ylab("Potential Score")
 
 
 #draw simple random sample from high potential districts 
 
 sample_size <- 14
 srs_treat <-
   ltw_hi_ps %>% sample_n(sample_size) #take unsampled high potential areas as control groups.
 #Subset later manually when dataframe is cleaned. Base assignment in groups later on the simple random sample
 
 
 
 
 
 
 ###Download of 2021 state level election data  ==========================================================
 # assign url ----------------------------------------------------------------------------
 ltw_url_21 <- "https://wahlergebnisse.komm.one/04/produktion/wahltermin-20210314/08416041/html5/Landtagswahl_BW_2021_Land_BW_172_Uebersicht_stbz.html"
 
 
 # parse url -----------------------------------------------------------------------------
 xml2::url_parse(ltw_url_21)
 
 
 # open url in browser -------------------------------------------------------------------
 browseURL(ltw_url_21)
 
 
 # name of current html ------------------------------------------------------------------
 LTW21 <- stringr::str_c(basename(ltw_url_21), ".html")
 
 # create a folder to store htmls --------------------------------------------------------
 path <- file.path("htmls")
 if (!dir.exists("htmls")) {
   path %>%
     dir.create(recursive = TRUE)
 }
 

 # download the html file ----------------------------------------------------------------
 xml2::download_html(url = ltw_url_21,
                     file = file.path(path, LTW21))
 
 # adjust user_agent ---------------------------------------------------------------------
 str_c("felixcaspari@live.de", 
       "collecting data for study purposes",
       R.version$platform,
       R.version$version.string,
       sep = ", ") %>%
   httr::user_agent() %>%
   httr::set_config()
 
 # assign and parse urls ---------------------------------------------------------------------------
 ltw_url_parsed_21 <- xml2::url_parse(ltw_url_21)
 
 
 
 # ask for permission --------------------------------------------------------------------
 ltw_robotstxt <- ltw_url_parsed_21$server %>%  
   robotstxt()
 ltw_url_parsed_21$path %>%
   ltw_robotstxt_21$check()
 ltw_robotstxt_21$crawl_delay
 
 if(! ltw_url_parsed_21$path %>%
    ltw_robotstxt$check())
   (
     stop(message="STOPPING, we are not allowed to scrape")) #just if it is the case I included an abort command. 
 
 
 # parse html files ----------------------------------------------------------------------
 ltw_parsed_21 <- path %>%
   file.path(LTW21) %>%
   xml2::read_html()
 ltw_parsed_21
 
 # absolute path to node
 ltw_node_21 <- ltw_parsed_21%>%
   rvest::html_node(xpath = "//table")
 
 
 # content of node
 ltw_node_21 %>%
   rvest::html_table(fill = TRUE) %>% 
   View()
 
 
 
 tables21 <- ltw_parsed_21 %>% html_table(fill = TRUE)
 
 
 fi_ltw_table21 <- tables21[[1]]
 
 
 #Due to the fact that the district numbers changed the old numbers from 2016. Hence
 #We do not need them anymore and delete the digits. 
 
 fi_ltw_table21$Wahlbezirk <- gsub("[[:digit:]]","",fi_ltw_table21$Wahlbezirk) 
 
 
 fi_ltw_table21 <- select(fi_ltw_table21, -c(12:13))
 fi_ltw_table21 <- select(fi_ltw_table21, -c(2))#delete klimaliste and sonstiges
 
 
 oldnames1 = c("Lede Abal, GRÜNE","Arnold, CDU","Reetzke, AfD", "Dr. Kliche-Behnke, SPD", "Schuster, FDP",  "Haydt, DIE LINKE")
 newnames1 = c("GRÜNE","CDU","AfD", "SPD", "FDP", "Linke")
 
 fi_ltw_table21 <- fi_ltw_table21 %>% rename_at(vars(oldnames1), ~ newnames1)

 
 fi_ltw_table21 <- fi_ltw_table21 %>% slice(c(1:29))#delete the minor parties because of better visualization
 fi_ltw_table21$GRÜNE <- gsub("%","",fi_ltw_table21$GRÜNE) 
 fi_ltw_table21$GRÜNE <- gsub(",",".",fi_ltw_table21$GRÜNE) 
 
 map(fi_ltw_table21, class)
 fi_ltw_table21$GRÜNE<- as.numeric(fi_ltw_table21$GRÜNE)
 fi_ltw_table21$Wahlbezirk<- as.factor(fi_ltw_table21$Wahlbezirk)
 
 fi_ltw_table21$Wahlbezirk <-  tolower(fi_ltw_table21$Wahlbezirk)
 
 
 
 map(ltw_hi_ps, class)
 ltw_hi_ps$Wahlbezirk<- as.factor(ltw_hi_ps$Wahlbezirk)
 ltw_hi_ps$Wahlbezirk <-  tolower(ltw_hi_ps$Wahlbezirk)
 
 ltw_hi_ps$Wahlbezirk <- gsub("[[:space:]]","",ltw_hi_ps$Wahlbezirk)
 sample_16<- subset(ltw_hi_ps, Wahlbezirk %in% c("österberg", "frischlinstraße", "schönblick", "steinlach", "volksgarten" , "lustnau-süd/alteweberei", "kilchberg", "hagelloch-west"))
 
 sample_16$Wahlbezirk <- gsub("-west","",sample_16$Wahlbezirk)
 sample_16$Wahlbezirk <- gsub("lustnau-süd/","",sample_16$Wahlbezirk) 
 

 
 sample_21<- subset(fi_ltw_table21, Wahlbezirk %in% c(" österberg", " frischlinstraße/denzenberg", " schönblick/horemer", " steinlach", " volksgarten" , " alte weberei/gartenstraße", " kilchberg", " hagelloch"))
 
 sample_21$Wahlbezirk <- gsub("/denzenberg","",sample_21$Wahlbezirk) 
 sample_21$Wahlbezirk <- gsub("/horemer","",sample_21$Wahlbezirk) 
 sample_21$Wahlbezirk <- gsub("/gartenstraße","",sample_21$Wahlbezirk) 
 
 sample_16$Wahlbezirk<- as.factor(sample_16$Wahlbezirk)
 sample_21$Wahlbezirk<- as.factor(sample_21$Wahlbezirk)
 
 sample_21$Wahlbezirk <- gsub("[[:space:]]","",sample_21$Wahlbezirk)
 sample_16$Wahlbezirk <- gsub("[[:space:]]","",sample_16$Wahlbezirk)
 
 
 #Finally join final sample of data after previously cleaning them
 
 sample_fin <- left_join(sample_21, sample_16, by = "Wahlbezirk")
 sample_fin$Wahlbeteiligung <- gsub("%","",sample_fin$Wahlbeteiligung)
 sample_fin$Wahlbeteiligung <- gsub("[[:space:]]","", sample_fin$Wahlbeteiligung)
 sample_fin$Wahlbeteiligung <- gsub(",",".", sample_fin$Wahlbeteiligung)
 sample_fin$Wahlbeteiligung<- as.numeric(sample_fin$Wahlbeteiligung)
 map(sample_fin, class)
 
 
 sample_fin$Change <- (sample_fin$GRÜNE.y - sample_fin$GRÜNE.x)#calculate difference of vote share result

 sample_fin$Change_Tu <- (sample_fin$Wahlbeteiligung - sample_fin$turnout)#calculate difference of turnout result
 
 win_loss <- sample_fin %>% mutate(Color = ifelse(Change > 0, "green", "red")) 
 map(win_loss, class)
 
 win_loss_tu <- sample_fin %>% mutate(Color = ifelse(Change_Tu > 0, "green", "red")) 
 
 
 #Vote share change first plot
 ggplot(data = win_loss, mapping = aes(x=Wahlbezirk, y=Change, fill = Color)) +
   geom_bar(stat="identity", width = 0.8, position = "dodge") + theme_bw() + theme_bw()  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual("legend", values = c( "red" = "darkred", "green" = "darkgreen")) + labs(x = "District") + ylab("Votes share")+ geom_text(aes(label=Change), position=position_stack(vjust=0.5))
 
 
 
 #Turnout first plot
 
 ggplot(data = win_loss_tu, mapping = aes(x=Wahlbezirk, y=Change_Tu, fill = Color)) +
   geom_bar(stat="identity", width = 0.8, position = "dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual("legend", values = c( "red" = "darkred", "green" = "darkolivegreen")) + labs(x ="Party") + ylab("turnout")+ geom_text(aes(label=Change), position=position_stack(vjust=1.1)) 
 
 
 
 
 
 
 
 
 sample_21<- subset(fi_ltw_table21, Wahlbezirk %in% c(" österberg", " frischlinstraße/denzenberg", " schönblick/horemer", " steinlach", " volksgarten" , " alte weberei/gartenstraße", " kilchberg", " hagelloch"))
 
 
 ###Put back the dataframe in the order of the sample vs. control group. sample_treated represents treated areas, while sample control represents non-sampled areas
 
 sample_fin$Treated <- (sample_fin$Wahlbezirk == c("österberg", "frischlinstraße", "kilchberg", "hagelloch"))
 
 sample_Treated <- subset(sample_fin, Wahlbezirk %in% c("österberg", "frischlinstraße", "kilchberg", "hagelloch"))
 
 sample_control <- subset(sample_fin, Wahlbezirk %in% c("schönblick", "steinlach", "volksgarten" , "alteweberei"))
 
 sample_fin$Wahlbezirk <-  toupper(sample_fin$Wahlbezirk)#only for visual purposes 
 

 #Final plot for vote share change between the groups in the sampled districts (Figure 3)
 
 ggplot(data = sample_fin, mapping = aes(x=Wahlbezirk, y=Change, fill = Treated)) +
   geom_bar(stat="identity", width = 0.8, position = "dodge") + theme_bw() + ggtitle('Green vote share change across groups') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual("Group", values = c( "TRUE" = "darkgreen", "FALSE" = "darkred"),labels=c("Control", "Treatment")) + labs(x = "Party") + ylab("Votes share change ")+ geom_text(aes(label=Change), position=position_stack(vjust=1.29)) 
 
 
 #Final plot for turnout change between the groups in the sampled districts (Figure 4)
 ggplot(data = sample_fin, mapping = aes(x=Wahlbezirk, y=Change_Tu, fill = Treated)) +
   geom_bar(stat="identity", width = 0.8, position = "dodge") + theme_bw() + ggtitle("Turnout change across groups")+ylim(-35,20) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual("Group", values = c( "TRUE" = "green", "FALSE" = "chartreuse4"),labels=c("Control", "Treatment")) + labs(x = "Party") + ylab("Turnout change") + labs(fill='Groups') 
 
 
 
 
 # Small statistical tests and summary statistics==========================================================

 qqnorm(sample_fin$Change) #F(Figure 5)
 shapiro.test(sample_fin$Change)
 
 #From the output obtained we can assume normality. The p-value is greater than 0.05. Hence, 
 #the distribution of the given data is not different from normal distribution significantly
 
 
 sd(sample_fin$Change)
 mean(sample_fin$Change)
 sd(sample_fin$Change_Tu)
 mean(sample_fin$Change_Tu)
 
 min(sample_fin$Change)
 max(sample_fin$Change)
 min(sample_fin$Change_Tu)
 max(sample_fin$Change_Tu)
 

 
 
 sd(sample_Treated$Change)
 sd(sample_control$Change)
 
 mean(sample_Treated$Change)
 mean(sample_control$Change)
 
 


res <- t.test(Change ~ Treated, data = sample_fin) # ttest for vote share change --> no effect 
res

res_tu <- t.test(Change_Tu ~ Treated, data = sample_fin) # ttest for turnout change --> no effect and not included in the paper
res_tu

mean(sample_fin$Change_Tu)
# =======================================================================================================================================================================

#pepare some dataframes for github repo

write.csv(x=sample_fin, file="CompleteSample.csv")
write.csv(x=ge_tables_16, file="LTW_21.csv")
write.csv(x=tables_21ge, file="LTW_21.csv")


# =======================================================================================================================================================================


