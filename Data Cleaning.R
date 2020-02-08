#########################################################
#     Group members:  Stephanie Beyer Diaz              #
#                     Shabenoor Kamal                   #
#                     Omar Abdelgelil                   #
#########################################################

#Importing all packages used
if (!require("haven")) install.packages("haven")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("pdftools")) install.packages("pdftools")
if (!require("plotly")) install.packages("plotly")
if (!require("shiny")) install.packages("shiny")
if (!require("rgeos")) install.packages("rgeos")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
library(haven)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(pdftools)
library(plotly)
library(shiny)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)

#######################################################
#     Step 1: reading and cleaning data               #
#######################################################

#1.1 Adding filenames as string
gambling <- "AnalyticDataInternetGambling.sas7bdat"
demograph <- "RawDataIDemographics.sas7bdat"
conversions <- "RawDataIIIPokerChipConversions.sas7bdat"
agg <- "RawDataIIUserDailyAggregation.sas7bdat"

#1.2 Reading files
gambling <- read_sas(gambling)
demograph <- read_sas(demograph)
conversions <- read_sas(conversions)
agg <- read_sas(agg)

#1.3 Formatting values 
#1.3.1 Date Values (gambling table already has date values)
agg$Date <- as.POSIXct(agg$Date,format="%Y%m%d")

demograph$RegDate <- as.POSIXct(demograph$RegDate,format="%Y-%m-%d")
demograph$FirstPay <- as.POSIXct(demograph$FirstPay,format="%Y%m%d")
demograph$FirstAct <- as.POSIXct(demograph$FirstAct,format="%Y%m%d")
demograph$FirstSp <- as.POSIXct(demograph$FirstSp,format="%Y%m%d")
demograph$FirstCa <- as.POSIXct(demograph$FirstCa,format="%Y%m%d")
demograph$FirstGa <- as.POSIXct(demograph$FirstGa,format="%Y%m%d")
demograph$FirstPo <- as.POSIXct(demograph$FirstPo,format="%Y%m%d")

#1.4 Creating new variables
#1.4.1 Separating date and time into new columns
conversions <- separate(conversions,TransDateTime, c("date", "time"),sep =" ")
conversions$date <- as.Date(conversions$date)
#1.4.2 get the hour of the transaction for further analysis
conversions$time <- format(as.POSIXct(conversions$time, format="%H:%M:%S"),"%H")
#1.4.3 now that we have the transaction hour, we can divide them into two groups Morning and Night
# AM being from 01 to 11 and PM from 12 to 24
conversions$AM_Trans <- 0
conversions$AM_Trans <- ifelse(conversions$time >= 0 & conversions$time < 12, 1, 0)
#1.4.4 create a variable called day in the conversion table that shows which day did the trans took place
conversions$Day <- weekdays(as.Date(conversions$date))

#1.4.5 create a variable called month in the conversion table that shows which month did the trans took place
conversions$Month <- month(as.Date(conversions$date))

#1.5 Cleaning values
#1.5.1 remove the negative numbers from the amount column as metadata minimum is positive
conversions$TransAmount <- ifelse(conversions$TransAmount < 0 , 0, conversions$TransAmount)

#1.5.2 clean the TransType column to have 124 as Sell and 24 as Buy
conversions$TransType <- ifelse(conversions$TransType == 124, "Sell", "Buy")

#1.6 Aggregating data
#1.6.1 creating datamart by transaction type to later use in Shiny
datamart_transtype <- conversions %>% group_by(TransType,date) %>% 
  summarise(TotalAmount = sum(TransAmount),
             MeanAmount = mean(TransAmount),
             MaxAmount = max(TransAmount),
             MinAmount = min(TransAmount),
             NUsers = n_distinct(UserID),
             NTrans = n(),
             NumOfMorningTrans = sum(AM_Trans==1),
             NumOfNightTrans = sum(AM_Trans==0))

datamart_transtype$month <- month(datamart_transtype$date)

#1.6.2 Creating Product datamart for later use in Shiny
datamart_product <- as.data.frame(unique(agg$UserID))
colnames(datamart_product) <- 'UserID'
for (i in 1:8) {
  prod <- agg %>% filter(ProductID==i) %>% 
    group_by(UserID) %>%
    dplyr::summarise(avg_stks = mean(Stakes),
                     avg_wins = mean(Winnings),
                     count=n()) %>% 
    mutate(win_vs_stake = avg_wins/avg_stks) %>%
    transmute(UserID,
              !!paste0("win_vs_stake_",i):=win_vs_stake,
              !!paste0("count_",i):=count)
  
  datamart_product <- left_join(datamart_product,prod,by='UserID')
}

#1.6.3. aggreggating userdailyaggregation data to get one row per user: 
agg_summary <- agg %>% group_by(UserID) %>%
  dplyr::summarise(total_win_amount = sum(Winnings),
                   total_stake_amount = sum(Stakes),
                   total_bets = sum(Bets),
                   count = n()) 

#1.6.4 create the following variables in the conversions table:
#the ddply function splits the dataframe by a variable and apply a function to each piece of it
#so here i split the conversions table by user Id and summarize or group by UserId then apply each
#of the following newly created variables and add them back to the table
conversions <-ddply(conversions,.(UserID),summarize,
                    FirstTransDate = min(date),
                    LastTransDate = max(date),
                    buy_min_trans = min(date[TransType == "Buy"]),
                    sell_min_trans = min(date[TransType == "Sell"]),
                    buy_max_trans = max(date[TransType == "Buy"]),
                    sell_max_trans = max(date[TransType == "Sell"]),
                    MostPlayedDayByUser = names(which.max(table(Day))),
                    NumDaysPlayedByUser = n_distinct(date),
                    NumMonthsPlayedByUser = n_distinct(month(date)),
                    MostPlayedMonthByUser = names(which.max(table(month(date)))),
                    TotalAmountSold = sum(TransAmount[TransType == "Sell"]),
                    TotalAmountBought = sum(TransAmount[TransType == "Buy"]),
                    MeanAmountSold = mean(TransAmount[TransType == "Sell"]),
                    MeanAmountBought = mean(TransAmount[TransType == "Buy"]),
                    MaxAmountSold = max(TransAmount[TransType == "Sell"]),
                    MaxAmountBought = max(TransAmount[TransType == "Buy"]),
                    MinAmountSold = min(TransAmount[TransType == "Sell"]),
                    MinAmountBought = min(TransAmount[TransType == "Buy"]),
                    NumOfMorningTrans = sum(AM_Trans==1),
                    NumOfNightTrans = sum(AM_Trans==0),
                    .drop = FALSE)

#1.6.5 Active Period Variable which show the diff between first and last tran
conversions$ActivePeriod <- conversions$LastTransDate - conversions$FirstTransDate

#1.7 Format Factors
demograph$Gender <- factor(demograph$Gender)
demograph$Country <- factor(demograph$Country)
demograph$Language <- factor(demograph$Language)
demograph$ApplicationID <- factor(demograph$ApplicationID)

#1.8 Creating new variables in demograph table
#the following variables shows how many days did it take a customer to start a certain product
demograph <- demograph %>% mutate(diff_regdate_firstact = difftime(FirstAct, RegDate, units="days"),
                                  diff_regdate_firstpay = difftime(FirstPay, RegDate, units="days"),
                                  diff_regdate_firstsp = difftime(FirstSp, RegDate, units="days"),
                                  diff_regdate_firstca = difftime(FirstCa, RegDate, units="days"),
                                  diff_regdate_firstga = difftime(FirstGa, RegDate, units="days"),
                                  diff_regdate_firstpo = difftime(FirstPo, RegDate, units="days"))

demograph$diff_regdate_firstpo <- ifelse(is.na(demograph$diff_regdate_firstpo),"Not Played",demograph$diff_regdate_firstpo) 

######################################################
#     Step 2: Cleaning metadata                      #
######################################################
#2.1 PDF with metadata is read in from page 8 (where appendix starts)
codebookpdf <- "metadata.pdf"
codebook <- pdf_text(codebookpdf)
codebook <- str_c(codebook[8:length(codebook)],collapse='\n')
pattern <- "Appendix.*\r\n"
pos = str_locate_all(codebook, pattern)

df <- data.frame() 

#2.2 dataframe with positions of word "Appendix" is created
for (i in 1:length(pos)) {
  if ( length(pos[[i]])>0 ) { 
    df <- rbind(df,pos[[i]]) }
}

#2.3 word positions are used as cutoff points for each table
for (j in 1:nrow(df)) {
  
  k <- df[j,]$end
  m <- if(is.na(df[j+1,]$start))
          {nchar(codebook)}
          else{df[j+1,]$start}
  table <- substr(codebook,k+1,m-1)
  table <- str_replace_all(table, "\r", "|")
  print(table)
  title <- if(is.na(str_locate(table, "Description"))){
              str_locate(table, "Name")}
              else{str_locate(table, "Description")}
  table <- substring(table,title[2]+2,nchar(table))
  
  #2.4 numeric IDs are kept in separate column
  matches <- regmatches(table, gregexpr("[[:digit:]]+", table))
  table <-gsub("[[:digit:]]+", "", table)
  
  #2.5 strings are passed as a text file to separate and format values
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|", header=FALSE
                         , strip.white=TRUE)
  colnames(data_table)=c("Name","ID")
  data_table$ID <- as.numeric(unlist(matches))
  assign(paste0("table",j), data_table)
}

######################################################
#     Step 3: creating final dataset to analyse      #
######################################################
#3.1 getting all unique user IDs
colnames(gambling)[1] <- 'UserID'
users <- union(agg_summary$UserID,demograph$UserID)
users <- union(users,gambling$UserID)
users <- union(users,conversions$UserID)
users <- as.data.frame(users)
colnames(users) <- "UserID"

#3.2 dropping redundant columns
gambling <- subset(gambling, select=-c(COUNTRY,LANGUAGE,GENDER))

#3.3 joining data to user ids
datamart <- left_join(users,demograph,by='UserID')
datamart <- left_join(datamart,gambling,by='UserID')
datamart <- left_join(datamart,conversions,by='UserID')
datamart <- left_join(datamart,agg_summary,by='UserID')

#3.3 adding metadata info
datamart$Country <- mapvalues(datamart$Country, as.vector(table2$ID), as.vector(table2$Name))
datamart$Language <- mapvalues(datamart$Language, as.vector(table3$ID), as.vector(table3$Name))
datamart$ApplicationID <- mapvalues(datamart$ApplicationID, as.vector(table4$ID), as.vector(table4$Name))
datamart$Gender <- mapvalues(datamart$Gender, c(0,1), c("Female","Male"))

######################################################
#     Step 4: preparing App data                     #
######################################################
#4.1 downloading latitude and longitude data
world <- ne_countries(scale = "medium", returnclass = "sf")[c("continent","name_sort","geometry")]
colnames(world) <- c("Continent","Country","geometry")

#4.2 grouping data by country
datamart_country <- datamart %>% group_by(Country) %>% 
  dplyr::summarise(Users = n(),
                  FemtoMales = round(sum(Gender=="Female")/sum(Gender=="Male"),2),
                  Wins = sum(total_win_amount),
                  Stakes = sum(total_stake_amount),
                  Bets = sum(total_bets),
                  Age = mean(AGE))

#4.3 Calculating Revenue Margin: (Stakes - Wins)/Stakes*100
datamart_country$RevenueMargin <- round((datamart_country$Stakes-datamart_country$Wins)/datamart_country$Stakes*100,2)

#4.6 grouping data by product to use in Shiny
datamart_product_temp <- datamart_product %>% transmute(Sportsbook_fixedodd = mean(win_vs_stake_1, na.rm=TRUE),
                                           Sportsbook_liveaction = mean(win_vs_stake_2, na.rm=TRUE), 
                                           Casino_Bossmedia = mean(win_vs_stake_4[win_vs_stake_4!=Inf], na.rm=TRUE),
                                           Casino_Chartwell = mean(win_vs_stake_8, na.rm=TRUE),
                                           Supertoto = mean(win_vs_stake_5, na.rm=TRUE),
                                           Games_VS = mean(win_vs_stake_6, na.rm=TRUE),
                                           Games_bwin = mean(win_vs_stake_7, na.rm=TRUE))

datamart_product_temp <- datamart_product_temp[1,]
datamart_product_temp <- gather(datamart_product_temp, product, winvsstkratio)   

datamart_product2 <- datamart_product %>% transmute(Sportsbook_fixedodd = 0,
                                            Sportsbook_fixedodd = Sportsbook_fixedodd + sum(ifelse(count_1 > 0, 1, 0), na.rm = TRUE),
                                            Sportsbook_liveaction = 0,
                                            Sportsbook_liveaction = Sportsbook_liveaction + sum(ifelse(count_2 > 0, 1, 0), na.rm = TRUE),
                                            Casino_Bossmedia = 0,
                                            Casino_Bossmedia = Casino_Bossmedia + sum(ifelse(count_4 > 0, 1, 0), na.rm = TRUE),
                                            Casino_Chartwell = 0,
                                            Casino_Chartwell = Casino_Chartwell + sum(ifelse(count_5 > 0, 1, 0), na.rm = TRUE),
                                            Supertoto = 0,
                                            Supertoto = Supertoto + sum(ifelse(count_6 > 0, 1, 0), na.rm = TRUE),
                                            Games_VS = 0,
                                            Games_VS = Games_VS + sum(ifelse(count_7 > 0, 1, 0), na.rm = TRUE),
                                            Games_bwin = 0,
                                            Games_bwin = Games_bwin + sum(ifelse(count_8 > 0, 1, 0), na.rm = TRUE))
datamart_product2 <- datamart_product2[1,]
datamart_product2 <- gather(datamart_product2, product, count_of_users)
datamart_product_temp <- merge(datamart_product_temp, datamart_product2, by="product")

datamart_product3 <- datamart_product %>% transmute(Sportsbook_fixedodd = sum(count_1, na.rm = TRUE),
                                            Sportsbook_liveaction = sum(count_2, na.rm = TRUE),
                                            Casino_Bossmedia = sum(count_4, na.rm = TRUE),
                                            Casino_Chartwell = sum(count_5, na.rm = TRUE),
                                            Supertoto = sum(count_6, na.rm = TRUE),
                                            Games_VS = sum(count_7, na.rm = TRUE),
                                            Games_bwin = sum(count_8, na.rm = TRUE))
datamart_product3 <- datamart_product3[1,]
datamart_product3 <- gather(datamart_product3, product, count_of_transactions)
datamart_product <- merge(datamart_product_temp, datamart_product3, by="product")

datamart_product$ptype <- ifelse(grepl("Casino", datamart_product$product), "Casino", 
                                 ifelse(grepl("Sports", datamart_product$product), "Sports",
                                 ifelse(grepl("Games", datamart_product$product), "Games",datamart_product$product)))

#4.7 grouping data by month to use in Shiny
agg$month <- month(agg$Date)
head(agg)
agg_summary2 <- as.data.frame(unique(agg$month))
colnames(agg_summary2) <- 'month'
for (i in 1:8) {
  prod2 <- agg %>% filter(ProductID==i) %>% 
    group_by(month) %>%
    dplyr::summarise(
      avg_stks = mean(Stakes),
      avg_wins = mean(Winnings),
      avg_bets = mean(Bets),
      count = n()) %>% 
    transmute(month,
              !!paste0("avgstks_",i):=avg_stks,
              !!paste0("avgwins_",i):=avg_wins,
              !!paste0("avgbets_",i):=avg_bets,
              !!paste0("count_",i):=count) 
  agg_summary2 <- left_join(agg_summary2,prod2,by='month')
}

agg_summary2 <- gather(agg_summary2, metric, value, -month)
datamart_month <- separate(agg_summary2, metric, c('metric', 'product'), sep='_')

#4.7.1 mapping product ids to metadata info
datamart_month$product <- mapvalues(datamart_month$product, as.vector(table1$ID), as.vector(table1$Name))
datamart_month <- spread(datamart_month, metric, value)
#4.7.2 adding product type data
datamart_month$ptype <- ifelse(grepl("Casino", datamart_month$product), "Casino", 
                        ifelse(grepl("Sports", datamart_month$product), "Sports",
                        ifelse(grepl("Games", datamart_month$product), "Games",datamart_month$product)))
#4.7.3 dropping null values (no Poker data)
datamart_month <- datamart_month[!is.na(datamart_month$avgbets),]

#4.8 grouping data by user as starting point to create basetables for Shiny
agg_avgs <- agg %>% group_by(UserID) %>%
  dplyr::summarise(
    sum_stks = sum(Stakes),
    sum_wins = sum(Winnings),
    sum_bets = sum(Bets),
    avg_stks = mean(Stakes),
    avg_wins = mean(Winnings),
    avg_bets = mean(Bets),
    last_play = max(Date),
    freq = n())

#4.8.2 grouping data by user to use in Shiny
datamart_user <- datamart[,c("UserID","Country","Language","Gender",
                             "RegDate","AGE")]
datamart_user <- left_join(datamart_user,agg_avgs,by="UserID")
#"loyalty": difference between 30-05-2005 and the customer's last day of playing 
datamart_user$Loyalty <- difftime(strptime("30.05.2005", format = "%d.%m.%Y"), datamart_user$last_play,units="days")
#Profit: stakes-winnings
datamart_user$Profit <- datamart_user$sum_stks - datamart_user$sum_wins
#"AverageProfitPerUsr": total profits over frequency
datamart_user$AverageProfitPerUsr <- datamart_user$Profit/datamart_user$freq
#"RevenueMargin": stakes - winnings /stakes *100 
datamart_user$RevenueMargin <- (datamart_user$avg_stks - datamart_user$avg_wins)/datamart_user$avg_stks * 100
#create high, medium, low margins using approx. cutoff points from summary(datamart_user$RevenueMargin)
datamart_user$RevSegment <- ifelse(datamart_user$RevenueMargin<=8,"Low",
                                   ifelse(datamart_user$RevenueMargin<=30,"Medium",
                                   "High"))
#Adding longitude and latitude
datamart_user <- left_join(datamart_user,
                           subset(as.data.frame(world),select = c("Country","Continent")),
                           by='Country')

#Creating csv files to upload for Shiny app use
write.csv(datamart_country, file = "datamart_country.csv")
write.csv(datamart_user, file = "datamart_user.csv")
write.csv(datamart_product, file = "datamart_product.csv")
write.csv(datamart_month, file = "datamart_month.csv")
write.csv(datamart_transtype, file = "datamart_transtype.csv")

