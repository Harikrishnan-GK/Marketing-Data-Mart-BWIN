###########################################################################################
#                                       Team-Group2                                      #
###########################################################################################


###### Harikrishnan GOPALAKRISHNAN ####
###### Tatiane DUTRABUNO  ##########
###### Charlotte GALLET  ##########





###########################################################################################
#                                       Libraries                                         #
###########################################################################################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(rfm)
library(readr)
library(readxl)
library(viridis)
#library(plyr)

###########################################################################################
#                                  Importing Datasets                                     #
###########################################################################################

setwd("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211")
rdata <- get(load('DataGroupAssignment.Rdata')) # load the file.Try loading the file separately by opening it directly from the source.


products <- read_excel("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/Appendices Group Assignment.xlsx", 
                         sheet = "Appendix 1")
country <- read_excel("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/Appendices Group Assignment.xlsx", 
                         sheet = "Appendix 2")
language <- read_excel("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/Appendices Group Assignment.xlsx", 
                            sheet = "Appendix 3")
application <- read_excel("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/Appendices Group Assignment.xlsx", 
                          sheet = "Appendix 4")

#Removing the space of the variables names
names(products) <- sub(" ", "_", names(products))
names(country) <- sub(" ", "_", names(country))
names(language) <- sub(" ", "_", names(language))
names(application) <- sub(" ", "_", names(application))

###########################################################################################
#####################                Data Preparation                 #####################
###########################################################################################

###########################################################################################
#                                 UserDailyAggregation                                    #
###########################################################################################

### Inspecting Data

#Glimpse at Data Types
glimpse(UserDailyAggregation)

#Finding out of Range Values on Stakes
# Min and Max Values
minmax_stakes <- c(min(UserDailyAggregation$Stakes), max(UserDailyAggregation$Stakes))
# Quantiles
quantiles_stakes <- quantile(x = UserDailyAggregation$Stakes, probs = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.999, 0.9999))
quantiles_stakes
# Check with team if we will exclude/winsorize outliers 
outliers_stakes <- UserDailyAggregation %>% 
  filter (UserDailyAggregation$Stakes > quantile(x = UserDailyAggregation$Stakes, probs = 0.9999))
outliers_stakes

#Finding out of Range Values on Winnings
# Min and Max Values
minmax_winnings <- c(min(UserDailyAggregation$Winnings), max(UserDailyAggregation$Winnings))
# Quantiles
quantiles_winnings <- quantile(x = UserDailyAggregation$Winnings, probs = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.999, 0.9999))
quantiles_winnings
# Check with team if we will exclude/winsorize outliers 
outliers_winnings <- UserDailyAggregation %>% 
  filter (UserDailyAggregation$Winnings > quantile(x = UserDailyAggregation$Winnings, probs = 0.9999))
outliers_winnings

### Data Manipulation

# Get the FirstPay value from Demographic Dataset
stakes_df <- merge(x= UserDailyAggregation, y= Demographics[ , c("UserID", "FirstPay")], by= "UserID", all.x = TRUE)

# Transform String in Date Type
stakes_df <- stakes_df %>%
  mutate(Date = paste(substr(Date,1,4),substr(Date,5,6),substr(Date,7,8), sep = '-'),
         FirstPay = paste(substr(FirstPay,1,4),substr(FirstPay,5,6),substr(FirstPay,7,8), sep = '-')
  )
stakes_df$Date <- as.Date(stakes_df$Date)  
stakes_df$FirstPay <- as.Date(stakes_df$FirstPay)    

# Check the Range of Dates of the Transactions (Should be between 01/02/2005 and 30/09/2005)
min(stakes_df$Date)
max(stakes_df$Date)

# Define the end date of the period analysis
end_date = ymd("2005-09-30")

# Filter only records that took place after the first pay-in date 
stakes_df <- stakes_df %>%
  filter(Date > FirstPay)

# Summarize Data by UserID
totals_UserStakes <- stakes_df %>%
  group_by(UserID) %>%
  summarize(
    Stakes_Total = round(sum(Stakes),0),
    Stakes_Avg = round(mean(Stakes),2),
    Stakes_Min = round(min(Stakes),0),
    Stakes_Max = round(max(Stakes),0),
    Winnings_Total = round(sum(Winnings),0),
    Winnings_Avg = round(mean(Winnings),2),
    Winnings_Min = round(min(Winnings),0),
    Winnings_Max = round(max(Winnings),0),
    Bets_Total = round(sum(Bets),0),
    Bets_Avg = round(mean(Bets),2),
    Bets_Min = round(min(Bets),0),
    Bets_Max = round(max(Bets),0),
    Frequency = n(),
    First_Date = min(Date), 
    Last_Date = max(Date)
  ) %>%
  mutate(
    GGR_User = round(Winnings_Total - Stakes_Total,0),#GGR-Gross Gaming revenue.
    GGR_Avg_User = round(GGR_User/Frequency,2),
    GGR_Company = round(Stakes_Total - Winnings_Total,0), 
    GGR_Avg_Company = round(GGR_Company/Frequency,0),
  )

# Summarize Data by UserID and Product
products_UserStakes <- stakes_df %>%
  group_by(UserID, ProductID) %>%
  summarize(
    Frequency = n(),
    Bets_Total = round(sum(Bets),0),
    GGR_Company = round(sum(Stakes) - sum(Winnings),0)
  )

# Transpose the column Product ID 
products_UserStakes <- products_UserStakes %>% 
  pivot_wider(id_cols = UserID,
              names_from = ProductID, 
              values_from = c(Frequency, Bets_Total, GGR_Company),
              names_prefix = "Prod")

# Replace Null values by zero since the user didn't do any transaction
products_UserStakes[is.na(products_UserStakes)] = 0

# Merge Tables
final_stakes <- merge(x= totals_UserStakes, y= products_UserStakes, by= "UserID", all.x = TRUE)

###########################################################################################
#                                     Demographics                                        #
###########################################################################################

#Reading the demographics dataset
summary(Demographics)

#Assigning new Dataframe for Datacleaning
Demo_df <- Demographics

#Change the Character variables to date variables
Demo_df[,5:10]<- c(Demo_df[,5:10])

Demo_df$RegDate <- as.Date(Demo_df$RegDate)
Demo_df$FirstPay <- ymd(as.numeric(Demo_df$FirstPay))
Demo_df$FirstAct <- ymd(as.numeric(Demo_df$FirstAct))
Demo_df$FirstSp <- ymd(as.numeric(Demo_df$FirstSp))
Demo_df$FirstCa <- ymd(as.numeric(Demo_df$FirstCa))
Demo_df$FirstGa <- ymd(as.numeric(Demo_df$FirstGa)) 
Demo_df$FirstPo <- ymd(as.numeric(Demo_df$FirstPo))

#To get the summary and checking for NULL values
summary(Demo_df)

#Convert the list to dataframe
Demo_df <- as.data.frame(Demo_df)

#Replacing the NULL values of Gender by 1 (Male, since it is the Mode)
Demo_df$Gender[is.na(Demo_df$Gender)] = 1

#Adding columns to check whether the user played or not played
Demo_df <-  mutate(Demo_df, FirstSp_info = ifelse(is.na(FirstSp), 'Not Played' , 'Played'))
Demo_df <-  mutate(Demo_df, FirstCa_info = ifelse(is.na(FirstCa), 'Not Played' , 'Played'))
Demo_df <-  mutate(Demo_df, FirstGa_info = ifelse(is.na(FirstGa), 'Not Played' , 'Played'))
Demo_df <-  mutate(Demo_df, FirstPo_info = ifelse(is.na(FirstPo), 'Not Played' , 'Played'))
min(Demographics$RegDate) # to check the start Registration date -01/02/2005
max(Demographics$RegDate) # to check the end Registration date-27/02/2005

#Length of Relationship
Demo_df$LOR <- round(as.numeric(difftime(end_date, Demo_df$RegDate, units = "days")),2)

#Time to first Purchase 
Demo_df$time_1Purchase = round(as.numeric(difftime(Demo_df$FirstPay, Demo_df$RegDate, units = "days")),2)

# Converting the Gender into the labels
Demo_df$Gender <- ifelse(Demo_df$Gender == 1, "Male", "Female")

#Merge Tables
Demo_df <- merge(x= Demo_df, y= country, by= "Country", all.x = TRUE)
Demo_df <- merge(x= Demo_df, y= language, by= "Language", all.x = TRUE)
Demo_df <- merge(x= Demo_df, y= application, by= "ApplicationID", all.x = TRUE)

# Exclude Columns
Demo_df = subset(Demo_df, select = -c(Country,Language,ApplicationID))

###########################################################################################
#                                 PokerChipConversion                                     #
###########################################################################################

#Assigning new Dataframe for Datacleaning
Poker_df <- PokerChipConversions

# We also need to convert the variable TransDateTime into date format  
Poker_df$TransDateTime <- ymd_hms(Poker_df$TransDateTime)
Poker_df$TransDateTime <- as_date(Poker_df$TransDateTime)

# Round TransAmount to 2 decimals
Poker_df$TransAmount <- round(Poker_df$TransAmount,2)

# Converting the transaction types into the labels
Poker_df$TransType <- ifelse(Poker_df$TransType == 124, "Buy", "Sell")

#Checking for any missing values   
sum(is.na(Poker_df)) #There are no missing values, we can start creating new variables 

# Removing all observations where the date is after the 30th of September 2005
Poker_df <- subset(Poker_df, TransDateTime <= end_date)

#Function to calculate the mode of the transaction per user, if he sells or buys more
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply the function mode for the type of transaction
poker_mode <- Poker_df %>% 
  group_by(UserID) %>%
  dplyr::summarize(
    tran_type = Mode(TransType) 
  )

# Calculate the Frequency, this is, the number of days the user played and also the number of bets
# Since the Poker table has one line for each transaction, it was necessary first get this information for each day before
poker_bets <- Poker_df %>%
  group_by(UserID, TransDateTime)%>%
  summarize(Bets_poker = n(),
            )
poker_bets <- poker_bets %>%
  group_by(UserID)%>%
  summarize(First_Date_poker = min(TransDateTime), 
            Last_Date_poker = max(TransDateTime),
            Bets_Total_poker = round(sum(Bets_poker),0),
            Bets_Avg_poker = round(mean(Bets_poker),2),
            Bets_Min_poker = round(min(Bets_poker),0),
            Bets_Max_poker = round(max(Bets_poker),0),
            Frequency_poker = n()
            )

# Calculate the total amount in trans type "buy" or "sell"
Poker_metrics <- Poker_df %>%
  group_by(UserID, TransType)%>%
  dplyr::summarize(Amount_avg = round(mean(TransAmount), 2),
                   Amount_total = round(sum(TransAmount), 0),
                   Amount_min = round(min(TransAmount), 0),
                   Amount_max = round(max(TransAmount),0)
                   )

# Transpose the columns values of buy and sell
Poker_metrics <- Poker_metrics %>% 
  pivot_wider(id_cols = UserID, names_from = TransType, values_from = c(Amount_avg, Amount_total,Amount_min ,Amount_max))

# Replace Null values by zero since the user didn't do any transaction
Poker_metrics[is.na(Poker_metrics)] = 0

# Merge Tables
final_poker <- merge(x= poker_bets, y= poker_mode, by= "UserID", all.x = TRUE)
final_poker <- merge(x= final_poker, y= Poker_metrics, by= "UserID", all.x = TRUE)

# Checking number of missing values 
sum(is.na(final_poker))
summary(final_poker)

# Adding others metrics
final_poker <- final_poker %>% 
  mutate(
        GGR_User_poker = round(Amount_total_Sell - Amount_total_Buy,0),
        GGR_Avg_User_poker = round(GGR_User_poker/Frequency_poker,2),
        GGR_Company_poker = round(Amount_total_Buy - Amount_total_Sell,0), 
        GGR_Avg_Company_poker = round(GGR_Company_poker/Frequency_poker,0),
        )

###########################################################################################
#####################                    Basetable                    #####################
###########################################################################################

# Merge Tables
marketing_dm <- merge(x= Demo_df, y= final_stakes, by= "UserID", all.x = TRUE)
marketing_dm <- merge(x= marketing_dm, y= final_poker, by= "UserID", all.x = TRUE)

marketing_dm <- marketing_dm %>% 
  mutate(
    All_Frequency = rowSums(cbind(Frequency,Frequency_poker),na.rm=TRUE),
    All_Bets = rowSums(cbind(Bets_Total,Bets_Total_poker),na.rm=TRUE),
    All_GGR = rowSums(cbind(GGR_Company,GGR_Company_poker),na.rm=TRUE),
    All_Spend = rowSums(cbind(Stakes_Total,Amount_total_Buy),na.rm=TRUE),
    Stickiness = ifelse(All_Frequency>100, "Addicted", "Not Addicted")
    )
 
marketing_dm$All_Last_Date <- apply(marketing_dm[, c("Last_Date", "Last_Date_poker")], MARGIN =  1, FUN = max, na.rm = TRUE)
marketing_dm$All_Last_Date = ymd(marketing_dm$All_Last_Date)

marketing_dm$Recency <- as.numeric(end_date - marketing_dm$All_Last_Date)
summary(marketing_dm)

summary(marketing_dm)

#Calculating the RFM of customers
rfm_stakes <- marketing_dm  %>% 
  filter(!is.na(All_Last_Date))

rfm_stakes <- rfm_table_customer_2(data = rfm_stakes, 
                                   customer_id = UserID, 
                                   n_transactions = All_Frequency, 
                                   latest_visit_date = All_Last_Date, 
                                   total_revenue = All_GGR, 
                                   analysis_date = end_date)

rfm <- as.data.frame(rfm_stakes$rfm)
rfm <- select(rfm, UserID=customer_id, rfm=rfm_score)

# Merge RFM
marketing_dm <- merge(x= marketing_dm, y= rfm, by= "UserID", all.x = TRUE)

# Converting table into a csv file 
write.table(marketing_dm,file="C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/marketing_df.csv",sep=",",append=FALSE,row.names = FALSE)
write.table(marketing_dm,file="C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/marketing_df.Rdata",sep=",",append=FALSE,row.names = FALSE)
###########################################################################################
#####################       Graphs for Shiny and Markdown             #####################
###########################################################################################

#	Total Customers per Gender
ggplot(marketing_dm, aes(x = Gender)) +
  geom_bar(fill="#69b3a2") +
  theme_light() +
  labs(x = "Gender", y = "Frequency",
       title ="Total Customers per Gender")

#	Total Customers per Region
# Since there are lots of regions, in order to have a better visualization, we are selecting the top10
#Top 10 Region
dm_region_ncust <- marketing_dm %>%
  group_by(Country_Name) %>%
  summarize(ncust = n())%>%
  slice_max(ncust,n= 10)

ggplot(dm_region_ncust, aes(x = Country_Name, y = ncust)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Country", y = "Total Customers",
       title ="Total Customers per Region Top 10")

#Product Popular
dm_prod_pop <- marketing_dm %>%
  summarize(Sports_fixed = sum(Frequency_Prod1, na.rm = TRUE),
            Sports_live = sum(Frequency_Prod2, na.rm = TRUE),
            Cassino_Boss = sum(Frequency_Prod4, na.rm = TRUE),
            Supertoto = sum(Frequency_Prod5, na.rm = TRUE),
            Games_VS = sum(Frequency_Prod6, na.rm = TRUE),
            Games_Bwin = sum(Frequency_Prod7, na.rm = TRUE),
            Cassino_Chartwell = sum(Frequency_Prod8, na.rm = TRUE),
            Poker = sum(Frequency_poker, na.rm = TRUE)
            )
dm_prod_pop <- pivot_longer(
  dm_prod_pop,
  cols = c(Sports_fixed, Sports_live, Cassino_Boss, Supertoto, Games_VS, Games_Bwin, Cassino_Chartwell, Poker),values_to = "Freq")

ggplot(dm_prod_pop, aes(x = name, y = Freq,fill=name)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Products", y = "Frequency",
       title ="Popularity per Product")

# Company's Profit per Product
dm_prod_profit <- marketing_dm %>%
  summarize(Sports_fixed = sum(GGR_Company_Prod1, na.rm = TRUE),
            Sports_live = sum(GGR_Company_Prod2, na.rm = TRUE),
            Cassino_Boss = sum(GGR_Company_Prod4, na.rm = TRUE),
            Supertoto = sum(GGR_Company_Prod5, na.rm = TRUE),
            Games_VS = sum(GGR_Company_Prod6, na.rm = TRUE),
            Games_Bwin = sum(GGR_Company_Prod7, na.rm = TRUE),
            Cassino_Chartwell = sum(GGR_Company_Prod8, na.rm = TRUE),
            Poker = sum(GGR_Company_poker, na.rm = TRUE))
dm_prod_profit <- pivot_longer(
  dm_prod_profit,
  cols = c(Sports_fixed, Sports_live, Cassino_Boss, Supertoto, Games_VS, Games_Bwin, Cassino_Chartwell, Poker),values_to = "GGR_product")

ggplot(dm_prod_profit, aes(x = name, y = GGR_product,fill=name)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Product", y = "GGR",
       title ="Company's Profit per Product")


# Total Gross Profit per Region (select Top10)
#Top 10 Region
dm_region_top10 <- marketing_dm %>%
  group_by(Country_Name) %>%
  summarize(All_GGR = sum(All_GGR))%>%
  slice_max(All_GGR,n= 10)
ggplot(dm_region_top10, aes(x = Country_Name, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Website", y = "GGR",
       title ="Top 10 Gross Gaming Revenue per Website")
#Bottom 10 Region
dm_region_bot10 <- marketing_dm %>%
  group_by(Country_Name) %>%
  summarize(All_GGR = sum(All_GGR))%>%
  slice_min(All_GGR,n= 10)
ggplot(dm_region_bot10, aes(x = Country_Name, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="red") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Country", y = "GGR",
       title ="Bottom 10 Gross Gaming Revenue per Region")
          
# Company's Profit per Language 
ggplot(marketing_dm, aes(x = Language_Description, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Product", y = "GGR",
       title ="Company's Profit per Language")     

# Popularity (Frequency) per AplicationID/Website
ggplot(marketing_dm, aes(x = Application_Description, y = All_Frequency)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="red") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Website", y = "Number of customers",
       title ="Popularity per Website")

#### GGR per AplicationID/Website
ggplot(marketing_dm, aes(x = Application_Description, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="orange") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Website", y = "GGR",
       title ="Gross Gaming Revenue per Website")


# Distribution Time First Purchase
ggplot(marketing_dm, aes(x = time_1Purchase)) +
  geom_histogram (binwidth = 20,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light()+
  labs(x = "LOR (Days)", y = "Time First Purchase",
       title ="Users' First Time Purchase")

# Distribution Frequency
ggplot(marketing_dm, aes(x = All_Frequency)) +
  geom_histogram (binwidth = 20,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light()+
  labs(x = "Total Days", y = "Customer",
       title ="Users' Total Days Playing")

# Distribution Bets
ggplot(marketing_dm, aes(x = All_Bets)) +
  geom_histogram (binwidth = 10000,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light() +
  labs(x = "Bets Days", y = "Customers",
       title ="Users' Total Bets")

# Frequency vs GGR's Company 
ggplot(marketing_dm, aes(x = All_GGR , y = All_Frequency)) +
  geom_point() +
  theme_light() +
  labs(x = "Frequency", y = "GGR",
       title ="GGR vs Days Played")

# Distribution Recency (Churners Rate)
ggplot(marketing_dm, aes(x = Recency)) +
  geom_histogram (binwidth = 20,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light() +
  labs(x = "Recency", y = "Customers",
       title ="Users' Recency")

# Distribution RFM
rfm_dist <- marketing_dm %>%
  group_by(rfm) %>%
  summarize(rfm_total = n())

ggplot(rfm_dist,aes(x = rfm,
                    y = rfm_total)) +
  geom_line() +
  theme_light() + 
  labs(title = "RFM Distribution", x = "RFM Score",y = "Customers")  

# Lifetimevalue (Total Spend)

x <- quantile(marketing_dm$All_Spend, probs = 0.9)
All_Spend_df <- marketing_dm %>%
  filter(All_Spend<3418)
ggplot(All_Spend_df, aes(x = All_Spend)) +
  geom_histogram (binwidth = 50,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light() +
  labs(x = "Total_Spend", y = "Customer Size",
       title ="Users LTV")

# Customer addicted vs not addicted 
ggplot(marketing_dm, aes(x = Stickiness, y = Stickiness)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  theme_light()+
  labs(x = "Stickiness", y = "Total Customers",
       title ="Stickiness(Addiction)") 

# Distribution LOR
ggplot(marketing_dm, aes(x = LOR)) +
  geom_density(fill="mediumpurple", color="#e9ecef", alpha=0.8) +
  theme_light()+
  theme(axis.text.y = element_blank())+
  labs(x = "LOR (Days)", y = "Customer size",
       title ="Users' LOR")

###########################################################################################
#                                 Important Metrics Description                           #
###########################################################################################
#1.Gross Gaming Revenue (GGR) = Total bet amount-total winnings amount

#2.Lifetime value(LTV) = Number of Days of Engagement * Average Spend Per Day
#number of days of engagement = no of times he played (frequency)
#average spend per day(Avg stakes )

#3.Stickiness(Addiction level) = based on the no. of times the user played(Total Frequency)

#4.RFM-Recency Frequency and Monetary Value