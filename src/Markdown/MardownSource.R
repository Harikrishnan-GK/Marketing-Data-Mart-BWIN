###########################
# Source for the Markdown #
###########################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyr)
library(rfm)
library(readr)
library(readxl)
#library(plyr)

marketing_dm <- read.csv("C:/Users/cgallet/OneDrive - IESEG/MBD/Business Analytics Tools - Open Source/Group Assignment/marketing_df.csv")

###########################################################################################
#####################                      Graphs                     #####################
###########################################################################################

#	Total Customers per Gender
ggplot(marketing_dm, aes(x = Gender)) +
  geom_bar(fill="#69b3a2") +
  theme_light() +
  labs(x = "Gender", y = "Number of Customers",
       title ="Total Customers per Gender")

#	Total Customers per Region
# Since there are lots of regions, in order to have a better vizualization, we are selecting the top10
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
  cols = c(Sports_fixed, Sports_live, Cassino_Boss, Supertoto, Games_VS, Games_Bwin, Cassino_Chartwell, Poker))

ggplot(dm_prod_pop, aes(x = name, y = value)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Product", y = "Frequency",
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
  cols = c(Sports_fixed, Sports_live, Cassino_Boss, Supertoto, Games_VS, Games_Bwin, Cassino_Chartwell, Poker))

ggplot(dm_prod_profit, aes(x = name, y = value)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
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
  labs(x = "Country", y = "GGR",
       title ="Top 10 Gross Gaming Revenue per Region")
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
       title ="Top 10 Gross Gaming Revenue per Region")

# Company's Profit per Language 
ggplot(marketing_dm, aes(x = Language_Description, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Product", y = "GGR",
       title ="Company's Profit per Language")     

# Revenue per AplicationID/Website
ggplot(marketing_dm, aes(x = Application_Description, y = All_GGR)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="red") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Website", y = "GGR",
       title ="Gross Gaming Revenue per Website")

# Popularity (Frequency) per AplicationID/Website
ggplot(marketing_dm, aes(x = Application_Description, y = All_Frequency)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="red") +
  coord_flip() +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Website", y = "Number of customers",
       title ="Popularity per Website")

# Distribution LOR
ggplot(marketing_dm, aes(x = LOR)) +
  geom_density(fill="mediumpurple", color="#e9ecef", alpha=0.8) +
  theme_light()+
  theme(axis.text.y = element_blank())+
  labs(x = "LOR (Days)", y = "Customer size",
       title ="Users' LOR")

# Distribution Time First Purchase
ggplot(marketing_dm, aes(x = time_1Purchase)) +
  geom_histogram (binwidth = 20,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light()+
  labs(x = "LOR (Days)", y = "Time First Purchase",
       title ="Users' Time First Purchase")

# Distribution Frequency
ggplot(marketing_dm, aes(x = All_Frequency)) +
  geom_histogram (binwidth = 20,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light()+
  labs(x = "Total Days", y = "Number of customers",
       title ="Users' Total Days Playing")

# Distribution Bets
ggplot(marketing_dm, aes(x = All_Bets)) +
  geom_histogram (binwidth = 10000,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light() +
  labs(x = "Bets Days", y = "Number of Customers",
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
  labs(x = "Recency", y = "Number of Customers",
       title ="Users' Recency")

# Distribution RFM
rfm_dist <- marketing_dm %>%
  group_by(rfm) %>%
  summarize(rfm_total = n())

ggplot(rfm_dist,aes(x = rfm,
                    y = rfm_total)) +
  geom_line() +
  theme_light() + 
  labs(title = "RFM Distribution", x = "RFM Score",y = "Frequency")  

# LTV (Total Spend)

x <- quantile(marketing_dm$All_Spend, probs = 0.9)
x
All_Spend_df <- marketing_dm %>%
  filter(All_Spend<3418)

ggplot(All_Spend_df, aes(x = All_Spend)) +
  geom_histogram (binwidth = 50,
                  center = 0.05,
                  fill="#69b3a2") +
  theme_light() +
  labs(x = "Total_Spend", y = "Customer Size",
       title ="Users LTV")

# Custumer addicted vs not addicted 
ggplot(marketing_dm, aes(x = Stickiness, y = Stickiness)) +
  geom_bar(stat = "identity", na.rm = TRUE, fill="#69b3a2") +
  theme_light()+
  labs(x = "Stickiness", y = "Total Customers",
       title ="Stickiness")   
