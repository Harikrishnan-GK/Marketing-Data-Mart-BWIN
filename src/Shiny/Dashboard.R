#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages(c("shiny", "dplyr", "htmlwidgets", "digest", "bit","devtools"))
#devtools::install_github("hadley/shinySignals")
#install.packages("shinyWidgets")

###########################################################################################
#                                       Team-Group2                                      #
###########################################################################################


###### Harikrishnan GOPALAKRISHNAN ####
###### Tatiane DUTRABUNO  ##########
###### Charlotte GALLET  ##########


###########################################################################################
#                                      General Note                                       #
###########################################################################################

#In case if it displays with error, try Running the entire Projectscript.R once and run the Shiny app.

###########################################################################################
#                                       Libraries                                         #
###########################################################################################
library(shinyWidgets)
library(shiny)
library(shinySignals)
library(shinydashboard)
library('jsonlite')



###########################################################################################
#                                  Importing Datasets                                     #
###########################################################################################
marketing_dm <- read.csv("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Business Analytics Tools-Open Source Introduction/Group assignment folder-20211211/marketing_df.csv")
 
###########################################################################################
#                        Defining the variables for UI                                    #
###########################################################################################
c <- unique(marketing_dm$Country_Name)
country_sorted <- sort(c)
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
colnames(dm_prod_pop)
prod_name <- dm_prod_pop['name']
#Application
App <- unique(marketing_dm$Application_Description)
App <- sort(App)
#LOR
LOR_uni <- unique(marketing_dm$LOR)
LOR_uni <- sort(LOR_uni)

###########################################################################################
#                        User Interface                                                   #
###########################################################################################
ui <- dashboardPage(
    
    dashboardHeader(title = "BWIN"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(" Customer Overview ", tabName = "Cust_info"),
            menuItem("Products Profile", tabName = "Product_Info"),
            menuItem('Revenue',tabName="Revenue_Info"),
            menuItem('Metrics and Distributions',tabName="Metrics")
        )
    ),
    
    
    dashboardBody(
        tabItems(


            
            
            
            tabItem("Cust_info",   
            
            
                    fluidRow(
                        box(width = 4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Customers per Gender",
                            radioGroupButtons(
                                inputId = "Gen",
                                label = "Gender",
                                status = "primary",
                                choices = c("Female", "Male")),
                            plotOutput("Genplot")),
                        
                        box(width=4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Popularity per Website",
                            selectInput(
                                inputId = "POPW",
                                label = "Website",
                                choices = App),
                            plotOutput("POPWplot")
                            ),
                        
                        box(width=4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Customers per Region",
                            selectInput(
                                inputId = "Country",
                                label = "Country Name",
                                choices = country_sorted),
                            plotOutput("Conplot")
                        ),
                        
                    ),
                 
                    
                
                    
            ),
            tabItem("Product_Info",   
                    
                    
                    fluidRow(
                        box(width = 6,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Product Popularity",
                            pickerInput(
                                inputId = "Prod_pop",
                                label = "Select the Products",
                                options = list(
                                    `actions-box` = TRUE), 
                                multiple = TRUE,
                                selected = "Sports_live",
                                choices = prod_name),
                            plotOutput("Prodpopplot")
                            ),
                        
                        box(width = 6,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Profit per Product",
                            pickerInput(
                                inputId = "Prod_prof",
                                label = "Select the Products",
                                options = list(
                                    `actions-box` = TRUE), 
                                multiple = TRUE,
                                selected = "Sports_fixed",
                                choices = prod_name),
                            plotOutput("Prodprofplot")
                        ),
                       
                        
                    )
                    
                    
            ),
            tabItem("Revenue_Info",   
                      
                    
                    
                    fluidRow(
                        box(width = 4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Gross Gaming Revenue per Website",
                            pickerInput(
                                inputId = "GGRW",
                                label = "Select the website",
                                multiple = TRUE,
                                selected = "BALLS OF FIRE",
                                choices = App),
                                plotOutput("GGRWplot")),
                        
                        box(width = 8,
                                status = "info",
                                solidHeader = TRUE,
                                title = "Regions with High and Low Gross Gaming revenue",
                                radioGroupButtons(
                                    inputId = "MinMax",
                                    label = "GGR",
                                    status = "primary",
                                    choices = c("Best", "Worst")),
                                plotOutput("MinMaxplot")
                        ),
                            
                     
                        
                    )
                      
                      
                      
            ),
            tabItem("Metrics",   
                    
                    
                    fluidRow(
                        box(width = 4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "LOR distribution Over Customers and Their First time Purchase",
                            materialSwitch(
                                inputId = "LORC",
                                label = "LOR vs Customers / LOR vs First Time Purchase"
                            ),
                           
                            plotOutput("LORCplot")
                        ),
                        
                        box(width = 8,
                            status = "info",
                            solidHeader = TRUE,
                            title = "User's Total bets and Total Days Played",
                            materialSwitch(
                                inputId = "UserT",
                                label = "Total Bets/Total Days Played"
                            ),
                            plotOutput("Totalbetplot")
                        ),
                     
                    ),  
                    fluidRow(
                        box(width = 8,
                            status = "info",
                            solidHeader = TRUE,
                            title = "RFM distribution and Recency Distribution",
                            materialSwitch(
                                inputId = "RFM",
                                label = "Recency/RFM distribution"
                            ),
                            
                            plotOutput("RFMplot")
                        ),
                        
                        box(width = 4,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Addicted Users(Frequent Gamblers) and Life Time Value Users(High amount spending Gamblers)",
                            materialSwitch(
                                inputId = "LTV",
                                label = "Addicted users/Life Time Value Users"
                            ),
                            plotOutput("LTVplot")
                        )
                    
                    )
                    
            )
            
            
            
            
            
        )
    )
    
)   





###########################################################################################
#                                  Server                                                 #
###########################################################################################

server <- function(input, output) {

    
    output$Genplot<-renderPlot({
        dm_gen<- marketing_dm %>%
            group_by(Gender) %>%
            summarize(ngen = n())
        
        ggplot(dm_gen %>% filter(Gender == input$Gen) , aes(x = Gender,y = ngen)) +
            geom_col(fill="#69b3a2") +
            theme_light() +
           geom_text(aes(label=ngen), vjust=1.6, color="white", size=6) + 
            labs(x = "Gender", y = "Customers count")
    })
    output$Betplot<-renderPlot({
        ggplot(marketing_dm %>% filter(Gender == input$Bets), aes(x = All_Bets)) +
            geom_histogram (binwidth = 10000,
                            center = 0.05,
                            fill="#69b3a2") +
            theme_light() +
            labs(x = "Bets Days", y = "Frequency",
                 title ="Users' Total Bets")
        
    })
    output$POPWplot<-renderPlot({
        dm_web_nfreq <- marketing_dm %>%
            group_by(Application_Description) %>%
            summarize(nfreq = n())
        
        ggplot(dm_web_nfreq %>% filter( Application_Description == input$POPW), aes(x = Application_Description, y = nfreq)) +
            geom_bar(stat = "identity", na.rm = TRUE, fill="red") +
            theme_light()+
            geom_text(aes(label=nfreq ), vjust=1.6, color="white", size=6)+
            labs(x = "Website", y = "Number of customers")
        
        
    })  
    output$Conplot<-renderPlot({
        dm_region_ncust <- marketing_dm %>%
            group_by(Country_Name) %>%
            summarize(ncust = n())
        
        ggplot(dm_region_ncust %>% filter( Country_Name == input$Country) , aes(x = Country_Name, y = ncust)) +
            geom_bar(stat = "identity", na.rm = TRUE, fill="steelblue") +
            theme_light()+
            geom_text(aes(label=ncust ), vjust=1.6, color="white", size=6)+
            labs(x = "Country", y = "Total Customers")
    })
    output$Prodpopplot<-renderPlot({
    ggplot(dm_prod_pop  %>% filter( prod_name == input$Prod_pop) , aes(x = name, y = Freq,fill=name)) +
        geom_bar(stat = "identity", na.rm = TRUE) +
        theme_light()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(x = "Products", y = "Frequency",
             title ="Popularity per Product")
    })
    output$Prodprofplot<-renderPlot({
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
        
        ggplot(dm_prod_profit%>% filter( prod_name == input$Prod_prof), aes(x = name, y = GGR_product,fill=name)) +
            geom_bar(stat = "identity", na.rm = TRUE) +
            theme_light()+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            labs(x = "Product", y = "GGR",
                 title ="Company's Profit per Product")
    })
    output$GGRWplot<-renderPlot({
    # GGR per AplicationID/Website
    ggplot(marketing_dm %>% filter( Application_Description == input$GGRW), aes(x = Application_Description, y = All_GGR)) +
        geom_bar(stat = "identity", na.rm = TRUE, fill="orange") +
        coord_flip() +
        theme_light()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(x = "Website", y = "GGR",
             title ="Gross Gaming Revenue per Website")
    })
    output$MinMaxplot<-renderPlot({
        
        if (input$MinMax=="Best"){
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
             title ="Regions with High Gross Gaming Revenue(GGR)")
        } else{
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
             title ="Regions With Low Gross Gaming Revenue(GGR)")
        }
    })
    output$LORCplot <- renderPlot({
        if (input$LORC== TRUE){
    ggplot(marketing_dm  , aes(x = LOR)) +
        geom_density(fill="mediumpurple", color="#e9ecef", alpha=0.8) +
        theme_light()+
        theme(axis.text.y = element_blank())+
        labs(x = "LOR (Days)", y = "Customer size",
             title ="Users' LOR")
        } else {
            ggplot(marketing_dm, aes(x = time_1Purchase)) +
                geom_histogram (binwidth = 20,
                                center = 0.05,
                                fill="mediumpurple") +
                theme_light()+
                labs(x = "LOR (Days)", y = "Time First Purchase",
                     title ="Users' First Time Purchase")
        }
        
    })
    output$Totalbetplot <- renderPlot({
        if (input$UserT== TRUE){
            ggplot(marketing_dm, aes(x = All_Bets)) +
                geom_histogram (binwidth = 10000,
                                center = 0.05,
                                fill="#69b3a2") +
                theme_light() +
                labs(x = "Bets", y = "Customers",
                     title ="Users' Total Bets")
        } else {
            
            ggplot(marketing_dm, aes(x = All_Frequency)) +
                geom_histogram (binwidth = 20,
                                center = 0.05,
                                fill="#69b3a2") +
                theme_light()+
                labs(x = "Total Days", y = "Customer",
                     title ="Users' Total Days Playing")
        }
        
    })
    output$RFMplot <- renderPlot({
        if (input$RFM== TRUE){
            rfm_dist <- marketing_dm %>%
                group_by(rfm) %>%
                summarize(rfm_total = n())
            
            ggplot(rfm_dist,aes(x = rfm,
                                y = rfm_total)) +
                geom_line() +
                theme_light() + 
                labs(title = "RFM Distribution", x = "RFM Score",y = "Customers")
        } else {
            
            ggplot(marketing_dm, aes(x = Recency)) +
                geom_histogram (binwidth = 20,
                                center = 0.05,
                                fill="grey30") +
                theme_light() +
                labs(x = "Recency", y = "Customers",
                     title ="Users' Recency")
        }
        
    })
    output$LTVplot <- renderPlot({
        if (input$LTV== TRUE){
            x <- quantile(marketing_dm$All_Spend, probs = 0.9)
            All_Spend_df <- marketing_dm %>%
                filter(All_Spend<3418)
            ggplot(All_Spend_df, aes(x = All_Spend)) +
                geom_histogram (binwidth = 50,
                                center = 0.05,
                                fill="darkgoldenrod2") +
                theme_light() +
                labs(x = "Total_Spend", y = "Customer Size",
                     title ="Users LTV")
        } else {
            
            # Customer addicted vs not addicted 
            ggplot(marketing_dm, aes(x = Stickiness, y = Stickiness)) +
                geom_bar(stat = "identity", na.rm = TRUE, fill="darkgoldenrod2") +
                theme_light()+
                labs(x = "Stickiness", y = "Total Customers",
                     title ="Stickiness(Addiction)") 
        }
        
    })
    
    
    
}

###########################################################################################
#                                  Run the Application                                    #
###########################################################################################
shinyApp(ui = ui, server = server)


