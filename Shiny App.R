######################################################
#     Step 1: Reading in datamarts                   #
######################################################
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

datamart_user <- read.csv("datamart_user.csv")
datamart_country <- read.csv("datamart_country.csv")
datamart_product <- read.csv("datamart_product.csv")
datamart_month <- read.csv("datamart_month.csv")
datamart_transtype <- read.csv("datamart_transtype.csv")

#Downloading longitude and latitude data
world <- ne_countries(scale = "medium", returnclass = "sf")[c("continent","name_sort","geometry")]
colnames(world) <- c("Continent","Country","geometry")

#Adding longitude and latitude data to datamart
datamart_country <- left_join(world,datamart_country,by='Country')

#Changing value to Africa because only Mauritius is populated
datamart_country[datamart_country$Continent=="Seven seas (open ocean)",]$Continent <- "Africa"

#Creating variable to use as Shiny filter,
#excluding continents with no users
choices <- levels(as.factor(datamart_country[datamart_country$Continent!="Antarctica",]$Continent))

######################################################
#     Step 2: creating Shiny App                     #
######################################################
ui <- shinyUI(navbarPage("Online Gambling",
                         tabPanel("Country",
                                  sidebarPanel(textInput("title", "Title", "Revenue Margin by Country"),
                                               selectInput("continents", "Continents",
                                                           #Excluding choices with no users
                                                           choices = c("All",choices),
                                                           multiple = TRUE,
                                                           selected = "All")),
                                  mainPanel(plotlyOutput("plot"), tableOutput("wintable"))
                         ),
                         tabPanel("User Info",
                                  sidebarPanel(selectInput("user_cont", "Continents",
                                                           choices = c("All",choices),
                                                           multiple = TRUE,
                                                           selected = "All"),
                                               sliderInput(inputId = "age", label = "Age Range",
                                                           min = 20, max = 52,
                                                           value = c(20, 40)),
                                               tableOutput("usersummary")),
                                  mainPanel(plotlyOutput("hist"),plotlyOutput("plotuser"))),
                         tabPanel("Product",
                                  sidebarPanel(
                                    textInput("title2", "Title", "Metrics by Product"),
                                    selectInput("product","Product",
                                                choices =c("All", levels(factor(datamart_product$product))),
                                                multiple = TRUE,
                                                selected = "All"),
                                    selectInput("ptype","Product Type",
                                                choices =c("All", levels(factor(datamart_product$ptype))),
                                                multiple = TRUE,
                                                selected = "All")),
                                  mainPanel(plotlyOutput("plot2"), plotlyOutput("plot3"))),
                         tabPanel("Seasonality",
                                  sidebarPanel(
                                    textInput("title3", "Title", "Metrics by Sesonality"),
                                    selectInput("product2","Product",
                                                choices =c("All", levels(factor(datamart_product$product))),
                                                multiple = TRUE,
                                                selected = "All"),
                                    selectInput("ptype2","Product Type",
                                                choices =c("All", levels(factor(datamart_product$ptype))),
                                                multiple = TRUE,
                                                selected = "All")),
                                  mainPanel(plotlyOutput("plot4"), plotlyOutput("plot5"))),
                         tabPanel("Transaction Type",
                                  sidebarPanel(
                                    textInput("title4", "Title", "Transaction Type Metrics"),
                                    selectInput("metric","Select Metric for Transactions",
                                                choices =c("Amount","Number of Transactions","Number of Users"),
                                                multiple = FALSE,
                                                selected = "Amount"),
                                    selectInput("tmonth","Select Month for Metrics",
                                                choices =c("All",levels(factor(datamart_transtype$month))),
                                                multiple = FALSE,
                                                selected = "All")),
                                  mainPanel(plotlyOutput("plot6")))))

# Defining server logic
server <- function(input, output) {   
  filtered_data <- reactive({
    data <- datamart_country
    #Filtering data by continent selection
    if (input$continents != "All") {
      data[!data$Continent %in% input$continents,]$RevenueMargin <- NA
    }
    data })
  
  output$plot <- renderPlotly({ggplotly({      
    p <- ggplot(data = filtered_data(), aes(geometry = geometry, text = paste("Country:", Country))) +
      geom_sf(aes(fill = RevenueMargin)) +
      scale_fill_viridis_c(option = "plasma") +
      ggtitle(input$title) 
    
    p}) }) 
  
  #stakes-winnings = profit
  react_table <- reactive({
    ifelse(input$continents=="All",
           tablecont <- datamart_country[,c("Continent","Country","Users","Wins","Stakes","RevenueMargin")],
           tablecont <- datamart_country[datamart_country$Continent==input$continents,c("Continent","Country","Users","Age","Wins","Stakes","RevenueMargin")])
    tablecont$geometry <- NULL
    tablecont <- tablecont[!is.na(tablecont$Users),]
    tablecont$Avg_Profit <- (tablecont$Stakes-tablecont$Wins)/tablecont$Users
    tablecont$Stakes <- tablecont$Stakes/tablecont$Users
    tablecont$Wins <- tablecont$Wins/tablecont$Users
    tablecont <- tablecont[order(-tablecont$Avg_Profit),][1:10,]
    colnames(tablecont)[4:length(colnames(tablecont))] <- c("AvgWins","AvgStakes","RevenueMargin","AvgProfit")
    tablecont})
  
  output$wintable <- renderTable(react_table(), caption="Top 10 Countries by Average Profit",
                                 caption.placement = getOption("xtable.caption.placement", "top"))
  
  filtered_hist <- reactive({
    data_fem <- datamart_user
    #Filtering data by continent selection
    if (input$user_cont != "All") {
      data_fem <- data_fem[!data_fem$Continent %in% input$user_cont,c("Gender","AGE")]
    }
    data_fem <- data_fem[!(is.na(data_fem$AGE)|is.na(data_fem$Gender)),c("Gender","AGE")] 
    colnames(data_fem)[2] <- "Age"
    data_fem <- subset(data_fem, Age >= input$age[1] & Age <= input$age[2])
    data_fem})
  
  output$hist <- renderPlotly({
    hist <- ggplot(filtered_hist(), aes(Age, fill = Gender)) + 
      geom_density(alpha = 0.2) +
      ggtitle("Gender density chart")
    
    
    hist})
  
  plotuser_data <- reactive({lang_data <- datamart_user 
  lang_data <- lang_data[!is.na(lang_data$RevSegment),]
  lang_data <- subset(lang_data, AGE >= input$age[1] & AGE <= input$age[2])
  if(input$user_cont!="All")
  {lang_data <- subset(lang_data,Continent==input$user_cont)}
  lang_data$RevSegment <- as.factor(lang_data$RevSegment)
  lang_data
  })
  
  output$plotuser <- renderPlotly({ggplot(plotuser_data(), aes(x = avg_stks, y = avg_wins, size=avg_bets)) +
      geom_point(alpha = 0.4, position = "jitter", aes(col = RevSegment)) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
      xlab("Average Stakes") + ylab("Average Wins") + 
      labs(title="Avg Stakes by Avg Wins<br> Coloured by Revenue Segment with size reflecting Avg Bets",
           caption="Coloured by Revenue Segment with size reflecting Avg Bets",
           subtitle="Coloured by Revenue Segment with size reflecting Avg Bets",
           colour="Revenue Segment", size="") })
  
  summary_table <- reactive({
    lang_data <- datamart_user 
    lang_data <- lang_data[!is.na(lang_data$RevSegment),]
    lang_data <- subset(lang_data, AGE >= input$age[1] & AGE <= input$age[2])
    if(input$user_cont!="All")
    {lang_data <- subset(lang_data,Continent==input$user_cont)}
    lang_data %>% group_by(RevSegment) %>% summarise(Users = n(),
                                                     Average_Age = mean(AGE),
                                                     Freq = mean(freq)
    )
  })
  
  output$usersummary <- renderTable({summary_table()}, spacing="s"
                                    , caption="User Summary", caption.placement = getOption("xtable.caption.placement", "top"))
  
  filtered_product_data <- reactive({
    data2 <- datamart_product
    #Filtering data by product selection
    if (input$product != "All") {
      data2 <- data2[data2$product %in% input$product,]
    }
    #Filtering data by product type selection
    if (input$ptype != "All") {
      data2 <- data2[data2$ptype %in% input$ptype,]
    }
    data2 })
  
  output$plot2 <- renderPlotly({ggplotly({  
    p2 <- ggplot(data = filtered_product_data(), aes(product, winvsstkratio)) +
      geom_bar(stat = "identity", position = "stack") + 
      ggtitle(input$title2) +
      scale_size_area() + 
      xlab("Products") +
      ylab("Win vs Stake ratio") +
      theme(axis.text.x=element_text(size=5))
    
    p2}) }) 
  
  output$plot3 <- renderPlotly({ggplotly({  
    p3 <- plot_ly(data = filtered_product_data(), x = ~product, y = ~count_of_users, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgb(255,69,0)'),
                  showlegend = TRUE, name = 'Users') %>%
      add_trace(y = ~count_of_transactions, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(58, 83, 155, 1)'),
                showlegend = TRUE, name = 'Transactions') %>%
      layout(title = "Users and Transactions by Product",
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "Products",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          tickfont = list(size=5),
                          zeroline = FALSE),
             yaxis = list(title = "Count",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
    
    
    p3}) }) 
  
  filtered_season_data <- reactive({
    data3 <- datamart_month
    #Filtering data by product selection
    if (input$product2 != "All") {
      data3 <- datamart_month
      data3[!data3$product %in% input$product2,] <- NA
    }
    #Filtering data by product type selection
    if (input$ptype2 != "All") {
      data3 <- data3[data3$ptype %in% input$ptype2,]
    }
    data3 })
  
  output$plot4 <- renderPlotly({ggplotly({ 
    p4 <- plot_ly(data = filtered_season_data(), x = ~month, y = ~avgstks
                  , name = 'Average stakes', type = 'scatter', mode = 'none'
                  , stackgroup = 'one', fillcolor = '#F5FF8D') %>%
      add_trace(y = ~avgwins, name = 'Average wins', fillcolor = '#4C74C9') %>%
      layout(title = 'Gambling Seasonality',
             xaxis = list(title = "Month",
                          showgrid = FALSE),
             yaxis = list(title = "Amount",
                          showgrid = FALSE))
    
    p4}) }) 
  
  
  output$plot5 <- renderPlotly({ggplotly({ 
    p5 <- plot_ly(data = filtered_season_data(), x = ~month, y = ~avgbets, name = 'Average bets', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#F5FF8D') %>%
      layout(title = 'Betting Seasonality',
             xaxis = list(title = "Month",
                          showgrid = FALSE),
             yaxis = list(title = "Average number of bets",
                          showgrid = FALSE))
    p5}) }) 
  
  buy_tdata <- reactive({
    #Filtering data by metric input
    buy_temp <- datamart_transtype#[datamart_transtype$TransType=="Buy",]
    ifelse(input$metric=="Amount",
           data4b <- buy_temp[,c("TransType","date","month","MeanAmount")],
           ifelse(input$metric=="Number of Transactions",
                  data4b <- buy_temp[,c("TransType","date","month","NTrans")],
                  data4b <- buy_temp[,c("TransType","date","month","NUsers")]))
    #Filtering data by month selection
    ifelse(input$tmonth=="All",data4b <- data4b, 
           data4b <- data4b[data4b$month %in% input$tmonth,])
    colnames(data4b)[4] <- "y"
    data4b })
  
  output$plot6 <- renderPlotly({ggplotly({  
    p6 <- {plot_ly(data = buy_tdata(), x = ~date, y = ~y, color=~TransType, type = 'scatter', mode = 'lines',
                   showlegend = TRUE, name = ~TransType)%>%
        layout(title = "Buy Transactions",
               xaxis = list(title = ""),
               yaxis = list(title = input$metric))}
    p6}) })
  
}

shinyApp(ui = ui, server = server)
