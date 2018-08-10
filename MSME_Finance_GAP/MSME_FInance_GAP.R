## make sure that all packages are installed and loaded correctly

#install.packages("RSocrata")
#install.packages('countrycode')

library(shiny)
library(shinydashboard)
library(RSocrata)
library(countrycode)
library(stringr)
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(qdap)
library(ggplot2)
library(dplyr)
library(data.table)
library(plotly)
library(DT)


## implementing some useful functions
read_api <- function(endpoint){
  df <- read.socrata(
    endpoint,
    app_token = "MVfrrdaW6AT5gzMeP66C6lkL6",
    email     = "davidartsakh@mail.ru",
    password  = "David666!")
  
  return(df)
}

con_types <- function(df){
  df[] <- lapply(df, function(x) type.convert(as.character(x), as.is = TRUE))
  
  df[] <- lapply(df, type.convert, as.is = TRUE)
  
  return(df)
}


change_format <- function(df, idvar, var_name = 'indicator') {
  melt(setDT(df), id.vars = idvar, 
       variable.name = var_name)
  
}

## getting the list of countries
smse_countries <- data.frame(
  read_api("https://finances.worldbank.org/resource/xpm6-g5vt.json")) %>%
  select(country)

## adding a no default value for selectors
smse_countries_no_default <- rbind(smse_countries, '--select--')

## preparations for future use
var_name <- c( 'Current Volume',
             'Current Volume GDP',
             'Female fully constrained 1',
             'Female fully constrained 2',
             'Female partly constrained 1',
             'Female partly constrained 2',
             'Female sme option 1',
             'Female sme option 2',
             'Female unconstrained 1',
             'Female unconstrained 2',
             'Finance gap current volume',
             'Finance gap GDP',
             'GDP 2015',
             'Informal 2007',
             'Male fully constrained 1',
             'Male fully constrained 2',
             'Male partly constrained 1',
             'Male partly constrained 2',
             'Male unconstrained 1',
             'Male unconstrained 2',
             'Micro female option 1',
             'Micro female option 2',
             'Micro finance gap',
             'Micro partly constrained 1',
             'Micro partly constrained 2',
             'Micro potential demand',
             'Micro unconstrained 1',
             'Micro unconstrained 2',
             'Number of micro',
             'Number of micro men option 1',
             'Number of micro men option 2',
             'Number of micro women option 1',
             'Number of micro women option 2',
             'Number of msmes',
             'Number of SMEs',
             'Number of SMEs men option 1',
             'Number of SMEs men option 2',
             'Number of SMEs women option 1',
             'Number of SMEs women option 2',
             'Potential demand',
             'Potential demand GDP',
             'SME current volume',
             'SME finance gap',
             'SME fully constrained 1',
             'SME fully constrained 2',
             'SME men gap',
             'SME partly constrained 1',
             'SME potential demand',
             'SME unconstrained 1',
             'SME unconstrained 2',
             'SME women gap',
             'Informal as of formal',
             'Informal potential demand',
             'Micro current volume',
             'Micro women gap'
)

df_var <- c('current_volume',
            'current_volume_gdp',
            'female_fully_constrained_1',
            'female_fully_constrained_2',
            'female_partly_constrained_1',
            'female_partly_constrained_2',
            'female_sme_option_1',
            'female_sme_option_2',
            'female_unconstrained_1',
            'female_unconstrained_2',
            'finance_gap_current_volume',
            'finance_gap_gdp',
            'gdp_2015',
            'informal2007',
            'male_fully_constrained_1',
            'male_fully_constrained_2',
            'male_partly_constrained_1',
            'male_partly_constrained_2',
            'male_unconstrained_1',
            'male_unconstrained_2',
            'micro_female_option_1',
            'micro_female_option_2',
            'micro_finance_gap',
            'micro_partly_constrained_1',
            'micro_partly_constrained_2',
            'micro_potential_demand',
            'micro_unconstrained_1',
            'micro_unconstrained_2',
            'number_of_micro',
            'number_of_micro_men_option_1',
            'number_of_micro_men_option_2',
            'number_of_micro_women_option_1',
            'number_of_micro_women_option_2',
            'number_of_msmes',
            'number_of_smes',
            'number_of_smes_men_option_1',
            'number_of_smes_men_option_2',
            'number_of_smes_women_option_1',
            'number_of_smes_women_option_2',
            'potential_demand',
            'potential_demand_gdp',
            'sme_current_volume',
            'sme_finance_gap',
            'sme_fully_constrained_1',
            'sme_fully_constrained_2',
            'sme_men_gap',
            'sme_partly_constrained_1',
            'sme_potential_demand',
            'sme_unconstrained_1',
            'sme_unconstrained_2',
            'sme_women_gap',
            'informal_as_of_formal',
            'informal_potential_demand',
            'micro_current_volume',
            'micro_women_gap'
)

mapping_choices <- cbind(var_name, df_var)
mapping_choices <- data.frame(mapping_choices)

## selector choices renaming to look better in UI
smse_choices <- c('Current Volume'='current_volume',
                  'Current Volume GDP'='current_volume_gdp',
                  'Female fully constrained 1'='female_fully_constrained_1',
                  'Female fully constrained 2'='female_fully_constrained_2',
                  'Female partly constrained 1'='female_partly_constrained_1',
                  'Female partly constrained 2'='female_partly_constrained_2',
                  'Female sme option 1'='female_sme_option_1',
                  'Female sme option 2'='female_sme_option_2',
                  'Female unconstrained 1'='female_unconstrained_1',
                  'Female unconstrained 2'='female_unconstrained_2',
                  'Finance Gap'='finance_gap',
                  'Finance gap current volume'='finance_gap_current_volume',
                  'Finance gap GDP'='finance_gap_gdp',
                  'GDP 2015'='gdp_2015',
                  'Informal 2007'='informal2007',
                  'Male fully constrained 1'='male_fully_constrained_1',
                  'Male fully constrained 2'='male_fully_constrained_2',
                  'Male partly constrained 1'='male_partly_constrained_1',
                  'Male partly constrained 2'='male_partly_constrained_2',
                  'Male unconstrained 1'='male_unconstrained_1',
                  'Male unconstrained 2'='male_unconstrained_2',
                  'Micro female option 1'='micro_female_option_1',
                  'Micro female option 2'='micro_female_option_2',
                  'Micro finance gap'='micro_finance_gap',
                  'Micro partly constrained 1'='micro_partly_constrained_1',
                  'Micro partly constrained 2'='micro_partly_constrained_2',
                  'Micro potential demand'='micro_potential_demand',
                  'Micro unconstrained 1'='micro_unconstrained_1',
                  'Micro unconstrained 2'='micro_unconstrained_2',
                  'Number of micro'='number_of_micro',
                  'Number of micro men option 1'='number_of_micro_men_option_1',
                  'Number of micro men option 2'='number_of_micro_men_option_2',
                  'Number of micro women option 1'='number_of_micro_women_option_1',
                  'Number of micro women option 2'='number_of_micro_women_option_2',
                  'Number of MSMEs'='number_of_msmes',
                  'Number of SMEs'='number_of_smes',
                  'Number of SMEs men option 1'='number_of_smes_men_option_1',
                  'Number of SMEs men option 2'='number_of_smes_men_option_2',
                  'Number of SMEs women option 1'='number_of_smes_women_option_1',
                  'Number of SMEs women option 2'='number_of_smes_women_option_2',
                  'Potential demand'='potential_demand',
                  'Potential demand gdp'='potential_demand_gdp',
                  'SME current volume'='sme_current_volume',
                  'SME finance gap'='sme_finance_gap',
                  'SME fully constrained 1'='sme_fully_constrained_1',
                  'SME fully constrained 2'='sme_fully_constrained_2',
                  'SME men gap'='sme_men_gap',
                  'SME partly constrained 1'='sme_partly_constrained_1',
                  'SME potential demand'='sme_potential_demand',
                  'SME unconstrained 1'='sme_unconstrained_1',
                  'SME unconstrained 2'='sme_unconstrained_2',
                  'SME women gap'='sme_women_gap',
                  'Informal as of formal'='informal_as_of_formal',
                  'Informal potential demand'='informal_potential_demand',
                  'Micro current volume'='micro_current_volume',
                  'Micro women gap'='micro_women_gap'
)

## scrapping function
source_catcher <- function(sources,index,url,text_element,Year){
  txt <- read_html(url) %>%
    html_nodes(text_element) %>%
    html_text() %>%
    paste(collapse = " ")
  eval.parent(substitute(sources[index,1]<-txt))
  eval.parent(substitute(sources[index,2]<-Year)) 
  
}


## dashboard header
header <- dashboardHeader(title = 'Economic Analysis',
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "Welcome to Our Project!",
                                         icon = icon("info-circle"),
                                         status = "success"
                                       )
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
    menuItem("Explore Raw Data", tabName = "data", icon = icon("calendar", lib = 'glyphicon')),
    menuItem("Plots", tabName = "plots", icon = icon("bar-chart"), badgeLabel = "app", badgeColor = "purple"),
    menuItem("Map Representations", tabName = "maps", icon = icon("map-marker", lib = 'glyphicon'), badgeLabel = "app", badgeColor = "purple"),
    menuItem("Media Analysis", tabName = "media", icon = icon("bullhorn", lib = 'glyphicon'), badgeLabel = "app", badgeColor = "purple")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "instructions",
            h2("Finance GAP of MSME"),
            br(),
            h4("In this shiny dashboard we are exploring several aspects of economic development."),
            h4("First part of our analysis includes exploration of Small and Medium-Scale Enterprises."),
            h4("We have created several interactive plots in 'Plots' section for you to play with and explore our data."),
            h4("Furthermore, you can visit the 'Map Representations' section to explore the data in a real scaled world map view."),
            h4("There is also a basic table of our data in 'Explore Raw Data' section, where you can see the data set in raw format"),
            h4("Second part of our project explores the media via utilization of web scraping techniques."),
            h4("Feel free to open up the 'Media Analysis' section and play with some wordclouds and get some interesting results."),
            br(),
            h4("SMSE dataset is taken from:"),
            a("World Bank Group Finances", href = "https://finances.worldbank.org/Other/MSME-Finance-Gap/ijmu-5v4p", style = "font size: 14pt")
            
    ),
    
    tabItem(tabName = "plots",
            
            fluidRow(
              box(
                width = 12,
                selectInput(inputId = 'country1',
                            label = 'Choose a Country for Analysis',
                            choices = smse_countries,
                            selectize = TRUE,
                            selected = 'Armenia'),
                
                selectInput(inputId = 'country2',
                            label = 'Choose a Country for Comparison (Optional)',
                            choices = smse_countries_no_default,
                            selectize = TRUE,
                            selected = '--select--'
                )
              )
            ),
            
            fluidRow(
              h4('In case you are comparing 2 countries, select the variables for comparison',
                 style = "margin: 10px; font-weight: bold;"),
              box(
                width = 12,
                selectInput(inputId = 'var1',
                            label = 'Choose First Variable for Comparison',
                            choices = smse_choices,
                            selectize = TRUE,
                            selected = 'finance_gap'
                ),
                
                selectInput(inputId = 'var2',
                            label = 'Choose Second Variable for Comparison',
                            choices = smse_choices,
                            selectize = TRUE,
                            selected = 'current_volume'
                )
              )
            ),
            
            fluidRow(
              box(
                plotlyOutput('plot1'), 
                width = 12, 
                height = 500)
            ),
            
            
            fluidRow(
              box(
                plotlyOutput('plot2'), 
                width = 12, 
                height = 500)
              )
            
            ),
  
  tabItem(tabName = "maps",
          
          fluidRow(
            box(
              plotlyOutput("map1"),
              width = 12, 
              height = 500
              )
            ),
          
          fluidRow(
            box(
              width = 12,
              
              selectInput(inputId = 'var_map',
                          label = 'Choose a Variable to Output in the Following Map',
                          choices = smse_choices,
                          selectize = TRUE,
                          selected = 'potential_demand')
              )
            ),
          
          fluidRow(
            box(
              plotlyOutput("map2"),
              width = 12, 
              height = 500
              )
            )
          
          ),
  
  tabItem(tabName = "media",
    
    fluidRow(
      box(sliderInput("Year_slider", 
                       label = "Choose year range",
                       min = 2010, 
                       max = 2018, 
                       value = c(2013, 2014), 
                       sep = "")
           ),
      
      box(radioButtons("tldf", 
                       label = "Enable TL;DF weighting?", 
                       choices = c("Yes","No"), 
                       selected = "No")
          )
      
    ),
    
    fluidRow(
      box(
        plotOutput("word_plot")
        ),
      
      box(
        h4('Corresponding Polarity Score',
           style = "margin: 10px;"),
        textOutput("polarity")
        )
      )
    
    ),
  
  tabItem(tabName = "data",
          
          fluidRow(
            box(
              dataTableOutput("table"),
              width = 12
              )
            )
          
          )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = 'blue')



server <- function(input,output){
  
  options(scipen = 666)
  
  ## web scraping reactive function
  sources <- reactive({
    
    sources <- data.frame(Article = as.character(),
                          Year = as.numeric(),
                          stringsAsFactors = F)
    
    
    source_catcher(sources, 1,"http://www.shaulainternational.com/industrial-policy/","#content p",2010)
    source_catcher(sources, 2,"https://armenianweekly.com/2010/01/30/listening-to-the-wind-of-change-renewable-energy-in-armenia/",".alignright+ p , p+ p , .alignleft+ p",2010)
    source_catcher(sources, 3,"http://www.globalmetals.am/en/news/3023.html","#article_holder",2010)
    source_catcher(sources, 4,"https://news.am/eng/news/31849.html", "p", 2010)
    source_catcher(sources, 5,"http://www.azad-hye.net/news/viewnews.asp?newsId=846adsl99","div , p:nth-child(30) , p:nth-child(31) , p:nth-child(7) , font",2010)
    source_catcher(sources, 6,"https://news.am/eng/news/57228.html","p",2011)
    source_catcher(sources, 7,"https://mediamax.am/en/news/yerevan/354/","h2 , p",2011)
    source_catcher(sources, 8,"http://www.gov.am/en/news/item/5539/", "#content div p", 2011)
    source_catcher(sources, 9, "https://ayfwest.org/news/can-armenias-economy-thrive-on-services/", ".entry-content p", 2011)
    source_catcher(sources, 10, "http://hetq.am/eng/news/2927/diaspora-investors---easy-prey-for-armenias-authorities.html/", ".opennewstext", 2011 )
    source_catcher(sources, 11, "http://hetq.am/eng/news/19886/armenian-investors-letter-to-us-ambassador-to-armenia.html", "p", 2012 )
    source_catcher(sources, 12, "http://www.gov.am/en/news/item/6364/", "#content p",  2012 )
    source_catcher(sources, 13, "http://www.gov.am/en/news/item/6082/", "p", 2012 )
    source_catcher(sources, 14, "https://news.am/eng/news/102117.html", "p", 2012)
    source_catcher(sources, 15, "http://www.armenia.com.au/news/Armenia-News/English/7434/Businessman-blames-fraud-on-Armenian-President---s-brother", "p", 2012)
    source_catcher(sources, 16, "http://www.gov.am/en/news/item/6642/", "p", 2013)
    source_catcher(sources, 17, "https://www.osce.org/yerevan/108150", "p", 2013)
    source_catcher(sources, 18, "https://armenpress.am/eng/news/719161/john-heffern-considers-coca-cola-activity-in-armenia-to-be-best-example-of-american-investment.html", "#opennewscontainer p", 2013)
    source_catcher(sources, 19, "https://itel.am/en/news/interviews/5489", ".text p , strong , em", 2013)
    source_catcher(sources, 20, "http://hetq.am/eng/news/22694/armenias-struggling-vintners-government-promotes-cognac-people-prefer-vodka.html", ".opennewstext p", 2013)
    source_catcher(sources, 21, "http://www.gov.am/en/news/item/7115/", "#content div p", 2014)
    source_catcher(sources, 22, "https://armenpress.am/eng/news/766444/uwc-dilijan---huge-investment-for-future-and-youth.html", "p span", 2014)
    source_catcher(sources, 23, "http://www.fao.org/armenia/news/detail-events/en/c/253917/", "p", 2014)
    source_catcher(sources, 24, "https://armenianweekly.com/2014/03/21/armenia-us-relations-unfulfilled-promise/", "#main-content .clearfix p", 2014)
    source_catcher(sources, 25, "https://www.synopsys.com/company/newsroom/armenia-news-releases/synopsys-celebrates-10th-anniversary-in-armenia-during-synopsys-week.html", ".vert-pad-bottom-xs p", 2014)
    source_catcher(sources, 26, "https://news.am/eng/news/303018.html", "p", 2015)
    source_catcher(sources, 27, "https://massispost.com/2015/07/ceo-of-franck-muller-to-invest-150000-in-armenia-projects/", "p", 2015)
    source_catcher(sources, 28, "http://www.tert.am/en/news/2015/03/26/liana/1628440", "#news-content-container , p:nth-child(3) , #news-content-container p:nth-child(7) , #news-content-container p:nth-child(10) , #news-content-container p:nth-child(2) , #news-content-container strong", 2015)
    source_catcher(sources, 29, "https://hetq.am/eng/news/62887/who-owns-the-internet-in-armenia.html", "p , b", 2015)
    source_catcher(sources, 30, "https://itel.am/en/news/interviews/7370", ".text p , br+ strong , em", 2015)
    source_catcher(sources, 31, "https://news.am/eng/news/311057.html", "p", 2016)
    source_catcher(sources, 32, "https://armenpress.am/eng/news/863573/armenia%E2%80%99s-pharmaceutical-sector-recorded-an-annual-growth-of-20-over-last-5-years.html", ".opennewstext p", 2016)
    source_catcher(sources, 33, "https://www.armenianow.com/en/economy/2016/05/19/armenia-us-relations-economy-business-minister-minasyan-ambassador/3469/", "p", 2016)
    source_catcher(sources, 34, "https://news.am/eng/news/303821.html", "p", 2016)
    source_catcher(sources, 35, "https://am.am/eng/news/574/new-foreign-investment-in-armenia-forged-by-partnership-of-am-law-firm-with-fxtm.html", "p", 2016)
    source_catcher(sources, 36, "https://armenpress.am/eng/news/904305/2017-large-foreign-direct-investment-flows-to-armenia-recorded-from-germany-uk.html", "#opennewscontainer p", 2017)
    source_catcher(sources, 37, "https://armenpress.am/eng/news/879393/armenia%E2%80%99s-investment-attractiveness-to-be-presented-to-20-25-french-companies-business-forum-kicks.html", "p:nth-child(1)", 2017)
    source_catcher(sources, 38, "https://jam-news.net/?p=38573", "p", 2017)
    source_catcher(sources, 39, "https://news.am/eng/news/385870.html", "p", 2017)
    source_catcher(sources, 40, "https://www.azatutyun.am/a/28393572.html", "#article-content p", 2017)
    source_catcher(sources, 41, "https://news.am/eng/news/458534.html", "p", 2018)
    source_catcher(sources, 42, "https://armenpress.am/eng/news/928860.html", "#opennewscontainer p", 2018)
    source_catcher(sources, 43, "http://www.shaulainternational.com/2018/03/27/dfa-will-function-from-now-on-under-the-name-of-business-armenia/", "#content .clearfix p", 2018)
    source_catcher(sources, 44, "https://www.panorama.am/en/news/2018/04/12/Government-investment-project/1932644", "p , .fs26", 2018)
    
    sources[, 1] <- str_remove_all(sources[, 1], pattern = '[^a-zA-Z,^\\s]')
    sources[, 1] <- str_remove_all(sources[, 1], pattern = '[:punct:]')
    
    # write.csv(sources, file = 'investmentSources.csv')
    # You can uncomment the next two lines instead of having to scrap all 45 
    # sources <- read.csv('investmentSources.csv')
    # sources[,1] <- NULL
    
    sources
    
  })
  

  ###this portion is for polarity
  polarity_score <- reactive({
    
    result_polarity <- as.numeric()
    
    sources <- sources()
    
    sources2 <- sources[sources$Year %in% input$Year_slider, ]
    
    years_polarity <- scores(polarity(text.var = sources2$Article, grouping.var = sources2$Year))
    
    result_polarity <- head(ave(years_polarity$ave.polarity),n=1)
    
    result_polarity
  })
  
  ## titles for the maps
  pretty_plot_title <- reactive({
    req(input$var_map)
    
    input$var_map
  })
  
  ## smse data preparations
  smse <- reactive({
    smse <- data.frame(read_api("https://finances.worldbank.org/resource/xpm6-g5vt.json"))
    
    smse <- con_types(smse)
  })
  
  smse_long <- reactive({
    smse_long <- change_format(smse(), 'country')
  })
  
  by_country <- reactive({
    country <- smse() %>%
      filter(country %in% c(input$country1, input$country2))
  })
  
  by_country_long <- reactive({
    country <- smse_long() %>%
      filter(country %in% c(input$country1, input$country2))
  })
  
  smse_df1 <- reactive({
    smse_df1 <- filter(by_country_long(), 
                       indicator %in% c('finance_gap', 'current_volume'))
    smse_df1 <- con_types(smse_df1)
    
  })
  
  smse_df2 <- reactive({
    smse_df2 <- select(by_country(), 
                       country, input$var1, input$var2)
    smse_df2 <- con_types(smse_df2)
    
  })
  
  output$plot1 <- renderPlotly({
    if(input$country2 == '--select--'){
      plot_ly(
        smse_df1(), labels = ~indicator, values = ~value, type = 'pie') %>%
        layout(title = 'Formal MSME Finance Gap',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    } else {
      plot_ly(
        smse_df2(), x = ~country, y = ~get(input$var1), 
        type = 'bar', name = as.character(input$var1)) %>%
        add_trace(y = ~get(input$var2), name = as.character(input$var2)) %>%
        layout(title = 'Comparison Bar Plot',
          yaxis = list(title = 'Count'),
          barmode = 'group')
    }
    
  })
  
  output$plot2 <- renderPlotly({
    
    plot_ly(
      smse(), x = ~get(input$var1), y = ~get(input$var2), text = ~country,
      marker = list(size = 10,
                    color = 'rgba(255, 182, 193, .9)',
                    line = list(color = 'rgba(152, 0, 0, .8)',
                                width = 2))) %>%
      layout(title = 'Scatter Plot of Variables',
             xaxis = list(title = input$var1),
             yaxis = list(title = input$var2)
      )
    
  })

  
  ## creating maps
  output$map1 <- renderPlotly({
    smse <- smse()
    
    smse$iso_code <- countrycode(smse$country,'country.name', 'iso3c') #!!!!!!!!!!!
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    p <- plot_geo(smse) %>%
      add_trace(
        z = ~finance_gap_gdp, color = ~finance_gap_gdp, colors = 'Blues',
        text = ~country, locations = ~iso_code, marker = list(line = l)
      ) %>%
      colorbar(title = 'MSME Finance Gap as % of GDP', tickprefix = '$') %>%
      layout(
        title = 'MSME Finance Gap as % of GDP',
        geo = g
      )
    
  })
  
  
  output$map2 <- renderPlotly({
    smse <- data.frame(read_api("https://finances.worldbank.org/resource/xpm6-g5vt.json"))
    
    smse <- con_types(smse)
    smse$iso_code <- countrycode(smse$country,'country.name', 'iso3c') #!!!!!!!!!!!
    
    smse <- smse%>%
      dplyr::select(country, iso_code, input$var_map)
    
    colnames(smse)[3]<-'mapping_var'
    
    map_title <- as.character(mapping_choices[pretty_plot_title() == mapping_choices$df_var,][1]$var_name)
    
    
    # light grey boundaries
    l <- list(color = toRGB("black"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    p <- plot_geo(smse) %>%
      add_trace(
        z = ~mapping_var, color = ~mapping_var, colors = 'Greens',
        text = ~country, locations = ~iso_code, marker = list(line = l)
      ) %>%
      colorbar(title = paste(mapping_choices[pretty_plot_title()==mapping_choices$df_var,][1]$var_name)
               , tickprefix = '$') %>%
      layout(
        title = paste(mapping_choices[pretty_plot_title()==mapping_choices$df_var,][1]$var_name)
        ,
        geo = g
      )
    
  })
  
  ## text mining part
  
  output$word_plot <- renderPlot({
    
    sources <- sources()
    articles <- sources[sources$Year %in% input$Year_slider, 1]
    articles <- VCorpus(VectorSource(articles))
    articles <- tm_map(articles, content_transformer(tolower))
    
    ifelse(input$tldf == "Yes",
           tdm <- TermDocumentMatrix(articles, control = list(removeNumbers=T,removePunctuation=T,
                                                              stopwords=T,stemming=T,weighting = weightTfIdf)),
           
           tdm <- TermDocumentMatrix(articles, control = list(removeNumbers=T,removePunctuation=T,
                                                              stopwords=T,stemming=T))
    )
    
    mat <- as.matrix(tdm)
    freqs <- rowSums(mat)
    df_freq <- data.frame(terms = rownames(mat), freq = freqs, stringsAsFactors = F)
    df_freq <- df_freq[order(df_freq$freq, decreasing = T),]
    
    
    wordcloud(words = df_freq$terms, freq = df_freq$freq, 
              min.freq = 2, max.words = 50, 
              random.order = FALSE, colors = brewer.pal(8, "Dark2"))
    
    
    
  })
  
  
  
  output$polarity <- renderText({
    
    polarity_score()
    
    })
  
  output$table<-DT::renderDataTable({
    DT::datatable(select(smse(),country,current_volume,finance_gap_gdp,number_of_micro,number_of_smes),options = list(lengthMenu = c(5, 30, 50), pageLength = 20))
  })
  
}


shinyApp(ui = ui, server = server)