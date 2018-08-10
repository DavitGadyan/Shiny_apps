
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)
# install.packages('leaps')
# install.packages('bestglm')
library(leaps)
library(bestglm)
# install.packages('MASS')
library(MASS)
library(tools)



team_id_table<-read_html('https://github.com/seemethere/nba_py/wiki/stats.nba.com-Endpoint-Documentation#boxscoretraditionalv2')%>%
  html_table()

team_id_table<-data.frame(team_id_table)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'team',
                  label='Team: choose only one team and wait a bit till data is loaded :)',
                  choices = c('Atlanta Hawks'='1610612737',
                              'Boston Celtics'='1610612738',
                              'Brooklyn Nets'='1610612751',
                              'Charlotte Hornets'='1610612766',
                              'Chicago Bulls'='1610612741',
                              'Cleveland Cavaliers'='1610612739',
                              'Dallas Mavericks'='1610612742',
                              'Denver Nuggets'='1610612743',
                              'Detroit Pistons'='1610612765',
                              'Golden State Warriors'='1610612744',
                              'Houston Rockets'='1610612745',
                              'Indiana Pacers'='1610612754',
                              'Los Angeles Clippers'='1610612746',
                              'Los Angeles Lakers'='1610612747',
                              'Memphis Grizzlies'='1610612763',
                              'Miami Heat'='1610612748',
                              'Milwaukee Bucks'='1610612749',
                              'Minnesota Timberwolves'='1610612750',
                              'New Orleans Pelicans'='1610612740',
                              'New York Knicks'='1610612752',
                              'Oklahoma City Thunder'='1610612760',
                              'Orlando Magic'='1610612753',
                              'Philadelphia 76ers'='1610612755',
                              'Phoenix Suns'='1610612756',
                              'Portland Trail Blazers'='1610612757',
                              'Sacramento Kings'='1610612758',
                              'San Antonio Spurs'='1610612759',
                              'Toronto Raptors'='1610612759',
                              'Utah Jazz'='1610612762',
                              'Washington Wizards'='1610612764'),
                  selectize = TRUE, multiple = FALSE,selected = '1610612741'),
      selectInput(inputId = 'X_axis',
                  label='Choose from list of features for X_axis',
                  choices = c('Year'='YEAR',
                              'Total number of games'='GP',
                              'Wins'='WINS',
                              'Losses'='LOSSES',
                              'Winnig Percentage'='WIN_PCT',
                              'Conference ranking'='CONF_RANK',
                              'Division ranking'='DIV_RANK',
                              'Conference Count'='CONF_COUNT',
                              'Division Count'='DIV_COUNT',
                              'Field Goals Made'='FGM',
                              'Field Goals Attempted'='FGA',
                              'Field Goals Percentage'='FG_PCT',
                              'Three-point field goals Made'='FG3M',
                              'Three-point field goals Attempted'='FG3A',
                              'Three-point field goals Percentage (Made)'='FG3_PCT',
                              'Free Throws Made'='FTM',
                              'Free Throws Attempted'='FTA',
                              'Free Throws Percentage (Made)'='FT_PCT',
                              'Offensive Rebound'='OREB',
                              'Defensive Rebound'='DREB',
                              'Rebounds'='REB',
                              'Assists'='AST',
                              'Personal Fouls'='PF',
                              'Steals'='STL',
                              'Turnovers'='TOV',
                              'Blocks'='BLK',
                              'Points'='PTS',
                              'Points Rank'='PTS_RANK',
                              'Overall Winning Status for the period'='WON'
                  ),selectize = TRUE, multiple = FALSE,selected='AST'),
      selectInput(inputId = 'Y_axis',
                  label='Choose from list of features for Y_axis',
                  choices = c('Year'='YEAR',
                              'Total number of games'='GP',
                              'Wins'='WINS',
                              'Losses'='LOSSES',
                              'Winnig Percentage'='WIN_PCT',
                              'Conference ranking'='CONF_RANK',
                              'Division ranking'='DIV_RANK',
                              'Conference Count'='CONF_COUNT',
                              'Division Count'='DIV_COUNT',
                              'Free Goals Made'='FGM',
                              'Free Goals Attempted'='FGA',
                              'Free Goals Percentage'='FG_PCT',
                              'Three-point field goals Made'='FG3M',
                              'Three-point field goals Attempted'='FG3A',
                              'Three-point field goals Percentage (Made)'='FG3_PCT',
                              'Free Throws Made'='FTM',
                              'Free Throws Attempted'='FTA',
                              'Free Throws Percentage (Made)'='FT_PCT',
                              'Offensive Rebound'='OREB',
                              'Defensive Rebound'='DREB',
                              'Rebounds'='REB',
                              'Assists'='AST',
                              'Personal Fouls'='PF',
                              'Steals'='STL',
                              'Turnovers'='TOV',
                              'Blocks'='BLK',
                              'Points'='PTS',
                              'Points Rank'='PTS_RANK',
                              'Overall Winning Status for the period'='WON'
                  ),selectize = TRUE, multiple = FALSE, selected = 'FG_PCT')
      
    ),
    mainPanel(
      htmlOutput('intro'),
      dataTableOutput('mod_df_r'),
      htmlOutput('intro2'),
      dataTableOutput('cor_plt'),
      htmlOutput('cor_text'),
      verbatimTextOutput('sum_bestglmaic'),
      plotOutput('plot1'),
      htmlOutput('plot1_text'),
      plotOutput('plot2'),
      htmlOutput('plot2_text'),
      htmlOutput('concl')
      
      
    )
  ))

server<-function(input,output){
  team_id<-reactive({
    req(input$team)
    input$team})
  
  df_full<-reactive({
    mod_req<-modify_url('https://stats.nba.com/stats/teamyearbyyearstats',
                        query = list(TeamID=team_id(),
                                     PerMode='Totals',
                                     SeasonType='Playoffs',
                                     LeagueID='00')
    )
    
    team_hist<-readLines(mod_req)
    
    df_team_hist<-fromJSON(team_hist)
    
    str(df_team_hist)
    
    df_team_yearly<-df_team_hist$resultSets$rowSet[[1]]
    colnames(df_team_yearly)<-df_team_hist$resultSets$headers[[1]]
    
    df_team_yearly<-data.frame(df_team_yearly,stringsAsFactors = F)
    
    str(df_team_yearly)
    
    
    str(df_team_yearly)
    
    ### Let's select numerical columns 
    num_vec<-df_team_yearly%>%
      dplyr::select(-TEAM_ID,-TEAM_CITY,-TEAM_NAME,-YEAR,-NBA_FINALS_APPEARANCE)%>%
      colnames()
    
    df_team_yearly[,num_vec]<-apply(df_team_yearly[,num_vec],2,as.numeric)
    
    str(df_team_yearly)
    
    ### Let's deal with Year column...
    
    df_team_yearly$YEAR<-as.numeric(str_sub(df_team_yearly$YEAR,1,4))
    
    
    df_team_yearly$WON_m<-ifelse(df_team_yearly$WIN_PCT>0.5,1,0)
    
    df_team_yearly$WON<-ifelse(df_team_yearly$WON_m==1,'YES','NO')
    
    df_team_yearly
  })
  ####
  mod_df<-reactive({df_full()%>%
      dplyr::select(-TEAM_ID,-TEAM_CITY,-TEAM_NAME,-YEAR,-GP,-WINS,-LOSSES,-WIN_PCT,-NBA_FINALS_APPEARANCE,-REB,-CONF_COUNT,
                    -PO_LOSSES,-PO_WINS,-PTS,-DIV_RANK,-CONF_RANK,-PTS_RANK,-DIV_COUNT,-FTA,-WON)
  })
  
  cor_plot<-reactive({
    cor_won<-as.matrix(cor(mod_df())[dim(mod_df())[2],])
    class(cor_won)
    
    cor_won<-data.frame(Variable=rownames(cor_won),Values=cor_won)
    
    class(cor_won)
    
    dim(cor_won)
    
    cor_won_sorted<-cor_won%>%
      arrange(desc(Values))
    
    cor_won_sorted_asc<-cor_won%>%
      arrange(Values)
    cor_won_sorted_asc
  })
  output$intro<-renderUI(HTML(paste("This dashboard represents needy tool for a coach of NBA basketball team."),
                              "<br/>",
                              paste("The user may change the team id and all statistical information would be taken from nba.com API."),
                              "<br/>",
                              paste("Let's choose a team and look at most recent historical data!!!")))
  output$mod_df_r<-renderDataTable(tail(df_full()))
  
  output$intro2<-renderUI(HTML(paste('In the beginning we stated that this dashboard is needy right!!'),
                               "<br/>",
                               paste('The usefulness of this dashborad is simple and in the same time is very powerfull. 
                                     It is capable automatically detect the most important features or variables for winnig in the game based specific to each team!!!!
                                     In more technical language we take defined team, then apply a machine learning algorithm and it will select the most important features 
                                     from (FGM, FG3M, FG3_PCT, FTM, DREB, STL,TOV) etc. for a particular team based on actual outcome of each game!!!! With this info
                                     coaches may adjust and work on the most important features to achive higher results!!!'),
                               "<br/>","<br/>","<br/>",
                               paste('In the next steps we subset a list of general feaures which may have effects on the game of any team.
                                     After that we will construct a simple correalation table with WON_m manually added feature for ML, where 1 means team won the majority 
                                     of games in the season (>50%) and 0 means they lost the majority of games. This will help to identify the most correlated features and 
                                     will assist in automatically subsetting features for any team, as they are in many cases specific for each of them')))
  output$cor_plt<-renderDataTable(cor_plot())
  output$cor_text<-renderUI(HTML(paste('From this correaltion table we may observe in ascending order correlation between a feature and WON_m variable.
                                       The algorithm will automatically select values that are +/- 10% correlated with WON_m variable in order to succesfully execute logistic 
                                       regression algorithms which will automatically detect the most significant for a team variables!!!')))
  
  #####
  summ_bestglmaic_model<-reactive({

  
  cor_won_10<-cor_plot()%>%
    filter(Values>=0.1|Values<=-0.1)
  
  
  
  mod_df1<-mod_df()[cor_won_10$Variable]
  
  
  ### Automated bestglm
  
  best_mod_bestglmaic<-bestglm(Xy=mod_df1,IC='AIC',family = binomial)
  

  ### Final summaries for glm models
  
  glm(formula(best_mod_bestglmaic$BestModel),data = mod_df(),family = 'binomial')
  })
  output$sum_bestglmaic<-renderPrint({summary(summ_bestglmaic_model())})
  output$sum_text<-renderUI(HTML(paste('The above summary represents the best model and feautures represent the most significant
                                       variables that were used by model.The above summary uses bestglm package with parameter 
                                       equal to AIC. Alternativelly, we may use the BIC parameter or compltelly different function 
                                       called stepAIC from MASS package.'),
                                 '<br/>',
                                 paste("After we get this info let's use it to visualize the patterns and feautures discovered by 
                                       ML algorithms. P.S. Select the only positve or only negative features to derive useful insights.
                                       ")))
  pretty_plot_title <- reactive({
    req(input$team)
    toTitleCase(input$team)})
  output$plot1<-renderPlot({ggplot(df_full(),aes_string(x='YEAR',y='WIN_PCT'))+geom_line()+labs(title=paste('Winning percentage of ',team_id_table[pretty_plot_title()==team_id_table$TeamID,][1]))})
  output$plot1_text<-renderUI(HTML(paste('The line plot depicts winning percentage of',team_id_table[pretty_plot_title()==team_id_table$TeamID,][1],'over the period.')))
  
  output$plot2<-renderPlot({ggplot(df_full(),aes_string(x=input$X_axis,y=input$Y_axis))+geom_point(aes(color=WON))+labs(title='Graph for significant variables predicting winning outcome /select manually based on glm summary/')})
  output$concl<-renderUI(HTML(paste('The above graph may firstly does not show clear relationships but in order to gain useful insight and 
                                    prove the model correctness it needs little human interaction. Particularly the user should assign X and Y axis 
                                    to two biggest(positive) coefficients or two smallest(negative) coefficients of glm summary except intercept (if such coef. exists and in most cases they will).'),
                              '<br/>','<br/>',
                              paste('Based on glm summary and visualizations coach will choose the features which bring vicotry to the team
                                    and working excatly on these features will amplify positive results!!!!')
          ))
  
}

shinyApp(ui=ui,server = server)