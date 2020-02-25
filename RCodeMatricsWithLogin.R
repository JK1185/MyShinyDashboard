library(shiny)
library(shinydashboard)
#library(DT)
library(shinyjs)
library(sodium)
library(RODBC)
#library(shiny)
#require(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(shinyWidgets)
library(lazyeval)
library(rlang)
library(data.table)
library(tidyr)
#library(shinyjs)

#getwd()
oppid <- read.csv("oppidLatest.csv",header = TRUE)
oppid <- oppid[,-1]
oppid$FinalizedDt <- as.Date(oppid$FinalizedDt,format="%Y-%m-%d")
oppid$ConductDt <- as.Date(oppid$ConductDt,format="%Y-%m-%d")
#str(oppid)


oppid %>% distinct(ReviewId,.keep_all= TRUE) %>% distinct(PlanId,ReviewStageCd,FinalizedDt,.keep_all= TRUE) -> oppid

setnames(oppid,c("OperatingGroupDesc","ResponsibleBusinessEntityNm","RegionDesc"),c("OG","RBE","REG"))

oppid$OG <- gsub("Communications, Media & Technology","CMT",oppid$OG)
oppid$OG <- gsub("Financial Services","FS",oppid$OG)
oppid$OG <- gsub("Health & Public Service","HPS",oppid$OG)
oppid$OG <- gsub("Products","PRD",oppid$OG)
oppid$OG <- gsub("Resources","RES",oppid$OG)

oppid$REG <- gsub("Latin America","LA",oppid$REG)
oppid$REG <- gsub("North America","Na",oppid$REG)

oppid %>% mutate(Quarter = ifelse(oppid$FinalizedDt >= 	"2016-09-01" & oppid$FinalizedDt <= "2016-11-30","Fy17Q1",
                                  ifelse(oppid$FinalizedDt >= 	"2016-12-01" & oppid$FinalizedDt <= "2017-02-28","Fy17Q2",
                                         ifelse(oppid$FinalizedDt >= 	"2017-03-01" & oppid$FinalizedDt <= "2017-05-31","Fy17Q3",
                                                ifelse(oppid$FinalizedDt >= 	"2017-06-01" & oppid$FinalizedDt <= "2017-08-31","Fy17Q4",
                                                       ifelse(oppid$FinalizedDt >= 	"2017-09-01" & oppid$FinalizedDt <= "2017-11-30","Fy18Q1",
                                                              ifelse(oppid$FinalizedDt >= 	"2017-12-01" & oppid$FinalizedDt <= "2018-02-28","Fy18Q2",
                                                                     ifelse(oppid$FinalizedDt >= 	"2018-03-01" & oppid$FinalizedDt <= "2018-05-31","Fy18Q3",
                                                                            ifelse(oppid$FinalizedDt >= 	"2018-06-01" & oppid$FinalizedDt <= "2018-08-31","Fy18Q4",
                                                                                   ifelse(oppid$FinalizedDt >= 	"2018-09-01" & oppid$FinalizedDt <= "2018-11-30","Fy19Q1",
                                                                                          ifelse(oppid$FinalizedDt >= "2018-12-01" & oppid$FinalizedDt <= "2019-02-28","Fy19Q2",
                                                                                                 ifelse(oppid$FinalizedDt >= 	"2019-03-01" & oppid$FinalizedDt <= "2019-05-31","Fy19Q3",
                                                                                                        "Fy19Q4")))))))))))) %>% 
  mutate(QuarterInNum = ifelse(Quarter == "Fy17Q1",1,
                               ifelse(Quarter == "Fy17Q2",2,
                                      ifelse(Quarter == "Fy17Q3",3,
                                             ifelse(Quarter == "Fy17Q4",4,
                                                    ifelse(Quarter == "Fy18Q1",5,
                                                           ifelse(Quarter == "Fy18Q2",6,
                                                                  ifelse(Quarter == "Fy18Q3",7,
                                                                         ifelse(Quarter == "Fy18Q4",8,
                                                                                ifelse(Quarter == "Fy19Q1",9,
                                                                                       ifelse(Quarter == "Fy19Q2",10,
                                                                                              ifelse(Quarter == "Fy19Q3",11,12)))))))))))) %>% 
  
  
  mutate(OGNum = ifelse(OG == "CMT",2,
                        ifelse(OG == "FS",3,
                               ifelse(OG == "HPS",4,
                                      ifelse(OG == "PRD",5,6))))) %>%
  
  
  mutate(REGNum = ifelse(REG == "AAPAC",2,
                         ifelse(REG == "Europe",3,
                                ifelse(REG == "LA",4,5)))) %>%
  
  
  mutate(RBENum = ifelse(RBE == "Digital",2,
                         ifelse(RBE == "OG",3,
                                ifelse(RBE == "Operations",4,
                                       ifelse(RBE == "Security",5,
                                              ifelse(RBE == "Strategy",6,7)))))) -> oppid

FUNSS <- function(.)(ifelse(is.na(.), 0, .))
oppid %>% mutate_if(is.numeric,FUNSS ) %>% filter(QuarterInNum != 12 )  -> oppid

oppid <- na.omit(oppid) 
#oppid <- as.data.frame(oppid)
#write.csv(oppid,"TestMetric2.csv")

######################################################################################################################

# setwd(choose.dir())
# oppid<- read.csv("TestMetric1.csv",header=TRUE,row.names = NULL,check.names = F)
#names(oppid)
# oppid <- oppid[,-1]
#str(oppid)

oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(Freq = length(Quarter)) %>% mutate(FreqPercent = round(((Freq/sum(Freq))*100),0)) -> GroupFreq
oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(TR = sum(TNRinM)) %>% mutate(TRPercent = round(((TR/sum(TR))*100),0)) -> GroupTR
oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(TCR = sum(TCNRinM)) %>% mutate(TCRPercent = round(((TCR/sum(TCR))*100),0)) -> GroupTCR

FileFreqTRTCR <- cbind.data.frame(GroupFreq, GroupTR[,(4:5)], GroupTCR[,(4:5)])
#FileFreqTRTCR <- FileFreqTRTCR_OG %>% complete(nesting(Quarter,QuarterInNum),RiskTierDesc,fill = list(Freq=0,FreqPercent=0,TR=0,TRPercent=0,TCR=0,TCRPercent =0))
FileFreqTRTCR$RiskTierDesc <- factor(FileFreqTRTCR$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))
#colnames(FileFreqTRTCR)

oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(Freq_OG = length(Quarter)) %>% mutate(Freq_OGPercent = round(((Freq_OG/sum(Freq_OG))*100),0)) -> GroupFreq_OG
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(TR_OG = sum(TNRinM)) %>% mutate(TR_OGPercent = round(((TR_OG/sum(TR_OG))*100),0)) -> GroupTR_OG  #colnames(GroupFreq_OG)#
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(TCR_OG = sum(TCNRinM)) %>% mutate(TCR_OGPercent = round(((TCR_OG/sum(TCR_OG))*100),0)) -> GroupTCR_OG

FileFreqTRTCR_OG <- cbind.data.frame(GroupFreq_OG, GroupTR_OG[,(6:7)], GroupTCR_OG[,(6:7)])
FileFreqTRTCR_OG <- FileFreqTRTCR_OG %>% complete(nesting(Quarter,QuarterInNum,OG,OGNum),RiskTierDesc,fill = list(Freq_OG=0,Freq_OGPercent=0,TR_OG=0,TR_OGPercent=0,TCR_OG=0,TCR_OGPercent =0))
FileFreqTRTCR_OG$RiskTierDesc <- factor(FileFreqTRTCR_OG$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(Freq_Region = length(Quarter)) %>% mutate(Freq_RegionPercent = round(((Freq_Region/sum(Freq_Region))*100),0)) -> GroupFreq_Region 
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(TR_Region = sum(TNRinM)) %>% mutate(TR_RegionPercent = round(((TR_Region/sum(TR_Region))*100),0)) -> GroupTR_Region
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(TCR_Region = sum(TCNRinM)) %>% mutate(TCR_RegionPercent = round(((TCR_Region/sum(TCR_Region))*100),0)) -> GroupTCR_Region

FileFreqTRTCR_Region <- cbind.data.frame(GroupFreq_Region, GroupTR_Region[,(6:7)], GroupTCR_Region[,(6:7)])
FileFreqTRTCR_Region<- FileFreqTRTCR_Region %>% complete(nesting(Quarter,QuarterInNum,REG,REGNum),RiskTierDesc,fill = list(Freq_Region=0,Freq_RegionPercent=0,TR_Region=0,TR_RegionPercent=0,TCR_Region=0,TCR_RegionPercent =0))
FileFreqTRTCR_Region$RiskTierDesc <- factor(FileFreqTRTCR_Region$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_RBE = length(Quarter)) %>% mutate(Freq_RBEPercent = round(((Freq_RBE/sum(Freq_RBE))*100),0)) -> Group_Freq_RBE 
oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_RBE = sum(TNRinM)) %>% mutate(TR_RBEPercent = round(((TR_RBE/sum(TR_RBE))*100),0)) -> GroupTR_RBE
oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_RBE = sum(TCNRinM)) %>% mutate(TCR_RBEPercent = round(((TCR_RBE/sum(TCR_RBE))*100),0)) -> GroupTCR_RBE

FileFreqTRTCR_RBE <- cbind.data.frame(Group_Freq_RBE, GroupTR_RBE[,(6:7)], GroupTCR_RBE[,(6:7)])
FileFreqTRTCR_RBE<- FileFreqTRTCR_RBE %>% complete(nesting(Quarter,QuarterInNum,RBE,RBENum),RiskTierDesc,fill = list(Freq_RBE=0,Freq_RBEPercent=0,TR_RBE=0,TR_RBEPercent=0,TCR_RBE=0,TCR_RBEPercent =0))
FileFreqTRTCR_RBE$RiskTierDesc <- factor(FileFreqTRTCR_RBE$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_Region_RBE = length(Quarter)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) -> GroupFreq_Region_RBE 
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_Region_RBE = sum(TNRinM)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) -> GroupTR_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_Region_RBE = sum(TCNRinM)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) -> GroupTCR_Region_RBE

FileFreqTRTCR_Region_RBE <- cbind.data.frame(GroupFreq_Region_RBE, GroupTR_Region_RBE[,(8:9)], GroupTCR_Region_RBE[,(8:9)])
FileFreqTRTCR_Region_RBE <- FileFreqTRTCR_Region_RBE %>% complete(nesting(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum),RiskTierDesc,fill = list(Freq_Region_RBE=0,Freq_Region_RBEPercent=0,TR_Region_RBE=0,TR_Region_RBEPercent=0,TCR_Region_RBE=0,TCR_Region_RBEPercent =0))
FileFreqTRTCR_Region_RBE$RiskTierDesc <- factor(FileFreqTRTCR_Region_RBE$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_OG_RBE = length(Quarter)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE /sum(Freq_OG_RBE ))*100),0)) -> GroupFreq_OG_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_OG_RBE = sum(TNRinM)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) -> GroupTR_OG_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_OG_RBE = sum(TCNRinM)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) -> GroupTCR_OG_RBE

FileFreqTRTCR_OG_RBE <- cbind.data.frame(GroupFreq_OG_RBE, GroupTR_OG_RBE[,(8:9)], GroupTCR_OG_RBE[,(8:9)])
FileFreqTRTCR_OG_RBE <- FileFreqTRTCR_OG_RBE %>% complete(nesting(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum),RiskTierDesc,fill = list(Freq_OG_RBE=0,Freq_OG_RBEPercent=0,TR_OG_RBE=0,TR_OG_RBEPercent=0,TCR_OG_RBE=0,TCR_OG_RBEPercent =0))
FileFreqTRTCR_OG_RBE$RiskTierDesc <- factor(FileFreqTRTCR_OG_RBE$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(Freq_OG_Region = length(Quarter)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region /sum(Freq_OG_Region))*100),0)) -> GroupFreq_OG_Region  #colnames(GroupTR_OG_Region_RBE)
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(TR_OG_Region = sum(TNRinM)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) -> GroupTR_OG_Region
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(TCR_OG_Region = sum(TCNRinM)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) -> GroupTCR_OG_Region

FileFreqTRTCR_OG_Region <- cbind.data.frame(GroupFreq_OG_Region, GroupTR_OG_Region[,(8:9)], GroupTCR_OG_Region[,(8:9)])
FileFreqTRTCR_OG_Region <- FileFreqTRTCR_OG_Region %>% complete(nesting(Quarter,QuarterInNum,OG,OGNum,REG,REGNum),RiskTierDesc,fill = list(Freq_OG_Region=0,Freq_OG_RegionPercent=0,TR_OG_Region=0,TR_OG_RegionPercent=0,TCR_OG_Region=0,TCR_OG_RegionPercent =0))
FileFreqTRTCR_OG_Region$RiskTierDesc <- factor(FileFreqTRTCR_OG_Region$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))


oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_OG_Region_RBE = length(Quarter)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE /sum(Freq_OG_Region_RBE))*100),0)) -> GroupFreq_OG_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_OG_Region_RBE = sum(TNRinM)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) -> GroupTR_OG_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_OG_Region_RBE = sum(TCNRinM)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) -> GroupTCR_OG_Region_RBE

FileFreqTRTCR_OG_Region_RBE <- cbind.data.frame(GroupFreq_OG_Region_RBE, GroupTR_OG_Region_RBE[,(10:11)], GroupTCR_OG_Region_RBE[,(10:11)])
FileFreqTRTCR_OG_Region_RBE <- FileFreqTRTCR_OG_Region_RBE %>% complete(nesting(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,OG,OGNum),RiskTierDesc,fill = list(Freq_OG_Region_RBE=0,Freq_OG_Region_RBEPercent=0,TR_OG_Region_RBE=0,TR_OG_Region_RBEPercent=0,TCR_OG_Region_RBE=0,TCR_OG_Region_RBEPercent =0))
FileFreqTRTCR_OG_Region_RBE$RiskTierDesc <- factor(FileFreqTRTCR_OG_Region_RBE$RiskTierDesc, levels=c("High", "Above Normal", "Normal"))



# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center")))
                     # ,
                     # br(),
                     # br(),
                     # tags$code("Username: myuser  Password: mypass"),
                     # br(),
                     # tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  #passod   = c("mypass", "mypass1"),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)
  
  # credentials = data.frame(
  #   username_id = c("myuser", "myuser1"),
  #   passod   = sapply(c("mypass", "mypass1"),password_store),
  #   permission  = c("basic", "advanced"), 
  #   stringsAsFactors = F
 # )


header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 5px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        selectInput('app', 'Select:', c("Risk tier by Count","Risk tier by TR","Risk tier by TCNR"), selectize=TRUE)
        # )
        #   checkboxGroupInput("app", 
        #                      "Select:", 
        #                      choiceNames = c("Risk tier by Count","Risk tier by TR","Risk tier by TCNR"),
        #                      choiceValues = c("Risk tier by Count","Risk tier by TR","Risk tier by TCNR"),
        #                      selected = NULL,
        #                      inline = T
        #                      # multiple = F,
        #                      # selectize = F,
        #                      # width = NULL,
        #                      # size = NULL
        #   )
        ,
        sliderTextInput("Quarter","Select Quarter:",
                        choices =  c("Fy17Q1","Fy17Q2","Fy17Q3","Fy17Q4","Fy18Q1","Fy18Q2","Fy18Q3","Fy18Q4","Fy19Q1","Fy19Q2","Fy19Q3"),
                        selected =  c("Fy17Q2","Fy18Q1")),
        #hr(),
        #verbatimTextOutput('out6'),
        useShinyjs(),
        div(
          id = "form",
          sidebarMenu(
            menuItem(
              "Filters",style='padding:4px; font-size:10px',
              
              div(style = "font-size: 10px; padding: 5px 0px; margin-top:-2em",
                  fluidRow(
                    pickerInput(
                      inputId = "variable",
                      label = "OG to show:", 
                      choices = c("All","CMT","FS","HPS","PRD","RES"),
                      multiple = TRUE,
                      selected = "All"
                      ,
                      # options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                      #                `selected-text-format`= "static", title = "Commodity List"),
                      choicesOpt = list(
                        style = rep(("color: black; background: lightgrey; font-weight: bold;font-size: 70%;"),6)))))
              # status = "primary"
              # choicesOpt = list(
              #   `style` = "btn-warning"
              #   # content = sprintf( "<span class='label label-%s'>%s</span>",
              #   #                    c("All","CMT","FS","HPS","PRD","RES"),
              #   #                   c("All","CMT","FS","HPS","PRD","RES"))
              
              # checkboxGroupInput(inputId="variable", label="OG to show:", 
              #                    choiceNames=c("All","CMT","FS","HPS","PRD","RES"),
              #                    choiceValues=c("All","CMT","FS","HPS","PRD","RES"),
              #                    selected = "All",
              #                    inline = T
              #                    #, multiple = T,selectize = T
              # )
              ,
              
              #hr(),
              #verbatimTextOutput('out6'),
              # div(style = "font-size: 10px; padding: 0px 0px; margin-top:-2em", 
              #     fluidRow(
              # selectInput('variable1', 'Region to show:', c("All","AAPAC","Europe","LA","Na"), multiple=TRUE, selectize=TRUE, selected = "All")
              #     ))
              div(style = "font-size: 10px; padding: 5px 0px; margin-top:-2em",
                  fluidRow(
                    pickerInput(
                      inputId = "variable1",
                      label = "Region to show:", 
                      choices = c("All","AAPAC","Europe","LA","Na"),
                      multiple = TRUE,
                      selected = "All"
                      ,
                      # options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                      #                `selected-text-format`= "static", title = "Commodity List"),
                      choicesOpt = list(
                        style = rep(("color: black; background: lightgrey; font-weight: bold;font-size: 70%;"),5)))))
              # checkboxGroupInput(inputId="variable1", label="Region to show:", 
              #                    choiceNames=c("All","AAPAC","Europe","LA","Na"), 
              #                    choiceValues=c("All","AAPAC","Europe","LA","Na"),
              #                    selected = "All",
              #                    inline = T
              #                    #, multiple = T,selectize = T
              # )
              ,
              #hr(),
              #verbatimTextOutput('out6'),
              # div(style = "font-size: 10px; padding: 0px 0px; margin-top:-2em", 
              #     fluidRow(
              # selectInput('variable2', 'RBE to show:', c("All","Digital","OG","Operations","Security","Strategy","Technology"), multiple=TRUE, selectize=TRUE,selected = "All")
              #     ))
              div(style = "font-size: 10px; padding: 5px 0px; margin-top:-2em",
                  fluidRow(
                    pickerInput(
                      inputId = "variable2",
                      label = "RBE to show:", 
                      choices = c("All","Digital","OG","Operations","Security","Strategy","Technology"),
                      
                      multiple = TRUE,
                      selected = "All"
                      ,
                      # options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                      #                `selected-text-format`= "static", title = "Commodity List"),
                      choicesOpt = list(
                        style = rep(("color: black; background: lightgrey; font-weight: bold;font-size: 70%;"),7)))))
              ,fluidRow(actionButton("reset", "Reset",style='padding:4px; font-size:80%;display:right-align'))
            )
          )
        )
        # checkboxGroupInput(inputId="variable2", label="RBE to show:", 
        #                    choiceNames=c("All","Digital","OG","Operations","Security","Strategy","Technology"),
        #                    choiceValues=c("All","Digital","OG","Operations","Security","Strategy","Technology"),
        #                    selected = "All",
        #                    inline = T
        #                    #, multiple = T,selectize = T
        # )
        ,
        # conditionalPanel(
        #   condition = "input.app == 'Risk tier by Count'|'Risk tier by TR'|'Risk tier by TCNR'",
        #   df <- Group),
        downloadButton('downloadData', 'Download Data',style='padding:4px; font-size:80%;display:right-align'),
        tags$style(type='text/css', "button#downloadData { margin-bottom: 9px; }")
      
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                
                column(width = 12,
                       box(solidHeader = TRUE 
                           ,collapsible = TRUE,align="center",title = "PercentPlots",status = "warning", plotOutput("l", height = "300px"), width = 12)),
                
                column(width = 12,
                       box(solidHeader = TRUE 
                           ,collapsible = TRUE,collapsed = TRUE, align="center",title = "TotalPlots",status = "warning", plotOutput("k", height = "300px"), width = 12)
                ),
                
                #textOutput('table1'),
                textOutput('table2'),
                # textOutput('table3'),
                # textOutput('table4'),
                tableOutput('table')
                
              ))
    }
    else {
      loginpage
    }
  })
  
  XX <- reactive({
    req(input$app)
    sapply(input$app, switch, 
           "Risk tier by Count" = 1,"Risk tier by TR" = 2,"Risk tier by TCNR" = 3)
  })
  
  
  QUAR <- reactive({
    req(input$Quarter)
    sapply(input$Quarter, switch,
           "Fy17Q1" = 1, "Fy17Q2" = 2, "Fy17Q3" = 3, "Fy17Q4" = 4, "Fy18Q1" = 5, "Fy18Q2" = 6, "Fy18Q3" = 7, "Fy18Q4" = 8,"Fy19Q1" = 9,"Fy19Q2" = 10,"Fy19Q3" = 11)
  })
  
  LL1 <- reactive(QUAR()[1])
  LL2 <- reactive(QUAR()[2])
  
  
  OG <- reactive({
    req(input$variable)
    sapply(input$variable, switch,
           "All" = 1, "CMT" = 2, "FS" = 3, "HPS" = 4, "PRD" = 5, "RES" = 6)
  })
  
  
  Region <- reactive({
    req(input$variable1)
    sapply(input$variable1,switch,
           "All" = 1,
           "AAPAC" = 2,
           "Europe" = 3,
           "LA" = 4,
           "Na" = 5)
  })
  
  RBE <- reactive({
    req(input$variable2)
    sapply(input$variable2,switch,
           "All" = 1,
           "Digital" = 2,
           "OG" = 3,
           "Operations" = 4,
           "Security" = 5,
           "Strategy" = 6,
           "Technology" = 7)
  })
  
  observeEvent(input$reset, {
    reset("form")})
  # reset("variable1")
  # reset("variable2")
  #shinyjs::hide("results")
  
  
  cond0 <- reactive({is.null(input$variable) && is.null(input$variable1) && is.null(input$variable2)})
  cond1 <- reactive({(OG() %in% c(2:6)) && is.null(input$variable1) && is.null(input$variable2)})
  cond1.1 <- reactive({(OG() %in% c(1)) && is.null(input$variable1) && is.null(input$variable2)})
  cond1.2 <- reactive({(OG() %in% c(1)) && (Region() %in% c(2:5)) && is.null(input$variable2)})
  cond1.3 <- reactive({(OG() %in% c(1)) && is.null(input$variable1) && (RBE() %in% c(2:7))})
  cond1.4 <- reactive({(OG() %in% c(1)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))})
  cond2 <- reactive({(is.null(input$variable)) && (Region() %in% c(2:5)) && (is.null(input$variable2))})
  cond2.1 <- reactive({(is.null(input$variable)) && (Region() %in% c(1)) && (is.null(input$variable2))})
  cond2.2 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(1)) && (is.null(input$variable2))})
  cond2.3 <- reactive({(is.null(input$variable)) && (Region() %in% c(1)) && (RBE() %in% c(2:7))})
  cond2.4 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(1)) && (RBE() %in% c(2:7))})
  cond3 <- reactive({(is.null(input$variable)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))})
  cond3.1 <- reactive({(is.null(input$variable)) && (is.null(input$variable1)) && (RBE() %in% c(1))})
  cond3.2 <- reactive({(OG() %in% c(2:6)) && (is.null(input$variable1)) && (RBE() %in% c(1))})
  cond3.3 <- reactive({(is.null(input$variable)) && (Region() %in% c(2:5)) && (RBE() %in% c(1))})
  cond3.4 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (RBE() %in% c(1))})
  cond4 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (is.null(input$variable2))})
  cond4.1 <- reactive({(OG() %in% c(1)) && (Region() %in% c(1)) && (is.null(input$variable2))})
  cond4.2 <- reactive({(OG() %in% c(1)) && (Region() %in% c(1)) && (RBE() %in% c(2:7))})
  cond5 <- reactive({(OG() %in% c(2:6)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))})
  cond5.1 <- reactive({(OG() %in% c(1)) && (is.null(input$variable1)) && (RBE() %in% c(1))})
  cond5.2 <- reactive({(OG() %in% c(1)) && (Region() %in% c(2:5)) && (RBE() %in% c(1))})
  cond6 <- reactive({(is.null(input$variable)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))}) 
  cond6.1 <- reactive({(is.null(input$variable)) && (Region() %in% c(1)) && (RBE() %in% c(1))})
  cond6.2 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(1)) && (RBE() %in% c(1))})
  cond7 <- reactive({(OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))})
  cond7.1 <- reactive({(OG() %in% c(1)) && (Region() %in% c(1)) && (RBE() %in% c(1))})
  
  
  df <-reactive({
    if (XX() == 1 ){
      if (isTRUE(cond0()))
      {return(df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:5)])
      }
      else if (isTRUE(cond3()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_RBE),funs(sum)) %>% mutate(Freq_RBEPercent = round(((Freq_RBE/sum(Freq_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond3.1()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(2:7))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_RBE),funs(sum)) %>% mutate(Freq_RBEPercent = round(((Freq_RBE/sum(Freq_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond2()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region),funs(sum)) %>% mutate(Freq_RegionPercent = round(((Freq_Region/sum(Freq_Region))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond2.1()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(2:5))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region),funs(sum)) %>% mutate(Freq_RegionPercent = round(((Freq_Region/sum(Freq_Region))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond6()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region_RBE),funs(sum)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond6.1()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region_RBE),funs(sum)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond2.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region_RBE),funs(sum)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond3.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_Region_RBE),funs(sum)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG),funs(sum)) %>% mutate(Freq_OGPercent = round(((Freq_OG/sum(Freq_OG))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1.1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(2:6))),c(1:7)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG),funs(sum)) %>% mutate(Freq_OGPercent = round(((Freq_OG/sum(Freq_OG))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_RBE),funs(sum)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE/sum(Freq_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5.1()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_RBE),funs(sum)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE/sum(Freq_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1.3()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_RBE),funs(sum)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE/sum(Freq_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond3.2()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_RBE),funs(sum)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE/sum(Freq_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond4()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region),funs(sum)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region/sum(Freq_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond4.1()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region),funs(sum)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region/sum(Freq_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region),funs(sum)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region/sum(Freq_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond2.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5))),c(1:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region),funs(sum)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region/sum(Freq_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond7()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond7.1()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond1.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond4.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond6.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond5.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond3.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond2.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(Freq_OG_Region_RBE),funs(sum)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE/sum(Freq_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
    }
    else if (XX() == 2 ){
      if (isTRUE(cond0()))
      {return(df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:3,6:7)])
      }
      else if (isTRUE(cond3()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_RBE),funs(sum)) %>% mutate(TR_RBEPercent = round(((TR_RBE/sum(TR_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond3.1()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(2:7))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_RBE),funs(sum)) %>% mutate(TR_RBEPercent = round(((TR_RBE/sum(TR_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond2()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region),funs(sum)) %>% mutate(TR_RegionPercent = round(((TR_Region/sum(TR_Region))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond2.1()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(2:5))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region),funs(sum)) %>% mutate(TR_RegionPercent = round(((TR_Region/sum(TR_Region))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond6()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region_RBE),funs(sum)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond6.1()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region_RBE),funs(sum)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond2.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region_RBE),funs(sum)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond3.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_Region_RBE),funs(sum)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG),funs(sum)) %>% mutate(TR_OGPercent = round(((TR_OG/sum(TR_OG))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(2:6))),c(1:5,8:9)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG),funs(sum)) %>% mutate(TR_OGPercent = round(((TR_OG/sum(TR_OG))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_RBE),funs(sum)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5.1()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_RBE),funs(sum)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1.3()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_RBE),funs(sum)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond3.2()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_RBE),funs(sum)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond4()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region),funs(sum)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond4.1()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region),funs(sum)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region),funs(sum)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond2.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG()) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5)))),c(1:7,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region),funs(sum)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond7()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      
      else if (isTRUE(cond7.1()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond4.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond6.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond3.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond5.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond2.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in%  c(RBE()))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in%  c(RBE()))),c(1:9,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TR_OG_Region_RBE),funs(sum)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
    }
    else if (XX() == 3 ){
      if (isTRUE(cond0()))
      {return(df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:3,8:9)])
      }
      else if (isTRUE(cond3()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_RBE),funs(sum)) %>% mutate(TCR_RBEPercent = round(((TCR_RBE/sum(TCR_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond3.1()))
      {return(df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(2:7))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_RBE),funs(sum)) %>% mutate(TCR_RBEPercent = round(((TCR_RBE/sum(TCR_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond2()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region),funs(sum)) %>% mutate(TCR_RegionPercent = round(((TCR_Region/sum(TCR_Region))*100),0)) %>% as.data.frame(XK)) 
      }
      else if (isTRUE(cond2.1()))
      {return(df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(2:5))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region),funs(sum)) %>% mutate(TCR_RegionPercent = round(((TCR_Region/sum(TCR_Region))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond6()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region_RBE),funs(sum)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond6.1()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region_RBE),funs(sum)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond2.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(RBE()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region_RBE),funs(sum)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond3.3()))
      {return(df <- FileFreqTRTCR_Region_RBE[((FileFreqTRTCR_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_Region_RBE$RBENum %in% c(2:7))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_Region_RBE),funs(sum)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) %>% as.data.frame(XK))
      }
      else if (isTRUE(cond1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG),funs(sum)) %>% mutate(TCR_OGRPercent = round(((TCR_OG/sum(TCR_OG))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.1())) 
      {return(df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(2:6))),c(1:5,10:11)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG),funs(sum)) %>% mutate(TCR_OGRPercent = round(((TCR_OG/sum(TCR_OG))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_RBE),funs(sum)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond5.1()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_RBE),funs(sum)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond1.3()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_RBE),funs(sum)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      else if (isTRUE(cond3.2()))
      {return(df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(2:7))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_RBE),funs(sum)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) %>% as.data.frame(XK))  
      }
      
      else if (isTRUE(cond4()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region),funs(sum)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond4.1()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region),funs(sum)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond1.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region),funs(sum)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond2.2()))
      {return(df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(2:5))),c(1:7,12:13)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region),funs(sum)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond7()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   #colnames(FileFreqTRTCR_OG_Region_RBE)
      }
      else if (isTRUE(cond7.1()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond4.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond2.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      
      else if (isTRUE(cond1.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond6.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(2:5)) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond3.4()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
      else if (isTRUE(cond5.2()))
      {return(df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(2:6)) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(2:7))),c(1:9,14:15)] %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise_at(vars(TCR_OG_Region_RBE),funs(sum)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) %>% as.data.frame(XK))   
      }
    }
    
  })
  
  FUN1 <- function(fun.data, fun.y) {
    fun.data$fun.y <- fun.data[, fun.y]
    ggplot(fun.data, aes(x=Quarter, y=fun.y , group = RiskTierDesc , colour= RiskTierDesc)) + 
      geom_line(aes(size= RiskTierDesc)) +
      #geom_point() + ylim(0,100) +
      geom_point() + expand_limits(y=0) + 
      #scale_y_continuous(labels = percent) +
      scale_color_manual(values=c("red","orange","green")) +
      scale_size_manual(values=c(1,1,1)) +
      labs( x = "Quarter", y = ifelse (input$app == "Risk tier by Count","FreqbyPercent(%)",ifelse(input$app == "Risk tier by TR","TotalNRinMPercent(%)","TotalCNRinMPercent(%)"))) +
      ggtitle(ifelse (input$app == "Risk tier by Count","FreqPercent",ifelse (input$app =="Risk tier by TR","TotalNRinMPercent","TotalCNRinMPercent"))) +
      geom_text(aes(label = fun.y), position = position_dodge(0),vjust = -0.2) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   
  }
  
  FUN2 <- function(fun.data, fun.y) {
    fun.data$fun.y <- fun.data[, fun.y]
    ggplot(fun.data, aes(x=Quarter, y=fun.y , group = RiskTierDesc , colour= RiskTierDesc)) + 
      geom_line(aes(size= RiskTierDesc)) +
      #geom_point() + ylim(0,100) +
      geom_point() + expand_limits(y=0) + 
      #scale_y_continuous(labels = percent) +
      scale_color_manual(values=c("red","orange","green")) +
      scale_size_manual(values=c(1,1,1)) +
      labs( x = "Quarter", y = ifelse (input$app == "Risk tier by Count","Freq",ifelse(input$app == "Risk tier by TR","TotalNRinM","TotalCNRinM"))) +
      ggtitle(ifelse (input$app == "Risk tier by Count","Freq",ifelse(input$app == "Risk tier by TR","TotalNRinM","TotalCNRinM"))) +
      geom_text(aes(label = fun.y), position = position_dodge(0),vjust = -0.2) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   
  }
  
  
  output$table2 <- renderText(PP1())
  
  output$table <- renderTable(df())
  
  PP  <- reactive({colnames(df()[grepl("^Freq",colnames(df()))])})
  PP1 <- reactive({colnames(df()[grepl("^TR",colnames(df()))])})
  PP2 <- reactive({colnames(df()[grepl("^TCR",colnames(df()))])})
  
  output$l<- renderPlot({
    if (XX()==1 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN1(df(), PP()[2]))}
    if (XX()==2 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN1(df(), PP1()[2]))}
    if (XX()==3 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN1(df(), PP2()[2]))}
    
  })
  
  output$k<- renderPlot({
    
    if (XX()==1 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN2(df(), PP()[1]))}
    if (XX()==2 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN2(df(), PP1()[1]))}
    if (XX()==3 && (cond0() == 1||cond3() == 1||cond3.1() == 1||cond2() == 1||cond2.1() == 1||cond6() == 1||cond6.1() == 1||cond2.3() == 1||cond3.3() == 1||cond1() == 1||cond1.1() == 1||cond5() == 1||cond5.1() == 1||cond1.3() == 1||cond3.2() == 1||cond4() == 1||cond4.1() == 1||cond7() == 1||cond1.2() == 1||cond2.2() == 1||cond7.1() == 1||cond4.2() == 1||cond5.2() == 1||cond6.2() == 1||cond1.4() == 1||cond2.4() == 1||cond3.4() == 1)) {plot(FUN2(df(), PP2()[1]))}
    
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(df(), filename, row.names = TRUE)
    }
  )
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
