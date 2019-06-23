# MyShinyDashboard

setwd(choose.dir())
oppid<- read.csv("MetricsData.csv",header=TRUE,row.names = NULL,check.names = F)
oppid <- oppid[,-1]
str(oppid)

oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(Freq = length(Quarter)) %>% mutate(FreqPercent = round(((Freq/sum(Freq))*100),0)) -> GroupFreq
oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(TR = sum(TNRinM)) %>% mutate(TRPercent = round(((TR/sum(TR))*100),0)) -> GroupTR
oppid %>% group_by(Quarter,QuarterInNum,RiskTierDesc) %>% summarise(TCR = sum(TCNRinM)) %>% mutate(TCRPercent = round(((TCR/sum(TCR))*100),0)) -> GroupTCR

FileFreqTRTCR <- cbind.data.frame(GroupFreq, GroupTR[,(4:5)], GroupTCR[,(4:5)])


oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(Freq_OG = length(Quarter)) %>% mutate(Freq_OGPercent = round(((Freq_OG/sum(Freq_OG))*100),0)) -> GroupFreq_OG
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(TR_OG = sum(TNRinM)) %>% mutate(TR_OGPercent = round(((TR_OG/sum(TR_OG))*100),0)) -> GroupTR_OG  #colnames(GroupFreq_OG)#
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RiskTierDesc) %>% summarise(TCR_OG = sum(TCNRinM)) %>% mutate(TCR_OGRPercent = round(((TCR_OG/sum(TCR_OG))*100),0)) -> GroupTCR_OG

FileFreqTRTCR_OG <- cbind.data.frame(GroupFreq_OG, GroupTR_OG[,(6:7)], GroupTCR_OG[,(6:7)])


oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(Freq_Region = length(Quarter)) %>% mutate(Freq_RegionPercent = round(((Freq_Region/sum(Freq_Region))*100),0)) -> GroupFreq_Region 
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(TR_Region = sum(TNRinM)) %>% mutate(TR_RegionPercent = round(((TR_Region/sum(TR_Region))*100),0)) -> GroupTR_Region
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RiskTierDesc) %>% summarise(TCR_Region = sum(TCNRinM)) %>% mutate(TCR_RegionPercent = round(((TCR_Region/sum(TCR_Region))*100),0)) -> GroupTCR_Region

FileFreqTRTCR_Region <- cbind.data.frame(GroupFreq_Region, GroupTR_Region[,(6:7)], GroupTCR_Region[,(6:7)])

oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_RBE = length(Quarter)) %>% mutate(Freq_RBEPercent = round(((Freq_RBE/sum(Freq_RBE))*100),0)) -> Group_Freq_RBE 
oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_RBE = sum(TNRinM)) %>% mutate(TR_RBEPercent = round(((TR_RBE/sum(TR_RBE))*100),0)) -> GroupTR_RBE
oppid %>% group_by(Quarter,QuarterInNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_RBE = sum(TCNRinM)) %>% mutate(TCR_RBEPercent = round(((TCR_RBE/sum(TCR_RBE))*100),0)) -> GroupTCR_RBE

FileFreqTRTCR_RBE <- cbind.data.frame(Group_Freq_RBE, GroupTR_RBE[,(6:7)], GroupTCR_RBE[,(6:7)])


oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_Region_RBE = length(Quarter)) %>% mutate(Freq_Region_RBEPercent = round(((Freq_Region_RBE/sum(Freq_Region_RBE))*100),0)) -> GroupFreq_Region_RBE 
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_Region_RBE = sum(TNRinM)) %>% mutate(TR_Region_RBEPercent = round(((TR_Region_RBE/sum(TR_Region_RBE))*100),0)) -> GroupTR_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_Region_RBE = sum(TCNRinM)) %>% mutate(TCR_Region_RBEPercent = round(((TCR_Region_RBE/sum(TCR_Region_RBE))*100),0)) -> GroupTCR_Region_RBE

FileFreqTRTCR__Region_RBE <- cbind.data.frame(GroupFreq_Region_RBE, GroupTR_Region_RBE[,(8:9)], GroupTCR_Region_RBE[,(8:9)])


oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_OG_RBE = length(Quarter)) %>% mutate(Freq_OG_RBEPercent = round(((Freq_OG_RBE /sum(Freq_OG_RBE ))*100),0)) -> GroupFreq_OG_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_OG_RBE = sum(TNRinM)) %>% mutate(TR_OG_RBEPercent = round(((TR_OG_RBE/sum(TR_OG_RBE))*100),0)) -> GroupTR_OG_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_OG_RBE = sum(TCNRinM)) %>% mutate(TCR_OG_RBEPercent = round(((TCR_OG_RBE/sum(TCR_OG_RBE))*100),0)) -> GroupTCR_OG_RBE

FileFreqTRTCR_OG_RBE <- cbind.data.frame(GroupFreq_OG_RBE, GroupTR_OG_RBE[,(8:9)], GroupTCR_OG_RBE[,(8:9)])

oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(Freq_OG_Region = length(Quarter)) %>% mutate(Freq_OG_RegionPercent = round(((Freq_OG_Region /sum(Freq_OG_Region))*100),0)) -> GroupFreq_OG_Region  #colnames(GroupTR_OG_Region_RBE)
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(TR_OG_Region = sum(TNRinM)) %>% mutate(TR_OG_RegionPercent = round(((TR_OG_Region/sum(TR_OG_Region))*100),0)) -> GroupTR_OG_Region
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RiskTierDesc) %>% summarise(TCR_OG_Region = sum(TCNRinM)) %>% mutate(TCR_OG_RegionPercent = round(((TCR_OG_Region/sum(TCR_OG_Region))*100),0)) -> GroupTCR_OG_Region

FileFreqTRTCR_OG_Region <- cbind.data.frame(GroupFreq_OG_Region, GroupTR_OG_Region[,(8:9)], GroupTCR_OG_Region[,(8:9)])

oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(Freq_OG_Region_RBE = length(Quarter)) %>% mutate(Freq_OG_Region_RBEPercent = round(((Freq_OG_Region_RBE /sum(Freq_OG_Region_RBE))*100),0)) -> GroupFreq_OG_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TR_OG_Region_RBE = sum(TNRinM)) %>% mutate(TR_OG_Region_RBEPercent = round(((TR_OG_Region_RBE/sum(TR_OG_Region_RBE))*100),0)) -> GroupTR_OG_Region_RBE
oppid %>% group_by(Quarter,QuarterInNum,OG,OGNum,REG,REGNum,RBE,RBENum,RiskTierDesc) %>% summarise(TCR_OG_Region_RBE = sum(TCNRinM)) %>% mutate(TCR_OG_Region_RBEPercent = round(((TCR_OG_Region_RBE/sum(TCR_OG_Region_RBE))*100),0)) -> GroupTCR_OG_Region_RBE

FileFreqTRTCR_OG_Region_RBE <- cbind.data.frame(GroupFreq_OG_Region_RBE, GroupTR_OG_Region_RBE[,(10:11)], GroupTCR_OG_Region_RBE[,(10:11)])



ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  
  dashboardSidebar(
    checkboxGroupInput("app", 
                       "Select:", 
                       choiceNames = c("Risk tier by Count","Risk tier by TR","Risk tier by TCNR"),
                       choiceValues = c("Risk tier by Count","Risk tier by TR","Risk tier by TCNR"),
                       selected = NULL,
                       inline = T
                       # multiple = F,
                       # selectize = F,
                       # width = NULL,
                       # size = NULL
    ),
    sliderTextInput("Quarter","Select Quarter:",
                    choices =  c("Fy17Q1","Fy17Q2","Fy17Q3","Fy17Q4","Fy18Q1","Fy18Q2","Fy18Q3","Fy18Q4"),
                    selected =  c("Fy17Q2","Fy18Q1")),
    checkboxGroupInput(inputId="variable", label="OG to show:", 
                       choiceNames=c("All","CMT","FS","HPS","PRD","RES"),
                       choiceValues=c("All","CMT","FS","HPS","PRD","RES"),
                       #selected = NULL,
                       inline = T
                       #, multiple = T,selectize = T
    ),
    checkboxGroupInput(inputId="variable1", label="Region to show:", 
                       choiceNames=c("All","AAPAC","Europe","LA","Na"), 
                       choiceValues=c("All","AAPAC","Europe","LA","Na"),
                      # selected = NULL,
                       inline = T
                       #, multiple = T,selectize = T
    ),
    checkboxGroupInput(inputId="variable2", label="RBE to show:", 
                       choiceNames=c("All","Digital","OG","Operations","Security","Strategy","Technology"),
                       choiceValues=c("All","Digital","OG","Operations","Security","Strategy","Technology"),
                       #selected = NULL,
                       inline = T
                       #, multiple = T,selectize = T
    ),
    # conditionalPanel(
    #   condition = "input.app == 'Risk tier by Count'|'Risk tier by TR'|'Risk tier by TCNR'",
    #   df <- Group),
    downloadButton('downloadData', 'Download Data'),
    tags$style(type='text/css', "button#downloadData { margin-bottom: 9px; }")),
  
  
  
  dashboardBody(
    fluidRow(
      
      column(width = 12,
             box(solidHeader = TRUE 
                 ,collapsible = TRUE,align="center",title = "PercentPlots",status = "warning", plotOutput("l", height = "300px"), width = 12)),
      
      column(width = 12,
             box(solidHeader = TRUE 
                 ,collapsible = TRUE,collapsed = TRUE, align="center",title = "TotalPlots",status = "warning", plotOutput("k", height = "300px"), width = 12)
      ),
      
      textOutput('table1'),
      textOutput('table2'),
      textOutput('table3'),
      textOutput('table4'),
      tableOutput('table')
      
    )))

server <- function(input, output, session) {
   
XX <- reactive({
    req(input$app)
    sapply(input$app, switch, 
           "Risk tier by Count" = 1,"Risk tier by TR" = 2,"Risk tier by TCNR" = 3)
  })
  LL1 <- reactive(QUAR()[1])
  LL2 <- reactive(QUAR()[2])
  
  QUAR <- reactive({
    req(input$Quarter)
    sapply(input$Quarter, switch,
           "Fy17Q1" = 1, "Fy17Q2" = 2, "Fy17Q3" = 3, "Fy17Q4" = 4, "Fy18Q1" = 5, "Fy18Q2" = 6, "Fy18Q3" = 7, "Fy18Q4" = 8)
  })
  
  
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
  
  df <-reactive({
    # if(XX() == 1){ ifelse(is.null(input$variable) && is.null(input$variable1) && is.null(input$variable2),df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:5)],
    #                       ifelse((OG() %in% c(2:6)) && is.null(input$variable1) && is.null(input$variable2)))
    # 
    if(XX() == 1){if (is.null(input$variable) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:5)]
  }
    else  if ((OG() %in% c(2:6)) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:7)]
  }
      #NW
    else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:7)]

    }
      #NW
    else  if ((is.null(input$variable)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))){df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:7)]

    }
      #Working
    else  if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:9)]
    }
      #NW
    else  if ((OG() %in% c(2:6)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:9)]

    }
      #NW
    else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR__Region_RBE[((FileFreqTRTCR__Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR__Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR__Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR__Region_RBE$RBENum %in% c(RBE()))),c(1:9)]

    }
      #Working
    else if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:11)]
    }
      #browser()   
      }


  else if (XX() == 2){if (is.null(input$variable) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:3,6,7)]
  }
    else  if ((OG() %in% c(2:6)) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:5,8,9)]
    }
    #NW
    else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:5,8,9)]
    }
    #NW
    else  if ((is.null(input$variable)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))){df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:5,8,9)]
    }
    #Working
    else  if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,10,11)]
    }
    #NW
    else  if ((OG() %in% c(2:6)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,10,11)]
    }
    #NW
    else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR__Region_RBE[((FileFreqTRTCR__Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR__Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR__Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR__Region_RBE$RBENum %in% c(RBE()))),c(1:7,10,11)]
    }
    else if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,12,13)]
    } }

    else if (XX() == 3){if (is.null(input$variable) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR[((FileFreqTRTCR$QuarterInNum >= LL1()) & (FileFreqTRTCR$QuarterInNum <= LL2())),c(1:3,8,9)]
    }
      else  if ((OG() %in% c(2:6)) && is.null(input$variable1) && is.null(input$variable2)){df <- FileFreqTRTCR_OG[((FileFreqTRTCR_OG$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG$OGNum %in% c(OG()))),c(1:5,10,11)]
      }
      #NW
      else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_Region[((FileFreqTRTCR_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_Region$REGNum %in% c(Region()))),c(1:5,10,11)]
      }
      #NW
      else  if ((is.null(input$variable)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))){df <- FileFreqTRTCR_RBE[((FileFreqTRTCR_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_RBE$RBENum %in% c(RBE()))),c(1:5,10,11)]
      }
      #Working
      else  if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (is.null(input$variable2))){df <- FileFreqTRTCR_OG_Region[((FileFreqTRTCR_OG_Region$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region$REGNum %in% c(Region()))),c(1:7,12,13)]
      }
      #NW
      else  if ((OG() %in% c(2:6)) && (is.null(input$variable1)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_RBE[((FileFreqTRTCR_OG_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_RBE$RBENum %in% c(RBE()))),c(1:7,12,13)]
      }
      #NW
      else  if ((is.null(input$variable)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR__Region_RBE[((FileFreqTRTCR__Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR__Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR__Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR__Region_RBE$RBENum %in% c(RBE()))),c(1:7,12,13)]
      }
      else if ((OG() %in% c(2:6)) && (Region() %in% c(2:5)) && (RBE() %in% c(2:7))) {df <- FileFreqTRTCR_OG_Region_RBE[((FileFreqTRTCR_OG_Region_RBE$QuarterInNum >= LL1()) & (FileFreqTRTCR_OG_Region_RBE$QuarterInNum <= LL2()) & (FileFreqTRTCR_OG_Region_RBE$OGNum %in% c(OG())) & (FileFreqTRTCR_OG_Region_RBE$REGNum %in% c(Region())) & (FileFreqTRTCR_OG_Region_RBE$RBENum %in% c(RBE()))),c(1:9,14,15)]
      } }
    
    
    return(df)
  })
  
  
    output$table2 <- renderText(is.null(input$variable))
      #eval(parse(text = Region1())))## # OG
    output$table3 <- renderText(is.null(input$variable1)) # Region
    output$table4 <- renderText(is.null(input$variable2)) # RBE
  
  output$table <- renderTable(df())
  
  
}

runApp(shinyApp(ui, server),launch.browser = TRUE)

