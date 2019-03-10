library(shinydashboard)
library(rhandsontable)
library(deSolve)
options(shiny.sanitize.errors = TRUE)

#*************************************************************************************************
sidebar <- dashboardSidebar(
  

  #*************************************************************************************************
  sidebarMenu(id="tabs",
              menuItem("Introdução", tabName="introducao", icon=icon("heartbeat"), selected=TRUE),
              menuItem("Gráfico", tabName="plot", icon=icon("line-chart"),selected=FALSE),
              menuItem("Códigos",  icon = icon("file-text-o"),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))),
              menuItem("Questionário", tabName = "question", icon = icon("question-circle")),
              menuItem("Sobre", tabName = "about", icon = icon("info-circle"))),
  
  #*************************************************************************************************
  hr()

)

#*************************************************************************************************
body <- dashboardBody(
  tabItems(
    
    #*************************************************************************************************
    #tabItem(tabName = "introducao", withMathJax(), includeHTML("/home/adw733/Shiny/introducao.html")),
    tabItem(tabName = "introducao", withMathJax(), includeHTML("/srv/shiny-server/SCHSim/introducao.html")),
    
    tabItem(tabName = "plot",
            fluidRow(
              #*************************************************************************************************
              column(width = 3, 
                     box(width = NULL,  
                         h4("Seleção Plot"),
                         checkboxInput("k1", "PLV", TRUE),
                         checkboxInput("k2", "PA1", TRUE),
                         checkboxInput("k3", "PRV", FALSE),
                         checkboxInput("k4", "PP1", FALSE),
                         checkboxInput("k5", "FLV", FALSE),
                         checkboxInput("k6", "FA3", FALSE),
                         checkboxInput("k7", "FRV", FALSE),
                         checkboxInput("k8", "FP3", FALSE),
                         checkboxInput("k9", "SLV", FALSE),
                         checkboxInput("k10", "SRV",FALSE),
                         checkboxInput("k11", "QLA", FALSE),
                         checkboxInput("k12", "QRV", FALSE),
                         #*************************************************************************************************
                         hr(),
                         h4("Ajustes ODE"),
                         numericInput("tsim", "Tempo de Simulação", 3, max = 50, min = 0, step = 0.5),
                         numericInput("step", "Passo de integração", 400, max = 1000, min = 10, step = 10),
                         textOutput("temp")

                         )),
                     
                     
              #*************************************************************************************************
              column(width = 9,
                     box(width = NULL,
                         h4("Gráfico"),
                         tabPanel("PLV x PA1", plotOutput("plot")) 
                         )),
                             
        
              
              #*************************************************************************************************
              column(width = 9,
                     box(width = NULL,
                         h4("Tabela de Parâmetros "),
                         rHandsontableOutput('table')))
                         )),
                     
    

    #*************************************************************************************************
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                 pre(includeText("ui.R")))),
    
    #*************************************************************************************************
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                 pre(includeText("server.R")))),
    
    #tabItem(tabName = "question", uiOutput('markdown')),
    
    tabItem(tabName = "question",  
            box(width = NULL,  
                h4("1 - Pergunta 1"),
                  checkboxInput("ex_1_a", "a) Alternativa A", FALSE),
                  checkboxInput("ex_1_b", "b) Alternativa B", FALSE),
                  checkboxInput("ex_1_c", "c) Alternativa C", FALSE),
                  checkboxInput("ex_1_d", "d) Alternativa D", FALSE),
                  checkboxInput("ex_1_e", "e) Alternativa E", FALSE),
                  actionButton("con1", "Confirme"),
                  textOutput("resp1")),
            box(width = NULL,  
                h4("2 - Pergunta 2"),
                  checkboxInput("ex_2_a", "a) Alternativa A", FALSE),
                  checkboxInput("ex_2_b", "b) Alternativa B", FALSE),
                  checkboxInput("ex_2_c", "c) Alternativa C", FALSE),
                  checkboxInput("ex_2_d", "d) Alternativa D", FALSE),
                  checkboxInput("ex_2_e", "e) Alternativa E", FALSE),
                  actionButton("con2", "Confirme"),
                  textOutput("resp2"))
         
            ),
                                       
    
    
    #*************************************************************************************************
    #tabItem(tabName = "about", includeHTML("/home/adw733/Shiny/sobre.html"))  ) 
    tabItem(tabName = "about", includeHTML("/srv/shiny-server/SCHSim/sobre.html"))  )
)

#*************************************************************************************************

dashboardPage(dashboardHeader(title = "SCHSim"), sidebar,  body)


#*************************************************************************************************
