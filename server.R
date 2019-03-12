library(shinydashboard)
library(plotly)
library(rhandsontable)
library(deSolve)
library(tictoc)
options(shiny.sanitize.errors = TRUE)


shinyServer(function(input, output, session){      
  
  
  #*********************************************************************************************
  #Importa os dados de um arquivo do Excel e monta a tabela
  data <- rio::import("/srv/shiny-server/SCHSim/variaveis.xlsx")
  
  #Criando um data frame
  df = data.frame(data)
  
  #Definir um objeto que sempre quando houver alterações afete todo o projeto
  datavalues <- reactiveValues(data=df)
  
  # Mostra o
  output$table <- renderRHandsontable({rhandsontable(datavalues$data, manualColumnResize = TRUE,
                                       width = 850, height = 250, search = TRUE)%>%
                  hot_cols(colWidths = c(60, 80, 120,510)) %>%
                  hot_col(col = 1, readOnly = TRUE,halign = "htCenter")%>%
                  hot_col(col = 2, readOnly = FALSE, format = "0.000",halign = "htCenter")%>%
                  hot_col(col = 3, readOnly = TRUE,halign = "htCenter")%>%
                  hot_col(col = 4, readOnly = TRUE,halign = "htCenter") })
  
  #Importa os dados de um arquivo do Excel e monta a tabela
  #*********************************************************************************************
  
  
  
  
  #*********************************************************************************************
  #Verifica alterações na tabela
  observeEvent(
    (input$table$changes$changes), #Observa qualquer alteração na tabela
    {
      datavalues$data <- hot_to_r(input$table) # Converte os dados da tabela em em um data frame

    })
  #Verifica alterações na tabela
  #*********************************************************************************************
 
  #*********************************************************************************************
  #Verifica alteração nos campos calcula a ODE
  observeEvent(((input$tsim)|(input$step)|(unlist((input$table$changes$changes), use.names=FALSE))), {
  
    tic()   
   
    #Lê a variável transformada para data.frame
    data=datavalues$data
    
    #************************************************************************************************
    #Constantes do modelo
    RPW1  <- data[1,2]   # Resistência Artéria Pulmonar 0
    RP1   <- data[2,2]   # Resistência Artéria Pulmonar 1
    RP2   <- data[3,2]   # Resistência Artéria Pulmonar 2
    RP3   <- data[4,2]   # Resistência Artéria Pulmonar 3
    RL1   <- data[5,2]   # Resistência Veia Pulmonar 1
    RL2   <- data[6,2]   # Resistência Veia Pulmonar 2
    RLA   <- data[7,2]   # Resistência Átrio Esquerdo
    RLV   <- data[8,2]   # Resistência Ventrículo Esquerdo
    RA1   <- data[9,2]   # Resistência Artéria Aorta 0
    RA2   <- data[10,2]   # Resistência Artéria Aorta 1
    RA3   <- data[11,2]   # Resistência Artéria Aorta 2
    RV1   <- data[12,2]   # Resistência Circulação Sistêmica
    RV2   <- data[13,2]   # Resistência Veias Sistêmicas 1
    RRA   <- data[14,2]   # Resistência Veias Sistêmicas 2
    RRV   <- data[15,2]   # Resistência Átrio Direito
    RPW2  <- data[16,2]   # Resistência Ventrículo Direito
    LP1   <- data[17,2]   # Inércia Artéria Pulmonar
    LL2   <- data[18,2]   # Inércia Veia Pulmonar
    LLA   <- data[19,2]   # Inércia Átrio Esquerdo 
    LLV   <- data[20,2]   # Inércia Ventrículo Esquerdo 
    LA1   <- data[21,2]   # Inércia Artéria Aorta
    LV2   <- data[22,2]   # Inércia Veias Sistêmicas
    LRA   <- data[23,2]   # Inércia Átrio Direito
    LRV   <- data[24,2]   # Inércia Ventrículo Direito
    CP1  <- data[25,2]   # Complacência Artéria Pulmonar 1
    CP2  <- data[26,2]   # Complacência Artéria Pulmonar 2
    CP3  <- data[27,2]   # Complacência Artéria Pulmonar 3
    CL1  <- data[28,2]   # Complacência Veia Pulmonar 1
    CL2  <- data[29,2]   # Complacência Veia Pulmonar 2
    CLA  <- data[30,2]   # Complacência Átrio Esquerdo
    CA1  <- data[31,2]   # Complacência Artéria Aorta 1
    CA2  <- data[32,2]   # Complacência Artéria Aorta 2
    CA3  <- data[33,2]   # Complacência Circulação Sistêmica
    CV1  <- data[34,2]   # Complacência Veias Sistêmicas 1
    CV2  <- data[35,2]   # Complacência Veias Sistêmicas 2
    CRA  <- data[36,2]   # Complacência Átrio Direito
    PP1EDM  <- data[37,2]   # Pressão Diastólica Final Artéria Pulmonar 1
    PP2EDM  <- data[38,2]   # Pressão Diastólica Final  Artéria Pulmonar 2
    PP3EDM  <- data[39,2]   # Pressão Diastólica Final  Artéria Pulmonar 3
    PL1EDM  <- data[40,2]   # Pressão Diastólica Final  Veias Pulmonares 1
    PL2EDM  <- data[41,2]   # Pressão Diastólica Final  Veias Pulmonares 2
    PLAEDM  <- data[42,2]   # Pressão Diastólica Final  Átrio Esquerdo
    PLVEDM  <- data[43,2]   # Pressão Diastólica Final  Ventrículo Esquerdo
    PA1EDM  <- data[44,2]   # Pressão Diastólica Final  Artéria Aorta 1 
    PA2EDM  <- data[45,2]   # Pressão Diastólica Final  Artéria Aorta 2
    PA3EDM  <- data[46,2]   # Pressão Diastólica Final  Artéria Sistêmica
    PV1EDM  <- data[47,2]   # Pressão Diastólica Final  Veias Sistêmicas 1
    PV2EDM  <- data[48,2]   # Pressão Diastólica Final  Veias Sistêmicas 2 
    PRAEDM  <- data[49,2]   # Pressão Diastólica Final  Átrio Direito
    PRVEDM  <- data[50,2]   # Pressão Diastólica Final  Ventrículo Direito
    FLAIC  <- data[51,2]   # Fluxo Inicial Átrio Esquerdo
    FLVIC  <- data[52,2]   # Fluxo Inicial Ventrículo Esquerdo
    FA1IC  <- data[53,2]   # Fluxo Inicial Artéria Aorta
    FV2IC  <- data[54,2]   # Fluxo Inicial Veias Sistêmicas
    FRAIC  <- data[55,2]   # Fluxo Inicial Átrio Direito
    FRVIC  <- data[56,2]   # Fluxo Inicial Ventrículo Direito
    FP1IC  <- data[57,2]   # Fluxo Inicial Artéria Pulmonar
    FL2IC  <- data[58,2]   # Fluxo Inicial Veias Pulmonares
    FIS <- data[59,2]   # Sudden change in FLV at t > TIS
    QP1U <- data[60,2]   # Pulm. Art. 1 Unstressed Volume
    QP2U <- data[61,2]   # Pulm. Art. 2 Unstressed Volume
    QP3U <- data[62,2]   # Pulm. Art. 3 Unstressed Volume
    QL1U <- data[63,2]   # Pulm. Veins 1 Unstressed Volume
    QL2U <- data[64,2]   # Pulm. Veins 2 Unstressed Volume
    QLAU <- data[65,2]   # Left Atrium Unstressed Volume
    QLVU <- data[66,2]   # Left Ventricle Unstressed Volume
    QA1U <- data[67,2]   # Aortic 1 Unstressed Volume
    QA2U <- data[68,2]   # Aortic 2 Unstressed Volume
    QA3U <- data[69,2]   # Syst. Art. Unstressed Volume
    QV1U <- data[70,2]   # Syst. Veins 1 Unstressed Volume
    QV2U <- data[71,2]   # Syst. Veins 2 Unstressed Volume
    QRAU <- data[72,2]   # Right Atrium Unstressed Volume
    QRVU <- data[73,2]   # Right Ventricle Unstressed Volume
    LD <- data[74,2]   # Diastolic L Ventricle Stiffness
    LSI <- data[75,2]   # Systolic L Ventricle Init. Stiffness
    RD <- data[76,2]   # Diastolic R Ventricle Stiffness
    RSI <- data[77,2]   # Systolic R Ventricle Init. Stiffness
    DLS <- data[78,2]   # Sudden change in LS at t > THI
    DRS <- data[79,2]   # Sudden change in RD at t > THI
    
    #*******************************************************************************************
    QLAIC = QLAU + PLAEDM * CLA
    QLVIC = QLVU + PLVEDM / LD
    QA1IC = QA1U + PA1EDM * CA1
    QA2IC = QA2U + PA2EDM * CA2
    QA3IC = QA3U + PA3EDM * CA3
    QV1IC = QV1U + PV1EDM * CV1
    QV2IC = QV2U + PV2EDM * CV2
    QRAIC = QRAU + PRAEDM * CRA
    QRVIC = QRVU + PRVEDM / RD
    QP1IC = QP1U + PP1EDM * CP1
    QP2IC = QP2U + PP2EDM * CP2
    QP3IC = QP3U + PP3EDM * CP3
    QL1IC = QL1U + PL1EDM * CL1
    QL2IC = QL2U + PL2EDM * CL2
    
    SV1 = .9	      # Stiffness first harmonic factor
    SV2 = .25	    # Stiffness second harmonic factor
    
    tsim = input$tsim #Tempo de simulação
    TH = 0.8          #Perido do ciclo cardiaco
    TS = 0.3          #Periodo sístole
    step <- input$step      # Precisão da amostra
    
    #Vetor com as condições inicias ODE
    y0 = c(QLA=QLAIC, QLV=QLVIC, QA1=QA1IC, QA2=QA2IC, QA3=QA3IC, QV1=QV1IC, 
           QV2=QV2IC, QRA=QRAIC, QRV=QRVIC, QP1=QP1IC, QP2=QP2IC, QP3=QP3IC,
           QL1=QL1IC, QL2=QL2IC, FLA=FLAIC, FLV=FLVIC, FA1=FA1IC, FV2=FV2IC, 
           FRA=FRAIC, FRV=FRVIC, FP1=FP1IC, FL2=FL2IC)    
    
    t <- seq(0,tsim,length=step)	    #Vetor tempo
    
    
    #************************************************************************************************
    #Função piecewise
    fpw <- function(t) 
    {
      return(ifelse(t%%TH >= 0  & t%%TH <  TS, ((SV1*sin((t%%TH*pi)/TS))-(SV2*sin((2*pi*t%%TH)/TS))), 
                    ifelse(t%%TH >= TS & t%%TH <= TH,0,0)))
      
    }
    
    #************************************************************************************************
    #Função que resolve a ODE
    odefcn <- function(t, y, p) 
    { 
      QLA=y[1]
      QLV=y[2] 
      QA1=y[3] 
      QA2=y[4]  
      QA3=y[5]  
      QV1=y[6]  
      QV2=y[7]
      QRA=y[8] 
      QRV=y[9] 
      QP1=y[10] 
      QP2=y[11]
      QP3=y[12]
      QL1=y[13]
      QL2=y[14]
      FLA=y[15]
      FLV=y[16]
      FA1=y[17]  
      FV2=y[18] 
      FRA=y[19]
      FRV=y[20]
      FP1=y[21] 
      FL2=y[22] 
      
      SSW <- fpw(t)
      ACT <- pmin(pmax(SSW, 0), 1)
      SLV <- LD * (1 - ACT) + LSI * ACT
      SRV <- RD * (1 - ACT) + RSI * ACT
      
      PLA <- (QLA-QLAU)/CLA
      PLV <- (QLV-QLVU)*SLV
      PA1 <- ((QA1-QA1U)/CA1)+RPW2*(FLV-FA1)
      PA2 <- (QA2-QA2U)/CA2
      PA3 <- (QA3-QA3U)/CA3
      PV1 <- (QV1-QV1U)/CV1
      PV2 <- (QV2-QV2U)/CV2
      PRA <- (QRA-QRAU)/CRA
      PRV <- (QRV-QRVU)*SRV
      PP1 <- ((QP1-QP1U)/CP1)+RPW1*(FRV-FP1)
      PP2 <- (QP2-QP2U)/CP2
      PP3 <- (QP3-QP3U)/CP3
      PL1 <- (QL1-QL1U)/CL1
      PL2 <- (QL2-QL2U)/CL2
      
      FLAD <- (PLA-PLV-FLA*RLA)/LLA       #p/ FLA>0
      FLVD <- (PLV-PA1-FLV*RLV)/LLV       #p/ FLV>0
      FA2 <- (PA2-PA3)/RA2
      FA3 <- (PA3-PV1)/RA3
      FV1 <- (PV1-PV2)/RV1
      FRAD <- (PRA-PRV-FRA*RRA)/LRA       #p/ FRA>0
      FRVD <- (PRV-PP1-FRV*RRV)/LRV       #p/ FRV>0
      FP2 <- (PP2-PP3)/RP2
      FP3 <- (PP3-PL1)/RP3
      FL1 <- (PL1-PL2)/RL1
      
      dy1 <- FL2-FLA      #QLA
      dy2 <- FLA-FLV      #QLV
      dy3 <- FLV-FA1      #QA1
      dy4 <- FA1-FA2      #QA2
      dy5 <- FA2-FA3      #QA3
      dy6 <- FA3-FV1      #QV1
      dy7 <- FV1-FV2      #QV2
      dy8 <- FV2-FRA      #QRA
      dy9 <- FRA-FRV      #QRV
      dy10 <- FRV-FP1     #QP1 
      dy11 <- FP1-FP2     #QP2
      dy12 <- FP2-FP3     #QP3
      dy13 <- FP3-FL1     #QL1
      dy14 <- FL1-FL2     #QL2
      
      dy15 <- FLAD * (FLAD >= 0 && FLA < 1e4 || FLAD <= 0 && FLA > 0)    #FLA
      dy16 <- FLVD * (FLVD >= 0 && FLV < 1e5 || FLVD <= 0 && FLV > 0)    #FLV
      dy17 <- (PA1 - PA2 - RA1 * FA1) / LA1                              #FA1
      dy18 <- (PV2 - PRA - RV2 * FV2) / LV2                              #FV2
      dy19 <- FRAD * (FRAD >= 0 && FRA < 1e4 || FRAD <= 0 && FRA > 0)    #FRA
      dy20 <- FRVD * (FRVD >= 0 && FRV < 1e5 || FRVD <= 0 && FRV > 0)    #FRV
      dy21 <- (PP1 - PP2 - RP1 * FP1) / LP1                              #FP1
      dy22 <- (PL2 - PLA - RL2 * FL2) / LL2                              #FL2
      
      
      list(c(dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,dy9,dy10,dy11,dy12,
             dy13,dy14,dy15,dy16,dy17,dy18,dy19,dy20,dy21,dy22)) #Vetor com as equações diferenciais
    }
    
    #Configuração para a resolução da ODE
    out <- ode(func = odefcn, times = t, y = y0, parms = NULL, ynames = FALSE, method="adams") #rk4 #adams #impAdams_d #bdf_d #ode45
    
    #************************************************************************************************
    #Resultados 
    t <- out[,1] #Variável de tempo da matriz de saída
    QLA <- out[,2]
    QLV <- out[,3] 
    QA1 <- out[,4] 
    QA2 <- out[,5]  
    QA3 <- out[,6]  
    QV1 <- out[,7]  
    QV2 <- out[,8]
    QRA <- out[,9] 
    QRV <- out[,10] 
    QP1 <- out[,11] 
    QP2 <- out[,12]
    QP3 <- out[,13]
    QL1 <- out[,14]
    QL2 <- out[,15]
    FLA <- out[,16]
    FLV <- out[,17]
    FA1 <- out[,18]  
    FV2 <- out[,19] 
    FRA <- out[,20]
    FRV <- out[,21]
    FP1 <- out[,22] 
    FL2 <- out[,23] 
    
    SSW <- fpw(t)
    ACT <- pmin(pmax(SSW, 0), 1)
    SLV <- LD * (1 - ACT) + LSI * ACT
    SRV <- RD * (1 - ACT) + RSI * ACT
    
    
    PLA <- (QLA-QLAU)/CLA
    PLV <- (QLV-QLVU)*SLV
    PA1 <- ((QA1-QA1U)/CA1)+RPW2*(FLV-FA1)
    PA2 <- (QA2-QA2U)/CA2
    PA3 <- (QA3-QA3U)/CA3
    PV1 <- (QV1-QV1U)/CV1
    PV2 <- (QV2-QV2U)/CV2
    PRA <- (QRA-QRAU)/CRA
    PRV <- (QRV-QRVU)*SRV
    PP1 <- ((QP1-QP1U)/CP1)+RPW1*(FRV-FP1)
    PP2 <- (QP2-QP2U)/CP2
    PP3 <- (QP3-QP3U)/CP3
    PL1 <- (QL1-QL1U)/CL1
    PL2 <- (QL2-QL2U)/CL2
    
    FA2 <- (PA2-PA3)/RA2
    FA3 <- (PA3-PV1)/RA3
    FV1 <- (PV1-PV2)/RV1
    FP2 <- (PP2-PP3)/RP2
    FP3 <- (PP3-PL1)/RP3
    FL1 <- (PL1-PL2)/RL1
    

    x <<- t
    y1 <<- PLV
    y2 <<- PA1
    y3 <<- PRV
    y4 <<- PP1
    y5 <<- FLV
    y6 <<- FA3
    y7 <<- FRV
    y8 <<- FP3
    y9 <<- SLV
    y10 <<- SRV
    y11 <<- QLA
    y12 <<- QRV
    
    exe <- toc()
    exe <- as.numeric(exe)
    exe <- exe[2]-exe[1]
    output$temp <- renderText({exe})
  
})
  #Verifica alteração nos campos calcula a ODE
  #*********************************************************************************************
  

  
  #********************************************************************************************
  #Verifica alteração nos campos e plota os resultados
  observeEvent(((input$tsim)|(input$step)|(unlist((input$table$changes$changes), use.names=FALSE))), {
  output$plot <- renderPlotly({ 
    
    n1 = "PLV"
    n2 = "PA1"
    n3 = "PRV"
    n4 = "PP1"
    n5 = "FLV"
    n6 = "FA3"
    n7 = "FRV"
    n8 = "FP3"
    n9 = "SLV"
    n10 = "SRV"
    n11 = "QLA"
    n12 = "QRV"
    
    k1 <<- input$k1
    k2 <<- input$k2
    k3 <<- input$k3
    k4 <<- input$k4
    k5 <<- input$k5
    k6 <<- input$k6
    k7 <<- input$k7
    k8 <<- input$k8
    k9 <<- input$k9
    k10 <<- input$k10
    k11 <<- input$k11
    k12 <<- input$k12
    
    
    ifelse((k1==FALSE),(y1<-NULL)&(l1<-FALSE),(y1<-y1)&(l1<-TRUE))
    ifelse((k2==FALSE),(y2<-NULL)&(l2<-FALSE),(y2<-y2)&(l2<-TRUE))
    ifelse((k3==FALSE),(y3<-NULL)&(l3<-FALSE),(y3<-y3)&(l3<-TRUE))
    ifelse((k4==FALSE),(y4<-NULL)&(l4<-FALSE),(y4<-y4)&(l4<-TRUE))
    ifelse((k5==FALSE),(y5<-NULL)&(l5<-FALSE),(y5<-y5)&(l5<-TRUE))
    ifelse((k6==FALSE),(y6<-NULL)&(l6<-FALSE),(y6<-y6)&(l6<-TRUE))
    ifelse((k7==FALSE),(y7<-NULL)&(l7<-FALSE),(y7<-y7)&(l7<-TRUE))
    ifelse((k8==FALSE),(y8<-NULL)&(l8<-FALSE),(y8<-y8)&(l8<-TRUE))
    ifelse((k9==FALSE),(y9<-NULL)&(l9<-FALSE),(y9<-y9)&(l9<-TRUE))
    ifelse((k10==FALSE),(y10<-NULL)&(l10<-FALSE),(y10<-y10)&(l10<-TRUE))
    ifelse((k11==FALSE),(y11<-NULL)&(l11<-FALSE),(y11<-y11)&(l11<-TRUE))
    ifelse((k11==FALSE),(y12<-NULL)&(l12<-FALSE),(y12<-y12)&(l12<-TRUE))
    
    
      plot_ly(x=0, y=0,name='Legendas',type='scatter',mode='lines') %>%
      add_trace(x=x,y=y1,name=n1,mode='lines',showlegend=l1,visible=l1)%>%
      add_trace(x=x,y=y2,name=n2,mode='lines',showlegend=l2,visible=l2)%>%
      add_trace(x=x,y=y3,name=n3,mode='lines',showlegend=l3,visible=l3)%>%
      add_trace(x=x,y=y4,name=n4,mode='lines',showlegend=l4,visible=l4)%>%
      add_trace(x=x,y=y5,name=n5,mode='lines',showlegend=l5,visible=l5)%>%
      add_trace(x=x,y=y6,name=n6,mode='lines',showlegend=l6,visible=l6)%>%
      add_trace(x=x,y=y7,name=n7,mode='lines',showlegend=l7,visible=l7)%>%
      add_trace(x=x,y=y8,name=n8,mode='lines',showlegend=l8,visible=l8)%>%
      add_trace(x=x,y=y9,name=n9,mode='lines',showlegend=l9,visible=l9)%>%
      add_trace(x=x,y=y10,name=n10,mode='lines',showlegend=l10,visible=l10)%>%
      add_trace(x=x,y=y11,name=n11,mode='lines',showlegend=l11,visible=l11)%>%
      add_trace(x=x,y=y12,name=n12,mode='lines',showlegend=l12,visible=l12)
      
 
  }) 
  }) 
  #Verifica alteração nos campos e plota os resultados
  #********************************************************************************************
  
  #********************************************************************************************
  observeEvent((input$ex_1_a),{
    if(input$ex_1_a==TRUE){
      updateCheckboxInput(session,"ex_1_b", value = FALSE)
      updateCheckboxInput(session,"ex_1_c", value = FALSE)
      updateCheckboxInput(session,"ex_1_d", value = FALSE)
      updateCheckboxInput(session,"ex_1_e", value = FALSE)} })
  
  observeEvent((input$ex_1_b),{    
    if(input$ex_1_b==TRUE){
      updateCheckboxInput(session,"ex_1_a", value = FALSE)
      updateCheckboxInput(session,"ex_1_c", value = FALSE)
      updateCheckboxInput(session,"ex_1_d", value = FALSE)
      updateCheckboxInput(session,"ex_1_e", value = FALSE)} })
  
  observeEvent((input$ex_1_c),{
    if(input$ex_1_c==TRUE){
      updateCheckboxInput(session,"ex_1_a", value = FALSE)
      updateCheckboxInput(session,"ex_1_b", value = FALSE)
      updateCheckboxInput(session,"ex_1_d", value = FALSE)
      updateCheckboxInput(session,"ex_1_e", value = FALSE)} })
      
  observeEvent((input$ex_1_d),{
    if(input$ex_1_d==TRUE){
      updateCheckboxInput(session,"ex_1_a", value = FALSE)
      updateCheckboxInput(session,"ex_1_b", value = FALSE)
      updateCheckboxInput(session,"ex_1_c", value = FALSE)
      updateCheckboxInput(session,"ex_1_e", value = FALSE)} })
        
  observeEvent((input$ex_1_e),{
    if(input$ex_1_e==TRUE){
      updateCheckboxInput(session,"ex_1_a", value = FALSE)
      updateCheckboxInput(session,"ex_1_b", value = FALSE)
      updateCheckboxInput(session,"ex_1_c", value = FALSE)
      updateCheckboxInput(session,"ex_1_d", value = FALSE)} })
  
  observeEvent((input$con1),{
    if(input$ex_1_e==TRUE){
    output$resp1 <- renderText({"Você acertou!!"})
    } else {
    output$resp1 <- renderText({"Você errou."}) }  })
  #********************************************************************************************
  
  
  #********************************************************************************************
  observeEvent((input$ex_2_a),{
    if(input$ex_2_a==TRUE){
      updateCheckboxInput(session,"ex_2_b", value = FALSE)
      updateCheckboxInput(session,"ex_2_c", value = FALSE)
      updateCheckboxInput(session,"ex_2_d", value = FALSE)
      updateCheckboxInput(session,"ex_2_e", value = FALSE)} })
  
  observeEvent((input$ex_2_b),{    
    if(input$ex_2_b==TRUE){
      updateCheckboxInput(session,"ex_2_a", value = FALSE)
      updateCheckboxInput(session,"ex_2_c", value = FALSE)
      updateCheckboxInput(session,"ex_2_d", value = FALSE)
      updateCheckboxInput(session,"ex_2_e", value = FALSE)} })
  
  observeEvent((input$ex_2_c),{
    if(input$ex_2_c==TRUE){
      updateCheckboxInput(session,"ex_2_a", value = FALSE)
      updateCheckboxInput(session,"ex_2_b", value = FALSE)
      updateCheckboxInput(session,"ex_2_d", value = FALSE)
      updateCheckboxInput(session,"ex_2_e", value = FALSE)} })
  
  observeEvent((input$ex_2_d),{
    if(input$ex_2_d==TRUE){
      updateCheckboxInput(session,"ex_2_a", value = FALSE)
      updateCheckboxInput(session,"ex_2_b", value = FALSE)
      updateCheckboxInput(session,"ex_2_c", value = FALSE)
      updateCheckboxInput(session,"ex_2_e", value = FALSE)} })
  
  observeEvent((input$ex_2_e),{
    if(input$ex_2_e==TRUE){
      updateCheckboxInput(session,"ex_2_a", value = FALSE)
      updateCheckboxInput(session,"ex_2_b", value = FALSE)
      updateCheckboxInput(session,"ex_2_c", value = FALSE)
      updateCheckboxInput(session,"ex_2_d", value = FALSE)} })
  
  observeEvent((input$con2),{
    if(input$ex_2_e==TRUE){
      output$resp2 <- renderText({"Você acertou!!"})
    } else {
      output$resp2 <- renderText({"Você errou."}) }  })
  #********************************************************************************************
  
  
  
})
