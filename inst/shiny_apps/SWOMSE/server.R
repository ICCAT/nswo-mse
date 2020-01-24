library(shiny)


shinyServer(function(input, output, session) {

  # ---- OM 1 ----
  # Natural Mortality
  output$M_cb_1 <- renderUI({
    choices <- unique(OMAssumption$M) %>% sort()
    checkboxGroupInput("M_cb_1","Natural mortality (M)", choices = choices, selected = choices[1])
  })
  # sigma R
  output$sigmaR_cb_1 <- renderUI({
    choices <- unique(OMAssumption$sigmaR) %>% sort()
    checkboxGroupInput("sigmaR_cb_1","Recruitment variability (sigmaR)", choices = choices, selected = choices[1])
  })
  # steepness
  output$steepness_cb_1 <- renderUI({
    choices <- unique(OMAssumption$steepness) %>% sort()
    checkboxGroupInput("steepness_cb_1","Steepness (h)", choices = choices, selected = choices[1])
  })
  # cpuecv
  output$cpuecv_cb_1 <- renderUI({
    choices <- unique(OMAssumption$cpuecv) %>% sort()
    checkboxGroupInput("cpuecv_cb_1","CPUE CV", choices = choices, selected = choices[1])
  })
  # Effective sample size
  output$ESS_cb_1 <- renderUI({
    choices <- unique(OMAssumption$ess) %>% sort()
    checkboxGroupInput("ESS_cb_1","Effective Sample Size", choices = choices, selected = choices[1])
  })
  # Increase in q
  output$llq_cb_1 <- renderUI({
    choices <- unique(OMAssumption$llq) %>% sort()
    checkboxGroupInput("llq_cb_1","Increase in q?", choices = choices, selected = choices[1])
  })
  # Environmental covariate
  output$env_cb_1 <- renderUI({
    choices <- unique(OMAssumption$env) %>% sort()
    checkboxGroupInput("env_cb_1","Environmental covariate?", choices = choices, selected = choices[1])
  })

  # Observe events
  observeEvent(input$OM1,{
    OMind<-match(input$OM1,OMnames)
    updateCheckboxGroupInput(session,"M_cb_1",selected=OMAssumption$M[OMind])
    updateCheckboxGroupInput(session,"sigmaR_cb_1",selected=OMAssumption$sigmaR[OMind])
    updateCheckboxGroupInput(session,"steepness_cb_1",selected=OMAssumption$steepness[OMind])
    updateCheckboxGroupInput(session,"cpuecv_cb_1",selected=OMAssumption$cpuecv[OMind])
    updateCheckboxGroupInput(session,"ESS_cb_1",selected=OMAssumption$ess[OMind])
    updateCheckboxGroupInput(session,"llq_cb_1",selected=OMAssumption$llq[OMind])
    updateCheckboxGroupInput(session,"env_cb_1",selected=OMAssumption$env[OMind])

  })

  # Get OM Numbers
  getOM_1 <-function() {
    OMAssumption$Name[OMAssumption$M %in% input$M_cb_1 &
                       OMAssumption$sigmaR %in% input$sigmaR_cb_1 &
                       OMAssumption$steepness %in% input$steepness_cb_1 &
                       OMAssumption$cpuecv %in% input$cpuecv_cb_1 &
                       OMAssumption$ess %in% input$ESS_cb_1 &
                       OMAssumption$llq %in% input$llq_cb_1 &
                       OMAssumption$env %in% input$env_cb_1]
  }

  output$OM1_nums<-renderText({
    getOM_1()
  })


  # ---- OM 2 ----
  # Natural Mortality
  output$M_cb_2 <- renderUI({
    choices <- unique(OMAssumption$M) %>% sort()
    checkboxGroupInput("M_cb_2","Natural mortality (M)", choices = choices, selected = choices[1])
  })
  # sigma R
  output$sigmaR_cb_2 <- renderUI({
    choices <- unique(OMAssumption$sigmaR) %>% sort()
    checkboxGroupInput("sigmaR_cb_2","Recruitment variability (sigmaR)", choices = choices, selected = choices[1])
  })
  # steepness
  output$steepness_cb_2 <- renderUI({
    choices <- unique(OMAssumption$steepness) %>% sort()
    checkboxGroupInput("steepness_cb_2","Steepness (h)", choices = choices, selected = choices[1])
  })
  # cpuecv
  output$cpuecv_cb_2 <- renderUI({
    choices <- unique(OMAssumption$cpuecv) %>% sort()
    checkboxGroupInput("cpuecv_cb_2","CPUE CV", choices = choices, selected = choices[1])
  })
  # Effective sample size
  output$ESS_cb_2 <- renderUI({
    choices <- unique(OMAssumption$ess) %>% sort()
    checkboxGroupInput("ESS_cb_2","Effective Sample Size", choices = choices, selected = choices[1])
  })
  # Increase in q
  output$llq_cb_2 <- renderUI({
    choices <- unique(OMAssumption$llq) %>% sort()
    checkboxGroupInput("llq_cb_2","Increase in q?", choices = choices, selected = choices[1])
  })
  # Environmental covariate
  output$env_cb_2 <- renderUI({
    choices <- unique(OMAssumption$env) %>% sort()
    checkboxGroupInput("env_cb_2","Environmental covariate?", choices = choices, selected = choices[1])
  })

  # Observe events
  observeEvent(input$OM2,{
    OMind<-match(input$OM2,OMnames)
    updateCheckboxGroupInput(session,"M_cb_2",selected=OMAssumption$M[OMind])
    updateCheckboxGroupInput(session,"sigmaR_cb_2",selected=OMAssumption$sigmaR[OMind])
    updateCheckboxGroupInput(session,"steepness_cb_2",selected=OMAssumption$steepness[OMind])
    updateCheckboxGroupInput(session,"cpuecv_cb_2",selected=OMAssumption$cpuecv[OMind])
    updateCheckboxGroupInput(session,"ESS_cb_2",selected=OMAssumption$ess[OMind])
    updateCheckboxGroupInput(session,"llq_cb_2",selected=OMAssumption$llq[OMind])
    updateCheckboxGroupInput(session,"env_cb_2",selected=OMAssumption$env[OMind])

  })
  # Get OM Numbers
  getOM_2 <-function() {
    OMAssumption$Name[OMAssumption$M %in% input$M_cb_2 &
                       OMAssumption$sigmaR %in% input$sigmaR_cb_2 &
                       OMAssumption$steepness %in% input$steepness_cb_2 &
                       OMAssumption$cpuecv %in% input$cpuecv_cb_2 &
                       OMAssumption$ess %in% input$ESS_cb_2 &
                       OMAssumption$llq %in% input$llq_cb_2 &
                       OMAssumption$env %in% input$env_cb_2]
  }

  output$OM2_nums<-renderText({
    getOM_2()
  })

  # SubMSE
  SubMSE <- function(SWO_MSE, names) {
    sims <- which(SWOM@Source %in% names)
    MSE <- DLMtool::Sub(SWO_MSE, sims=sims)
    if (MSE@nsim ==1) {
      MSE <- joinMSE(list(MSE, MSE)) # temp fix for only 1 sim per OM
    }
    MSE
  }

  # Performance table
  makeTable <- function(names) {
    MSE <- SubMSE(SWO_MSE, names)
    tt <- summary(MSE)
    tt
  }
  output$PTable1 <- DT::renderDataTable(makeTable(getOM_1()),
                                        options = list(dom = 't'))
  output$PTable2 <- DT::renderDataTable(makeTable(getOM_2()),
                                        options = list(dom = 't'))
  # Zeh plots
  # Wormplots
  makeZehPlot <- function(names) {
    MSE <- SubMSE(SWO_MSE, names)
    DLMtool::DFO_bar(MSE)
  }

  output$zehplot1<-renderPlot(makeZehPlot(getOM_1()))
  output$zehplot2<-renderPlot(makeZehPlot(getOM_2()))

  # Wormplots
  makeWormPlot <- function(names) {
    MSE <- SubMSE(SWO_MSE, names)
    DLMtool::wormplot(MSE)
  }
  output$wormplot1 <- renderPlot(makeWormPlot(getOM_1()))
  output$wormplot2 <- renderPlot(makeWormPlot(getOM_2()))

  # Trade-off plots
  makeTradeoffPlot <- function(names, PM1, PM2) {
    MSE <<- SubMSE(SWO_MSE, names)
    TradePlot(MSE, Lims=0, PMlist=c(PM1, PM2))
  }

  output$tplot1 <- renderPlot(makeTradeoffPlot(getOM_1(), input$T_PMx, input$T_PMy))
  output$tplot2 <- renderPlot(makeTradeoffPlot(getOM_2(), input$T_PMx, input$T_PMy))

})
