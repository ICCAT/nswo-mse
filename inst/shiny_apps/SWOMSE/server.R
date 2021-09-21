library(shiny)


shinyServer(function(input, output, session) {

  # ---- OM 1 ----
  # Size Data
  output$Size_cb_1 <- renderUI({
    choices <- unique(OM_Axes$Size) %>% sort()
    checkboxGroupInput("Size_cb_1","Size Composition Data", choices = choices, selected = choices[1])
  })

  # Natural Mortality
  output$M_cb_1 <- renderUI({
    choices <- unique(OM_Axes$M) %>% sort()
    checkboxGroupInput("M_cb_1","Natural mortality (M)", choices = choices, selected = choices[1])
  })
  # sigma R
  output$sigmaR_cb_1 <- renderUI({
    choices <- unique(OM_Axes$sigmaR) %>% sort()
    checkboxGroupInput("sigmaR_cb_1","Recruitment variability (sigmaR)", choices = choices, selected = choices[1])
  })
  # steepness
  output$steepness_cb_1 <- renderUI({
    choices <- unique(OM_Axes$h) %>% sort()
    checkboxGroupInput("steepness_cb_1","Steepness (h)", choices = choices, selected = choices[1])
  })
  # Lambda - CPUE:CAL weighting
  output$lambda_cb_1 <- renderUI({
    choices <- unique(OM_Axes$lambda) %>% sort()
    checkboxGroupInput("lambda_cb_1","CPUE:CAL Weighting", choices = choices, selected = choices[1])
  })

  # Increase in q
  output$llq_cb_1 <- renderUI({
    choices <- unique(OM_Axes$incQ) %>% sort()
    checkboxGroupInput("llq_cb_1","Increase in q?", choices = choices, selected = choices[1])
  })
  # Environmental covariate
  output$env_cb_1 <- renderUI({
    choices <- unique(OM_Axes$incE) %>% sort()
    checkboxGroupInput("env_cb_1","Environmental covariate?", choices = choices, selected = choices[1])
  })

  # Observe events
  observeEvent(input$OM1,{
    OMind<-match(input$OM1,OMnames)
    updateCheckboxGroupInput(session,"Size_cb_1",selected=OM_Axes$Size[OMind])
    updateCheckboxGroupInput(session,"M_cb_1",selected=OM_Axes$M[OMind])
    updateCheckboxGroupInput(session,"sigmaR_cb_1",selected=OM_Axes$sigmaR[OMind])
    updateCheckboxGroupInput(session,"steepness_cb_1",selected=OM_Axes$h[OMind])
    updateCheckboxGroupInput(session,"lambda_cb_1",selected=OM_Axes$lambda[OMind])
    updateCheckboxGroupInput(session,"llq_cb_1",selected=OM_Axes$incQ[OMind])
    updateCheckboxGroupInput(session,"env_cb_1",selected=OM_Axes$incE[OMind])

  })

  # Get OM Numbers
  getOM_1 <-function() {
    OM_Axes$Name[OM_Axes$Size %in% input$Size_cb_1 &
                   OM_Axes$M %in% input$M_cb_1 &
                   OM_Axes$sigmaR %in% input$sigmaR_cb_1 &
                   OM_Axes$h %in% input$steepness_cb_1 &
                   OM_Axes$lambda %in% input$lambda_cb_1 &
                   OM_Axes$incQ %in% input$llq_cb_1 &
                   OM_Axes$incE %in% input$env_cb_1]
  }

  output$OM1_nums<-renderText({
    getOM_1()
  })


  # ---- OM 2 ----
  # Size Data
  output$Size_cb_2 <- renderUI({
    choices <- unique(OM_Axes$Size) %>% sort()
    checkboxGroupInput("Size_cb_2","Size Composition Data", choices = choices, selected = choices[1])
  })

  # Natural Mortality
  output$M_cb_2 <- renderUI({
    choices <- unique(OM_Axes$M) %>% sort()
    checkboxGroupInput("M_cb_2","Natural mortality (M)", choices = choices, selected = choices[1])
  })
  # sigma R
  output$sigmaR_cb_2 <- renderUI({
    choices <- unique(OM_Axes$sigmaR) %>% sort()
    checkboxGroupInput("sigmaR_cb_2","Recruitment variability (sigmaR)", choices = choices, selected = choices[1])
  })
  # steepness
  output$steepness_cb_2 <- renderUI({
    choices <- unique(OM_Axes$h) %>% sort()
    checkboxGroupInput("steepness_cb_2","Steepness (h)", choices = choices, selected = choices[1])
  })
  # Lambda - CPUE:CAL weighting
  output$lambda_cb_2 <- renderUI({
    choices <- unique(OM_Axes$lambda) %>% sort()
    checkboxGroupInput("lambda_cb_2","CPUE:CAL Weighting", choices = choices, selected = choices[1])
  })

  # Increase in q
  output$llq_cb_2 <- renderUI({
    choices <- unique(OM_Axes$incQ) %>% sort()
    checkboxGroupInput("llq_cb_2","Increase in q?", choices = choices, selected = choices[1])
  })
  # Environmental covariate
  output$env_cb_2 <- renderUI({
    choices <- unique(OM_Axes$incE) %>% sort()
    checkboxGroupInput("env_cb_2","Environmental covariate?", choices = choices, selected = choices[1])
  })

  # Observe events
  observeEvent(input$OM2,{
    OMind<-match(input$OM2,OMnames)
    updateCheckboxGroupInput(session,"Size_cb_2",selected=OM_Axes$Size[OMind])
    updateCheckboxGroupInput(session,"M_cb_2",selected=OM_Axes$M[OMind])
    updateCheckboxGroupInput(session,"sigmaR_cb_2",selected=OM_Axes$sigmaR[OMind])
    updateCheckboxGroupInput(session,"steepness_cb_2",selected=OM_Axes$h[OMind])
    updateCheckboxGroupInput(session,"lambda_cb_2",selected=OM_Axes$lambda[OMind])
    updateCheckboxGroupInput(session,"llq_cb_2",selected=OM_Axes$incQ[OMind])
    updateCheckboxGroupInput(session,"env_cb_2",selected=OM_Axes$incE[OMind])

  })

  # Get OM Numbers
  getOM_2 <-function() {
    OM_Axes$Name[OM_Axes$Size %in% input$Size_cb_2 &
                   OM_Axes$M %in% input$M_cb_2 &
                   OM_Axes$sigmaR %in% input$sigmaR_cb_2 &
                   OM_Axes$h %in% input$steepness_cb_2 &
                   OM_Axes$lambda %in% input$lambda_cb_2 &
                   OM_Axes$incQ %in% input$llq_cb_2 &
                   OM_Axes$incE %in% input$env_cb_2]
  }


  output$OM2_nums<-renderText({
    getOM_2()
  })


  # Load MSEs
  LoadMSE <- function(names) {
    List <- list()
    for (i in seq_along(names)) {
      row <- match(names[i], OM_Axes$Name)
      df <- OM_Axes[row,]
      if (df$Size=='Adjusted') {
        mse <- readRDS(paste0(file.path('data/grid_May2021_shifted/', df$i, '/MSE.rda')))
        mse@Name <-'name'
        List[[i]] <- mse
      } else {
        mse <- readRDS(paste0(file.path('data/grid_May2021/', df$i, '/MSE.rda')))
        mse@Name <- 'name'
        List[[i]] <- mse
      }
    }
    if (length(List)>1) {
      MSE <- joinMSE2(List)
    } else {
      MSE <- List[[1]]
    }
    MSE
  }


  # Performance table
  makeTable <- function(names) {

    MSE <- LoadMSE(names)
    tt <- summary(MSE, PMnames)
    tt
  }
  output$PTable1 <- DT::renderDataTable(makeTable(getOM_1()),
                                        extensions = c('Responsive', 'Buttons'),
                                        selection='none',
                                        options = list(
                                          paging = TRUE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          dom = 'Brtip',
                                          buttons = c('copy', 'csv', 'excel')
                                        ))

  output$PTable2 <- DT::renderDataTable(makeTable(getOM_2()),
                                        extensions = c('Responsive', 'Buttons'),
                                        selection='none',
                                        options = list(
                                          paging = TRUE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          dom = 'Brtip',
                                          buttons = c('copy', 'csv', 'excel')
                                        ))


  # Zeh plots
  # Wormplots
  makeZehPlot <- function(names) {
    MSE <- LoadMSE(names)
    MSEtool::DFO_bar(MSE)
  }

  output$zehplot1<-renderPlot(makeZehPlot(getOM_1()))
  output$zehplot2<-renderPlot(makeZehPlot(getOM_2()))

  # Wormplots
  makeWormPlot <- function(names) {
    MSE <- LoadMSE(names)
    MSEtool::wormplot(MSE)
  }
  output$wormplot1 <- renderPlot(makeWormPlot(getOM_1()))
  output$wormplot2 <- renderPlot(makeWormPlot(getOM_2()))

  # Trade-off plots
  makeTradeoffPlot <- function(names, PM1, PM2) {
    MSElist <- list()

    for (i in 1:length(names)) {
      MSElist[[i]] <- LoadMSE(names[[i]])
    }
    # MSElist <<- MSElist
    # PMlist <<- c(PM1, PM2)
    # TradePlot_2(MSElist, Lims=0, PMlist=PMlist, legend = TRUE)

    TradePlot_2(MSElist, Lims=0, PMlist=c(PM1, PM2), legend = TRUE,
                point.size=3, label.size=6)
  }

  output$tplot1 <- renderPlot(makeTradeoffPlot(list(getOM_1(), getOM_2()),
                                               input$T_PMx, input$T_PMy))

  output$tplot2 <- NULL

})


