CMPCompare_UI <- function(id, label="CMPCompare") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    box(title='CMP Compare', width=12,
        h4('Compare the performance across CMPs and OMs'),

        box(width=12,
            column(3,
                   column(6,
                          uiOutput(ns('OM_global')),
                          ),
                   column(6,
                          uiOutput(ns('MP_global')),
                          ),
                   hr(),
                   uiOutput(ns('updateplots')),
                   p('Set individual OMs and/or CMPs'),
                   uiOutput(ns('plot1select')),
                   uiOutput(ns('plot2select')),

                   uiOutput(ns('plot3select'))
                   ),
            column(9,
                   uiOutput(ns('displayplot'))
                   )
        )
    )
  )
}



CMPCompare_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$OM_global <- renderUI({
                   tagList(
                     selectInput(ns('Model_Select'),
                                 'Select Global OM',
                                 choices=metadf$OMnames,
                                 selected = metadf$OMnames[1]),
                     p('Updates the OM for all plots'),
                     actionBttn(ns('UpdateGlobalOM'), 'Update Global OM', size='sm'),
                     hr()

                   )
                 })

                 output$MP_global <- renderUI({
                   tagList(
                     selectInput(ns('MP_Model_Select'),
                                 'Select Global CMP',
                                 choices=allMPs,
                                 selected = allMPs[1]),
                     p('Updates the CMP for all plots'),
                     actionBttn(ns('UpdateGlobalMP'), 'Update Global MP', size='sm'),
                     hr()

                   )
                 })

                 output$updateplots <- renderUI({
                   tagList(
                     shinyjs::hidden(
                       actionBttn(ns('UpdateMPCompare'), 'Update Plots',
                                  color='danger')
                       ),
                     shinyjs::delay(50,
                                    shinyjs::hide('UpdateMPCompare'))
                   )
                 })

                 output$plot1select <- renderUI({
                   tagList(
                     column(6,
                            selectInput(ns('OM_Select1'),
                                        'Select OM 1',
                                        choices=metadf$OMnames,
                                        selected = results$metadf$OMnames)
                     ),
                     column(6,
                            selectInput(ns('MP_Select1'),
                                        'Select MP 1',
                                        choices=allMPs,
                                        selected = allMPs[1])
                     )
                   )
                 })

                 output$plot2select <- renderUI({
                   tagList(
                     column(6,
                            selectInput(ns('OM_Select2'),
                                        'Select OM 2',
                                        choices=metadf$OMnames,
                                        selected = results$metadf$OMnames)
                     ),
                     column(6,
                            selectInput(ns('MP_Select2'),
                                        'Select MP 2',
                                        choices=allMPs,
                                        selected = allMPs[3])
                     )
                   )
                 })
                 output$plot3select <- renderUI({
                   tagList(
                     column(6,
                            selectInput(ns('OM_Select3'),
                                        'Select OM 3',
                                        choices=metadf$OMnames,
                                        selected = results$metadf$OMnames)
                     ),
                     column(6,
                            selectInput(ns('MP_Select3'),
                                        'Select MP 3',
                                        choices=allMPs,
                                        selected = allMPs[5])
                     )
                   )
                 })


                 observeEvent(list(input$MP_Select1, input$MP_Select2, input$MP_Select3,
                                   input$OM_Select1, input$OM_Select2, input$OM_Select3), {
                                     shinyjs::show('UpdateMPCompare')
                 }, ignoreInit = TRUE)

                 observeEvent(input$UpdateGlobalOM, {
                   updateSelectInput(session, inputId='OM_Select1', selected = input$Model_Select)
                   updateSelectInput(session, inputId='OM_Select2', selected = input$Model_Select)
                   updateSelectInput(session, inputId='OM_Select3', selected = input$Model_Select)
                 })

                 observeEvent(input$UpdateGlobalMP, {
                   updateSelectInput(session, inputId='MP_Select1', selected = input$MP_Model_Select)
                   updateSelectInput(session, inputId='MP_Select2', selected = input$MP_Model_Select)
                   updateSelectInput(session, inputId='MP_Select3', selected = input$MP_Model_Select)
                 })

                 observeEvent(input$UpdateMPCompare, {
                   shinyjs::hide('UpdateMPCompare')
                   results$compare_MPs <- c(input$MP_Select1, input$MP_Select2, input$MP_Select3)
                   results$Selected_Model <- c(input$OM_Select1, input$OM_Select2, input$OM_Select3)
                 })

                 list1 <- reactive({
                   mp_PM_results1 <- PM_results %>% filter(MP==results$compare_MPs[1], Model== results$Selected_Model[1])
                   mp_TS_results1 <- TS_results %>% filter(MP==results$compare_MPs[1],  Model== results$Selected_Model[1])
                   list(mp_TS_results1, mp_PM_results1)
                 })

                 list2 <- reactive({
                   mp_PM_results2 <- PM_results %>% filter(MP==results$compare_MPs[2], Model== results$Selected_Model[2])
                   mp_TS_results2 <- TS_results %>% filter(MP==results$compare_MPs[2],  Model== results$Selected_Model[2])
                   list(mp_TS_results2, mp_PM_results2)
                 })

                 list3 <- reactive({
                   mp_PM_results1 <- PM_results %>% filter(MP==results$compare_MPs[3], Model== results$Selected_Model[3])
                   mp_TS_results1 <- TS_results %>% filter(MP==results$compare_MPs[3],  Model== results$Selected_Model[3])
                   list(mp_TS_results1, mp_PM_results1)
                 })

                 output$displayplot <- renderUI({
                   plotOutput(ns('plot'), height='800px')

                 })

                 output$plot <- renderPlot({

                   DF <- list(list1()[[1]], list2()[[1]], list3()[[1]])
                   PM_ResultsMP <- list(list1()[[2]], list2()[[2]], list3()[[2]])
                   TimeSeriesPlot2(do.call('rbind',DF),
                                   do.call('rbind', PM_ResultsMP)
                   )
                 })


               }
  )
}







