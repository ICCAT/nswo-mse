CMPCompare_UI <- function(id, label="CMPCompare") {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    box(title=h3('CMP Compare'), width=12,
        h4('Compare the performance of three CMPs'),
        htmlOutput(ns('OM_global'))

        ),
    box(width=12,
        column(12,
               htmlOutput(ns('updateplots'))),

        column(4,
               uiOutput(ns('plot1'))
        ),
        column(4,
               uiOutput(ns('plot2'))
        ),
        column(4,
               uiOutput(ns('plot3'))
        )
        )
  )
}



CMPCompare_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$OM_global <- renderUI({
                   fluidRow(
                     column(2,
                            selectInput(session$ns('Model_Select'),
                                        'Select Global OM',
                                        choices=metadf$OMnames,
                                        selected = metadf$OMnames[1])
                     ),
                     column(3,
                            actionBttn(session$ns('UpdateGlobalOM'), 'Update Global OM')
                     ),
                     column(7)
                   )
                 })



                 output$updateplots <- renderUI({
                   tagList(
                     shinyjs::hidden(
                       actionBttn(ns('UpdateMPCompare'), 'Update Plots',
                                  color='danger')
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

                 ymax <- reactive({
                   dplyr::bind_rows(list1()[[1]], list2()[[1]], list3()[[1]]) |>
                     group_by(name) |> summarize(Max=max(Upper))
                 })

                 output$plot1 <- renderUI({
                   tagList(
                     selectInput(ns('OM_Select1'),
                                 'Select OM 1',
                                 choices=metadf$OMnames,
                                 selected = results$metadf$OMnames),
                     selectInput(ns('MP_Select1'),
                                 'Select MP 1',
                                 choices=allMPs,
                                 selected = allMPs[1]),
                     renderPlot({
                       Time_Series_Plot(list1(), ymax=ymax())
                     }, height=800, width=500)
                   )
                 })

                 output$plot2 <- renderUI({
                   tagList(
                     selectInput(session$ns('OM_Select2'),
                                 'Select OM 2',
                                 choices=metadf$OMnames,
                                 selected = results$metadf$OMnames),
                     selectInput(session$ns('MP_Select2'),
                                 'Select MP 2',
                                 choices=allMPs,
                                 selected = allMPs[3]),
                     renderPlot({
                       Time_Series_Plot(list2(), ymax=ymax())
                     }, height=800, width=500)
                   )
                 })

                 output$plot3 <- renderUI({
                   tagList(
                     selectInput(session$ns('OM_Select3'),
                                 'Select OM 3',
                                 choices=metadf$OMnames,
                                 selected = results$metadf$OMnames),
                     selectInput(session$ns('MP_Select3'),
                                 'Select MP 3',
                                 choices=allMPs,
                                 selected = allMPs[5]),
                     renderPlot({
                       Time_Series_Plot(list3(), ymax=ymax())
                     }, height=800, width=500)
                   )
                 })

               }
  )
}







