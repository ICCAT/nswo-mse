CMPCompare_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$Model_MP_Select <- renderUI({

                   pPM_results <- results$pPM_results
                   pTS_results <- results$pTS_results
                   MPs <- allMPs

                   tagList(
                     column(12,
                            fluidRow(
                              column(6,
                                     selectInput(session$ns('Model_Select'),
                                                 'Select Model',
                                                 choices=metadf$OMnames,
                                                 selected = results$Selected_Model)),
                              column(6,
                                     actionBttn(session$ns('UpdateMPCompare'), 'Update Plot')
                              )
                            )
                            ),
                     column(12,
                            fluidRow(
                              column(4, selectInput(session$ns('MP_Select1'),
                                                    'Select MP 1',
                                                    choices=MPs,
                                                    selected = results$compare_MPs[1])),
                              column(4, selectInput(session$ns('MP_Select2'),
                                                    'Select MP 2',
                                                    choices=MPs,
                                                    selected = results$compare_MPs[2])),
                              column(4, selectInput(session$ns('MP_Select3'),
                                                    'Select MP 3',
                                                    choices=MPs,
                                                    selected = results$compare_MPs[3]))
                            )
                            )



                   )
                 })

                 observeEvent(input$UpdateMPCompare, {
                   results$compare_MPs <- c(input$MP_Select1, input$MP_Select2, input$MP_Select3)
                   results$Selected_Model <- input$Model_Select
                 })

                 output$MP_Compare <- renderUI({

                   mp_PM_results1 <- PM_results %>% filter(MP==results$compare_MPs[1], Model== results$Selected_Model)
                   mp_TS_results1 <- TS_results %>% filter(MP==results$compare_MPs[1],  Model== results$Selected_Model)
                   l1 <- list(mp_TS_results1, mp_PM_results1)

                   mp_PM_results2 <- PM_results %>% filter(MP==results$compare_MPs[2], Model== results$Selected_Model)
                   mp_TS_results2 <- TS_results %>% filter(MP==results$compare_MPs[2],  Model== results$Selected_Model)
                   l2 <- list(mp_TS_results2, mp_PM_results2)

                   mp_PM_results3 <- PM_results %>% filter(MP==results$compare_MPs[3], Model== results$Selected_Model)
                   mp_TS_results3 <- TS_results %>% filter(MP==results$compare_MPs[3],  Model== results$Selected_Model)
                   l3 <- list(mp_TS_results3, mp_PM_results3)


                   tagList(
                     h4(paste('Selected Model:', results$Selected_Model)),
                     column(4,
                            renderPlot({
                              Time_Series_Plot(l1)
                            }, height=800, width=500)
                     ),
                     column(4,
                            renderPlot({
                              Time_Series_Plot(l2)
                            }, height=800, width=500)
                     ),
                     column(4,
                            renderPlot({
                              Time_Series_Plot(l3)
                            }, height=800, width=500)
                     )
                   )


                   })

               }
  )

}



CMPCompare_UI <- function(id, label="CMPCompare") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('CMP Compare'),
      h5('Select 3 of all available CMPs (filters do not apply)'),
      fluidRow(
        column(12, htmlOutput(ns('Model_MP_Select')))
      ),
      fluidRow(
        column(12,
               htmlOutput(ns('MP_Compare'))
               )

      )

    )
  )
}




