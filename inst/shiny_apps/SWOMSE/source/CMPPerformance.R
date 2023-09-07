CMPPerf_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$Model_MP_Select <- renderUI({



                   tagList(
                     column(12,
                            fluidRow(
                              column(6,
                                     selectInput(session$ns('Model_Select'),
                                                 'Select Model',
                                                 choices=metadf$OMnames,
                                                 selected = results$Selected_Model))
                            )
                     )
                   )
                 })

                 output$CMPPerf <- renderUI({

                   Models <- metadf$OMnames %>% unique()
                   nModels <- length(Models)

                   if (length(input$Model_Select)==0) {
                     return(NULL)
                   }
                   pPM_results <- results$pPM_results %>% filter(Model==input$Model_Select)
                   pTS_results <- results$pTS_results %>% filter(Model==input$Model_Select)

                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)
                   nTabs <- length(MPnames)
                   if(nTabs<1) {
                     mpTabs <- NULL
                   } else {
                     mpTabs <- lapply(1:nTabs,
                                      function(x) {
                                        tabPanel(MPnames[x], {
                                                   mp_PM_results <- pPM_results %>% filter(MP_name==MPnames[x], MP%in%MPs)
                                                   mp_TS_results <- pTS_results %>% filter(MP_name==MPnames[x],  MP%in%MPs)
                                                   ll <- list(mp_TS_results, mp_PM_results)

                                                   nMPs <- length(unique(mp_PM_results$MP))
                                                   renderPlot({
                                                     Time_Series_Plot(ll)
                                                   }, height=800, width=500*nMPs)



                                                   })

                                      })






                   }
                   if (!is.null(mpTabs)) {
                     return(do.call(tabsetPanel, mpTabs))
                   } else {
                     return(h4('No CMPs pass the CMP Filters',style="color:red"))
                   }
                 })
               }
  )

}



CMPPerf_UI <- function(id, label="Viz") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('CMP Performance'),
      p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
      fluidRow(
        column(12, htmlOutput(ns('Model_MP_Select')))
      ),
      fluidRow(
        column(12,
               htmlOutput(ns('CMPPerf'))
        )
      )
    )
  )
}



