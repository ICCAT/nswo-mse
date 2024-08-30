CMPPerf_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 selected_OM <- reactiveVal(1)
                 output$Model_MP_Select <- renderUI({
                   tagList(
                     column(12,
                            fluidRow(
                              shinyWidgets::actionGroupButtons(inputIds = ns(paste0('btn',seq_along(OMnames))),
                                                             labels=sort(OMnames))
                            )
                     )
                   )
                 })

                 observeEvent(input$btn1, {
                   selected_OM(1)
                 })

                 observeEvent(input$btn2, {
                   selected_OM(2)
                 })

                 observeEvent(input$btn3, {
                   selected_OM(3)
                 })

                 observeEvent(input$btn4, {
                   selected_OM(4)
                 })
                 observeEvent(input$btn5, {
                   selected_OM(5)
                 })
                 observeEvent(input$btn6, {
                   selected_OM(6)
                 })
                 observeEvent(input$btn7, {
                   selected_OM(7)
                 })

                 observeEvent(input$btn8, {
                   selected_OM(8)
                 })

                 observeEvent(input$btn9, {
                   selected_OM(9)
                 })



                 output$CMPPerf <- renderUI({

                   Models <- metadf$OMnames %>% unique()
                   nModels <- length(Models)


                   pPM_results <- results$pPM_results %>% filter(Model==sort(OMnames)[selected_OM()])
                   pTS_results <- results$pTS_results %>% filter(Model==sort(OMnames)[selected_OM()])

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
    box(title='CMP Performance', width=12,
        p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
        fluidRow(
          column(12,
          h4('Select Operating Model'),
          htmlOutput(ns('Model_MP_Select'))
          )
        ),
        fluidRow(
          column(12,
                 h4('Select Candidate Management Procedure'),
                 htmlOutput(ns('CMPPerf'))
          )
        )

        )
  )
}



