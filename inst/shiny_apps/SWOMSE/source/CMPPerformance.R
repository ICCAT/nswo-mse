CMPPerf_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$CMPPerf <- renderUI({

                   Models <- metadf$OMnames %>% unique()
                   nModels <- length(Models)

                   pPM_results <- results$pPM_results
                   pTS_results <- results$pTS_results

                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)
                   nTabs <- length(MPnames)
                   if(nTabs<1) {
                     mpTabs <- NULL
                   } else {
                     mpTabs <- lapply(1:nTabs,
                                      function(x) {
                                        tabPanel(MPnames[x],
                                                 renderUI({
                                                   do.call(tabsetPanel,
                                                           lapply(1:nModels, function(y) {

                                                             mp_PM_results <- pPM_results %>% filter(MP_name==MPnames[x], Model==Models[y], MP%in%MPs)

                                                             mp_TS_results <- pTS_results %>% filter(MP_name==MPnames[x],  Model==Models[y],  MP%in%MPs)
                                                             ll <- list(mp_TS_results, mp_PM_results)

                                                             nMPs <- length(unique(mp_PM_results$MP))
                                                             tabPanel(Models[y],
                                                                      renderPlot({
                                                                        Time_Series_Plot(ll)
                                                                      }, height=800, width=500*nMPs)
                                                             )

                                                           }))
                                                 })
                                        )
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
      htmlOutput(ns('CMPPerf'))
    )
  )
}



