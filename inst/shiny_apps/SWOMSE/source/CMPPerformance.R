CMPPerf_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$CMPPerf <- renderUI({

                   Models <- metadf$OMnames %>% unique()

                   pPM_results <- PM_results
                   pTS_results <- TS_results

                   # filter pass or fail LRP
                   pass_mps <- unique(pPM_results$MP)
                   if (results$LRP_models=='Reference Only') {
                     fail_MPs <- pPM_results %>% filter(Model=='Reference', PM=='LRP', Value>results$LRP_value) %>%
                       distinct(MP)
                   } else {
                     fail_MPs <- pPM_results %>% filter(PM=='LRP', Value>results$LRP_value) %>%
                       distinct(MP)
                   }

                   if (results$LRP_pass=='Pass') {
                     pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs$MP)
                     pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs$MP)
                   } else if(results$LRP_pass=='Fail') {
                     pPM_results <- pPM_results %>% filter(MP %in% fail_MPs$MP)
                     pTS_results <- pTS_results %>% filter(MP %in% fail_MPs$MP)
                   }

                   # filter for PGK medium
                   if (results$PGK_models=='Reference Only') {
                     fail_MPs <- pPM_results %>% filter(Model=='Reference', PM=='PGK_med', Value<results$PGK_pass) %>%
                       distinct(MP)
                   } else {
                     fail_MPs <- pPM_results %>% filter(PM=='PGK_med', Value<results$PGK_pass) %>%
                       distinct(MP)
                   }

                   pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs$MP)
                   pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs$MP)

                   nModels <- length(Models)

                   # filter by MP
                   pPM_results <- pPM_results %>% filter(MP %in% results$select_MPs)
                   pTS_results <- pTS_results %>% filter(MP %in% results$select_MPs)
                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)
                   nTabs <- length(MPnames)
                   if(nTabs<1) return(NULL)


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
                   do.call(tabsetPanel, mpTabs)})


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



