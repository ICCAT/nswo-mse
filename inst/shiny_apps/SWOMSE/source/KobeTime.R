KobeTime_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$KobeTime <- renderUI({

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
                                      kobe_results <- pTS_results %>%
                                        filter(Year>=2024) %>%
                                        filter(MP_name==MPnames[x], MP%in%MPs) %>%
                                        group_by(Year, MP, Model) %>%
                                        summarise(nsim=sum(SB_SBMSY>0),
                                                  bl=sum(SB_SBMSY<1 & F_FMSY<1)/nsim*100,
                                                  tl=sum(SB_SBMSY<1 & F_FMSY>1)/nsim*100,
                                                  br=sum(SB_SBMSY>1 & F_FMSY<1)/nsim*100,
                                                  tr=sum(SB_SBMSY>1 & F_FMSY>1)/nsim*100)
                                      nMPs <- length(unique(kobe_results$MP))
                                      df <- kobe_results %>% tidyr::pivot_longer(., cols=5:8)
                                      df$name <- factor(df$name, levels=c('br', 'tr', 'bl', 'tl'), ordered = TRUE)
                                      df$Model <- factor(df$Model, levels=OMnames, ordered = TRUE)

                                      cols <- c('green', 'orange', 'yellow', 'red')

                                      tabPanel(MPnames[x],
                                               br(),
                                               renderPlot({
                                                 ggplot(df, aes(x=Year, y=value, fill=name)) +
                                                   facet_grid(Model~MP) +
                                                   geom_bar(position="stack", stat="identity", width = 1) +
                                                   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
                                                   scale_fill_manual(values=cols) +
                                                   guides(fill='none') +
                                                   labs(y="Percent of total simulatons (%)",
                                                        x='Projection Year') +
                                                   theme_bw()
                                               }, height=1000, width=600*nMPs)

                                      )
                                    }
                   )

                   # mpTabs <- lapply(1:nTabs,
                   #                  function(x) {
                   #                    tabPanel(MPnames[x],
                   #                             renderUI({
                   #                               do.call(tabsetPanel,
                   #                                       lapply(1:nModels, function(y) {
                   #
                   #                                         kobe_results <- TS_results %>%
                   #                                           filter(Year>=2024) %>%
                   #                                           filter(MP_name==MPnames[x], Model==Models[y], MP%in%MPs) %>%
                   #                                           group_by(Year, MP) %>%
                   #                                           summarise(nsim=sum(SB_SBMSY>0),
                   #                                                     bl=sum(SB_SBMSY<1 & F_FMSY<1)/nsim*100,
                   #                                                     tl=sum(SB_SBMSY<1 & F_FMSY>1)/nsim*100,
                   #                                                     br=sum(SB_SBMSY>1 & F_FMSY<1)/nsim*100,
                   #                                                     tr=sum(SB_SBMSY>1 & F_FMSY>1)/nsim*100)
                   #
                   #                                         df <- kobe_results %>% tidyr::pivot_longer(., cols=4:7)
                   #                                         df$name <- factor(df$name, levels=c('br', 'tr', 'bl', 'tl'), ordered = TRUE)
                   #
                   #                                         cols <- c('green', 'orange', 'yellow', 'red')
                   #
                   #                                         nMPs <- length(unique(kobe_results$MP))
                   #                                         tabPanel(Models[y],
                   #                                                  renderPlot({
                   #                                                    ggplot(df, aes(x=Year, y=value, fill=name)) +
                   #                                                      facet_wrap(~MP, nrow=1) +
                   #                                                      geom_bar(position="stack", stat="identity", width = 1) +
                   #                                                      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
                   #                                                      scale_fill_manual(values=cols) +
                   #                                                      guides(fill='none') +
                   #                                                      labs(y="Percent of total simulatons (%)",
                   #                                                           x='Projection Year') +
                   #                                                      theme_bw()
                   #                                                  }, height=600, width=500*nMPs)
                   #                                         )
                   #
                   #                                       }))
                   #                             })
                   #                    )
                   #                  })
                   do.call(tabsetPanel, mpTabs)})
               }
  )

}

KobeTime_UI <- function(id, label="KobeTime") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('Kobe Time Plots'),
      htmlOutput(ns('KobeTime'))
    )
  )
}



