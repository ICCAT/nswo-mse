KobeTime_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$KobeTime <- renderUI({

                   pTS_results <- results$pTS_results
                   MPs <- unique(pTS_results$MP)
                   MPnames <- unique(pTS_results$MP_name)
                   nTabs <- length(MPnames)
                   if(nTabs<1) {
                     mpTabs <- NULL
                   } else {
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
                                                    tr=sum(SB_SBMSY>1 & F_FMSY>1)/nsim*100,
                                                    .groups='drop')
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
                   }
                   if (is.null(mpTabs)) {
                     return(   h4('No CMPs pass the CMP Filters',style="color:red"))
                   } else {
                     return( do.call(tabsetPanel, mpTabs))
                   }




                  })
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



