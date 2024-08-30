KobeTime_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$KobeTime <- renderUI({

                   kobe_results <- results$pkobe_results
                   MPs <- unique(kobe_results$MP)
                   MPnames <- unique(kobe_results$MP_name)

                   nTabs <- length(MPnames)

                   if(nTabs<1) {
                     mpTabs <- NULL
                   } else {
                     mpTabs <- lapply(1:nTabs,
                                      function(x) {
                                        this_kobe_results <- kobe_results %>% filter(MP_name==MPnames[x], MP%in%MPs)
                                        nMPs <- length(unique(this_kobe_results$MP))
                                        df <- this_kobe_results %>% tidyr::pivot_longer(., cols=6:9)
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
                                                     theme_bw() +
                                                     theme(strip.text = element_text(size=14),
                                                           axis.title = element_text(size=16),
                                                           axis.text=element_text(size=14))
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
    box(width=12, title='Kobe Time Plots',


          p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
          htmlOutput(ns('KobeTime'))

        )
  )
}



