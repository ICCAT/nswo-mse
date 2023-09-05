Violin_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$Violin <- renderUI({

                   pTS_results <- results$pTS_results

                   if (nrow(pTS_results)<1) {
                     return( h4('No CMPs pass the CMP Filters',style="color:red"))
                   } else {
                     Models <- unique(pTS_results$Model)
                     MPs <- unique(pTS_results$MP)
                     pViolin_results <- Violin_results %>% filter(Model%in% Models, MP %in%MPs)
                     renderPlot({
                       Violin_Plot(pViolin_results)
                     }, height=1000, width=800)
                   }

                 })
               }
  )

}


Violin_UI <- function(id, label="Violin") {

  ns <- NS(id)
  tagList(
    fluidRow(
      h4('Violin Plots'),
      htmlOutput(ns('Violin'))
    )
  )
}


Violin_Plot <- function(pTS_results) {


  ggplot(pTS_results, aes(x=MP, y=pc*100, fill=MP)) +
    facet_wrap(~Model, ncol=1) +
    geom_violin(scale='width') +
    theme_bw() +
    guides(fill='none') +
    labs(x='Candiate Management Procedure',
         y='Median absolute change in TAC (%)') +
    theme(axis.text.x = element_text(angle=90))

}
