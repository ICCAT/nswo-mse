Violin_Server <- function(id, results) {
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
                                                 selected = results$Selected_Model)
                                     ),
                              column(6,
                                     checkboxInput(session$ns('violin_select'),
                                                  'By Year?'),
                                     p('Shows the plot faceted by management cycle (TAC) year')
                                     )
                            )
                     )
                   )
                 })


                 output$Violin <- renderUI({
                   if (length(input$Model_Select)==0) return(NULL)

                   pViolin_results <- results$pViolin_results %>% filter(Model %in% input$Model_Select)


                   if (nrow(pTS_results)<1) {
                     return( h4('No CMPs pass the CMP Filters',style="color:red"))
                   } else {
                     renderPlot({
                       Violin_Plot(pViolin_results, input$violin_select)
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
      p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
      fluidRow(
        column(12, htmlOutput(ns('Model_MP_Select')))
      ),
      fluidRow(
        column(12,
               htmlOutput(ns('Violin'))
        )
      )
    )

  )
}


Violin_Plot <- function(pViolin_results, byyear) {

  pViolin_results %>% filter(MP=='CE_a', Sim==1, Model=='R4')

  if (byyear) {
   p <- ggplot(pViolin_results, aes(x=MP, y=Value*100, fill=MP)) +
      facet_wrap(~Management_Year) +
      geom_violin(scale='width') +
      theme_bw() +
      guides(fill='none') +
      labs(x='Candidate Management Procedure',
           y='Absolute change in TAC (%)') +
      theme(axis.text.x = element_text(angle=90))
  } else {
    p <- ggplot(pViolin_results, aes(x=MP, y=Value*100, fill=MP)) +
      geom_violin(scale='width') +
      theme_bw() +
      guides(fill='none') +
      labs(x='Candidate Management Procedure',
           y='Absolute change in TAC (%)') +
      theme(axis.text.x = element_text(angle=90))
  }
 p
}
