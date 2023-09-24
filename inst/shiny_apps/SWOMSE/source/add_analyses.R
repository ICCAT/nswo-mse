add_analyses_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$add_analyses <- renderUI({

                   tabsetPanel(
                     id = "tab_panel",
                     tabPanel("Minimum TAC Change", {
                       tagList(
                         br(),
                         p('CMPs CE, FX4, and MCC7 were tested with a minimum TAC change of 200 t and compared to the default where there was no minimum value for the TAC adjustment.'),
                         p('The results showed the minimum TAC change of 200 t had no impact on the performance of the CMPs, as the change in TAC between management cycles was always greater than 200 t'),
                         DT::datatable(MinTACChange)

                       )


                       }),

                     tabPanel("Management Interval", {
                       tagList(
                         br(),
                         p('CMPs CE, FX4, and MCC7 were with a 4-year management cycle and compared to the default where the management interval was every 3-years.'),
                         p('The results showed the 4-year management cycles had a very small impact on the performance of these CMPs compared to the 3-year interval.'),
                         DT::datatable(DiffMngCycle)

                       )
                     })
                   )


               })
               }
  )

}

add_analyses_UI <- function(id, label="add_analyses") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('Additional Analyses'),
      p('Static Results. Filters do not apply.'),
      htmlOutput(ns('add_analyses'))
    )
  )
}


