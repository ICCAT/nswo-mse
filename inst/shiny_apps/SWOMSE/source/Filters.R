


FiltersServer <- function(id, results) {

  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 output$LRP_options <- renderUI({
                   tagList(
                     h4('Limit Reference Point (0.4BMSY)'),
                     sliderInput(session$ns('LRP_value'), 'Maximum acceptable value',
                                 min=0, max=0.15, value=results$LRP_value, step=0.01),
                     radioButtons(session$ns("LRP_models"),
                                  label='Models',
                                  choices=c('Reference + Robustness', 'Reference Only'),
                                  selected='Reference + Robustness',
                                  inline=TRUE),
                     radioButtons(inputId= session$ns("LRP_pass"), paste0("Less than ", results$LRP_value, "% probability of breaching LRP"),
                                  choices=c("Pass","Fail", 'All'),
                                  selected='Pass', inline=TRUE)

                   )

                 })

                 observeEvent(input$LRP_value,{results$LRP_value=input$LRP_value})
                 observeEvent(input$LRP_models,{results$LRP_models=input$LRP_models})
                 observeEvent(input$LRP_pass,{results$LRP_pass=input$LRP_pass})

                 output$PGK_options <- renderUI ({
                   tagList(
                     h4('PGK Medium'),
                     sliderInput(session$ns('PGK_pass'), 'Minimum acceptable value',
                                 min=0.5, max=1, value=0.51, step=0.01),
                     radioButtons(session$ns("PGK_models"),
                                  label='Models',
                                  choices=c('Reference + Robustness', 'Reference Only'),
                                  selected='Reference Only',
                                  inline=TRUE)
                   )

                 })

                 observeEvent(input$PGK_pass,{results$PGK_pass=input$PGK_pass})
                 observeEvent(input$PGK_models,{results$PGK_models=input$PGK_models})


                 output$TO_options <- renderUI({

                   tagList(
                     h4('Trade-Off Options'),
                     h5('Trade-Off 1'),
                     column(6,
                            selectInput(session$ns('TO1x'), 'PM x',
                                        choices=allPMs,
                                        selected=tradeoffPMs$x[1])
                            ),
                     column(6,
                            selectInput(session$ns('TO1y'), 'PM y',
                                        choices=allPMs,
                                        selected=tradeoffPMs$y[1])
                     ),
                     h5('Trade-Off 2'),
                     column(6,
                            selectInput(session$ns('TO2x'), 'PM x',
                                        choices=allPMs,
                                        selected=tradeoffPMs$x[2])
                     ),
                     column(6,
                            selectInput(session$ns('TO2y'), 'PM y',
                                        choices=allPMs,
                                        selected=tradeoffPMs$y[2])
                     ),
                     h5('Trade-Off 3'),
                     column(6,
                            selectInput(session$ns('TO3x'), 'PM x',
                                        choices=allPMs,
                                        selected=tradeoffPMs$x[3])
                     ),
                     column(6,
                            selectInput(session$ns('TO3y'), 'PM y',
                                        choices=allPMs,
                                        selected=tradeoffPMs$y[3])
                     ),
                     h5('Trade-Off 4'),
                     column(6,
                            selectInput(session$ns('TO4x'), 'PM x',
                                        choices=allPMs,
                                        selected=tradeoffPMs$x[4])
                     ),
                     column(6,
                            selectInput(session$ns('TO4y'), 'PM y',
                                        choices=allPMs,
                                        selected=tradeoffPMs$y[4])
                     )
                   )
                 })

                 observeEvent(input$TO1x,{results$TO1x=input$TO1x})
                 observeEvent(input$TO1y,{results$TO1y=input$TO1y})

                 observeEvent(input$TO2x,{results$TO2x=input$TO2x})
                 observeEvent(input$TO2y,{results$TO2y=input$TO2y})

                 observeEvent(input$TO3x,{results$TO3x=input$TO3x})
                 observeEvent(input$TO3y,{results$TO3y=input$TO3y})

                 observeEvent(input$TO4x,{results$TO4x=input$TO4x})
                 observeEvent(input$TO4y,{results$TO4y=input$TO4y})


                 output$MP_filters <- renderUI({

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
                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)

                   tagList(
                     h4('Manually Select MPs'),
                     checkboxGroupInput(session$ns("mp_select"), "Select MPs:",
                                        choices=MPs,
                                        selected=MPs, inline=T)

                   )
                 })
                 observeEvent(input$mp_select,{results$select_MPs=input$mp_select})

                 output$filters <- renderUI({
                   tagList(
                     column(12,
                            hr(),
                            uiOutput(session$ns('LRP_options')),
                            hr(),
                            uiOutput(session$ns('PGK_options')),
                            hr(),
                            uiOutput(session$ns('TO_options')),
                            br(),
                            hr(),

                            uiOutput(session$ns('MP_filters'))
                     )
                   )
                 })



               }
  )
}



FiltersUI <- function(id, label="filters") {

  ns <- NS(id)
  tagList(
     fluidRow(
     column(12,
            htmlOutput(ns('filters'))
     )
    )
  )
}






