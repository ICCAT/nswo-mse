


FiltersServer <- function(id, results) {

  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 # CMP Filters
                 output$LRP_options <- renderUI({
                   tagList(
                     hr(),
                     h4('LRP: Probability of breaching the LRP (0.4BMSY)'),
                     radioButtons(session$ns("LRP_models"),
                                  label='Models',
                                  choices=c('Reference Only', 'Reference + Robustness'),
                                  selected=results$LRP_models,
                                  inline=TRUE),
                     conditionalPanel("input.LRP_models == 'Reference + Robustness'", ns=ns,
                                      tags$div(align = 'left',
                                               class = 'multicol',
                                               checkboxGroupInput(session$ns('LRP_models_cb'),
                                                                  label='Robustness Models',
                                                                  choices=levels(OMnames)[2:length(levels(OMnames))],
                                                                  select=results$LRP_models_cb,
                                                                  inline=TRUE)
                                      )
                     ),
                     sliderInput(session$ns('LRP_value'), 'Maximum acceptable value',
                                 min=0, max=0.15, value=results$LRP_value, step=0.01),
                     radioButtons(inputId= session$ns("LRP_pass"), paste0("Less than ", results$LRP_value, "% probability of breaching LRP"),
                                  choices=c("Pass","Fail", 'All'),
                                  selected=results$LRP_pass, inline=TRUE)

                   )

                 })

                 output$PGK_options <- renderUI ({
                   tagList(
                     hr(),
                     h4('PGK_short: 2024-2033 (Ref Models)'),
                     tags$div(align = 'left',
                              class = 'multicol',
                              checkboxGroupInput(session$ns('PGK_short'),
                                                 label='Tuning Values',
                                                 choices=c('0.51', '0.60', '0.70'),
                                                 select=results$PGK_short,
                                                 inline=TRUE)
                     ),
                     hr(),
                     h4('PGK_med: 2034-2043'),
                     radioButtons(session$ns("PGK_models"),
                                  label='Models',
                                  choices=c('Reference Only', 'Reference + Robustness'),
                                  selected='Reference Only',
                                  inline=TRUE),
                     conditionalPanel("input.PGK_models == 'Reference + Robustness'", ns=ns,
                                      tags$div(align = 'left',
                                               class = 'multicol',
                                               checkboxGroupInput(session$ns('PGK_models_cb'),
                                                                  label='Robustness Models',
                                                                  choices=levels(OMnames)[2:length(levels(OMnames))],
                                                                  select=results$PGK_models_cb,
                                                                  inline=TRUE)
                                      )
                                      ),
                     sliderInput(session$ns('PGK_pass'), 'Minimum acceptable value',
                                 min=0.1, max=1, value=0.51, step=0.01)
                   )

                 })



                 output$MP_filters <- renderUI({

                   tagList(
                     h4('Manually De-select MPs'),
                     p('All CMPs are shown here, but only those that pass the above filters will be shown in plots, even if they are selected here'),
                     p('Results for de-selected CMPs will not be shown.'),
                     selectInput(session$ns('short_list_select'), 'Select Shortlisted CMPs', choices=names(results$short_list),
                                 selected=results$selected_shortlist),
                     actionButton(session$ns('short_list_MPs'), 'Select Short-Listed CMPs'),
                     actionButton(session$ns('select_all_MPs'), 'Select All CMPs'),
                     actionButton(session$ns('deselect_all_MPs'), 'De-select All CMPs'),
                     actionButton(session$ns('add_shortlist_MPs'), 'Create new Shortlist'),
                     tags$div(align = 'left',
                              class = 'multicol',
                              checkboxGroupInput(session$ns("mp_select"), "Select MPs:",
                                                 choices=allMPs,
                                                 selected=results$mp_select, inline=T)
                     )
                   )
                 })

                 observeEvent(input$short_list_MPs, {
                   short_list_mps <- results$short_list[[input$short_list_select]]
                   results$mp_select <- short_list_mps
                   results$selected_shortlist <- input$short_list_select
                 })

                 observeEvent(input$select_all_MPs, {
                   results$mp_select <- allMPs
                 })

                 observeEvent(input$deselect_all_MPs, {
                   results$mp_select <- NULL
                 })


                 observeEvent(input$add_shortlist_MPs, {
                   inputSweetAlert(
                     session = session,
                     session$ns("short_list_name"),
                     input = "text",
                     title = "Name of short-list",
                     inputPlaceholder = "Top Performing CMPs",
                     allowOutsideClick = FALSE,
                     showCloseButton = TRUE
                   )
                 })

                 observeEvent(input$short_list_name, {
                   short_list_mps <- input$mp_select
                   l <- length(results$short_list)
                   ll <- append(results$short_list, list(short_list_mps))
                   names(ll)[l+1] <- input$short_list_name #input$mytext

                   results$short_list <- ll
                   results$mp_select <- short_list_mps
                 })


                 changed_filter <- reactive({
                   list(input$LRP_value,input$LRP_models, input$LRP_pass, input$LRP_models_cb,
                        input$PGK_short,
                        input$PGK_pass, input$PGK_models,
                        input$mp_select)
                 })


                 observeEvent(changed_filter(), {
                   results$Filt <- TRUE
                 })


                 observeEvent(input$UpdateFilters, {
                   results$Filt <- FALSE
                   results$LRP_value <- input$LRP_value
                   results$LRP_models <- input$LRP_models
                   results$LRP_pass  <- input$LRP_pass
                   results$LRP_models_cb <- input$LRP_models_cb
                   results$PGK_short  <- input$PGK_short
                   results$PGK_pass  <- input$PGK_pass
                   results$PGK_models  <- input$PGK_models
                   results$mp_select  <- input$mp_select

                   pPM_results <- PM_results
                   pTS_results <- TS_results

                   if (!is.null(input$LRP_models)) {
                     # filter pass or fail LRP
                     if (input$LRP_models=='Reference Only') {
                       fail_MPs <- PM_results %>% filter(Model=='Reference', PM=='LRP', Value>input$LRP_value) %>%
                         distinct(MP)
                     } else {
                       fail_MPs <- PM_results %>% filter(Model %in% c('Reference',input$LRP_models_cb), PM=='LRP', Value>input$LRP_value) %>%
                         distinct(MP)
                     }

                     if (input$LRP_pass=='Pass') {
                       pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs$MP)
                       pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs$MP)
                     } else if (input$LRP_pass=='Fail') {
                       pPM_results <- pPM_results %>% filter(MP %in% fail_MPs$MP)
                       pTS_results <- pTS_results %>% filter(MP %in% fail_MPs$MP)
                     }

                     # PGK short
                     pPM_results$Target <- as.numeric(as.character(pPM_results$Target))
                     keep_MPs <- pPM_results %>% filter(Target %in% as.numeric(input$PGK_short))
                     pPM_results <- pPM_results %>% filter(MP %in% keep_MPs$MP)
                     pTS_results <- pTS_results %>% filter(MP %in% keep_MPs$MP)

                     # PGK med
                     if (input$PGK_models=='Reference Only') {
                       fail_MPs_PGK <- PM_results %>% filter(Model=='Reference', PM=='PGK_med', Value<input$PGK_pass) %>%
                         distinct(MP)
                     } else {
                       fail_MPs_PGK <- PM_results %>% filter(Model %in% c('Reference',input$PGK_models_cb), PM=='PGK_med', Value<input$PGK_pass) %>%
                         distinct(MP)
                     }

                     pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs_PGK$MP)
                     pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs_PGK$MP)

                     results$passMPs <- unique(pPM_results$MP)
                     # MPs
                     pPM_results <- pPM_results %>% filter(MP %in% input$mp_select)
                     pTS_results <- pTS_results %>% filter(MP %in% input$mp_select)
                     results$select_MPs <- unique(pPM_results$MP)

                   }

                   results$pPM_results <- pPM_results
                   results$pTS_results <- pTS_results

                   results$pkobe_results <- kobe_results %>% filter(MP %in% unique(pPM_results$MP))
                   results$pViolin_results <- Violin_results %>% filter(MP %in% unique(pPM_results$MP))

                 })




                 # TradeOffs
                 output$TO_options <- renderUI({

                   tagList(
                     column(12,
                            hr(),
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
                            ),
                            actionBttn(session$ns('UpdateTO'), 'Update Trade-Off Plot')
                     )
                   )
                 })


                 observeEvent(input$UpdateTO, {
                   results$TO1x <- input$TO1x
                   results$TO1y <- input$TO1y
                   results$TO2x <- input$TO2x
                   results$TO2y <- input$TO2y
                   results$TO3x <- input$TO3x
                   results$TO3y <- input$TO3y
                   results$TO4x <- input$TO4x
                   results$TO4y <- input$TO4y

                 })

                 output$Quilt_options <- renderUI({

                   tagList(
                     column(12,
                            hr(),
                            h4('Quilt PM Selection'),
                            tags$div(align = 'left',
                                     class = 'multicol',
                                     checkboxGroupInput(session$ns('Quilt_Select'),
                                                        label='Quilt PMs ',
                                                        choices=allPMs,
                                                        select=results$QuiltPMs,
                                                        inline=TRUE)
                            ),
                            checkboxInput(session$ns('calc_dominated'),
                                          'Identify outperformed CMPs?',
                                          results$show_dominated),
                            actionBttn(session$ns('UpdateQuilt'), 'Update Quilt Plot')
                     )
                   )
                 })


                 observeEvent(input$UpdateQuilt, {
                   results$QuiltPMs <- input$Quilt_Select
                   results$show_dominated <- input$calc_dominated
                 })


                 output$filters <- renderUI({
                   tagList(
                     column(12,
                            br(),
                            conditionalPanel("output.Filt",
                                             actionBttn(session$ns('UpdateFilters'), 'Update Plots'),
                                             p('Click `Update Plots` after any filters are changed')
                                             ),
                            uiOutput(session$ns('LRP_options')),
                            uiOutput(session$ns('PGK_options')),
                            uiOutput(session$ns('MP_filters'))

                     )
                   )
                 })






                 output$download <- renderUI({
                   tagList(
                     column(12,
                            hr(),
                            fluidRow(
                              column(6,
                                     p('Download Performance Metrics values for each CMP and Operating Model')
                              ),
                              column(6,
                                     downloadButton(session$ns("downloadPMData"), label = "PM Results")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     p('Download SB/SBMSY, F/FMSY, and TAC for each Simulation, Year, CMP and Operating Model')
                                     ),
                              column(6,
                                     downloadButton(session$ns("downloadTSData"), label = "Time-Series Results")
                                     )
                            ),
                            hr(),
                   )
                   )
                 })

                 output$downloadPMData <- downloadHandler(
                   filename <- function() {
                     'PM_results.rda'
                   },

                   content <- function(file) {
                     file.copy("./data/PM_results.rda", file)
                   },
                   contentType = NULL
                 )
                 output$downloadTSData <- downloadHandler(
                   filename <- function() {
                     'TS_results.rda'
                   },

                   content <- function(file) {
                     file.copy("./data/TS_results.rda", file)
                   },
                   contentType = NULL
                 )

               }
  )
}



FiltersUI <- function(id, label="filters") {

  ns <- NS(id)
  tagList(
     fluidRow(
     column(12,
            tabsetPanel(
              tabPanel('CMP Filters',
                       htmlOutput(ns('filters'))
                       ),
              tabPanel('Trade-Off Selection',
                       htmlOutput(ns('TO_options'))
                       ),
              tabPanel('Quilt Selection',
                       htmlOutput(ns('Quilt_options'))
              ),
              tabPanel('Download',
                       htmlOutput(ns('download'))
              )
            )

     )
    )
  )
}








