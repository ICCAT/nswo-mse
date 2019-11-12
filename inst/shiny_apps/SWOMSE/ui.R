library(shiny)


shinyUI(
    fluidPage(
        hr(),
        titlePanel("North Atlantic Swordfish Management Strategy Evaluation"),

        fluidRow(
            column(12,
                   fluidRow(
                       column(2,
                              img(src = "ICCAT3.jpg", height = 70, width = 110),
                              img(src = "swo_noaa.png", height = 70, width = 110)),
                       column(8,
                              h4("SWO MSE Results"),
                              p("The Swordfish Management Strategy Evaluation (SWO MSE) evaluates
                              a set of candidate management procedures (MPs) in a simulation model of
                              the fishery system with the aim of identifying a management approach that
                              is robust to uncertainties in data collection and the system dynamics.
                              These uncertainties are represented by a collection of alternative operating
                              models (OMs), each with different assumptions regarding population and fleet dynamics."),
                              h4("Instructions"),
                              p('This interactive app has been designed for pairwise comparison between two sets of
                              OMs. Single OMs can be selected from the dropdown menus in each sidebar,
                              or a collection of OMs can be selected by using the checkboxs to
                                include different OM assumptions.'),
                              p('NOTE: This app is in active development and the primary objective at this stage
                                 is to develop and test a suitable framework for presenting the MSE results.
                                 The MSE results presented here are for testing purposes only.
                                Please contact'
                                , a("Adrian Hordyk", href="mailto:ar.hordyk@gmail.com", target="_blank"),
                                'for technical questions or bug reports.')
                              ),
                       column(2),
                       column(12, hr(), br()),
                       )
                   )),
        fluidRow(
            column(2,
                   br(),
                   h4('OM SET 1'),
                   selectInput("OM1","Select a single OM",choices=OMnames,selected=OMnames[1]),
                   h4('Assumptions:'),
                   uiOutput('M_cb_1'),
                   uiOutput('sigmaR_cb_1'),
                   uiOutput('steepness_cb_1'),
                   uiOutput('cpuecv_cb_1'),
                   uiOutput('ESS_cb_1'),
                   uiOutput('llq_cb_1'),
                   uiOutput('env_cb_1'),
                   hr(),
                   h5("Operating models included:",style = "font-weight:bold"),
                   textOutput("OM1_nums")
            ),
            column(8,
                   tabsetPanel(
                       tabPanel(h5("Performance table",style = "color:black"),
                                column(width=6,
                                       h5('OM Set 1'),
                                       DT::dataTableOutput('PTable1')),

                                column(width=6,
                                       h5('OM Set 2'),
                                       DT::dataTableOutput('PTable2'))
                       ),
                       tabPanel(h5("Zeh plots",style = "color:black"),
                                h5("< under construction >",style="color:grey")
                       ),
                       tabPanel(h5("Worm plots",style = "color:black"),
                                column(6,
                                       h5('OM Set 1'),
                                       plotOutput('wormplot1')),
                                column(6,
                                       h5('OM Set 2'),
                                       plotOutput('wormplot2'))
                       ),
                       tabPanel(h5("Trade-off plots",style = "color:black"),
                                column(12,
                                       column(3,
                                              selectInput("T_PMx", "Performance Metric (x-axis)",
                                                          choices=PMnames, selected="P50")
                                              ),
                                       column(3,
                                              selectInput("T_PMy", "Performance Metric (y-axis)",
                                                          choices=PMnames, selected="LTY")
                                              )
                                       ),
                                column(6,
                                       h5('OM Set 1'),
                                       plotOutput('tplot1')),
                                column(6,
                                       h5('OM Set 2'),
                                       plotOutput('tplot2'))
                       )
                   )
            ),
            column(2,
                   br(),
                   h4('OM SET 2'),
                   selectInput("OM2","Select a single OM",choices=OMnames,selected=OMnames[1]),
                   h4('Assumptions:'),
                   uiOutput('M_cb_2'),
                   uiOutput('sigmaR_cb_2'),
                   uiOutput('steepness_cb_2'),
                   uiOutput('cpuecv_cb_2'),
                   uiOutput('ESS_cb_2'),
                   uiOutput('llq_cb_2'),
                   uiOutput('env_cb_2'),
                   hr(),
                   h5("Operating models included:",style = "font-weight:bold"),
                   textOutput("OM2_nums")
            )
        )


))
