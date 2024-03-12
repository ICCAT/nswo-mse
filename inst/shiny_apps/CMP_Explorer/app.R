

#
# thisData <- new('Data')
# thisData@Year <- data$Year
# thisData@Cat <- matrix(data$Catch, nrow=1)
# thisData@Ind <- matrix(data$Index, nrow=1)
#
# mp <- get(CMPs[3])
# mp(1, thisData)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CMP Explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          shiny::checkboxGroupInput('cmps', 'CMPs',
                                    choices=CMPs,
                                    selected=CMPs),
          fluidRow(
            column(6, sliderInput('IndTrend', 'Index Annual Change (%)', value=0, min=-5, max=5, step=0.5)),
            # column(6, numericInput('pyears', 'N projection years', 10, 2, 20, step=1))
            column(6, sliderInput('histyears', 'Historical Years', value=min(data$Year),
                                  min=min(data$Year), max=max(data$Year), step=1, sep=''))
          ),
          uiOutput('index_trend_by_year'),
          fluidRow(
            column(6, sliderInput('interval', 'Interval', 1, 5, 2, step=1)),
            column(6, sliderInput('lag', 'Data Lag', 1, 5, 2, step=1))
          )
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput('plot_index')),
            column(6, plotOutput('plot_data2'))
          )
        )
    )
)

server <- function(input, output) {

  Data <- reactiveValues(Year=data$Year,
                         Index=data$Index,
                         Catch=data$Catch,
                         Period=data$Period)



  set_pyears <- reactive(10)
  output$index_trend_by_year <- renderUI({
    pyears <- set_pyears() # input$pyears
    bsCollapse(
      bsCollapsePanel('Modify Index by Year',
                      lapply(1:pyears, function(i) {
                        column(width=4,
                               numericInput(inputId = paste0("ind", i), label = paste("Year", i),
                                            min = 0.5, max = 2, value = 1, step = 0.1)
                        )

                      })
      )
    )
  })

  get_yr_vals <- reactive({
    pyears <- set_pyears()
    get_vals <- rep(NA, pyears)
    ids <- paste0('ind', 1:pyears)
    for (i in 1:pyears) {
      req(input[[ids[i]]])
      get_vals[i] <- input[[ids[i]]]
    }
    get_vals
  })

  update_index_data <- reactive({
    pyears <- set_pyears()
    lastInd <- data$Index[length(data$Index)]
    p_index <- lastInd*((1+input$IndTrend/100)^(1:pyears))
    year_mods <- get_yr_vals()
    p_index <- p_index * year_mods

    new_years <- seq(from=max(data$Year)+1, by=1, length.out=pyears)
    Data$Year <- c(data$Year, new_years)
    Data$Index <- c(data$Index, p_index)
    Data$Period <- c(data$Period, rep('Projection', pyears))

  })

  plot_index <- function() {
    mydf <- data.frame(Year=Data$Year,
                     Index=Data$Index,
                     Period=Data$Period)
    sub <- mydf %>% dplyr::filter(Period=='Historical') %>%
      filter(Year==max(Year))
    sub$Period <- 'Projection'
    mydf <- dplyr::bind_rows(mydf, sub) %>% dplyr::arrange(Year) %>%
      dplyr::filter(Year>=as.numeric(input$histyears))


    ggplot(mydf, aes(x=Year, y=Index, color=Period)) +
      geom_line() +
      expand_limits(y=c(0, 3)) +
      theme_bw() +
      theme(legend.position = 'bottom',
            )


  }
  output$plot_index <- renderPlot({
    update_index_data()
    plot_index()
      # par(mfrow=c(2,1))
      # plot(Data$Year, Data$Index, type='l', ylim=c(0, max(Data$Index)), xlab='Year', ylab='Index',
      #      xaxs='i', yaxs='i', bty='l')
      # # plot(Data$Year, Data$Catch, type='l')

  })

}

# Run the application
shinyApp(ui = ui, server = server)
