library(shiny)
library(ggplot2)

fls <- list.files('CMPs')
for (fl in fls) source(file.path('CMPs', fl))

MCC5_60 <- MCC5_b


MCC5_70 <- MCC5_c


MCC7_60 <- MCC7_b

MCC7_70 <- MCC7_c

CE_60 <- CE_c

CMPs <- c('MCC5_60',
          'MCC5_70',
          'MCC7_60',
          'MCC7_70',
          'CE_60')


Data <- SWOMSE::SWOData

data <- data.frame(Year=Data@Year,
                   Catch=Data@Cat[1,],
                   Index=Data@Ind[1,],
                   Type='Reported')

## add extra years
data <- rbind(data,
              data.frame(Year=2021:2023,
                         Catch=Catchdf$Catch,
                         Index=NA,
                         Type=c('Reported', 'Assumed', 'Assumed')
              ))

# update index
dat = read.csv("../../../TAC1/SWOForTom.csv")

data$Index[data$Year %in% dat$Year] <- dat$CombinedIndex


p1 <- ggplot(data, aes(x=Year, y=Index)) +
  expand_limits(y=0) +
  geom_line() +
  theme_bw()

p2 <- ggplot(data, aes(x=Year, y=Catch)) +
  expand_limits(y=0) +
  geom_line() +
  theme_bw()





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
            column(6,                   )
          ),


            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel('Index Scenarios',
                         plotOutput("distPlot")
                         ),
            tabPanel('Historical Data',
                         plotOutput("distPlot1"))
          )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
