cmp_project_UI <- function(id) {

  ns <- NS(id)

  fluidPage(
    shinydashboard::box(collapsible = TRUE, width=4,
                        title = 'Projected Index Settings',
                        sliderInput(ns('IndTrend'), 'Index Annual Change (%)', value=0, min=-25, max=25, step=0.5),
                        uiOutput(ns('index_trend_by_year')),
                        sliderInput(ns('histyears'), 'Historical Years', value=2005,
                                    min=min(data$Year), max=max(data$Year), step=1, sep='')
    ),
    shinydashboard::box(collapsible = TRUE, width=8,
                        title =' Index Plot',
                        plotOutput(ns('plot_index'))
    ),
    shinydashboard::box(collapsible = TRUE, width=4,
                        title =' CMP Settings',
                        column(6,
                               shiny::checkboxGroupInput(ns('cmps'), 'CMPs',
                                                         choices=CMPs,
                                                         selected=CMPs[grepl('_b', CMPs)],
                                                         inline=TRUE)
                        ),
                        column(6,
                               sliderInput(ns('lag'), 'Data Lag', 1, 3, 2, step=1),
                               sliderInput(ns('interval'), 'Interval', 1, 5, 3, step=1)
                        ),
                        column(4, actionButton(ns('runMPs'), 'Calculate CMPs', icon('calculator')))
    ),
    shinydashboard::box(collapsible = TRUE, width=8,
                        title =' TAC Plot',
                        plotOutput(ns('plot_tac'))
    ),
    shinydashboard::box(collapsible = TRUE, width=12,
                        title='TAC Table',
                        DT::DTOutput(ns('tac_table'))
    )
  )

}


cmp_project_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 Index_Data <- reactiveValues(Year=data$Year,
                                              Index=data$Index,
                                              Period=data$Period)



                 set_pyears <- reactive(12)
                 output$index_trend_by_year <- renderUI({
                   pyears <- (set_pyears())  + 2 # input$pyears
                   bsCollapse(
                     bsCollapsePanel('Modify Index by Year',
                                     lapply(1:pyears, function(i) {
                                       column(width=4,
                                              numericInput(inputId = ns(paste0("ind", i)), label = paste("Year", i),
                                                           min = 0.5, max = 2, value = 1, step = 0.1)
                                       )

                                     }),
                                     column(4,
                                            br(),
                                            shiny::actionButton(ns('reset_ind_years'), 'Reset', icon('refresh'))
                                            )

                     )
                   )
                 })

                 observeEvent(input$reset_ind_years, {
                   pyears <- (set_pyears()) + 2
                   for (i in 1:pyears) {
                     updateNumericInput(session, inputId = paste0("ind", i), value = 1)
                   }
                 })


                 get_yr_vals <- reactive({
                   pyears <- set_pyears()  +2
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

                   index <- data$Index[!is.na(data$Index)]
                   lastInd <- index[length(index)] # data$Index[length(data$Index)]
                   p_index <- lastInd*((1+input$IndTrend/100)^(1:(pyears+2)))
                   year_mods <- get_yr_vals()
                   p_index <- p_index * year_mods

                   new_years <- seq(from=2025, by=1, length.out=pyears)
                   Index_Data$Year <- c(data$Year, new_years) |> unique()
                   Index_Data$Index <- c(data$Index[1:73], p_index)
                   Index_Data$Period <- c(data$Period, rep('Projection', pyears))

                 })

                 plot_index <- function() {
                   mydf <- data.frame(Year=Index_Data$Year,
                                      Index=Index_Data$Index,
                                      Period=Index_Data$Period)
                   sub <- mydf %>% dplyr::filter(Period=='Historical') %>%
                     filter(Year==max(Year))
                   sub$Period <- 'Projection'
                   mydf <- dplyr::bind_rows(mydf, sub) %>% dplyr::arrange(Year) %>%
                     dplyr::filter(Year>=as.numeric(input$histyears))

                   mydf$Year <- lubridate::ymd(mydf$Year, truncated = 2L)

                   # mydf <<- mydf
                   ggplot(mydf, aes(x=Year, y=Index, color=Period)) +
                     geom_line(linewidth=1.2) +
                     expand_limits(y=c(0, 3)) +
                     theme_bw() +
                     scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
                     theme(legend.position = 'bottom',
                           axis.text=element_text(size=12),
                           axis.title=element_text(size=14),
                           axis.text.x=element_text(angle=60, hjust=1)
                     )
                 }

                 observeEvent(list(input$IndTrend, get_yr_vals()), {
                   update_index_data()
                   TAC_df$tac_df <- make_TAC_df()
                 })
                 output$plot_index <- renderPlot({
                   suppressWarnings(print(plot_index()))
                 })

                 ## ---- TAC ---- ###
                 TAC_df <- reactiveValues(tac_df=NULL)

                 make_TAC_df <- reactive({
                   pyears <- set_pyears()
                   CMPs <- input$cmps
                   nCMPs <- length(CMPs)
                   if (nCMPs<1) return(NULL)
                   hist_years <- 1950:2024 # SWOData@Year
                   new_years <- seq(from=max(hist_years)+1, by=1, length.out=pyears)
                   all_years <- c(hist_years, new_years)
                   # TTT <<- Index_Data$Year
                   # TTTT <<- Index_Data$Index
                   df <- data.frame(Year=rep(all_years, nCMPs),
                              MP=rep(CMPs, each=length(all_years)),
                              TAC=NA,
                              Index=NA)
                   hist_catches <- c(SWOData@Cat[1,], Catchdf$Catch[3:4])
                   catches <- c(hist_catches, rep(NA, pyears))
                   df <- df %>% dplyr::mutate(TAC=rep(catches, nCMPs),
                                              Period=ifelse(Year%in%1950:2024, 'Historical', 'Projection'))
                   df
                 })

                 run_MP <- function(mp_name, data, interval, lag) {

                   # DDD <<- data
                   # # lag <<- lag
                   # # interval <<- interval
                   # MM <<- mp_name

                   mp <- get(mp_name)
                   formals(mp)$Interval <- interval
                   formals(mp)$Data_Lag <- lag

                   mp(1, data)
                 }

                 observeEvent(list(input$cmps, input$lag, input$interval), {
                   TAC_df$tac_df <- make_TAC_df()
                 })


                 run_CMPs <- reactive({

                   TAC_df$tac_df <- make_TAC_df()

                   CMPs <- unique(TAC_df$tac_df$MP)
                   ProYears <- TAC_df$tac_df %>% dplyr::filter(Period=='Projection') %>%
                     dplyr::distinct(Year)

                   for (y in seq_along(ProYears$Year)) {
                     print(ProYears$Year[y])

                     # make data for this year
                     mydf <- data.frame(Year=Index_Data$Year,
                                         Index=Index_Data$Index)

                     proj_years <- max(data$Year):(max(data$Year)+y)
                     sel_years <- max(proj_years) - 1
                     mydf <- mydf %>% dplyr::filter(Year<=max(sel_years))

                     CMP_data <- new('Data')
                     CMP_data@Year <- mydf$Year # [1:(length(mydf$Year)-1)]
                     CMP_data@Ind <- matrix(mydf$Index, nrow=1)
                     cv_ind <- SWOData@CV_Ind
                     addyrs <- length(mydf$Index) - length(cv_ind)
                     cv_ind <- c(cv_ind, rep(cv_ind [1,length(cv_ind[1,])], addyrs))
                     CMP_data@CV_Ind <- array(cv_ind, dim=dim(CMP_data@Ind ))
                     CMP_data@Mort <- SWOData@Mort


                     for (mp in seq_along(CMPs)) {
                       # print(CMPs[mp])


                       tac_df <- TAC_df$tac_df
                       # mydf <<- mydf

                       # add MP specific catch/TAC
                       this_mp <- tac_df %>% dplyr::filter(MP==CMPs[mp], Year%in% mydf$Year)
                       CMP_data@Cat <- matrix(this_mp$TAC, nrow=1)
                       # last TAC
                       lastcatches <- CMP_data@Cat[!is.na(CMP_data@Cat)]
                       CMP_data@MPrec <- lastcatches[length(lastcatches)]
                       # run MP
                       # MYDATA <<- CMP_data
                       # interval <<- as.numeric(input$interval)
                       # datalag <<-as.numeric(input$lag)
                       # cbind(MYDATA@Year, MYDATA@Ind[1,], MYDATA@Cat[1,])
                       # CE_60(1, MYDATA, datalag, interval)

                       tac <- run_MP(CMPs[mp], CMP_data, as.numeric(input$interval), as.numeric(input$lag))


                       # update TAC_df
                       TAC_df$tac_df <- TAC_df$tac_df %>%
                         dplyr::mutate(TAC= ifelse(MP==CMPs[mp] & Year==ProYears$Year[y], tac@TAC, TAC))

                     }

                   }
                   TAC_df$tac_df

                 })


                 observeEvent(input$runMPs, {
                   out <- run_CMPs()
                 })


                 plot_TAC <- reactive({
                   plot_df <-  TAC_df$tac_df %>%
                     dplyr::filter(Year>=as.numeric(input$histyears))

                   plot_df$Year <- lubridate::ymd(plot_df$Year, truncated = 2L)

                   # hist plot
                   hist_df <- plot_df %>% dplyr::filter(Period=='Historical', MP==unique(plot_df$MP)[1])
                   proj_df <- plot_df %>% dplyr::filter(Period!='Historical', MP==unique(plot_df$MP)[1])
                   proj_df$TAC <- NA
                   hist_df <- dplyr::bind_rows(hist_df, proj_df)


                   p1 <- ggplot(hist_df, aes(x=Year, y=TAC)) +
                     geom_line(linewidth=1.2) +
                     scale_x_date(date_breaks = "1 year",date_labels = "%Y")

                   proj_df <- plot_df %>% dplyr::filter(Period!='Historical')

                   p1 +
                     geom_line(data=proj_df, aes(x=Year, y=TAC, color=MP),
                               linewidth=1.2) +
                     geom_point(data=proj_df, aes(x=Year, y=TAC, color=MP),
                                size=2)+
                     expand_limits(y=c(0, 20000)) +
                     theme_bw() +
                     labs(y='Catch/TAC') +

                     theme(legend.position = 'bottom',
                           axis.text=element_text(size=12),
                           axis.title=element_text(size=14),
                           axis.text.x=element_text(angle=60, hjust=1))

                 })


                 output$plot_tac <- renderPlot({
                   suppressWarnings(print(plot_TAC()))
                 })

                 output$tac_table <- DT::renderDataTable({
                   df <- TAC_df$tac_df %>% dplyr::filter(Year>=2025)
                   df$TAC <- round(df$TAC, 0)

                   # index <- df %>% dplyr::filter(MP==unique(df$MP)[1]) %>%
                   #   dplyr::select(Index)

                   df <- df %>% dplyr::select(Year, MP, TAC) %>%
                     tidyr::pivot_wider(names_from = MP, values_from = TAC)
                   # df$Index <- index$Index
                   DT::datatable(df, rownames = FALSE,
                                 options=list(pageLength =20,
                                              dom = 't'))
                 })
               }
  )
}
