options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output, session) {
  useShinyjs()



  results <- reactiveValues(PM_results=PM_results,
                            LRP_value=0.15,
                            LRP_models='Reference + Robustness',
                            LRP_pass='Pass',
                            PGK_models='Reference + Robustness',
                            PGK_pass=0.51,
                            TO1x=tradeoffPMs$x[1],
                            TO1y=tradeoffPMs$y[1],
                            TO2x=tradeoffPMs$x[2],
                            TO2y=tradeoffPMs$y[2],
                            TO3x=tradeoffPMs$x[3],
                            TO3y=tradeoffPMs$y[3],
                            TO4x=tradeoffPMs$x[4],
                            TO4y=tradeoffPMs$y[4],
                            select_MPs=allMPs)


  output$PMs <- function(){
    PM_desc <- PM_desc %>% filter(Name!='PGK_6_10')

    PM_desc %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  FiltersServer('filters', results)
  CMPPerf_Server('CMPPerf',results)
  TradeOff_Server('TradeOff',results)
  KobeTime_Server('KobeTime',results)
  QuiltPlot_Server('QuiltPlot',results)

}

