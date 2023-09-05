options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output, session) {
  useShinyjs()



  results <- reactiveValues(PM_results=PM_results,
                            LRP_value=0.15,
                            LRP_models='Reference Only',
                            LRP_models_cb=levels(OMnames)[2:length(levels(OMnames))],
                            LRP_pass='Pass',
                            PGK_short=c('0.51', '0.60', '0.70'),
                            PGK_models='Reference + Robustness',
                            PGK_models_cb=levels(OMnames)[2:length(levels(OMnames))],
                            PGK_pass=0.51,
                            TO1x=tradeoffPMs$x[1],
                            TO1y=tradeoffPMs$y[1],
                            TO2x=tradeoffPMs$x[2],
                            TO2y=tradeoffPMs$y[2],
                            TO3x=tradeoffPMs$x[3],
                            TO3y=tradeoffPMs$y[3],
                            TO4x=tradeoffPMs$x[4],
                            TO4y=tradeoffPMs$y[4],
                            QuiltPMs=quiltPMs,
                            compare_MPs=allMPs[1:3],
                            Selected_Model=metadf$OMnames[1],
                            show_dominated=FALSE,
                            select_MPs=allMPs,
                            passMPs=allMPs,
                            pPM_results=PM_results,
                            pTS_results=TS_results)


  output$PMs <- function(){
    PM_desc <- PM_desc %>% filter(Name!='PGK_6_10')

    PM_desc %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  output$MPs <- function(){
    # CMP_desc <- read.csv('inst/CMP_description.csv')
    CMP_desc <- read.csv('../../CMP_description.csv') %>% arrange(CMP.name)
    colnames(CMP_desc)[1] <- 'Name'

    CMP_desc %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  output$OMs <- function(){
    OM_desc <- read.csv('../../OM_Description.csv')
    OM_desc %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  output$tunings <- function(){
    TuneTargets %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  output$links <- renderUI({
      tagList(
        p(a("Homepage", href="https://iccat.github.io/nswo-mse/", target="_blank")),
        p(a("Trial Specs Doc", href="https://iccat.github.io/nswo-mse/TS/Trial_Specs.html", target="_blank"))
      )
    })




  FiltersServer('filters', results)
  CMPPerf_Server('CMPPerf',results)
  CMPCompare_Server('CMPCompare',results)
  TradeOff_Server('TradeOff',results)
  KobeTime_Server('KobeTime',results)
  QuiltPlot_Server('QuiltPlot',results)
  Violin_Server('Violin',results)

}

