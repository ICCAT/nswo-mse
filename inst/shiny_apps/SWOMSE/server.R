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
                            PGK_pass=0.50,
                            filterbuttonColor=1,
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
                            mp_select=short_list_mps[[1]],
                            short_list=short_list,
                            selected_shortlist=names(short_list_mps)[1],
                            passMPs=allMPs,
                            pPM_results=pPM_results,
                            pTS_results=pTS_results,
                            kobe_results=kobe_results,
                            pkobe_results=pkobe_results,
                            pViolin_results=pViolin_results,
                            Filt=FALSE)

  output$Filt <- reactive({ results$Filt })
  outputOptions(output, "Filt", suspendWhenHidden = FALSE)

  output$PMs <- function(){
    PM_desc <- PM_desc %>% filter(Name!='PGK_6_10')

    PM_desc %>% knitr::kable(escape = T,
                             booktabs=TRUE) %>%
      kableExtra::kable_styling("striped", full_width = T) %>%
      kableExtra::column_spec(1, bold=T)
  }

  output$MPs <- function(){
    # CMP_desc <- read.csv('inst/CMP_description.csv')
    CMP_desc <- read.csv('../../CMP_description2.csv') %>% arrange(CMP.name)
    colnames(CMP_desc)[1] <- 'Name'
    CMP_desc <- CMP_desc %>% filter(Name %in% c('CE', 'FX4',
                                              'MCC5', 'MCC7', 'SPSSFox'))
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
  add_analyses_Server('add_analyses',results)
  # cmp_project_server('cmp_project_1')

}

