QuiltPlot_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$QuiltPlots <- renderUI({

                   Models <- metadf$OMnames %>% unique()

                   pPM_results <- PM_results

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
                   } else if(results$LRP_pass=='Fail') {
                     pPM_results <- pPM_results %>% filter(MP %in% fail_MPs$MP)
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

                   nModels <- length(Models)

                   # filter by MP
                   pPM_results <- pPM_results %>% filter(MP %in% results$select_MPs)

                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)

                   nTabs <- length(Models)
                   if(nTabs<1) return(NULL)

                   mpTabs <- lapply(1:nTabs,
                                    function(x) {
                                      qPM_results <- pPM_results %>% filter(Model==Models[x])
                                      quiltPMs <- c('AvTAC_med', 'AvTAC_short', 'TAC1', 'PGK_med', 'VarC', 'PNOF')
                                      tabPanel(Models[x],
                                               br(),
                                               DT::renderDataTable({
                                                 Quilt(qPM_results, PMs=quiltPMs)
                                               }
                                               )

                                      )
                                    }
                   )


                   do.call(tabsetPanel, mpTabs)})
               }
  )

}

QuiltPlot_UI <- function(id, label="QuiltPlot") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('Quilt Plots'),
      htmlOutput(ns('QuiltPlots'))
    )
  )
}



Quilt <- function(PM_results, PMs=NULL) {

  PM_results$Value <- round(PM_results$Value,2)
  tab <- PM_results %>% select(PM, MP, Value) %>% filter(PM %in% PMs)

  tab$PM <- factor(tab$PM, levels=PMs, ordered = TRUE)
  tab <- tab %>% group_by(PM) %>% arrange()
  tab <- tab  %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)


  colfunc <- colorRampPalette(c("blue", "white"), alpha=TRUE)

  # Probability colors
  probs <- seq(0, 1.01, length.out=50)
  prob_colors <- rev(colfunc(length(probs)+1))
  rev_prob_colors <- rev(prob_colors)

  # TAC colors
  TAC_PMs <- PM_results$Name[grepl('TAC', PM_results$Name )] %>% unique()


  # Variability colors


  # Make table
  tab$MP <- factor(tab$MP)
  quilt <-  DT::datatable(tab, extensions = 'Buttons',
                          options = list( pageLength =30, buttons=c('copy', 'csv')),
                          filter = list(
                            position = 'top', clear = FALSE
                          ))

  for (i in 2:ncol(tab)) {
    pm <- colnames(tab)[i]

    if (grepl('TAC', pm)) {
      cuts <- seq(min(tab[,i]), max(tab[,i])*1.1, length.out=10)
      values <- rev(colfunc(length(cuts)+1))

    } else if (grepl('VarC', pm)) {
      # variability
      cuts <- seq(0, 1, length.out=100)
      values <- (colfunc(length(cuts)+1))

    } else {
      # probabilities
      cuts <- seq(0, 1.01, length.out=50)
      values <- rev(colfunc(length(cuts)+1))
    }
    quilt <- quilt %>%
      formatStyle(
        pm,
        backgroundColor = styleInterval(cuts=cuts,
                                        values=values)

      )

  }
  quilt
}




