QuiltPlot_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$QuiltPlots <- renderUI({

                   pPM_results <- results$pPM_results
                   Models <- metadf$OMnames %>% unique()
                   nTabs <- length(Models)
                   mpTabs <- lapply(1:nTabs,
                                    function(x) {
                                      qPM_results <- pPM_results %>% filter(Model==Models[x])
                                      quiltPMs <- results$QuiltPMs

                                      tabPanel(Models[x],
                                               br(),
                                               if(nrow(qPM_results)<1) {
                                                 h4('No CMPs pass the CMP Filters',style="color:red")
                                               } else if ( length(quiltPMs)<1) {
                                                 h4('Need to select at least 1 PM',style="color:red")
                                               } else {
                                                 DT::renderDataTable({
                                                   Quilt(qPM_results, PMs=quiltPMs, results$show_dominated)
                                                 })
                                               }


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
      p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
      htmlOutput(ns('QuiltPlots'))
    )
  )
}



Quilt <- function(PM_results, PMs=NULL, show_dominated=FALSE) {

  PM_results$Value <- round(PM_results$Value,2)
  tab <- PM_results %>% select(PM, MP, Value) %>% filter(PM %in% PMs)

  tab$PM <- factor(tab$PM, levels=PMs, ordered = TRUE)
  tab <- tab %>% group_by(PM) %>% arrange()
  tab <- tab  %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)


  colorRampAlpha <- function(..., n, alpha) {
    colors <- colorRampPalette(...)(n)
    paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
  }

  cols <- c("blue", "white")


  # Probability colors
  probs <- seq(0, 1.01, length.out=50)
  prob_colors <- rev(colorRampAlpha(cols, n=length(probs)+1, alpha=0.5))
  rev_prob_colors <- rev(prob_colors)

  # TAC colors
  TAC_PMs <- PM_results$Name[grepl('TAC', PM_results$Name )] %>% unique()


  # Variability colors


  # Make table
  tab$MP <- factor(tab$MP)

  # calculate dominated
  if (show_dominated)
    tab <- calc_dominated(tab)



  quilt <-  DT::datatable(tab, extensions = 'Buttons',
                          options = list( dom = 'tB', pageLength =100, buttons=c('copy', 'csv')),
                          filter = list(
                            position = 'top', clear = FALSE
                          ))

  for (i in 2:ncol(tab)) {
    pm <- colnames(tab)[i]

    if (grepl('TAC', pm)) {
      cuts <- seq(min(tab[,i]), max(tab[,i])*1.1, length.out=10)
      values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5))

    } else if (grepl('VarC', pm)) {
      # variability
      cuts <- seq(0, 1, length.out=100)
      values <- colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5)

    } else {
      # probabilities
      cuts <- seq(0, 1.01, length.out=50)
      values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5) )
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


calc_dominated <- function(tab) {

  nPMs <- ncol(tab)-1
  MPs <- as.character(tab$MP) %>% unique()
  nMPs <- length(MPs)
  if (!is.null(tab[['VarC']])) {
    tab[['VarC']] <- -tab[['VarC']]
  }

  dom_grid <- matrix('', nrow=nMPs, ncol=1)
  rownames(dom_grid) <- MPs


  for (i in 1:nMPs) {
    tab_mp <- tab %>% filter(MP==MPs[i])
    tab_all <- tab %>% filter(MP!=MPs[i])

    tab_mp <- tab_mp  %>% pivot_longer(cols=2:ncol(tab_mp))
    tab_all <- tab_all  %>% pivot_longer(cols=2:ncol(tab_all))
    nmp <- tab_all$MP %>% unique() %>% length()

    tab_all$compare_val <- rep(tab_mp$value,nmp)
    domCMP <- tab_all %>%
      filter(value>compare_val) %>%
      group_by(MP) %>% distinct(name) %>%
      summarise(n=length(name)) %>%
      filter(n==nPMs)

    dom_grid[i, ] <- paste(domCMP$MP, collapse=', ')


  }
  tab$Out_Performed <- FALSE
  tab$Out_Performed_By <-dom_grid[,1]

  tab$Out_Performed[nchar(tab$Out_Performed_By)>0] <- TRUE

  if (!is.null(tab[['VarC']])) {
    tab[['VarC']] <- -tab[['VarC']]
  }
  tab





}

