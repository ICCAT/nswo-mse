TradeOff_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$TradeOff <- renderUI({

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
                   nTabs <- length(MPnames)
                   if(nTabs<1) return(NULL)


                   MPs <- unique(pPM_results$MP)
                   MPnames <- unique(pPM_results$MP_name)
                   if(nModels<1) return(NULL)

                   mpTabs <- lapply(1:nModels,
                                    function(x) {
                                      # trade-off plots
                                      pPM_results2 <- pPM_results %>% filter(Model==Models[x])
                                      tabPanel(Models[x],
                                               br(),

                                               renderPlot({
                                                 p1 <- tradeoffplot(pPM_results2, results$TO1x, results$TO1y)
                                                 p2 <- tradeoffplot(pPM_results2, results$TO2x, results$TO2y)
                                                 p3 <- tradeoffplot(pPM_results2, results$TO3x, results$TO3y)
                                                 p4 <- tradeoffplot(pPM_results2, results$TO4x, results$TO4y)

                                                 cowplot::plot_grid(p1,p2,p3,p4)
                                               }, height=1000, width=1000)
                                      )
                                    })
                   do.call(tabsetPanel, mpTabs)})
               }
  )

}



TradeOff_UI <- function(id, label="TradeOff") {

  ns <- NS(id)

  tagList(

    fluidRow(
      h4('Trade-Off Plots'),
      htmlOutput(ns('TradeOff'))



    )
  )
}


tradeoffplot <- function(df, tox, toy) {
  df <- df %>% filter(PM %in% c(tox, toy))
  caption_df <- df %>% distinct(PM, caption) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = caption)

  df2 <- df %>% select(PM, Value, MP) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)


  ind <- match(tox, colnames(df2))
  colnames(df2)[ind] <- 'x'
  ind <- match(toy, colnames(df2))
  colnames(df2)[ind] <- 'y'

  ind <- match(tox, colnames(caption_df))
  colnames(caption_df)[ind] <- 'x'
  ind <- match(toy, colnames(caption_df))
  colnames(caption_df)[ind] <- 'y'

  # x and y limits
  caption_df$x <- paste0(tox, ":", caption_df$x)
  caption_df$y <- paste0(toy, ":", caption_df$y)
  caption_df <<- caption_df
  # PGK
  xlim <- c(0,1)
  ylim <- c(0,1)
  if (grepl('PGK', caption_df$x))
    xlim <- c(0.5, 1)

  if (grepl('PGK', caption_df$y))
    ylim <- c(0.5, 1)

  # TAC
  if (grepl('TAC', caption_df$x))
    xlim <- c(-Inf, Inf)

  if (grepl('TAC', caption_df$y))
    ylim <- c(-Inf, Inf)


  # LRP
  if (grepl('LRP', caption_df$x))
    xlim <- c(0.8, 1)

  if (grepl('LRP', caption_df$y))
    ylim <- c(0.8, 1)

  # VarC
  if (grepl('VarC', caption_df$x))
    xlim <- c(-0.25, 0)

  if (grepl('VarC', caption_df$y))
    ylim <- c(-0.25, 0)



  if (grepl('VarC', caption_df$x))
    df2$x <- -df2$x

  if (grepl('VarC', caption_df$y))
    df2$y <- -df2$y

  ggplot(df2, aes(x=x, y=y)) +
    theme_bw() +
    labs(x=caption_df$x, y=caption_df$y) +
    geom_point(data=df2, aes(color=MP)) +
    ggrepel::geom_text_repel(data=df2, aes(label=MP, color=MP)) +
    guides(color='none') +
    expand_limits(x=xlim, y=ylim)
}







