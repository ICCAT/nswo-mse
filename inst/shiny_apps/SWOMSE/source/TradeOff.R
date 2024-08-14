TradeOff_Server <- function(id, results) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$TradeOff <- renderUI({

                   Models <- metadf$OMnames %>% unique()
                   nModels <- length(Models)

                   pPM_results <- results$pPM_results

                   if(nModels<1) return(NULL)

                   mpTabs <- lapply(1:nModels,
                                    function(x) {
                                      # trade-off plots
                                      pPM_results2 <- pPM_results %>% filter(Model==Models[x])
                                      tabPanel(Models[x],
                                               br(),
                                               if(nrow(pPM_results2)<1) {
                                                 h4('No CMPs pass the CMP Filters',style="color:red")
                                               } else {
                                                 renderPlot({
                                                   p1 <- tradeoffplot(pPM_results2, results$TO1x, results$TO1y)
                                                   p2 <- tradeoffplot(pPM_results2, results$TO2x, results$TO2y)
                                                   p3 <- tradeoffplot(pPM_results2, results$TO3x, results$TO3y)
                                                   p4 <- tradeoffplot(pPM_results2, results$TO4x, results$TO4y)
                                                   cowplot::plot_grid(p1,p2,p3,p4)
                                                 }, height=1000, width=1000)
                                               }

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
      p('Only showing results for CMPs that pass filters and are selected in `CMP Filters`'),
      htmlOutput(ns('TradeOff'))



    )
  )
}


tradeoffplot <- function(df, tox, toy, size_point=1.2, size_text=6,
                         size_axis_text=12,
                         size_axis_title=14) {
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
    geom_point(data=df2, aes(color=MP), size=size_point) +
    ggrepel::geom_text_repel(data=df2, aes(label=MP, color=MP), size=size_text) +
    guides(color='none') +
    expand_limits(x=xlim, y=ylim) +
    theme(axis.title = element_text(size=size_axis_title),
          axis.text=element_text(size=size_axis_text))
}







