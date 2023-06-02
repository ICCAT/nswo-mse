library(SWOMSE)

# ---- Reference OMs ----
Ref_OMs <- OM_DF %>% filter(Class=='Reference')

MOM_Objects <- Ref_OMs$OM.object
for (i in seq_along(MOM_Objects)) {
  MOM <- get(MOM_Objects[i])
  multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)
  nm <- paste0(MOM_Objects[i], '.hist')
  saveRDS(multiHist, file.path('Hist_Objects', nm))
}




