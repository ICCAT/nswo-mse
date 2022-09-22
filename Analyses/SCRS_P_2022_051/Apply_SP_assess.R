library(SWOMSE)

img.dir <- 'img/SP_assess'
obj.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/OM_objects'

# Apply different Assessment models to SWOData ----

# openMSE assessment models

st <- Sys.time()
SP_assess <- SP(1, SWOData)
Sys.time() - st

SP_Fox_assess <- mod2 <- SP_Fox(1, SWOData)
SP_Fox_assess@Model <- 'SP_Fox'

st <- Sys.time()
SP_SS_assess <- SP_SS(1, SWOData)
Sys.time() - st

# JABBA
catch <- data.frame(SWOData@Year, SWOData@Cat[1,])
index <- data.frame(SWOData@Year,SWOData@Ind[1,])
index_se <- data.frame(SWOData@Year, SWOData@CV_Ind[1,])
index_se[,2] <- 0.23

input <- JABBA::build_jabba(catch=catch,
                            cpue=index,
                            se=index_se,
                            catch.cv=0.01,
                            assessment="SWO",
                            scenario = "1",
                            model.type = "Schaefer",
                            sigma.est = FALSE,
                            fixed.obsE = 0.01,
                            r.prior = c(0.42, 0.4),
                            psi.dist='beta',
                            psi.prior=c(0.95, 0.05),
                            verbose=FALSE)

# apply assessment model

st <- Sys.time()
JABBA_assess <- JABBA::fit_jabba(input,quickmcmc=TRUE, verbose=FALSE)
Sys.time() - st


JABBA_assess$timeseries[1,,3]

# SPICT
library(MSEextra)

st <- Sys.time()
SPICT_assess <- MSEextra::spict(1, SWOData)
Sys.time() - st

# Time:
# SP - 0.03
# SP_SS - 0.08
# JABBA - 23 seconds
# SPiCT - 1.4

Extract_Results <- function(mod) {
  Name <- mod@Model
  B <- mod@B
  B_BMSY <- mod@B_BMSY
  Year <- as.numeric(names(B) )
  data.frame(Name, Year, B_BMSY=B_BMSY, B=B)
}

res_List <- lapply(list(SP_assess, SP_Fox_assess, SP_SS_assess, SPICT_assess), Extract_Results)

res_DF <- do.call('rbind', res_List)
res_DF$Name[res_DF$Name=='spict'] <- 'SPiCT'

# add JABBA
jabba_df <- data.frame(Name='JABBA', Year=1950:2020,
                       B_BMSY=as.numeric(JABBA_assess$timeseries[,1,3]),
                       B=as.numeric(JABBA_assess$timeseries[,1,1]))
res_DF <- bind_rows(res_DF, jabba_df)


## Plot Assessments  ----
res_DF$Name <- factor(res_DF$Name, levels=unique(res_DF$Name), ordered = TRUE)
nms <- levels(res_DF$Name)

res_DF2 <- res_DF %>% tidyr::pivot_longer(., cols=3:4)

range_act <- res_DF2 %>% group_by(name, Name) %>% summarize(Year=2020,
                                                            value=max(value))

cols <- RColorBrewer::brewer.pal(5, 'Set1')

facet_names <- list(
  'B'="Biomass",
  'B_BMSY'=expression(B/B[MSY])
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

for (i in 1:length(nms)) {
  nm <- nms[1:i]
  df <- res_DF2 %>% filter(Name %in% nm)
  p <- ggplot(df,
              aes(x=Year, y=value, color=Name)) +
    facet_wrap(~name, scales='free', labeller=facet_labeller) +
    expand_limits(y=0) +
    geom_blank(data=range_act) +
    theme_bw() +
    geom_line() +
    scale_color_manual(values = cols)

  image.name <- paste0(i,'_', paste(nm, collapse = '_'), '.png')
  ggsave(file.path(img.dir, image.name), width=8, height=4)
}


