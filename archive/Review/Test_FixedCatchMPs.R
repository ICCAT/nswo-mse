# Test Fixed Catch MPs

library(SWOMSE)
packageVersion('SWOMSE') # 0.5.0
packageVersion('MSEtool') # 3.2.0

# Fixed TAC MPs from reviewer
# https://bitbucket.org/lfr_code/swo_codereview/src/master/stressTests/SWO_fixedCMPs.R
SWO_fixedTAC <- function(x, Data, reps=48, constC = 0, ... )
{
  TAC <- rep(constC, reps)
  TAC <- TACfilter(TAC) * 1e3
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}

makeGridFixedCMPs <- function(  Cgrid = seq(from = 0, to = 0.9, by = 0.01),
                                outFile = "autoCatchgridMPs.R",
                                baseMP = "SWO_fixedTAC" )
{
  cat("# Automatically generated MPs for SWO stress test./n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  for( gridIdx in 1:length(Cgrid) )
  {
    TAC <- Cgrid[gridIdx]

    mpName <- paste0("SWO_fixTAC",TAC)

    cat("# TAC = ", TAC , "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- ", baseMP, "\n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$constC <- ", TAC, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)

  }

  cat("# End automatically generated grid \n", sep = "", file = outFile, append = TRUE)

} # END makeGridCatchMPs

makeGridFixedCMPs(outFile = file.path('Review', 'autoCatchgridMPs.R'))

# Source CMPs
source(file.path('Review', 'autoCatchgridMPs.R'))

AllMPs <- avail('MP')
gridMPs <- AllMPs[grepl('SWO_fix', AllMPs)]

mpOut <- runMP(SWOData, MPs = gridMPs)

# run MSE
OM <- OM_1
OM@nsim <- 4
testMSE <- runMSE(OM, MPs = gridMPs)
saveRDS(testMSE, "Review/FixedTAC_MSE.rda")

# test Catch == TAC
MSE <- readRDS("Review/FixedTAC_MSE.rda")

Catch_mean <- apply(MSE@Catch, 2:3, mean)
Catch_min <- apply(MSE@Catch, 2:3, min)
Catch_max <- apply(MSE@Catch, 2:3, max)

plot(apply(mpOut@TAC, 1, mean), apply(Catch_mean, 1, mean), xlab="Fixed TAC",
     ylab="Simulated Catch", type="b", pch=16)
abline(0, 1, lty=3)


MSE@Catch[1,5,]
MSE@Removals[1,5,]
MSE@TAC[1,5,]





