
h2CR <- function(h) {
  (4*h)/(1-h)
}

CR2h <- function(CR) {
  CR/(CR+4)
}

BH_SRR <- function(R0, h, SB, SBpR) {
  (4*R0*h*SB)/(SBpR*R0*(1-h)+(5*h-1)*SB)
}

# Plot steepness BH
SB0 <- 1
R0 <- 1
SBpR <- SB0/R0
SB <- seq(0, SB0, length.out=1000)
h_vals <- c(0.75, 0.8, 0.85, 0.9, 0.95)
out_list <- list()
for (i in seq_along(h_vals)) {
  h <- h_vals[i]
  out_list[[i]] <- data.frame(SB=SB, Rec=BH_SRR(R0, h, SB, SBpR), h=h)
}

df <- do.call('rbind', out_list)
df$h <- factor(df$h)

library(ggplot2)
ggplot(df, aes(x=SB, y=Rec, color=h)) +
  geom_line(linewidth=2) +
  labs(x='Spawning Biomass', y='Recruitment', color='Steepness (h)') +
  geom_vline(xintercept = 0.2, linetype=2) +
  theme_bw()


steepness <- seq(0.2, 0.98, by=0.001)
CR <- h2CR(steepness)

plot(steepness, CR, type='l',
     xlab='Steepness (h)',
     ylab='Compensation Ratio (CR)',
     lwd=2)

CRs <- h2CR(c(0.74, 0.88))
CRs[3] <- (CRs[2] * CRs[2]/CRs[1])
hs <- CR2h(CRs)
hs

points(hs, CRs2, pch=16, cex=3)
abline(h=CRs2, lty=3)


-log(0.001)/0.1
-log(0.001)/0.2
-log(0.001)/0.3

69/34
34/23

CRs <- h2CR(c(0.69, 0.88))
CRs

tt <- exp(mean(log(CRs)))

exp(sum(log(CRs))/2)

CRs2 <- c(CRs[1], tt, CRs[2])

CRs2[2]/CRs2[1]
CRs2[3]/CRs2[2]

(CRs2[2] * CRs2[2]/CRs2[1])
(CRs2[2] * CRs2[2]/CRs2[3])

CRs2
hs <- CR2h(CRs2)





CRs[3] <- (CRs[2] * CRs[2]/CRs[1])
hs <- CR2h(CRs)
hs

CRs


h2CR(0.9)
h2CR(0.8989)

