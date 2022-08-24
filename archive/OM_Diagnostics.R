devtools::install_github("https://github.com/jabbamodel/ss3diags")

library(ss3diags)
library(r4ss)

sink('test.txt')
for (direct in c("G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/1-M0.1_sigmaR0.2_steepness0.6_cpuelambda0.05_llq1_env-4_iter1",
                 "G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/1-M0.1_sigmaR0.2_steepness0.6_cpuelambda0.05_llq1_env-4_iter1",
                 "G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/1-M0.1_sigmaR0.2_steepness0.6_cpuelambda0.05_llq1_env-4_iter1")) {

replist <- SS_output(dir = direct, printstats = F, covar=T, cormax=0.70,
                     forecast=T,printhighcor=20, printlowcor=20, ncols=201)

runtest.cpue <- SSrunstest(ss3rep = replist,
                           mixing = "less",
                           quants = c("cpue", "len", "age")[1],
                           indexselect = c(2,3,4,5,6,7,8,9,10,11,12,13,14),
                           verbose = TRUE
)

runtest.lth <- SSrunstest(ss3rep = replist,
                          mixing = "less",
                          quants = c("cpue", "len", "age")[2],
                          indexselect = c(1,2,3,4,7,8,9),
                          verbose = TRUE
)

print(direct)
print(runtest.cpue)
print(runtest.lth)
}
sink()
