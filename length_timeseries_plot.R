library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

FleetInfo <- readxl::read_xlsx('C:/users/Adrian/desktop/lths.xlsx', sheet=2)
Data <- readxl::read_xlsx('C:/users/Adrian/desktop/lths.xlsx', sheet=1)


Data <- Data %>% tidyr::pivot_longer(cols=7:ncol(Data))

Data$Bin <- Data$name
Data$Obs <- Data$value
Data$Year <- Data$`#_yr`
Data <- Data %>% dplyr::filter(Year>0)

Data <- dplyr::left_join(Data, FleetInfo, by='fleet')
Data$Name <- factor(Data$Name, ordered = TRUE, levels=FleetInfo$Name)

head(Data)


DF2 <- Data %>% dplyr::group_by(Name, Year) %>%
  summarize(`119`=sum(Obs[Bin <=119])/sum(Obs),
            `125`=sum(Obs[Bin <=125])/sum(Obs), .groups='keep')


DF2 <- tidyr::pivot_longer(DF2, 3:4)

ggplot(DF2, aes(x=Year, y=value*100, linetype=name)) +
  geom_line(size=1) +
  facet_wrap(~Name, ncol=4) +
  expand_limits(y=c(0,100)) +
  theme_bw() +
  labs(x="Year", y="%", linetype="% below")
