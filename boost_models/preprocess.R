library(readr)

# cb_wr contains all data for all WRs and CBs
cb_wr <- read_csv("~/Documents/GitHub/APM-Term-Project/additional_data/cb_wr.csv", col_types = cols(X1 = col_skip()))
nrow(cb_wr)

# all distance metrics
relative <- read_csv("Documents/GitHub/APM-Term-Project/boost_models/data/all_relative_info.csv",  col_types = cols(X1 = col_skip()))
nrow(relative)

# merge
df <- merge(cb_wr, relative, by=c("gameId","playId","nflId",'frameId'))
nrow(df)
write.csv(df,"~/Documents/GitHub/APM-Term-Project/additional_data//cb_wr_merge_relative.csv", row.names = FALSE)

# subset data to only include data when ball is snapped
library(tidyverse)
my_data <- as_tibble(df)
df1 <- my_data %>% filter(event == "ball_snap")

# import target data
targetedReceiver <- read_csv("Documents/GitHub/APM-Term-Project/nfl-big-data-bowl-2021-bonus/targetedReceiver.csv")
df2 <- merge(df1, targetedReceiver, by=c("gameId","playId"))
my_data2 <- as_tibble(df2)


# subset data to only keep targeted receivers
df3 <- df2[(df2$targetNflId == df2$nflId),] #  (data$V2 < 4), ]
df3 <- drop_na(df3)
View(df3)

# check duplicates
#duplicated(df)
#x[!duplicated(x)]


table(df$passResult)
