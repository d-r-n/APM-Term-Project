library(readr)
finaldata <- read_csv("Documents/GitHub/APM-Term-Project/boost_models/data/finaldata_w_xy.csv", 
                      col_types = cols(X1 = col_skip()))

pbp <- finaldata %>%
  filter(penalty == 0) %>%
  mutate(label = passResult) %>%
  select(
    label,
    Opp_Dist,
    Team_Dist,
    QB_Dist,
    FootDist,
    quarter,
    down,
    yardsToGo,
    defendersInTheBox,
    numberOfPassRushers,
    absoluteYardlineNumber
  )

saveRDS(pbp, "Documents/GitHub/APM-Term-Project/boost_models/models/_passResult_model_w_xy_data.rds")
