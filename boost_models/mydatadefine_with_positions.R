library(readr)
finaldata <- read_csv("Documents/GitHub/APM-Term-Project/boost_models/data/finaldata_w_xy_and_dummies.csv", 
                      col_types = cols(X1 = col_skip()))
names(finaldata)

pbp <- finaldata %>%
  filter(penalty == 0,
         route_undefined == 0) %>%
  mutate(label = passResult) %>%
  select(
    label,
    x,
    y,
    Opp_Dist,
    Team_Dist,
    QB_Dist,
    FootDist,
    quarter,
    down,
    yardsToGo,
    defendersInTheBox,
    numberOfPassRushers,
    absoluteYardlineNumber,
    position_CB, position_DB, position_DE, position_DT, position_FB,position_HB,
    position_K, position_QB, position_RB, position_TE, position_WR,
    route_ANGLE, route_CORNER,route_CROSS, route_FLAT, route_GO, route_HITCH,
    route_IN, route_OUT, route_POST, route_SCREEN, route_SLANT, route_WHEEL
  )

saveRDS(pbp, "Documents/GitHub/APM-Term-Project/boost_models/models/_passResult_w_positions_and_xy_model_data.rds")
