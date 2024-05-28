library(tidyverse)
library(fpp3)
library(showtext)
#library(prophet)
library(chron)
library(fable.prophet)
library(BSOLTheme)

font_add_google("Open Sans", "Open Sans")
showtext_auto()

# ggplot defaults
# ggplot defaults
theme_set(
  theme_classic(base_family = "Open Sans") +
    theme(
      text = element_text(family="Open Sans", size = 16),
      axis.title = element_text(family="Open Sans"),
      axis.text = element_text(family="Open Sans", size = 16),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour ="#425563"),
      strip.background = element_rect(fill = "#c8cfd3"),
      plot.title = element_text(face = "bold", size = 24),
      plot.subtitle = element_text(face = "italic", size = 20),
      strip.text = element_text(size = 14)
    )
)



# Load data
arrivals_1hr <- read_csv("data/arrivals_1hr.csv", 
                        col_types = cols(ArrivalTime1Hr = col_datetime(format = "%d/%m/%Y %H:%M")))

arrivals_1hr <- 
arrivals_1hr %>% 
  mutate(weekend = as.numeric(is.weekend(ArrivalTime1Hr)))



# Convert to tsibble
arrivals_1hrts <-
  arrivals_1hr %>% 
  as_tsibble(index = ArrivalTime1Hr)


# Check for gaps
has_gaps(arrivals_1hrts)
scan_gaps(arrivals_1hrts)

# Fill gaps
arrivals_1hrts <- fill_gaps(arrivals_1hrts, arrivals = 0)
has_gaps(arrivals_1hrts)


arrivals_1hrts %>% autoplot(arrivals)


# # seasonality
# arrivals_1hrts %>% 
#   gg_subseries(arrivals)

arrivals_1hrts %>% 
  gg_season(arrivals)


arrivals_1hrts %>%  ACF() %>%  autoplot()



# decompose the timeseries
dcmp <- arrivals_1hrts %>% 
  model(stl = STL(arrivals))

components(dcmp)

cmpt_plot <- components(dcmp) %>%  autoplot() + labs(x="Date")
cmpt_plot

# Looks like day, week and minute seasonality here.




# # Visualize effects of rolling average
# animal_wm_ts <- animal_wm_ts %>% 
#   mutate(
#     `4-MA` = slider::slide_dbl(Rescues, mean,
#                                .before = 4, .after = 0, .complete = TRUE)
#   )




# Apply models candidates

mods1 <-
  arrivals_1hrts %>% 
  #filter(Date >= yearmonth("2021 Apr")) %>% 
  #stretch_tsibble(.init = 10) %>% 
  model(
    mean = MEAN(arrivals),
    naive = NAIVE(arrivals),
    snaive = SNAIVE(arrivals ~ lag("year")),
    drift = RW(arrivals ~ drift()),
    ets = ETS(arrivals),
    ses = ETS(arrivals ~ error("A")+trend("N")+season("N")),
    `Holt-Winters additive` = ETS(arrivals ~ error("A")+trend("A")+season("A")),
    `Holt-Winters additive2` = ETS(arrivals ~ error("A")+trend("A")+season("A", period="1 month")+season("A", period="1 day")),
    `Holt-Winters additive damped` = ETS(arrivals ~ error("A")+trend("Ad")+season("A"))
    
  )


accuracy(mods1)
plot(mods1)


# For arima, need to make stationary:  transformation stabilizes variance, differencing stabilised mean.

# Does it need transformation?
lambda <- arrivals_1hrts |>
  features(arrivals, features = guerrero) |>
  pull(lambda_guerrero)

arrivals_1hrts %>% gg_tsdisplay(y = box_cox(arrivals, lambda))

arrivals_1hrts |>
  mutate(diff_close = difference(arrivals, 12)) |>
  features(diff_close, ljung_box, lag = 12)




arrivals_1hrts %>%  autoplot()
arrivals_1hrts %>%  ACF() %>%  autoplot()
arrivals_1hrts %>%  ACF(log(arrivals)) %>%  autoplot()

arrivals_1hrts %>%  ACF(difference(arrivals, differences = 24)) %>%  autoplot()
arrivals_1hrts %>%  ACF(difference(arrivals, differences = 1)) %>%  autoplot()

# Used difference (when in 15 min model): 
# 96 = 15 minute slot each day (96 slots per day)
# 16 = 4 hours, apping the incentive around 4 hour movement
# 4 = same time each hour
# 1 = correlated with last 

arrivals_1hrts |> autoplot(box_cox(arrivals, lambda) |> difference(24) |> difference(1))


arrivals_1hrts |> ACF(box_cox(arrivals, lambda) |> difference(24) |> difference(1)) %>%  autoplot()

# test for required difference:
arrivals_1hrts |>
  mutate(arrivals = box_cox(arrivals, lambda)) |>
  features(arrivals, feat_stl)

#Seasonal_strength_day is > 0.64 ('magic value from fpp3, proabably some theoreticla basis)
#, meanign you need atleast on seasonal difference

# test for required difference:
arrivals_1hrts |>
  mutate(arrivals = box_cox(arrivals, lambda)) |>
  features(arrivals, unitroot_nsdiffs)
# suggests one differencing is required


arrivals_1hrts |>
  mutate(arrivals = box_cox(arrivals, lambda)) |>
  features(difference(arrivals,24), unitroot_nsdiffs)
# No further differencing required.


arrivals_1hrts <- 
arrivals_1hrts |>
  mutate(arrivals_bc = box_cox(arrivals, lambda))

# p = autoregressive order, d = differencing (intergrated) order, q = moving average order
# pdq = non-seasonal, PDQ = seasonal.
# E.g. here we d or D =1 (single).  Probably D, as it's a seasonal component at 24 hours.
# simple selection gives (4,0,0)(2,1,0)
# complex selection gives (0,0,0)(0,1,1)

# 

# models2 <- 
#   arrivals_1hrts %>% 
#   model(
#     arima_1 = ARIMA(box_cox(arrivals, lambda) ~ 1 + pdq(0,0,3) + PDQ(2,1,0)),
#     #arima_2 = ARIMA(box_cox(arrivals, lambda) ~ 1 + pdq(1,1,0) + PDQ(2,1,2)),
#     arima_3 = ARIMA(box_cox(arrivals, lambda) ~ 1 +pdq(4,0,0) + PDQ(2,1,0)),
#     arima_4 = ARIMA(box_cox(arrivals, lambda) ~ 1 + pdq(0,0,0) + PDQ(0,1,1)),
#     arima_5 = ARIMA(box_cox(arrivals, lambda) ~ 1 + pdq(3,0,0) + PDQ(2,1,0)),
#     #arima_6 = ARIMA(box_cox(arrivals, lambda) ~ 0 + pdq(3,0,0) + PDQ(2,1,0)),
#     #arima_7 = ARIMA(box_cox(arrivals, lambda) ~ 0 +pdq(4,0,0) + PDQ(2,1,0))
#     # arima_6 =  ARIMA(box_cox(arrivals, lambda) ~ 1 + pdq(2,0,1) + PDQ(2,1,0)),
#     #arima_7 =  ARIMA(box_cox(arrivals, lambda) ~ 0 + pdq(3,1,1) + PDQ(2,1,0)),
#     
#   )


#write_rds(models2, file="./data/models2.rds")

models2 <- read_rds("./data/models2.rds")

models2_2 <- 
  arrivals_1hrts %>% 
  model(
    arima_6 = ARIMA(box_cox(arrivals, lambda) ~ 0 + pdq(3,0,0) + PDQ(2,1,0)),
    arima_7 = ARIMA(box_cox(arrivals, lambda) ~ 0 +pdq(4,0,0) + PDQ(2,1,0))
    #arima_7 =  ARIMA(box_cox(arrivals, lambda) ~ 0 + pdq(3,1,1) + PDQ(2,1,0)),
    
  )

#write_rds(models2_2, file="./data/models2_2.rds")

models2_2 <- read_rds("./data/models2_2.rds")

     # nnet = NNETAR(box_cox(arrivals, lambda)),
     # prophet = prophet(box_cox(arrivals, lambda))

accuracy(models2)
accuracy(models2_2)

 
models3 <- 
  arrivals_1hrts %>% 
  model(
    NNETAR = NNETAR(box_cox(arrivals, lambda))
    
  ) 

#write_rds(models3, file="./data/models3.rds")

models3 <- read_rds("./data/models3.rds")


models3 %>% 
  report()

accuracy(models3)

models3 |> 
  #select(arima_3) |> 
  gg_tsresiduals(lag_max=24)

models4 <- 
  arrivals_1hrts %>% 
  model(
    prophet(box_cox(arrivals, lambda))
  ) 


models42 <- 
  arrivals_1hrts %>% 
  model(
    prophet(box_cox(arrivals, lambda) ~ 
              season(period = 24, order = 10) +
              season(period = 168, order = 10) +
              season(period = 8760, order = 10))
  ) 


models42 |> 
  #select(arima_3) |> 
  gg_tsresiduals(lag_max=24)


accuracy(models42)

models2 |> 
  select(arima_3) |> 
  gg_tsresiduals(lag_max=24)


report(models2)

models2 %>% 
  select(arima_5) %>% 
  report()

fc48<-
  models2 %>% 
  forecast(h="48 hours")

fc48 |> autoplot()

fc482<-
  models3 %>% 
  forecast(h="48 hours")

fc482 |> autoplot()



  


fc_sub <-
  fc48 |>
  hilo()

fc_sub <-
  fc482 |>
  hilo() |>
  bind_rows(fc_sub)

fc_sub$lcl <-fc_sub$`95%`$lower
fc_sub$ucl <-fc_sub$`95%`$upper
  

fcst_48_plot<- arrivals_1hrts %>% 
  as.data.frame() %>% 
  select(-arrivals_bc, -weekend) %>% 
  slice_tail(n=48) %>% 
  mutate(prediction = "base"
         , lcl = as.numeric(NA)
         , ucl = as.numeric(NA)) %>% 
  union(select(as.data.frame(fc_sub), ArrivalTime1Hr, arrivals = .mean, prediction = .model, lcl, ucl)) %>% 
  as_tsibble(key=prediction, index = ArrivalTime1Hr)
  
fcst_48_plot |>
  filter(prediction %in% c("arima_3", "NNETAR")) |>
  autoplot(linewidth=1)

# test to see if residuals are white noise
augment(models2) |>
  filter(.model=='arima_auto') |>
  features(.innov, ljung_box, lag = 10, dof = 3)

accuracy(models2)

caf_fit |>
  forecast(h=5) |>
  filter(.model=='search') |>
  autoplot(global_economy)


a<- 
  arrivals_1hrts |>
  select(ArrivalTime1Hr, arrivals) |>
  as.data.frame() |>
  mutate(prediction = "baseline", NA, lcl= NA, ucl=NA) |>
  bind_rows(fcst_48_plot) |>
  filter(ArrivalTime1Hr > as.Date('2023-10-31') & 
           prediction %in% c("baseline", "NNETAR", "arima_3"))|>
  mutate(prediction = ifelse(prediction == "NNETAR", "Neural Network"
                             , ifelse(prediction == "arima_3", "Seasonal ARIMA", "baseline"))) |>
  ggplot(aes(y=arrivals, x = ArrivalTime1Hr, col = prediction), size=0.8)+
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = prediction), alpha=0.4, linewidth = 0)+
  geom_line(linewidth=1) +
  labs(title = "A&E hourly arrival prediction test",
       subtitle = "ARIMA and Neural Network methods applied to hourly arrival data",
       colour = "Prediction Method",
       fill = "Prediction Method",
       y = "Hourly arrivals",
       x = "Date") +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%d-%b")+
  #scale_color_brewer(palette = "Dark2")+
  scale_color_bsol()+
  scale_fill_bsol()
  
a

ggsave("./output/hourly_preds.png", a, device="png", width = 6, height =  4, scale = 1)

# model(NNETAR(sqrt(value)))
#   prophet(Demand ~ Temperature + Cooling + Working_Day +
#season(period = "day", order = 10) +
#  season(period = "week", order = 5) +
#  season(period = "year", order = 3))
