drought_data <- read.csv("D:/Bologna Project/Daily Meteorological Data/drought_data.csv")
head(drought_data)
drought_data$pet <- hargreaves(Tmin = drought_data$t_min, Tmax = drought_data$t_max, lat = 44)
head(drought_data)

CWBAL <- drought_data$P - drought_data$pet
head(CWBAL)

spei24 <- spei(CWBAL, 24)
head(spei24)

spei_fitted <- spei24$fitted
head(spei_fitted)

library(lubridate)
newyear = parse_date_time(Drought_Index[,1], "by")
dates = as.Date.POSIXct(newyear)


#spei index
Drought1 = spei_fitted_df
Drought_pos <- ifelse(Drought1$ET0_har <= 0, 0, Drought1$ET0_har)  

Drought2 = spei_fitted_df
Drought_neg <- ifelse(Drought1$ET0_har >= 0, 0, Drought1$ET0_har) 

df <- cbind.data.frame(dates, Drought_neg, Drought_pos)

drought.spei = ggplot(df) +
    geom_area(aes(dates, Drought_pos,
                  fill = "wetter_period"),
              alpha = 1) +
    geom_area(aes(dates, Drought_neg,
                  fill = "drier_period"), alpha = 1) +
    labs(x = "Time",
         y = "Normalized Value",
         title = "Standardized Precipitation Evaporation Index (SPEI)",
         tag = "B",
         subtitle = "Rainfall and temperature (Hagreave's Formula) parameterization") +
    theme_cowplot() +
    scale_fill_manual(
      " ",
      values = c(
        wetter_period = "blue",
        drier_period = "red",
        alpha = 0.5
      )
    ) +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y") +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line(),
      legend.position = "bottom"
    )
drought.spei

