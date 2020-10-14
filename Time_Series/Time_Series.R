## Packages Installed ----
# pkgs <-
(c("forecast", "patchwork", "ggplot2", "plotly", "tidyverse", "pander", "ggthemes", "reshape", "cowplot", "patchwork"))
# install.packages(pkgs)

library(forecast)   # For the moving avearage timeseries
library(patchwork)  # For combining plots
library(ggplot2)    # For Visualization
library(plotly)     # For interactive plot
library(lubridate)  #
library(knitr)
library(tidyverse)
library(pander)
library(ggthemes)
library(reshape2)
library(cowplot)


## Read file into R -----------------------------------------------------------------------
# Monthly Time series

san.mon.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Sanvitale.R/SanVitale Basin/san.mon.csv")
ras.mon.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Rasponi.R/Rasponi Basin/ras.mon.csv")
quin.mon.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Fosso_Ghiaia.R/Quinto Basin/quin.mon.csv")


# Yearly Time series

quin.ann.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Fosso_Ghiaia.R/Quinto Basin/quin.ann.csv")
ras.ann.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Rasponi.R/Rasponi Basin/ras.ann.csv")
san.ann.data <- read.csv("D:/Bologna Project/Land_Reclamation_Excel_Data/Sanvitale.R/SanVitale Basin/san.ann.csv")

# kable()
# pander()


## Time series ------------------------------------------------------------------------------

## Monthly Precipitation

## Date Conversion to POSXCIT
newyear = parse_date_time(san.mon.data[, 1], "by")
date = as.Date.POSIXct(newyear)


# Time Series of precipitation
rain.ts <- ts(san.mon.data$p,
              frequency = 12,
              start = c(1971, 1))

# moving average
rain.ma = ma(rain.ts, order = 12, centre = T)
# unclass rain moving average
unclass.rain <- unclass(rain.ma)


# combined different type of object
df.rain <- data.frame(date, unclass.rain)

# Data Visualization - ggplot2
(
  ppt = ggplot(df.rain,
               aes(x = date, y = unclass.rain)) +
    geom_line() +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      span = 0.3,
      level = 0.95,
      se = FALSE,
      na.rm = TRUE
    ) +
    labs(
      caption = "Data Source: ARPAe Emilia Romagna",
      title = "Monthly Rainfall",
      subtitle = "Rainfall (mm/month) representing the three coastal basins",
      y = "P (mm/month)",
      x = "Time",
      tag = "a"
    ) +
    scale_x_date(date_breaks = "5 year",
                 date_labels = "%Y") +
    theme_cowplot() 
  # annotate(
  #   geom = "curve",
  #   x = as.Date("2000-04-01"),
  #   y = 95,
  #   xend = as.Date("1998-04-01"),
  #   yend = 80,
  #   curvature = 0.5,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "text",
  #   x = as.Date("2001-04-01"),
  #   y = 95,
  #   label = "maximum rainfall",
  #   hjust = "left",
  #   size = 5
  # ) +
  # annotate(
  #   geom = "curve",
  #   x = as.Date("1993-04-01"),
  #   y = 30,
  #   xend = as.Date("1988-04-01"),
  #   yend = 35,
  #   curvature = -0.3,
  #   arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(
  #   geom = "text",
  #   x = as.Date("1994-04-01"),
  #   y = 35,
  #   label = "mininmum rainfall",
  #   hjust = "left",
  #   size = 5
  # )
)


# ggsave(filename = "ppt.png",
#        dpi = 1200, height = 4,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()




## Monthly AET

aet.ts <- ts(san.mon.data$aet, frequency = 12, start = c(1971,1))
# moving average
aet.ma = ma(aet.ts, order = 12, centre = T)
# unclass moving average
unclass.aet <- unclass(aet.ma)
# combined different type of object using tibble
df.aet <- tibble(date, unclass.aet)

## Data Visualization for AET
(aet = ggplot(df.aet,
              aes(x = date, y = unclass.aet)) +
    geom_line() +
    geom_smooth(method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = FALSE,
                na.rm = TRUE) +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y") +
    labs(title = "Actual Evapotranspiration",
         subtitle = "Obtained using Thornthwaite Mather procedure (1957)",
         y = "AET (mm/month)",
         x = "Time",
         tag = "b") +
    theme_cowplot() +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()
    ) 
  # annotate(
  #   geom = "text",
  #   x = as.Date("2003-04-01"),
  #   y = 65,
  #   label = "...depends on the level of soil \n moisture content and rainfall",
  #   hjust = "left",
  #   color = "#BB2D05"
  # )
)

# ggsave(filename = "aet.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()



## Monthly PET

pet.ts <- ts(san.mon.data$pet, frequency = 12, start = c(1971,1))
# moving average
pet.ma = ma(pet.ts, order = 12, centre = T)
# unclass moving average
unclass.pet <- unclass(pet.ma)
# combined different type of object using tibble
df.pet <- tibble(date, unclass.rain)


## Data Visualization for PET
(pet = ggplot(df.pet,
              aes(x = date, y = unclass.pet)) +
    geom_line() +
    geom_smooth(method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = FALSE,
                na.rm = TRUE) +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y") +
    labs(title = "Potential Evapotranspiration",
         subtitle = "Obtained using Thornthwaite method (1948)",
         y = "PET(mm/month)",
         x = "Time",
         tag = "c") +
    theme_cowplot() +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()
    ) 
  # annotate(
  #   geom = "text",
  #   x = as.Date("2000-04-01"),
  #   y = 60,
  #   label = "...depends on temperature",
  #   hjust = "left",
  #   color = "#BB2D05"
  # )
)

# ggsave(filename = "pet.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()



# ## Combine Plt1 ----
# (plt1 <- plot_grid(
#   ppt,
#   aet,
#   pet,
#   labels = c(''),
#   label_size = 12,
#   ncol = 1,
#   align = 'h',
#   axis = 1)
# )
# 
# x = ppt/aet/pet + plot_layout(guides = 'collect')

# ggsave(filename = "plt.png",
#        dpi = 1200, height =10,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## San Vitale Pu

san.pump.ts <- ts(san.mon.data$pu, frequency = 12, start = c(1971,1))
# forecast using moving average
san.pump.ma = ma(san.pump.ts, order = 12, centre = T)
# combined different object to dataframe
unclass.san.pump <- unclass(san.pump.ma)
df.san.pump <- tibble(date, unclass.san.pump)


## pumping in san vitale basin 
san.pump = ggplot(df.san.pump,
                  aes(x = date, y = unclass.san.pump)) +
  geom_line() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.3,
              level = 0.95,
              se = FALSE, 
              na.rm = TRUE) +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "5 year",
               date_labels = "%Y") +
  labs(caption = "Data source: Land Reclamation Authority (LRA)",
       title = "San Vitale Basin Pumping",
       y = "Pu (mm/month)",
       x = "Time", 
       tag = "") +
  theme_cowplot() +
  theme(
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    axis.title.y = element_text(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line.x = element_line())

#      annotate(
#       geom = "curve",
#       x = as.Date("1978-04-01"),
#       y = 45,
#       xend = as.Date("1973-04-01"),
#       yend = 55,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#    geom_label(
#        x = as.Date("1981-04-01"),
#       y = 60,
#       label = "1971 to 1980 \n high water head",
#       hjust = "right",
#       size = 5
#     ) +
#     annotate(
#       geom = "curve",
#       x = as.Date("1988-04-01"),
#       y = 18,
#       xend = as.Date("1991-04-01"),
#       yend = 40,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#     geom_label(
#       x = as.Date("1995-04-01"),
#       y = 48,
#       label = "1983 and 1993 \ndrought",
#       hjust = "middle",
#       size = 5
#     ) +
#     annotate(
#       geom = "curve",
#       x = as.Date("2018-04-01"),
#       y = 18,
#       xend = as.Date("2016-04-01"),
#       yend = 40,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#     geom_label(
#       x = as.Date("2015-04-01"),
#       y = 48,
#       label = "2000 and 2018 \nunexpected drop",
#       hjust = "middle",
#       size = 5
#     )
#   )
# ggsave(filename = "san.pump.png",
#        dpi = 1200, height = 4,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## San Vitale Pu/P

san.pup.ts <- ts(san.mon.data$pu.p, frequency = 12, start = c(1971,1))
# forecast using moving average
san.pup.ma = ma(san.pup.ts, order = 12, centre = T)
unclass.san.pup <- unclass(san.pup.ma)
# combined different object to data frame
df.san.pup <- tibble(date, unclass.san.pup)



## Visualization with ggplot
san.pup = ggplot(df.san.pup,
                 aes(x = date, y = unclass.san.pup)) +
  geom_line() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.3,
              level = 0.95,
              se = TRUE,
              na.rm = TRUE)  +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "5 year",
               date_labels = "%Y") +
  labs(caption = "",
       title = "San Vitale Pumping / Precipitation",
       x = "Time",
       y = "Pu/P",
       tag = "B") +
  theme_cowplot() +
  theme(
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    axis.title.y = element_text(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line.x = element_line())
#     annotate(
#       geom = "curve",
#       x = as.Date("1985-04-01"),
#       y = 2.5,
#       xend = as.Date("1982-04-01"),
#       yend = 1.6,
#       curvature = 0.3,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#     geom_label(
#       x = as.Date("1985-04-01"),
#       y = 2.6,
#       label = "high water head",
#       hjust = "middle",
#       size = 5
#     ) +
#     annotate(
#       geom = "curve",
#       x = as.Date("2005-04-01"),
#       y = 2.5,
#       xend = as.Date("2003-04-01"),
#       yend = 1.6,
#       curvature = 0.3,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#     geom_label(
#       x = as.Date("2005-04-01"),
#       y = 2.6,
#       label = "rainfall maximum peak",
#       hjust = "middle",
#       size = 5
#     )
# )

# ggsave(filename = "san.pup.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## Combined plt2 ----

# (plt2 <- plot_grid(
#   san.pump,
#   labels = c('AUTO'),
#   label_size = 12,
#   ncol = 1,
#   align = 'h',
#   axis = 1)
# )
# 
# ggsave(filename = "plt2.png",
#        dpi = 1200, height =12,
#        width = 11.5,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## San Vitale Infl

san.inf.ts <- ts(san.mon.data$infl, frequency = 12, start = c(1971,1))

# forecast using moving average
san.inf.ma = ma(san.inf.ts, order = 12, centre = T)
unclass.san.inf <- unclass(san.inf.ma)
# combined different object to data frame
df.san.inf <- tibble(date, unclass.san.inf)

## Infiltration in San Vitale
san.infl = ggplot(df.san.inf,
                  aes(x = date, y = unclass.san.inf)) +
  geom_line() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.3,
              level = 0.95,
              se = TRUE,
              na.rm = TRUE) +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "5 year",
               date_labels = "%Y") +
  labs(caption = "",
       title = "San Vitale - Infiltration Rate",
       subtitle = "Derived using empirical method",
       y = "Infl (mm/month)",
       x = "Time", 
       tag = "C") +
  theme_cowplot() +
  theme(
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    axis.title.y = element_text(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line.x = element_line())


# ggsave(filename = "san.infl.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()


## San Vitale dS

san.ds.ts <- ts(san.mon.data$ds, frequency = 12, start = c(1971,1))
# forecast using moving average
san.ds.ma = ma(san.ds.ts, order = 12, centre = T)

# combined different object to data frame
unclass.san.ds <- unclass(san.ds.ma)
df.san.ds <- tibble(date, unclass.san.ds)

## Data Visualization with ggplot2
(san.ds = ggplot(df.san.ds,
                 aes(x = date, y = unclass.san.ds)) +
    geom_line() +
    geom_smooth(aes(x = date, y = unclass.san.ds),
                method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE,
                na.rm = TRUE) +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y") +
    labs(caption = "",
         title = "San Vitale Change in Water Storage (empirical)",
         subtitle = "Derived using empirical method",
         y = "dS (mm/month)",
         x = "Time", 
         tag = "D") +
    theme_cowplot() +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line())
)

# ggsave(filename = "san.ds.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()



## Rasponi Pu

ras.pump.ts <- ts(ras.mon.data$pu, frequency = 12, start = c(1971,1))
# forecast using moving average
ras.pump.ma = ma(ras.pump.ts, order = 12, centre = T)
# combined different object to dataframe
unclass.ras.pump <- unclass(ras.pump.ma)
df.ras.pump <- tibble(date, unclass.ras.pump)


## pumping in san vitale basin 
ras.pump = ggplot(df.ras.pump,
                  aes(x = date, y = unclass.ras.pump)) +
  geom_line() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.3,
              level = 0.95,
              se = FALSE, 
              na.rm = TRUE) +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "5 year",
               date_labels = "%Y") +
  labs(caption = "Data Source: Land Reclamation Authority (LRA)",
       title = "Rasponi pumping",
       y = "Pu (mm/month)",
       x = "Time", 
       tag = "") +
  theme(
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    axis.title.y = element_text(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line.x = element_line()
  ) +
  theme_cowplot() 
#      annotate(
#       geom = "curve",
#       x = as.Date("1978-04-01"),
#       y = 25,
#       xend = as.Date("1975-04-01"),
#       yend = 35,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#    geom_label(
#        x = as.Date("1981-04-01"),
#       y = 40,
#       label = "1971 to 1980 \n high water head",
#       hjust = "right",
#       size = 5
#     ) +
#     annotate(
#       geom = "curve",
#       x = as.Date("1988-04-01"),
#       y = 10,
#       xend = as.Date("1991-04-01"),
#       yend = 35,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#     geom_label(
#       x = as.Date("1995-04-01"),
#       y = 40,
#       label = "1983 and 1993 \ndrought",
#       hjust = "middle",
#       size = 5
#     ) 
#   )
# 
# ggsave(filename = "ras.pump.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()



## Rasponi Pu / P

## Data Manipulation
# Times series
ras.pup.ts <- ts(ras.mon.data$pu.p, frequency = 12, start = c(1971,1))
# forecast using moving average
ras.pup.ma = ma(ras.pup.ts, order = 12, centre = T)

# combined different object to data frame
unclass.ras.pup <- unclass(ras.pup.ma)
df.ras.pup <- tibble(date, unclass.ras.pup)


## Pumping over Precipitation
(ras.pup = ggplot(df.ras.pup,
                  aes(x = date, y = unclass.ras.pup)) +
    geom_line() +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      span = 0.3,
      level = 0.95,
      se = TRUE,
      na.rm = TRUE)  +
    labs(
      caption = "",
      title = "pumping / precipitation",
      y = "Pu/P",
      x = "Time",
      tag = "B"
    ) +
    scale_x_date(
      date_breaks = "5 year",
      date_minor_breaks = "5 year",
      date_labels = "%Y"
    ) +
    theme_cowplot() +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()
    ) +
    annotate(
      geom = "curve",
      x = as.Date("1985-04-01"),
      y = 1.0,
      xend = as.Date("1982-04-01"),
      yend = 0.7,
      curvature = 0.3,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_label(
      x = as.Date("1985-04-01"),
      y = 1.0,
      label = "high water head",
      hjust = "middle",
      size = 5
    ) +
    annotate(
      geom = "curve",
      x = as.Date("2005-04-01"),
      y = 1.0,
      xend = as.Date("2003-04-01"),
      yend = 0.7,
      curvature = 0.7,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_label(
      x = as.Date("2003-04-01"),
      y = 1.0,
      label = "rainfall at maximum peak",
      hjust = "middle",
      size = 5
    )
)


ggsave(filename = "ras.pup.png",
       dpi = 1200,  height = 4,
       width = 12,
       path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
dev.off()


## Combined plt3 ----

(plt3 <- plot_grid(
  ppt,
  ras.pump,
  ras.pup,
  labels = c('AUTO'),
  label_size = 12,
  ncol = 1,
  align = 'h',
  axis = 1)
)

# ggsave(filename = "plt3.png",
#        dpi = 1200, height =12,
#        width = 11.5,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()



## Rasponi Infl

## Data Manipulation
# Time Series
ras.inf.ts <- ts(ras.mon.data$infl, frequency = 12, start = c(1971,1))
# forecast using moving average
ras.inf.ma = ma(ras.inf.ts, order = 12, centre = T)
# combined different object to data frame
unclass.ras.inf <- unclass(ras.inf.ma)
df.ras.inf <- tibble(date, unclass.ras.inf)


## Infiltration
(ras.infl = ggplot(df.ras.inf,
                   aes(x = date, y = unclass.ras.inf)) +
    geom_line() +
    geom_smooth(method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE, 
                na.rm = TRUE)  +
    labs(caption = "",
         title = "Infiltration",
         subtitle = "Obtained using emprical method",
         y = "Infl (mm/month)",
         x = "Time", 
         tag = "C") +
    scale_x_date(
      date_breaks = "5 year",
      date_minor_breaks = "5 year",
      date_labels = "%Y"
    ) +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()) +
    theme_cowplot()
)

# ggsave(filename = "ras.infl.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()


## Rasponi dS

ras.ds.ts <- ts(ras.mon.data$ds, frequency = 12, start = c(1971,1))
# forecast using moving average
ras.ds.ma = ma(ras.ds.ts, order = 12, centre = T)
unclass.ras.ds <- unclass(ras.ds.ma)

# combined different object to data frame
df.ras.ds <- tibble(date, unclass.ras.ds)


## Infiltration
(ras.ds = ggplot(df.ras.ds,
                 aes(x = date, y = unclass.ras.ds)) +
    geom_line() +
    geom_smooth(aes(x = date, y = unclass.ras.ds),
                method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE, 
                na.rm = TRUE)  +
    labs(caption = "",
         title = "Change in Water Storage",
         subtitle = "Obtained using empirical method",
         y = "dS(mm/month)",
         x = "Time", 
         tag = "D") + 
    scale_x_date(
      date_breaks = "5 year",
      date_minor_breaks = "5 year",
      date_labels = "%Y"
    ) +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()
    ) +
    theme_cowplot() 
)

# ggsave(filename = "ras.ds.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()




## Quinto Pu

## Data Manipulation
# Time Series
quin.pump.ts <- ts(quin.mon.data$pu, frequency = 12, start = c(1971,1))
# forecast using moving average
quin.pump.ma = ma(quin.pump.ts, order = 12, centre = T)
# combined different object to data frame
unclass.quin.pump <- unclass(quin.pump.ma)
df.quin.pump <- tibble(date, unclass.quin.pump)

## pumping 
quin.pump = ggplot(df.quin.pump,
                   aes(x = date, y = unclass.quin.pump)) +
  geom_line() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.3,
              level = 0.95,
              se = FALSE,
              na.rm = TRUE)  +
  labs(caption = "Data Source: Land Reclamation Consortium Ravenna",
       title = "Quinto Pumping",
       y = "Pu (mm/month)",
       x = "Time", 
       tag = "") +
  scale_x_date(date_breaks = "5 year",
               date_minor_breaks = "5 year",
               date_labels = "%Y") +
  theme_cowplot() +
  theme(
    axis.ticks.y = element_line(),
    axis.text.y = element_text(),
    axis.title.y = element_text(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line.x = element_line()
  ) 
#      annotate(
#       geom = "curve",
#       x = as.Date("1978-04-01"),
#       y = 20,
#       xend = as.Date("1975-04-01"),
#       yend = 30,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#    geom_label(
#        x = as.Date("1971-04-01"),
#       y = 35,
#       label = "1971 to 1980 \n high water head",
#       hjust = "left",
#       size = 5
#     ) +
#     annotate(
#       geom = "curve",
#       x = as.Date("1988-04-01"),
#       y = 10,
#       xend = as.Date("1991-04-01"),
#       yend = 25,
#       curvature = 0,
#       arrow = arrow(length = unit(2, "mm"))
#     ) +
#    geom_label(
#        x = as.Date("1986-04-01"),
#       y = 30,
#       label = "1983 and 1993 \ndrought",
#       hjust = "left",
#       size = 5
#     )
#   )
# 
# ggsave(filename = "quin.pump.png",
#        dpi = 1200, height = 4,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## Quinto Pu/P

quin.pup.ts <- ts(quin.mon.data$pu.p, frequency = 12, start = c(1971,1))
# forecast using moving average
quin.pup.ma = ma(quin.pup.ts, order = 12, centre = T)
# combined different object to data frame
unclass.quin.pup <- unclass(quin.pup.ma)
df.quin.pup <- tibble(date, unclass.quin.pup)

## Data Visualization with ggplot2
(quin.pup = ggplot(df.quin.pup,
                   aes(x = date, y = unclass.quin.pup)) +
    geom_line() +
    geom_smooth(method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE,
                na.rm = TRUE)  +
    labs(title = "Pumping / Precipitation",
         y = "Pu/P",
         x = "Time", 
         tag = "B") +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y")+
    theme_cowplot() +
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line(),
    ) + 
    annotate(
      geom = "curve",
      x = as.Date("1985-04-01"),
      y = 1.0,
      xend = as.Date("1982-04-01"),
      yend = 0.7,
      curvature = 0.,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_label(
      x = as.Date("1983-04-01"),
      y = 1.2,
      label = "more of subsidence \nlimited rainfall",
      hjust = "left",
      size = 5
    ) +
    annotate(
      geom = "curve",
      x = as.Date("2005-04-01"),
      y = 1.1,
      xend = as.Date("2003-04-01"),
      yend = 0.7,
      curvature = 0,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_label(
      x = as.Date("2000-04-01"),
      y = 1.2,
      label = "increasing trend",
      hjust = "left",
      size = 5
    )
)

# ggsave(filename = "quin.pup.png",
#        dpi = 1200, height = 4,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


# ## Combined Plt4---
# 
# (plt4 <- plot_grid(
#   san.pump,
#   ras.pump,
#   quin.pump,
#   labels = c('AUTO'),
#   label_size = 12,
#   ncol = 1,
#   align = 'h',
#   axis = 1)
# )

# ggsave(filename = "plt4.png",
#        dpi = 1200, height =10,
#        width = 10,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Project_Manuscript/Presentation")
# dev.off()


## Quinto Infl

quin.inf.ts <- ts(quin.mon.data$infl, frequency = 12, start = c(1971,1))
# forecast using moving average
quin.inf.ma = ma(quin.inf.ts, order = 12, centre = T)
# unclass the time series
unclass.quin.inf <- unclass(quin.inf.ma)
# combined different object to data frame
df.quin.inf <- tibble(date, unclass.quin.inf)


## Data Visualization with ggplot2
(quin.infl = ggplot(df.quin.inf,
                    aes(x = date, y = unclass.quin.inf)) +
    geom_line() +
    geom_smooth(method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE, 
                na.rm = TRUE)  +
    labs(caption = "",
         title = "Infiltration",
         subtitle = "Obtained using empirical method",
         y = "Infl (mm/month)",
         x = "Time", 
         tag = "C") +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y")+
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()) +
    theme_cowplot()
)

# ggsave(filename = "quin.infl.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()


## Quinto dS

quin.ds.ts <- ts(quin.mon.data$ds, frequency = 12, start = c(1971,1))
# forecast using moving average
quin.ds.ma = ma(quin.ds.ts, order = 12, centre = T)
# combined different object to data frame
unclass.quin.ds <- unclass(quin.ds.ma)
df.quin.ds <- tibble(date, unclass.quin.ds)


## Data Visualization with ggplot2
(quin.ds = ggplot(df.quin.ds,
                  aes(x = date, y = unclass.quin.ds)) +
    geom_line() +
    geom_smooth(aes(x = date, y = unclass.quin.ds),
                method = "loess",
                formula = y ~ x,
                span = 0.3,
                level = 0.95,
                se = TRUE, 
                na.rm = TRUE) +
    labs(title = "Change in Storage",
         subtitle = "Obtained using empirical method",
         y = "dS (mm/month)",
         x = "Time", 
         tag = "D") +
    scale_x_date(date_breaks = "5 year",
                 date_minor_breaks = "5 year",
                 date_labels = "%Y")+
    theme(
      axis.ticks.y = element_line(),
      axis.text.y = element_text(),
      axis.title.y = element_text(),
      axis.line.y = element_line(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(),
      axis.title.x = element_text(),
      axis.line.x = element_line()) +
    theme_cowplot()
)

# ggsave(filename = "quin.ds.png",
#        dpi = 1200, height = 4,
#        width = 12,
#        path = "D:/Bologna Project/Land_Reclamation_Excel_Data/Plot")
# dev.off()


