### CarexNardus2022__relevant_code ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2021-05-03
# Location created: Bern
# Author:           Raphael S. von Bueren (GitHub: vonbueren)
# Last Entry:       2021-12-20
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Description ----
# Relevant R code.
# Data available on request from the authors.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get started ----
rm(list = ls())
Sys.setenv(TZ = "GMT")

source( "CarexNardus2022__my_functions.R" )
library(tidyverse)
library(reshape2)
library(Rmisc)
library(ggthemes)
library(lubridate)
library(vegan)
#devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
library(ggrepel)
library(lme4)
library(lmerTest)
library(ggh4x)

theme_set(
  theme_clean() +
    theme(
      legend.title = element_blank(), 
      legend.position = "top", 
      legend.background = element_rect(colour = "white"))
)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Import data and adjustments ----
# Leakage data
excel_files         <- list.files(path = "DATA/raw_leakage", pattern = "*.xlsx", full.names = TRUE) 
leakage_list        <- sapply( excel_files, readxl::read_excel, na = "NA", simplify = FALSE, USE.NAMES = TRUE)  
names(leakage_list) <- gsub("DATA/raw_leakage/", "", names(leakage_list) )
eval.10             <- leakage_list$`Frost_Leakage_2020_08_17_Site9_11_12_Temp_8_11_13_15_18_Roehrchen_24h.xlsx`
eval.12             <- leakage_list$`Frost_Leakage_2020_08_25_Site8_10_11_Monol_MonolOrig_Temp_11_14_16_18_21_Roehrchen_24h.xlsx`

# R-created data frames (R_xxx_xxx.csv) and raw data (raw_xxx_xxx.csv)
logger <- read_csv("DATA/R_GMT_iButtons.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), Time_GMT = col_datetime() ),
                   na = "NA")
therm  <- read_csv("DATA/R_pos_temperature.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), wintersnow19_GMT = col_datetime(), wintersnow20_GMT = col_datetime(), snowfree20_GMT = col_datetime(), snowmelt20_GMT = col_datetime()),
                   na = "NA")
ALPFOR <- read_csv("DATA/R_GMT_ALPFORclimate.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), Time_GMT = col_datetime() ),
                   na = "NA")
obs    <-  read_csv("DATA/R_pos_characteristics.csv",
                    col_names = T,
                    col_types = cols(.default = col_double(),
                                     macroexposure = col_factor(), exposure = col_factor(), topography = col_factor(), wind_edge = col_logical(),
                                     species = col_factor(), control_spec_1 = col_factor(), control_spec_2 = col_factor(), control_spec_3 = col_factor(),
                                     position_description = col_character()),
                    na = "NA")
soil   <- read_csv("DATA/R_pos_soil.csv",
                   col_names = T,
                   col_types = cols(.default = col_double(), sample_GMT = col_datetime() ),
                   na = "NA")
spec   <- read_csv("DATA/R_species_information.csv",
                   col_names = T,
                   col_types = cols(.default = col_factor(),
                                    "ID_species" = col_double(), "aID_SP" = col_double(), "InfoFlora" = col_double(),
                                    "T" = col_double(), "N" = col_double(), "L" = col_double(), "F" = col_double(),
                                    "H" = col_double(), "EG" = col_double(), "W" = col_double(), "R" = col_double(),
                                    "MV" = col_double(), "KL" = col_double(), "Best_Bu" = col_logical(), "FrSV" = col_double(),
                                    "WV" = col_double(), "TV" = col_double(), "FW" = col_double(), "sla" = col_double(),
                                    "ch" = col_double(), "sm" = col_double()),
                   na = "NA")
veg    <- read_csv("DATA/raw_pos_vegetation.csv",
                   col_names = T,
                   col_types = cols(.default = col_double()),
                   na = "NA")
EIV  <- read_csv("DATA/R_pos_EIV.csv",
                 col_names = T,
                 col_types = cols(.default = col_double()),
                 na = "NA")
frost         <- read_csv("DATA/R_frostmeasure_freezingresistance.csv",
                          col_names = T,
                          col_types = cols(.default = col_double(), type = col_factor(), sample = col_factor(),
                                           tissue = col_factor(), age = col_factor(), date = col_datetime(), Date = col_date()),
                          na = "NA")
frost$sample  <- frost$sample %>% fct_recode(Carex = "Cc", Nardus = "Ns")
vigfrost <- read_csv("DATA/raw_vigmeasurefrost_vigour.csv",
                     col_names = T,
                     col_types = cols(.default = col_double(), species = col_factor(), pot_description = col_factor(), 
                                      date_timeGMT = col_datetime(format = "%d.%m.%y %H:%M"), fungi_visual = col_factor(),
                                      new_shoot_within_dead_tussock = col_logical(), notes = col_character()),
                     na = "NA")
hair.harvest <- read_csv("DATA/raw_pos_hairdresser.csv",
                         col_names = T,
                         col_types = cols(.default = col_double(), Harvest_GMT = col_datetime(format = "%d.%m.%y %H:%M") ),
                         na = "NA")
vigour   <- read_csv("DATA/raw_vigmeasure_vigour.csv",
                     col_names = T,
                     col_types = cols(.default = col_double(), date_timeGMT = col_datetime(format = "%d.%m.%y %H:%M"),
                                      notes = col_character()),
                     na = "NA") %>% print

# All observation data
obs.all <- obs %>%
  left_join(therm, by = "ID_position") %>% 
  left_join(EIV,   by = "ID_position") %>% 
  left_join(soil,  by = "ID_position") %>% 
  left_join(veg,   by = "ID_position") 
obs.all <- obs.all %>% dplyr::rename(Carex = Carexcurvula, Nardus = Nardusstricta)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Leakage curves (one example for Carex, one example for Nardus) ----
Nardus <- eval.10 %>% select(Temperature, c("Nardus_110_JUNG_1", "Nardus_110_JUNG_2", "Nardus_110_JUNG_3")) # k = -0.5, beta = 500
Carex  <- eval.12 %>% select(Temperature, c("Carex_Orig2_ALT_1", "Carex_Orig2_ALT_2"))                      # k = -0.5, beta = 500

# Choose data frame of interest (Carex or Nardus)
dat.1 <-  Carex
name  <- "Carex"

# Choose initial starting parameters
k     <- -0.5 # Gompertz
beta  <- 500  # Gompertz
x0    <- -10  # Boltzmann
dx    <- -1   # Boltzmann

# Create data frame
dat.2           <- reshape2::melt(dat.1, id = "Temperature")
zero.leakage    <- mean(dat.2$value[dat.2$Temperature == max(dat.2$Temperature)], na.rm = T)
max.leakage     <- mean(dat.2$value[dat.2$Temperature == -30], na.rm = T)
percent.damage  <- (dat.2$value - zero.leakage)/(max.leakage - zero.leakage)*100
dat.3           <- na.omit(cbind(dat.2, percent.damage))

# GOMPERTZ: Estimate parameters "beta" and "k" (alpha = 100 --> maximum value)
nls.gompertz    <- minpack.lm::nlsLM( percent.damage ~ 100 * exp( -beta * exp(-k * Temperature )),
                                      data    = dat.3,
                                      start   = list(beta = beta, k = k),
                                      control = list(maxiter = 1000))

# BOLTZMANN: Estimate parameters "x0" and "dx"
maximum <- 100 # 100% is maximum leakage
minimum <- 0   # 0% is minimum leakage
nls.boltzmann <- minpack.lm::nlsLM(percent.damage ~ (maximum + (minimum - maximum)/(1 + exp((Temperature - x0) / dx ))),
                                   start   = list(x0 = x0, dx = dx), 
                                   data    = dat.3,
                                   control = list(maxiter = 1000))

# Create functions and LT50-values
beta   <-  coef(nls.gompertz)[["beta"]]
k      <-  coef(nls.gompertz)[["k"]]
x0     <-  coef(nls.boltzmann)[[1]]
dx     <-  coef(nls.boltzmann)[[2]]

fun.gompertz <- function(x){
  y =  100 * exp( -beta * exp(-k * x ))
  return(y)
}

fun.boltz <- function(x){
  y = maximum + (minimum - maximum) / (1 + exp( (x - x0) / dx )) 
  return(y)
}

boltz.LT50 <- x0 
gomp.LT50  <- (-log( (log(100) - log(50) ) / coef(nls.gompertz)[["beta"]]) / coef(nls.gompertz)[["k"]])  # see: Lim, Arora and Townsed 1998

# ggplot
color  <- ifelse(name == "Carex", "darkorange2", "olivedrab4")
letter <- ifelse(name == "Carex", "A", "B")
p <- ggplot2::ggplot() + 
  stat_function(fun = fun.gompertz, aes(colour = "Gompertz"),
                size = 3, alpha = 0.5, xlim = c(-5, 35)) +
  stat_function(fun = fun.boltz,    aes(colour = "Boltzmann"),
                size = 3, alpha = 0.5, xlim = c(-5, 35)) +
  geom_point(data = dat.3, aes(x = Temperature, y = percent.damage),
             size = 12, alpha = 0.5, col = color) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_segment(aes(x = gomp.LT50,  xend = gomp.LT50,  y = 50, yend = -15), col = "red",   size = 1) +
  geom_segment(aes(x = boltz.LT50, xend = boltz.LT50, y = 50, yend = -15), col = "blue",  size = 1) +
  geom_segment(aes(x = 10, xend = min(gomp.LT50, boltz.LT50), y = 50, yend = 50), col = "black", size = 1) +
  geom_segment(aes(x = -20, xend = -16.3, y = 5, yend = -12), col = "black", size = 1, arrow = arrow(angle = 20, type = "closed")) + # only for Carex
  scale_x_reverse(   limits = c( 10, -40), breaks = seq(0,-30,-10)) +
  scale_y_continuous(limits = c(-15, 120), breaks = seq(0,100, 25)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Temperature (°C)", y = "Percent injury (%)") +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 50)) +
  annotate("text", x = 7, y = 110, angle = 0,
                        label = name, fontface = "italic",
                        hjust = 0, parse = F, size = 20, col = color) +
  annotate("text", x = 10,  y = 120, hjust = 1.5, vjust = 0.5, label = letter, size = 20) +
  annotate("text", x = -26, y = 8.5,   label = "LT50", size = 15) + # only for Carex
  theme(plot.title        = element_text(size = 35, face = "italic", colour=c("black"), vjust = 0),
        axis.title.x      = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y      = element_text(size = 35), 
        axis.text.x       = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y       = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank())+
  theme(legend.position   = c(0.8, 0.4),
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 35, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) # +
  # theme(legend.position = "none") # only for Nardus
p
# ggsave(p, file = paste("LeakagePlot_", name, ".png", sep = ""),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 25, units = "cm")

# Model output
summary(nls.boltzmann)
summary(nls.gompertz)

boltz.LT50
gomp.LT50

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Three temperature curves (air weather station, soil wind-exposed, soil shelter) ----
# Use these 4 lines for air temperature plot (weather station ALPFOR) --> but not for the two soil temperature plots
data        <- ALPFOR %>% select(Time_GMT, TempAir = TAIR1) %>%
  filter(Time_GMT > "2019-08-30") %>%
  filter(Time_GMT < "2020-10-10") %>% 
  data.frame
description <- "air (2 m)"
color       <- "darkblue"
i           <- 1 # Dirty: Use logger 1 for weather station because there only NAs for snow...

# Use these 8 lines for the two soil temperature plots --> but not for the air temperature plot
position.logger <- 111 # write logger number here (111 and 119)

description     <- ifelse(position.logger == 111, "soil (-3 cm)", "soil (-3 cm)")
color           <- "darkred"
i               <- 17-41 + position.logger
logger.d        <- as.data.frame(logger)
no.na           <- logger.d[!is.na(logger.d[ , (i+1) ]), ]
data            <- no.na[ , c(1, (i+1)) ]

# Plot for each separately
data$year <- year(data[ ,1])
year.start <- as.POSIXct("2019-08-30", format = "%Y-%m-%d", tz = "GMT")
year.end <- as.POSIXct("2020-10-10", format = "%Y-%m-%d", tz = "GMT")
min.tem  <- subset(data, data[ , 2] == min(data[ , 2]))
min.temp <- min.tem[1,] # take only first row (because sometimes, the same minimum value is reached multiple times)
p     <- ggplot(data, aes(x = data[ ,1], y = data[ , 2])) +
  ggtitle("")+
  theme(plot.title = element_text(size = 30, face = "bold")) +
  ylim(-20,25) +
  labs ( x = "", y = paste("Temperature (°C)") ) +
  theme(axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  scale_x_datetime(date_breaks = "1 month",
                   limits = lubridate::ymd_h(c("2019-08-30 00", "2020-10-10 00")),
                   labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                                                paste(lubridate::month(x, label = TRUE), "\n", year(x)),
                                                paste(lubridate::month(x, label = TRUE)))) +
  theme(panel.grid.major.x = element_line(colour = "grey", linetype = "dotted")) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_vline(xintercept = therm$snowfree20_GMT[i], colour = "lightblue", size = 1) +
  geom_vline(xintercept = therm$snowmelt20_GMT[i], colour = "purple", size = 1) +
  annotate("text", x = therm$snowfree20_GMT[i], y = 20.8, angle = 0,
           label = paste(" snow", "\n", "disappearance", sep = ""),
           hjust = -0.05, parse = F, size = 12, col = "lightblue") + # only for 111
  annotate("text", x = therm$snowmelt20_GMT[i], y = 25, angle = 0,
           label = paste("season start"),
           hjust = -0.05, parse = F, size = 12, col = "purple") +
  annotate("rect",
           xmin = therm$wintersnow19_GMT[i], xmax = as.POSIXct(therm$snowfree20_GMT[i]),
           ymin = -Inf, ymax = 0,  fill = "grey", alpha=.5) +
  annotate("text", x = as.POSIXct(therm$snowfree20_GMT[i]-((therm$snowfree20_GMT[i]-therm$wintersnow19_GMT[i])/2)),
           y = -15, size = 12, hjust = 0.5, color = "white",
           label = "snow") +
  geom_line(col = color, size = 0.7) +
  geom_point(aes(x = min.temp[ ,1], y = min.temp[ , 2]), color="blue", size = 12, shape = 1, stroke = 3) +
  annotate("text", x = as.POSIXct(min.temp[,1]), y = min.temp[,2],
           label = paste(round(min.temp[ , 2],1), "°C" ),
           hjust=ifelse(description == "air (2 m)", -0.15, 0), vjust=ifelse(description == "air (2 m)", 1, 2), size = 12, col = "blue") +
  annotate("text", x = year.start + 60*60*24*30*12.2, y = -15, label = description, size = 12, fontface = "bold.italic") +
  annotate("text", x = year.start, y = 25, hjust = 3.3, vjust = -0.2, 
           label = ifelse(description == "air (2 m)", "A", ifelse(position.logger == 111, "B", "C")),
           size = 15) +
  theme(panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.major.y = element_line(colour = "transparent")) +
  coord_cartesian(clip = "off")
p
# ggsave(p, file = paste0( colnames(data)[2], "2.png"),
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 46, height = 16, units = "cm" )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CCA ----
# Change column names of data frame *veg* from "genusspecies" to "genspec" (3+3 letters each)
fullname            <- colnames(veg) %>% data.frame %>% filter(. != "ID_position") %>% dplyr::rename("genusspecies" = ".")
allname             <- fullname %>% left_join(spec %>% select(genusspecies, genus, species, subspecies), by = "genusspecies")
allname$genspec     <- paste(substring(allname$genus, 1, 3), substring(allname$species, 1, 3), sep = "")
allname[allname$genus == "Ligusticum" & allname$species == "mutellinoides", "genspec"] <- "Ligmuo"
colnames(veg)[2:87] <- allname$genspec
veg.0.1             <- replace(veg, veg > 0 & veg <= 1, 1)   # change cover data to presence/absence data

# Preparation vegetation data
plants           <- veg %>% data.frame                # here, you could also try with veg.0.1
rownames(plants) <- plants$ID_position
plants           <- plants %>% select(-ID_position) 
# plants <- plants %>% select_if(colSums(plants)>0.5) # CCA with only abundant plants

# Preparation environmental data
env.all <- obs %>%
  left_join(therm, by = "ID_position") %>% 
  left_join(EIV,   by = "ID_position") %>% 
  left_join(soil,  by = "ID_position") %>% 
  filter(ID_position > 40) %>%
  data.frame
rownames(env.all) <- env.all$ID_position
env.all           <- env.all %>% select(-ID_position) 
str(env.all)
env.all$Temp_min  <- env.all$Temp_min*-1
env               <- env.all %>% select("Soil minimum temperature" = Temp_min,  "Growing degree hours ≥ 0 °C" = GDH.0_MA,
                                        "Soil moisture" = soil_moist,
                                        "pH-value" = pH, "C/N-ratio" = C_N_ratio, "Phosphorus " = citricacid_soluble_P_mg_per_kg)     

ccamodel   <- vegan::cca(plants~., env, na.action = na.exclude ) %>% print
finalmodel <- vegan::ordistep(ccamodel, scope=formula(ccamodel)) %>% print

vegan::anova.cca(finalmodel)
vegan::anova.cca(finalmodel, by="terms")
vegan::anova.cca(finalmodel, by="axis")

# finalmodel  # proportion of constrained on total inertia -> variability explained by my constraining variables (environmental factors) --> 16.9%

axis1 <- round(finalmodel$CCA$eig[1]*100,1)
axis2 <- round(finalmodel$CCA$eig[2]*100,1)
sp    <- fortify(finalmodel, display = "sp")
p.CCA <- ggplot2::autoplot(finalmodel,
                           arrows = T,
                           loadings = T, loadings.colour = "green",
                           geom = "point", 
                           layers = c("biplot", "species")) +
  ggrepel::geom_text_repel(data = sp,
                  aes(x = CCA1, y = CCA2,
                      label = Label, 
                      colour = Score),
                  max.overlaps = Inf) +
  theme(legend.position = "none") +
  xlim(c(-3,2.3)) +
  scale_colour_manual(values = c("darkred")) +
  theme(axis.title.x   = element_text(size = 20, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 20), 
        axis.text.x    = element_text(size = 20, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 20, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  xlab(paste("CCA1 (", axis1, " %)", sep = "")) +
  ylab(paste("CCA2 (", axis2, " %)", sep = ""))
p.CCA

# ggsave(p.CCA, file = "_Figure4_CCA.png",
#        path = "FIGURES",
#        width = 30, height = 25, units = "cm")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plots and linear models: Cover vs. Mintemp, Indval, Elevation ----
# Mintemp
obs.all.2 <- obs.all %>% filter(ID_position > 40)
df        <- reshape2::melt(obs.all.2[,c("Temp_min", "Carex", "Nardus")], id.vars = 1) %>%
  dplyr::rename(cover = value, species = variable)
p.min     <- ggplot(df, aes(x = Temp_min, y = cover,
                            color = factor(species),
                            shape = factor(species))) + 
  geom_point(size = 6, alpha = 0.5) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.9, 0.9),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 30, face = "italic", margin = margin(r = 20)),
        legend.spacing.x = unit(0.3, 'cm')) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Soil minimum temperature (°C)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  scale_x_reverse(limits = c(1, -14), breaks = seq(-14,1,2)) +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 0,  y = 0.9, hjust = 3.5, vjust = -1.7, label = "A", size = 15) +
  theme(legend.position = "none")
p.min

# ggsave(p.min, file = "Cover_Mintemp.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

# Indval
n.mean      <- c("T.0.1.mean", "N.0.1.mean", "L.0.1.mean", "F.0.1.mean", "H.0.1.mean", "R.0.1.mean")               # , "W.0.1.mean")
n.median    <- c("T.0.1.median", "N.0.1.median", "L.0.1.median", "F.0.1.median", "H.0.1.median", "R.0.1.median")   # , "W.0.1.median")
title       <- c("temperature", "nitrogen", "light", "moisture", "organic material", "acidity")                    # , "moisture variability")
EIV_CN      <- left_join(EIV, veg %>% select("ID_position", "Carcur", "Narstr")) %>% data.frame
EIV_CN_long <- reshape2::melt(EIV_CN[,c("ID_position", n.mean)], id.vars = 1) %>%   # HERE SWITCH BETWEEN MEAN AND MEDIAN (Median not good because e.g. mostly 1, 1.5 or 2 for Temp)
  dplyr::rename(EIV_value = value, EIV_type = variable) %>%  
  left_join(EIV_CN %>%
              select(ID_position, Carcur, Narstr) %>%
              dplyr::rename(Carex = Carcur, Nardus = Narstr)) %>% 
  gather("Carex", "Nardus",key = "species", value = "cover") %>% 
  tibble
p.ind <- ggplot(EIV_CN_long %>% filter(EIV_type == "T.0.1.mean"), aes(x = EIV_value, y = cover,
                                                                  col = species, shape = species)) +
  geom_point(size = 6, alpha = 0.5) +
  stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
  stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F,
              aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_x_continuous(limits = c(1,2), breaks = seq(1,2,0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(0,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.8,0.95),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 20, face = "italic", margin = margin(r = 10)),
        legend.spacing.x = unit(0.2, 'cm'))+
  theme(legend.key.height=unit(1.1,"cm")) +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "Temperature indicator value",
       y = "Cover (%)") +
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 1,  y = 0.9, hjust = 2.25, vjust = -1.7, label = "B", size = 15) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-0.05,1.15), clip = "off")
p.ind

# ggsave(p.ind, file = "Cover_Indval.Temp.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

EIV.T <- EIV_CN_long %>% filter(EIV_type == "T.0.1.mean")
EIV.T
fit <- with(EIV.T, lm(cover~EIV_value*species))
summary(fit)
confint(fit)

fitCarex <- with(EIV.T %>% filter(species == "Carex"), lm(cover~EIV_value))
summary(fitCarex)
confint(fitCarex)
EIV.T %>% filter(species == "Carex") %>% filter(!is.na(EIV_value)) %>% nrow
par(mfrow = c(2,2)); plot(fitCarex)

fitNardus <- with(EIV.T %>% filter(species == "Nardus"), lm(cover~EIV_value))
summary(fitNardus)
confint(fitNardus)
EIV.T %>% filter(species == "Nardus") %>% filter(!is.na(EIV_value)) %>% nrow
par(mfrow = c(2,2)); plot(fitNardus)

# Elevation
df <- reshape2::melt(obs.all.2[,c("elevation", "Carex", "Nardus")], id.vars = 1) %>%
  dplyr::rename(cover = value, species = variable)
p.ele <- ggplot(df,aes(x = elevation, y = cover,
                   color = factor(species),
                   shape = factor(species))) + 
  geom_point(size = 6, alpha = 0.5) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  stat_smooth(method = "loess", size = 2, alpha = 1, se = F, show.legend = F, span = 0.75) +
  stat_smooth(method = "loess", size = 0.2, alpha = 0.25, se = T, show.legend = F,
              aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..))) +
  scale_x_continuous(limits = c(2150, 2850),
                     breaks = seq(2200,2800,200)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     limits = c(-0.05,1.15),
                     breaks = seq(0,1,0.25)) +
  theme(legend.position = c(0.27, 0.95), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", linetype = "solid", colour = "grey"),
        legend.text = element_text(size = 30, face = "italic", margin = margin(r = 20)),
        legend.spacing.x = unit(0.3, 'cm')) +
  theme(legend.key.height=unit(1.1,"cm")) +
  coord_cartesian(clip = "off") +
  labs(x = "Elevation (m)",
       y = "Cover (%)") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 18, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10)) +
  annotate("text", x = 2200,  y = 0.9, hjust = 3.5, vjust = -1.7, label = "C", size = 15) +
  theme(legend.position = "none")
p.ele

# ggsave(p.ele, file = "Cover_Elevation.png",
#        path = "R_PLOTS/_FIGURES_THESIS",
#        width = 25, height = 15, units = "cm")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Frost hardening models with GDH and min temp (different intervals) ----
# Overview plots including model output
frost.all <- frost %>%
  left_join(obs, by = "ID_position")
CCNS <- frost.all %>%
  filter(sample == "Carex" | sample == "Nardus") %>%
  filter(!(age %in% "mixed")) %>%
  filter(tissue == "leaf")
c <- c(1,3,5,7,10,14,20)
vector <- c(paste("GDH.0_", c, "d", sep = ""), paste("min_", c, "d", sep = ""), "DOY", "DSS")
dat <- data.frame()
for (i in 1: length(vector)){
  CCNS.sub <- if(i >= 15) {
    CCNS
  } else {
    CCNS %>% filter(DSS > rep(c, 2)[i])
  }
  main     <- paste("leaves__LT50_vs_", vector[i], "__CCNS.lom.oy", sep = "")
  formula <- paste("LT50.gomp ~ ", vector[i], sep = "")
  fitCC <- with(CCNS.sub %>% filter(sample == "Carex"), lm(as.formula(formula)))
  fitNS <- with(CCNS.sub %>% filter(sample == "Nardus"), lm(as.formula(formula)))
  p        <- ggplot(CCNS.sub, aes_string(x = vector[i], y = "LT50.gomp", col = "sample", shape = "sample"))+
    geom_point(size = 6, alpha = 0.5) +
    stat_smooth(method = "lm", size = 2, alpha = 1, se = F, show.legend = F, fullrange = F) +
    stat_smooth(method = "lm", size = 0.2, alpha = 0.25, se = T, show.legend = F) +
    scale_color_manual(breaks = c("Carex", "Nardus"),
                       values = c("darkorange2", "olivedrab4")) +
    scale_y_continuous(limits = c(-23, -6),
                       breaks = seq(-10, -25, -5)) +
    coord_cartesian(clip = "off") +
    labs(x = if(i <= 7) {
      paste("Growing degree hours (≥ 0 °C)")
    } else if(i <= 14) {
      paste("Minimum temperature (°C)")
    } else if(i <= 15) {
      paste("DOY")
    } else {
      paste("DSS")
    },
    y = "LT50 (°C)") +
    theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
          axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.ticks.length = unit(10, "pt")) +
    theme(panel.grid.major.y = element_blank()) +
    theme(plot.margin = margin(t = 50, r = 20, b = 20, l = 20)) +
    theme(legend.position = "none") +
    ggtitle(label = main,
            subtitle = paste("Carex: adj R^2 = ", round(summary(fitCC)$adj.r.squared, 2), " / ",
                             "slope = ", round(dummy.coef(fitCC)[[2]], 3), " / ",
                             "CI = " , round(confint(fitCC)[[2,1]], 3), ", ", round(confint(fitCC)[[2,2]], 3), " / ",
                             "p = ", round(summary(fitCC)$coefficients[2,4], 7), " / ",
                             "BIC = ", round(BIC(fitCC), 1), "\n",
                             "Nardus: adj R^2 = ", round(summary(fitNS)$adj.r.squared, 2), " / ",
                             "slope = ", round(dummy.coef(fitNS)[[2]], 3), " / ",
                             "CI = " , round(confint(fitNS)[[2,1]], 3), ", ", round(confint(fitNS)[[2,2]], 3), " / ",
                             "p = ", round(summary(fitNS)$coefficients[2,4], 7), " / ",
                             "BIC = ", round(BIC(fitNS), 1),
                             sep = ""))
  p
  dat[i, "predictor"] <- vector[i]
  dat[i, "CC_slope"]  <- round(dummy.coef(fitCC)[[2]], 4)
  dat[i, "CC_CI_low"] <- round(confint(fitCC)[[2,1]], 4)
  dat[i, "CC_CI_up"]  <- round(confint(fitCC)[[2,2]], 4)
  dat[i, "CC_p"]      <- round(summary(fitCC)$coefficients[2,4], 7)
  dat[i, "CC_n"]      <- CCNS.sub %>% filter(sample == "Carex") %>% nrow
  dat[i, "CC_adjR2"]  <- round(summary(fitCC)$adj.r.squared, 2)
  dat[i, "NS_slope"]  <- round(dummy.coef(fitNS)[[2]], 4)
  dat[i, "NS_CI_low"] <- round(confint(fitNS)[[2,1]], 4)
  dat[i, "NS_CI_up"]  <- round(confint(fitNS)[[2,2]], 4)
  dat[i, "NS_p"]      <- round(summary(fitNS)$coefficients[2,4], 7)
  dat[i, "NS_adjR2"]  <- round(summary(fitNS)$adj.r.squared, 2)
  dat[i, "NS_n"]      <- CCNS.sub %>% filter(sample == "Nardus") %>% nrow
  # ggsave(p, file = paste("Overview__", main, ".png", sep = ""),
  #        path = "R_PLOTS/_FIGURES_THESIS/GDH_and_min",
  #        width = 25, height = 20, units = "cm" )
}
dat
# write.csv(dat, file = "R_PLOTS/_FIGURES_THESIS/GDH_and_min/LT50_predictors.csv")

# Carex model selection
CC <- CCNS %>% filter(sample == "Carex")
data <- CC %>% filter(DSS > 10)
fit1 <- lmerTest::lmer(LT50.gomp ~                         (1|elevation),                          data = data)
fit2 <- lmerTest::lmer(LT50.gomp ~ DOY +                   (1|elevation),                          data = data)
fit3 <- lmerTest::lmer(LT50.gomp ~ DSS +                   (1|elevation),                          data = data)
fit4 <- lmerTest::lmer(LT50.gomp ~ GDH.0_10d +             (1|elevation),                          data = data)
fit5 <- lmerTest::lmer(LT50.gomp ~ DSS + GDH.0_10d +       (1|elevation),                          data = data)
fit6 <- lmerTest::lmer(LT50.gomp ~ DOY + GDH.0_10d +       (1|elevation),                          data = data)
fit7 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS +             (1|elevation),                          data = data)
fit8 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS + GDH.0_10d + (1|elevation),                          data = data)
AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
summary(fit4)
confint(fit4, oldNames = F)
confint(fit4, oldNames = F)*1000 # for 1000 GDH
plot(fit4) # TA-plot
par(mfrow = c(1,2))
qqnorm(lme4::ranef(fit4)$elevation[, 1], main = "Random effects of site")
qqnorm(resid(fit4), main = "Residuals")
par(mfrow = c(1,1))

# Nardus model selection
NS <- CCNS %>% filter(sample == "Nardus")
data <- NS %>% filter(DSS > 1)
fit1 <- lmerTest::lmer(LT50.gomp ~                         (1|elevation),                          data = data)
fit2 <- lmerTest::lmer(LT50.gomp ~ DOY +                   (1|elevation),                          data = data)
fit3 <- lmerTest::lmer(LT50.gomp ~ DSS +                   (1|elevation),                          data = data)
fit4 <- lmerTest::lmer(LT50.gomp ~ GDH.0_1d +              (1|elevation),                          data = data)
fit5 <- lmerTest::lmer(LT50.gomp ~ DSS + GDH.0_1d +        (1|elevation),                          data = data)
fit6 <- lmerTest::lmer(LT50.gomp ~ DOY + GDH.0_1d +        (1|elevation),                          data = data)
fit7 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS +             (1|elevation),                          data = data)
fit8 <- lmerTest::lmer(LT50.gomp ~ DOY + DSS + GDH.0_1d +  (1|elevation),                          data = data)
fit9 <- lmerTest::lmer(LT50.gomp ~ DSS * GDH.0_1d +        (1|elevation),                          data = data)
AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)
BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)
summary(fit5)
confint(fit5, oldNames = F)*100 # for 100 GDH
confint(fit5, oldNames = F)*30  # for 30 days 
plot(fit5) # TA-plot
par(mfrow = c(1,2))
qqnorm(ranef(fit5)$elevation[, 1], main = "Random effects of site")
qqnorm(resid(fit5), main = "Residuals")
par(mfrow = c(1,1))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Survival experiment (growth rate vs. freezing temperature) ----
treatment <- data.frame(ID_Temp        = c(2,  -12,   -15,   -18,   -21,   -24,   -27, -30),
                        treatment_temp = c(2, -7.8, -12.3, -18.2, -21.1, -23.5, -26.9, -30))
vigfrost$treatment_temp <- treatment$treatment_temp[match(vigfrost$ID_Temp, treatment$ID_Temp)]   

# Add date as "day after snow removal"
vigfrost$day <- as.double(difftime(vigfrost$date_timeGMT, as.POSIXct("2021-02-18 10:00"), units = "days"))

# Extract growth rate as linear function
growth <- function(x, y){
  coef(glm(y ~ x, family = "gaussian"))[2]
}
pots <- vigfrost %>% dplyr::filter(ID_pot <= 130) %>% 
  group_by(ID_pot, treatment_temp, species, replicate, pot_description, palette) %>%
  dplyr::summarize(growth = growth(x = day, y = leaf_g_mean)) 
pots$species <- pots$species %>% recode_factor(Cc = "Carex", Ns = "Nardus")

# Plot growth rates without negative growth (boxplots)
pots$growth_no_neg <- as.numeric(ifelse(pots$growth > 0.02, pots$growth, 0))
p <- ggplot(pots, aes(x = treatment_temp,
                      y = growth_no_neg*10, # conversion from cm to mm
                      color = factor(species),
                      shape = factor(species)))+
  geom_point(size = 6, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = species), size = 2, show.legend = FALSE) +
  scale_color_manual(breaks = c("Carex", "Nardus"),
                     values = c("darkorange2", "olivedrab4")) +
  scale_x_reverse(limits = c(5, -33),
                  breaks = seq(-30,5,5),
                  labels = c(-30, "", -20, "", -10, "", 0, ""))+
  scale_y_continuous(limits = c(-0.1, 2.2),
                     breaks = seq(0, 2, 0.5),
                     labels = c(0, "", 1, "", 2)) +
  theme(legend.position = c(0.85, 0.85),
        legend.direction  = "vertical",
        legend.title      = element_blank(),
        legend.text       = element_text(size = 30, face = "italic", margin = margin(l = 10)),
        legend.key.height = unit(1.5,"cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key        = element_rect(fill = "transparent", color = "transparent")) +
  theme(legend.key.height=unit(1.5,"cm")) +
  coord_cartesian(clip = "off") +
  theme(axis.title.x   = element_text(size = 35, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.title.y   = element_text(size = 35, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.text.x    = element_text(size = 35, margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.text.y    = element_text(size = 35, margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.ticks.length = unit(10, "pt")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(plot.margin = margin(t = 50, r = 20, b = 20, l = 20)) +
  labs(x = "Frost treatment (°C)",
       y = expression("Leaf growth (mm day"^-1*")")) #+
  # annotate("text", x = 2,  y = 2, hjust = 5, vjust = -1.7, label = "C", size = 15) 
p
# ggsave(p, file = "Survival_experiment.png",
#        path = "FIGURES",
#        width = 35, height = 25, units = "cm" )
 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Species table ----
veg    <- read_csv("DATA/raw_pos_vegetation.csv",
                   col_names = T,
                   col_types = cols(.default = col_double()),
                   na = "NA")
veg.0.1 <- replace(veg, veg > 0 & veg <= 1, 1)   # change cover data to presence/absence data
abundance <- veg.0.1 %>%
  select(-ID_position) %>% 
  colSums() %>%
  data.frame %>% 
  tibble::rownames_to_column("row_names")
colnames(abundance) <- c("genusspecies", "microsites")
abundance$microsites_percent <- round((abundance$microsites/nrow(veg.0.1))*100,0)
abundance$cover <- round((veg %>% select(-ID_position) %>% colSums)/115*100, 2)
abundance <-  abundance %>% arrange(-cover)
veg.survey <- abundance %>% left_join(spec, by = "genusspecies")
veg.survey$name <- with(veg.survey, (paste(genus,
                                           paste("",species),
                                           ifelse(is.na(subspecies), "", paste(" ssp.", subspecies)),
                                           ifelse(is.na(author_citation), "", paste("", author_citation)),
                                           sep = "")))
veg.survey$abundance <- with(veg.survey, paste(microsites, " (", microsites_percent, " %)", sep = ""))
veg.survey$cover <- with(veg.survey, paste(cover, " %", sep = ""))
veg.survey <- veg.survey %>% select(name, family, functional_group = functional_group_3, cover, abundance) %>% print

# write.csv(veg.survey, "R_PLOTS/_FIGURES_THESIS/abundance_table.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# END OF SCRIPT ----