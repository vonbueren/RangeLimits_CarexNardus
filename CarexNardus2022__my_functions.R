### CarexNardus2022__my_functions ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Date created:     2020-07-09
# Location created: ALPFOR
# Author:           Raphael S. von Bueren (GitHub: vonbueren)
# Last Entry:       2021-12-20
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Description ----
# Code of some self-created functions.
# Sourced by "CarexNardus2022__relevant_code.R".
# 
# Output R functions:
# my.frost.damage()
# my.sum.pos()
# my.sum.neg()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get started ----
library(ggplot2)
library(reshape2)
library(minpack.lm)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Function "my.frost.damage" - Plot leakage data with Gompertz (alpha = 100) and Boltzmann function --> LT50 ----
my.frost.damage <- function(colnames = c("Nardus_95_BLUETE_1", "Nardus_95_BLUETE_2", "Nardus_95_BLUETE_3"),
                            data     = leakage_list$`./Frost_Leakage_2020_06_29_Site7_9_10_Temp_8_10_12_13_14_15_Roehrchen_24h.xlsx`,
                            beta     = 10,   k = -0.1, # estimates (Gompertz)
                            x0       = -10, dx = -1,   # estimates (Boltzmann)
                            temp.min      = -35, 
                            temp.max      = 20,  
                            zero.leakage  = mean(dat.2$value[dat.2$Temperature == max(dat.2$Temperature)], na.rm = T),
                            max.leakage   = mean(dat.2$value[dat.2$Temperature == -30], na.rm = T)){
  
  # Create dataframe
  dat.1           <- data[ c("Temperature", colnames) ]
  dat.2           <- reshape2::melt(dat.1, id = "Temperature")
  percent.damage  <- (dat.2$value - zero.leakage)/(max.leakage - zero.leakage)*100  # calculate percent leakage
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
  
  # Create functions
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
  
  # ggplot preparation
  boltz.LT50 <- x0
  gomp.LT50  <- -log( (log(100) - log(50) ) / coef(nls.gompertz)[["beta"]]) / coef(nls.gompertz)[["k"]]  # see: Lim, Arora and Townsed 1998
  
  x.minlim <- min(dat.3$Temperature) - 5
  x.maxlim <- max(dat.3$Temperature) + 2
  x.lim    <- c(x.minlim, x.maxlim)
  
  plot.title     <- paste(substr(colnames[1], 1, nchar(colnames[1])-2), "\n")
  boltz.title    <- paste("LT50 =", round(boltz.LT50, digits = 1), "°C (Boltzmann)", sep=" ")
  gompertz.title <- paste("LT50 =", round(gomp.LT50,  digits = 1), "°C (Gompertz)",  sep=" ")
  plot.subtitle  <- paste(boltz.title, gompertz.title, sep="\n")
  plot.tottitle  <- paste(plot.title, boltz.title, gompertz.title, sep="\n")
  
  print(summary(nls.boltzmann)) # output for frost.damage()
  print(summary(nls.gompertz))  # output for frost.damage()
  
  # ggplot
  ggplot2::ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
    stat_function(fun = fun.boltz, aes(colour = "Boltzmann")) +
    stat_function(fun = fun.gompertz, aes(colour = "Gompertz")) +
    scale_colour_manual("non-linear model", values = c("blue", "red")) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_classic() +
    theme(plot.title = element_text(size = 10, colour=c("black"), vjust = 0)) +
    theme(plot.margin = margin(30, 10, 10, 10),
          legend.position = c(0.8, 0.8),
          legend.direction = "vertical",
          legend.title = element_text(color = "black", size = 5),
          legend.text  = element_text(color = c("black"), size = 5)) +
    scale_x_continuous(limits = x.lim) +
    ylim(min(dat.3$percent.damage - 5), max(dat.3$percent.damage + 5)) +
    geom_point(data = dat.3, aes(x = Temperature, y = percent.damage)) +
    labs(x = "Temperature [°C]", y = "Leakage")

}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Function "my.sum.pos" - Add up only positive values --> used for "growing degree hours (GDH)" ----
my.sum.pos <- function(x){sum(x[x>0])}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# END OF SCRIPT ----