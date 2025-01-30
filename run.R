library(ggplot2)
library(writexl)
source("file_loader.R")
maindir <- getwd()
?version
R.version

filenumber()
filenames(2)

## control parameters
no_of_replicates <- 5
no_of_samples <- 3
#samples_name <- c("LDH_FT6_apo", "LDH_FT6_RV10", "LDH_FT6_RV20")
samples_name <- c("CS_Q20_50uM", "CS_Q20_5uM", "CS_Q30_5n")
enzyme_concentration <- 1/1000000000  ## CS 1 LDH 2
no_of_points_for_lm <- 11
epsilon <- 14000  ## CS 14000 LDH 6300
wavelength <- 412  ##CS 412 LDH 340


## which file to load
firstone <- fileloader(2)

##convert_time 
{
firstone$Time <- as.numeric( firstone$Time )
times <- firstone$Time
number_dt <- length(times)

for (x in 1:number_dt) {
  difference <- (times[x] - times[1])
  firstone[x,1] <- difference
}

}

##create clean dataframe

clean_data <- as.data.frame( firstone$Time )
colnames(clean_data) <- ("Time")

for (i in 1:96) {
  a <- i+2
  
  if (is.na(firstone[1,a]) == FALSE) 
  {
    clean_data <- cbind(clean_data, firstone[,a])
  }
}

## loop for no_of_samples
for (kk in 1:no_of_samples){
numb <- kk

## genertal plot
general_plot <-0
general_plot <- ggplot(data = clean_data, mapping = aes(x = Time)) 

for (i in 1:no_of_replicates) {
  general_plot <- general_plot + geom_point(alpha = 0.4, 
            y = clean_data[,(i)+1+(numb-1)*no_of_replicates], 
            colour = (i+1), size = 4)
}


general_plot <- general_plot +  labs(title=samples_name[numb],
         x="Time (s)", y = paste("Abs", wavelength, "nm",sep=" ")) + theme_bw() + 
  ylim((min(clean_data[, (5*numb-3):(5*numb+1)]) - 0.1 * min(clean_data[, (5*numb-3):(5*numb+1)])),
       1.1*max(clean_data[, (5*numb-3):(5*numb+1)]) ) +
  theme(plot.title = element_text(hjust = 0.5))
setwd("output")
ggsave(paste(samples_name[numb], ".jpeg",sep=""), general_plot)
setwd(maindir)


y_minimum <- (min(clean_data[, (5*numb-3):(5*numb+1)]) - 0.1 * min(clean_data[, (5*numb-3):(5*numb+1)]))

## zoomed single plot after lm
zoomed_data <- clean_data[1:no_of_points_for_lm,]

for (t in 1:no_of_replicates) {
  
powtorzonko <- t
fitting_data <- data.frame()

zoom_plot <- ggplot(data = zoomed_data, mapping = aes(x = Time, 
         y = zoomed_data[,powtorzonko + 1+(kk-1)*no_of_replicates])) + 
  geom_point(alpha = 0.2, y = zoomed_data[,powtorzonko + 1+(kk-1)*no_of_replicates], colour = (powtorzonko + 1), size = 4) +
  labs(title=paste(samples_name[numb], powtorzonko, sep=" "),
       x="Time (s)", y = paste("Abs", wavelength, "nm",sep=" ")) + theme_bw() + 
  ylim(y_minimum, ## here autoscaling plot
       1.1*max(zoomed_data[, (5*numb-3):(5*numb+1)]) ) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method=lm, colour = "black")

setwd("output")
ggsave(paste(samples_name[numb], powtorzonko, ".jpeg",sep=""), zoom_plot)
setwd(maindir)

}


## lm
fitting_data <- 0
fitting_data <- data.frame()
for (j in 1:no_of_replicates) {
  fitting <- lm(zoomed_data[,j+1 + (kk-1)*no_of_replicates ]~zoomed_data[,1])
  fitting_data[j,1] <- fitting$coefficients[2]
  colnames(fitting_data) <- paste(samples_name[numb], sep="")
  fitting <- NULL
}

fitting_data[, 2] <- (fitting_data[, 1])/epsilon
colnames(fitting_data)[2] <- ("M/s")
fitting_data[, 3] <- (fitting_data[, 2])/enzyme_concentration
colnames(fitting_data)[3] <- ("v/[E]")
fitting_data[1, 4] <- mean(fitting_data[,3])
colnames(fitting_data)[4] <- ("mean v/[E]")
fitting_data[1, 5] <- sd(fitting_data[,3])
colnames(fitting_data)[5] <- ("SD v/[E]")
setwd("output")
write_xlsx (fitting_data, paste("sum_up", samples_name[numb], ".xlsx",sep=""))
write_xlsx (clean_data, paste("clean_data_",samples_name[numb], ".xlsx",sep=""))
setwd(maindir)
}

