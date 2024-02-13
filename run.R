library(ggplot2)
library(writexl)
source("file_loader.R")

filenumber()
filenames(1)

## control parameters
no_of_replicates <- 5
no_of_samples <- 1
samples_name <- c("QC")
enzyme_concentration <- 1/1000000000
no_of_points_for_lm <- 11
epsilon <- 14000

## which file to load
firstone <- fileloader(1)

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
    print("Not NA")
  }
  
  else{
    print("is NA")
  }
  
}


## genertal plot
general_plot <-0
general_plot <- ggplot(data = clean_data, mapping = aes(x = Time)) 

for (i in 1:no_of_replicates) {
  general_plot <- general_plot + geom_point(alpha = 0.4, y = clean_data[,i], 
                                            colour = (i+1), size = 4)
  print(i)
}

general_plot <- general_plot +  labs(title=samples_name,
         x="Time (s)", y = "Abs340 nm") + theme_bw() + 
  ylim((min(clean_data[, 2:6]) - 0.1 * min(clean_data[, 2:6])),
       1.1*max(clean_data[, 2:6]) ) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(samples_name, ".jpeg",sep=""), general_plot)

## zoomed single plot after lm
zoomed_data <- clean_data[1:no_of_points_for_lm,]

for (t in 1:no_of_replicates) {
  
powtorzonko <- t
ppp <- powtorzonko+1
fitting_data <- data.frame()

zoom_plot <- ggplot(data = zoomed_data, mapping = aes(x = Time, y = zoomed_data[,ppp])) + 
  geom_point(alpha = 0.2, y = zoomed_data[,ppp], colour = (ppp), size = 4) +
  labs(title=paste(samples_name, powtorzonko, sep=" "),
       x="Time (s)", y = "Abs340 nm") + theme_bw() + 
  ylim((min(zoomed_data[, ppp]) - 0.1 * min(zoomed_data[, ppp])),
       1.1*max(zoomed_data[, 2:6]) ) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method=lm, colour = "black")

ggsave(paste(samples_name, powtorzonko, ".jpeg",sep=""), zoom_plot)
}


## lm
fitting_data <- 0
fitting_data <- data.frame()
for (j in 1:no_of_replicates) {
  fitting <- lm(zoomed_data[,j+1]~zoomed_data[,1])
  fitting_data[j,1] <- fitting$coefficients[2]
  colnames(fitting_data) <- paste(samples_name, sep="")
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

write_xlsx (fitting_data, paste(samples_name, "sum_up",".xlsx",sep=""))

