zoom_plot <- ggplot(data = zoomed_data, mapping = aes(x = Time, 
                                                      y = zoomed_data[,powtorzonko + 1+(kk-1)*no_of_replicates])) + 
  geom_point(alpha = 1, y = zoomed_data[,powtorzonko + 1+(kk-1)*no_of_replicates], colour = (powtorzonko + 1), size = 4, shape = 15) +
  labs(title="Citrate Synnthase",
       x="Time (s)", y = paste("Abs", wavelength, "nm",sep=" ")) + theme_bw() + 
  ylim(0.1,0.4 ) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method=lm, colour = "black")

zoom_plot

ggsave(paste("CS", ".jpeg",sep=""), zoom_plot)

?geom_point
