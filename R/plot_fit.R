plot_fit <- function(observed=output$fit, predicted=output$fit){


  fit.df <- data.frame(obs=observed, pred=predicted)

  fit.ggplot <- ggplot(data=fit.df, aes(x=obs,y=pred)) +
    geom_point(color="grey70",alpha=0.75) +
    geom_abline(intercept = 0, slope= 1, colour="red", linetype=2) +
    scale_x_continuous(name="Discrenpancy measure for observed data set") +
    scale_y_continuous(name="Discrenpancy measure for predicted data set") +
    theme(panel.background = element_rect(fill="white"),
          axis.text.x = element_text (size = 8, angle=0,vjust = 0, hjust=0),
          axis.text.y = element_text (size = 8, hjust = 0),
          axis.title.x= element_text (size = 10),
          axis.title.y= element_text (size = 10),
          legend.position="none",
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.line=element_line())

  fit.ggplot + geom_text(aes(x=round_any(min(fit.df[,1:2]),1),
                             y=round_any(max(fit.df[,1:2]),1),
                             label=paste("P = ", round(mean(observed > predcited),3), sep=""), vjust="inward", hjust="inward"))


 return(fit.ggplot)
}


