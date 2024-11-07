#function for plotting and exporting variable importance from random forests
varimpR <- function(mod, mod_name) {
  tmp <- varImp(mod)$importance
  tmp$Covariate <- row.names(tmp)
  tmp$Model <- mod_name
  tmp <- tmp[order(tmp$Overall, decreasing = TRUE), ]
  write.csv(tmp, paste0("output/varimp/varimp_", mod_name, ".csv"), row.names = F)
  
  tmp$Covariate <- factor(tmp$Covariate, levels = rev(tmp$Covariate))
  p <- ggplot(data = tmp) +
    geom_segment(aes(x = Covariate, xend = Covariate, y=0, yend = Overall), colour = "black") +
    geom_point(aes(x = Covariate, y = Overall), colour = "black") +
    coord_flip() +
    labs(x = "", y = "Relative variable importance", subtitle = unique(tmp$Model)) +
    theme_bw() +
    theme(text = element_text(colour = "black"),
          axis.text = element_text(colour = "black"))
  print(p)
  
  pdf(paste0("output/varimp/", mod_name, ".pdf"), height = 60/25.4, width = 75/25.4, useDingbats = FALSE)
  print(p)
  dev.off()
}