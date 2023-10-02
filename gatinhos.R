  library(ggplot2)
  #install.packages("ggpattern")
  library(ggpattern)
  
  if (require("magick")) {
    
    G <- ggplot(mpg, aes(class)) +
      geom_bar_pattern(
        aes(
          pattern_angle = class
        ), 
        pattern         = 'placeholder',
        pattern_type    = 'kitten',
        fill            = 'white', 
        colour          = 'black',
        pattern_spacing = 0.025
      ) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      labs(x= "Gatinhos lindos😻",
           y= "Ótimo final de semana 💜",
        title = "Hora de descontrair!",
        subtitle = "Será que eu amo gatos?") + 
      theme(legend.position = 'none') +
      coord_fixed(ratio = 1/15) + 
      scale_pattern_discrete(guide = guide_legend(nrow = 1))
    
    G
    
  }
  
  png("gatos.png")
  print(G)
  dev.off()
  