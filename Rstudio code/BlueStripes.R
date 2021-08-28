

BlueStripes <- function(vector,year){
  
  for (i in 1:length(vector)) {
    c9a <- ggplot(vector[[i]], aes(x = Date, y = Ammonia)) +
      geom_line()  + labs(title = paste(vector[[i]][1,3],year))
    nada <- is.na(vector[[i]]["Ammonia"])
    c9a <- c9a + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
    
    c910 <- ggplot(vector[[i]], aes(x = Date, y = PM10)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM10"])
    c910 <- c910 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    
    c925 <- ggplot(vector[[i]], aes(x = Date, y = PM25)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM25"])
    c925 <- c925 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    jpeg(filename =paste(vector[[i]][1,3],paste(year,".jpeg")),width = 1280, height = 720 )
    multiplot(c9a, c910, c925)
    dev.off()
    
  }
  
}