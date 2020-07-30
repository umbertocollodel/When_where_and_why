# Main script for the comparison of WEO forecasts with Consensus, EC and WB:


# Function to use pipe with ggsave: ----

save.plot <- function(x,filename){
  ggsave(filename,x)
}
