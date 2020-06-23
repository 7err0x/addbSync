# importing data, from the AD database
my_ADinput <- function(ADinput.URL="https://raw.githubusercontent.com/7err0x/addbSync/master/userAD.csv"){
  ADfile <- read.csv(url(ADinput.URL)
                      , sep = ";", header = TRUE, colClasses="character")
  # formating columns
  ADfile <- transform(ADfile,Enabled = as.logical(Enabled),
                      passwordneverexpires = as.logical(passwordneverexpires))
  colnames(ADfile)[1] = "AccountName"
  
  return(ADfile)
}

# importing ticket requests, from G-forms
my_GformIn <- function(GFinput.URL="https://docs.google.com/spreadsheets/d/e/2PACX-1vT2geOBgAF5lklLb__ht3UVvQD0MZd0PwsYglXcU4DovP4eaUU2FipdzzhI-5gANGPZKkhKyr-Ne7Qx/pub?gid=544924986&single=true&output=csv") {
  GFfile <- read.csv(url(GFinput.URL)
                      , sep = ",", header = TRUE, colClasses="character")
  # drop first column
  GFfile[,1] <- NULL
  return(GFfile)
}

my_main <- function(){
  
  #finding user row position in AD
  GFpos <- match(GF$AccountName,AD$AccountName, nomatch = NULL) 
  PReportNA <- GF[which(is.na(GFpos)),] #select NA rows
  GFpos <- na.omit(GFpos) # delete NA rows
  
  ADrows <- AD[GFpos,] #extracting rows from AD
  
  DuplicateUID <- ADrows[,1]  == ADrows[,4] # see if AccountName == UID
  PReport.UID <- ADrows[DuplicateUID,] #get wrong accounts info
  PReport.Enabled <- ADrows[!ADrows$Enabled,] #get disabled accounts
  PReport.password <- ADrows[!ADrows$passwordneverexpires,] #get expired accounts
  
  # adding identified problem information to datasets
  # PReportNA$problem <- "not found"
  PReport.UID$problem <- "wrong data"
  PReport.Enabled$problem <- "disabled"
  PReport.password$problem <- "expired"
  
  joinReport <- rbind(PReport.Enabled,PReport.UID,PReport.password)
  Report <<- merge( x= PReportNA, y = joinReport, by ="AccountName", all =  TRUE)
  
  
  #saving results in a dataframe
  Treport <- data.frame( 
    category= c("Not found", "wrong info", "disabled", "expired"),
    counter= c(nrow(PReportNA),nrow(PReport.UID), nrow(PReport.Enabled),nrow(PReport.password))
  )
 return(Treport)
}

my_graphycs <- function(Graph="none") {
 if(isTRUE(Graph == "pie") | isTRUE(Graph == "bars")) { #compare parameter
   
  library(ggplot2)
  Treport <- my_main()

  Treport$fraction <- Treport$counter / sum(Treport$counter) #percentage
  Treport$ymax <- cumsum(Treport$fraction) #cumulative percentages
  Treport$ymin <- c(0, head(Treport$ymax, n=-1)) # Compute the bottom of each rectangle
  
  switch (Graph,
      pie = {  #case pie
        ggplot(Treport, aes(ymax=ymax, ymin=ymin,xmax=4,xmin=3, fill=category)) +
          geom_rect() +
          coord_polar(theta="y") +
          xlim(c(2, 4))
      },
    
      bars = {  #case bars
      ggplot(Treport, aes(x=category, y=counter,ymax=ymax, fill=category )) + 
        geom_bar(stat = "identity" ) +
        scale_fill_brewer(palette = "Set1") +
        theme(legend.position="none")
      }
  )
 }
}
 