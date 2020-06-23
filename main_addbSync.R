# setwd(choose.dir())
directory <- getwd() # be shure R workspace is cleared before proceeding next
ADinput.URL <- "https://raw.githubusercontent.com/7err0x/addbSync/master/userAD.csv"
source("myEngine.R")

AD <- my_ADinput(ADinput.URL)
GF <- my_GformIn()
Graph <- "bars" #generate graphics? "pie"/"bars" default none

my_main()
my_graphycs(Graph)

Report
