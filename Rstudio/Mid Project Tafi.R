#importing .csv file 
dataset1 <- read.csv("projectdata.csv", header = TRUE, sep = ",")
dataset1
#Replacing NA in column Assault with mean of the remaining values
dataset1$Assault[is.na(dataset1$Assault)]<-mean(dataset1$Assault,na.rm=TRUE)
dataset1
#Data Formatting... To round up the murder and assualt variable
dataset1$Murder = as.numeric(format(round(dataset1$Murder, 0)))
dataset1
dataset1$Assault = as.numeric(format(round(dataset1$Assault, 0)))
dataset1

#Merging Type variable in dataset 
dataset2 <- dataset1
dataset2 <- transform(dataset2, Population_level = Urban.Population...)
dataset2
#Data intrigation  prepare the dataset to integrate a new column (named population_level) based on the urban population variable.
Population_level <- function(Urban.Population...){
  if (Urban.Population... < 50) {
    return("Small")
  } else if (Urban.Population... >= 50 & Urban.Population... < 60) {
    return("Medium")
  } else if (Urban.Population... >= 60 & Urban.Population... < 70) {
    return("Large")
  } else {
    return("Extra-large")
  }
}

dataset2$Population_level <- sapply(dataset2$Urban.Population..., Population_level)
dataset2


#Data reduction- (as nation 15 is too high and and nation 32 is too low to be a percentage)  
dataset2 <- dataset2[-c(15, 32), ]
dataset2

#integrate new column OrderedFactorPopulation like (small=1,medium=2,large=3,extralarge=4)

levels <- c("Small", "Medium", "Large", "Extra-large")

dataset2$OrderedFactorPopulation <- factor(
  dataset2$Population_level,
  levels = levels,
  ordered = TRUE,
  labels = 1:length(levels)
)
dataset2


