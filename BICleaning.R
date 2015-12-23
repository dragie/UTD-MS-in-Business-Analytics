#' ---------------------------------------------------------------
#' @version 1.1.0 Census Data Analysis
#' @title census
#' 
#' @description 
#' This script is used to analyze the Indian census data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#Setting the environment variable for Java 
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_60\\")

#Adding libraries
options(java.parameters = "-Xmx1024m")
#http://stackoverflow.com/questions/7963393/out-of-memory-error-java-when-using-r-and-xlconnect-package
library(rJava)
library(XLConnect)

#Data Reading
data <- loadWorkbook("D:\\Business Intellingence Software and Techniques\\Data Sets\\BI Project Sample Data\\2014\\Original Data\\DDW-0100C-10.xlsx")
data.jammu<- readWorksheet(data, sheet = 1, header = TRUE)
#head(View(data.jammu))

#Data Preprocessing
data.jammu <- data.jammu[-(5:6), ]
data.jammu <- data.jammu[-(1:4), ]
#colnames(data.jammu)
colnames(data.jammu)[6] <- c('Col6')
data.jammu <- data.jammu[data.jammu$Col6=="All ages",]

# summary(data.education)
# 
# #Subsetting total population data
# total <- subset(data.education, AreaName == 'INDIA')
# summary(total)
# View(total)
# total <- as.matrix(total)
# total[is.na(total)] <- 0
# 
# (meanIllietrate <- mean(as.numeric(total[,7])))
# 
# #Subsetting state wise
# #totalIlliterateStateWise <- subset(data.education, ) 
# par(mfrow = c(2,2))

finalData <- data.jammu
filenames <- list.files("D:\\Business Intellingence Software and Techniques\\Data Sets\\BI Project Sample Data\\2014\\Original Data\\", pattern="*.xlsx", full.names=TRUE)
for(i in 2:length(filenames)){
  book <- loadWorkbook(filenames[i])
  data<- readWorksheet(book, sheet = 1, header = TRUE)
  #head(View(data))
  
  #Data Preprocessing
  data <- data[-(5:6), ]
  data <- data[-(1:4), ]
  #colnames(data)
  colnames(data)[6] <- c('Col6')
  data <- data[data$Col6=="All ages",]
  finalData <- rbind(finalData, data)
}

#Write Data into csv
write.csv(finalData, file = "D:\\Business Intellingence Software and Techniques\\Data Sets\\BI Project Sample Data\\Output.csv", row.names = FALSE)

colnames(finalData) <- c('TableName', 'StateCode', 'DistCode', 'AreaName', 'AreaType', 'AgeGroup', 'TotPerson',
                         'TotMale', 'TotFemale', 'EduPerson', 'EduMale', 'EduFemale', 'SchoolPerson', 'SchoolMale',
                         'SchoolFemale', 'CollegePerson', 'CollegeMale', 'CollegeFemale', 'VocTotal', 'VocMale', 
                         'VocFemale', 'DisabledPerson', 'DisabledMale', 'DisabledFemale', 'LitPerson', 'LitMale',
                         'LitFemale', 'OtherPerson', 'OtherMale', 'OtherFemale', 'AttendedPerson', 'AttendedMale',
                         'AttendedFemale', 'NeverAttPerson', 'NeverAttMale', 'NeverAttFemale')

#Clustering
#Remove missing values
clusterData <- finalData[,c(10, 13, 16, 19, 22, 25, 28)]
nonEduData <- finalData[, 34]
clusterData <- na.omit(clusterData)
#Standardize variables
clusterData <- scale(finalData) 
fit <- kmeans(clusterData[, -(1:2)], 5)
summary(fit)
plot(clusterData,  col = fit$cluster)
points(fit$centers, pch = 16)


#household data
houseData <- read.csv("D:\\Business Intellingence Software and Techniques\\Data Sets\\BI Project Sample Data\\Household good status.csv")
houseDataCluster <- na.omit(houseData[, -(1:6)])
clusterHouse <- kmeans(houseDataCluster, 5)
summary(clusterHouse)
par(mar=c(5,7,1,1)+0.1)
par(mar = rep(2, 4))
plot(houseDataCluster[,c(5,6)],  col = clusterHouse$cluster)
points(clusterHouse$centers, pch = 16)
