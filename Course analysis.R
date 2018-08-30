#Seed
set.seed(123)


library(cluster)
library(dplyr)
library(reshape2)

#Read data
data <- read.csv2("coursedetails.csv",fileEncoding = "UTF-8-BOM")

#First 6 rows
head(data)

#Change to lower case
data$details <- tolower(data$details)

#Par information. Par is always presented like Y in following: 'Pituus xx metriä. Par Y'
data$par <- as.numeric(gsub('pituus.*metriä. par ([0-9]).*','\\1',data$details))
table(data$par)

#Hole distance
data$distance <- as.numeric(gsub('pituus (.*) metriä. par.*','\\1',data$details))
table(data$distance)

# 1) Discgolf hole can in general be eather straight, turn to left or turn to right
# 2) Hole can eather be on flat ground, go downhill or uphill
# 3) Hole can have some obstacles in the middle of the court, usually trees, that you should avoid hitting. There can also be
#     mandos that are marked spots that you should bypass.
# 4) If the basket is located on top of hill or on top of rock, there is risk of 'rolling' which means that disc starts rolling
#     after it lands because of hard ground or downhill.
# 5) There can also be marked "Out-of-Bounds" areas which you should avoid


#Lets create variable straight which will tell, does the discription include word suora/straight in some format
data$straight <- ifelse(grepl("suora",data$details),1,0)
table(data$straight)

#Lets see how many of the courses have curve / kaartoa.
data$curve <- ifelse(grepl("kaarta|kaartu|kaarto|mutka",data$details),1,0)
table(data$curve)

#Downhill
data$downhill <- ifelse(grepl("alamäk",data$details),1,0)
table(data$downhill)


#Uphill
data$uphill <- ifelse(grepl("ylämäk",data$details),1,0)
table(data$uphill)

#Ränni is a narrow route between trees
data$narrow <- ifelse(grepl("ränni",data$details),1,0)
table(data$narrow)

#Mando
data$mando <- ifelse(grepl("mando",data$details),1,0)
table(data$mando)

#Rolling
data$rolling <- ifelse(grepl("rollau",data$details),1,0)
table(data$rolling)

#OB
data$ob <- ifelse(grepl("ob",data$details),1,0)
table(data$ob)

#Rock
data$rock <- ifelse(grepl("kallio",data$details),1,0)
table(data$rock)

#Left
data$left <- ifelse(grepl("vasempaan|vasemmalle",data$details),1,0)

#Right
data$right <- ifelse(grepl("Oikeaan|oikealle",data$details),1,0)



###################################################################################################
#########################   DESCRIPTION BASED SIMILARITY   ########################################
###################################################################################################


#Lets select only descriptive variables and twist data frame so that each descriptive variable is a row and each course/hole
#is column.
descriptives <- data %>% select(course,straight, curve,downhill, uphill, narrow, mando,rolling,ob,rock,left,right) %>%
  melt(.,id.vars="course") %>%
  dcast(variable~course,value.var="value") 


#Calculate correlation between descriptive vectors
descriptives_correlation <- descriptives %>% select(-variable) %>% cor(.)


#Change all lower triangular values to NA because we want to melt this data and have only unique values
diag(descriptives_correlation) <- NA


#Add row based ranking
descriptives_ranking <- data.frame(t(apply(-descriptives_correlation,1,rank)))


#Melt rankings to long format and remove rankings between the hole with itself
descriptives_ranking <- descriptives_ranking %>% mutate(hole=row.names(.)) %>% 
  melt(.,id.vars="hole") %>%
  rename("compared_hole"=variable, "descriptive_ranking"=value) %>%
  filter(hole!=compared_hole) 




###################################################################################################
#########################   HOLE DISTANCE BASED SIMILARITY    #####################################
###################################################################################################



#Lets create similarity df based on just the lenght. Before calculating similarity, lets normalize lenghts. 
mean_distance <- mean(data$distance)
mean_distance

sd_distance <- sd(data$distance)
sd_distance

data$normalized_distance <- (data$distance-mean_distance)/sd_distance


#Calculate distancies between normalized hole distancies
distance_similarity <- as.data.frame(as.matrix(dist(data$normalized_distance)))


#Rename row and column names
colnames(distance_similarity) <- paste(data$course)
rownames(distance_similarity) <- data$course
diag(distance_similarity) <- NA


#Add row based ranking
distance_ranking <- data.frame(t(apply(distance_similarity,1,rank)))


#Melt rankings to long format and remove rankings between the hole with itself
distance_ranking <- distance_ranking %>% mutate(hole=row.names(.)) %>% 
  melt(.,id.vars="hole") %>%
  rename("compared_hole"=variable, "distance_ranking"=value) %>%
  filter(hole!=compared_hole) 



###################################################################################################
#########################   COMBINE RANKINGS  #####################################################
###################################################################################################



#Merge ranking tables
rankings <- distance_ranking %>% merge(.,descriptives_ranking,by=c("hole","compared_hole"),all.x=TRUE) %>%
  mutate(ranking=(distance_ranking+descriptive_ranking)/2) %>% arrange(hole,ranking)


