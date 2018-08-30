library(dplyr)
library(reshape2)
library(rvest)


#Seed
set.seed(123)




#######################################################################################################
########################    FETCHING DATA STARTS  #####################################################
#######################################################################################################



#Lets first read the full opening page
full_page <- read_html("https://frisbeegolfradat.fi/")

#On the first page there is a dropdown list of all the courses, lets hunt this down from full_page html element. This list is 
#located in the class "selectRata" and each course is after tag "option"
course_list <- full_page %>% html_nodes(".selectRata") %>% html_nodes("option") 
course_list


#Change class from html-object to pure text. 'course_value_list' has values that are used in backend of the webpage,
#'course_name_list' has frontend names
course_value_list <- course_list %>%  html_attrs()
course_name_list <- course_list %>%  html_text()
head(course_value_list)
head(course_name_list)

#Unlist course_value_list, change to matrix and finally create data frame that has column named course. Add also course_name_list
#as the course name to df. The first element of course_name_list is header of the dropdown list, "Valitse rata" / "Choose course" 
#and this is removed. On course_value_list first element is blank for the same reason. 
courses <- data.frame(course_value=matrix(unlist(course_value_list),nrow=649,byrow=TRUE),course_name=course_name_list[2:650])




#Lets read courses fairway details in a loop. Running the loop took around 20 minutes.
for(i in 1:length(courses$course_value)){
  
  #If i divided by 150 equals zero, loop will wait for 15 seconds. This is to avoid visiting page too many times in short period
  if(i%%150==0){
    Sys.sleep(15)
  }
  
  #Set address
  address <- paste("https://frisbeegolfradat.fi/rata/",courses$course_value[i],sep="")
  
  #Read the webpage
  temp_page <- read_html(address)
  
  #Fairway details are located on a class named fairway
  temp_details <- temp_page %>% html_nodes(".fairway") %>% html_text()
  
  #Create a data frame that has fairway details for the course in loop, add also course name
  temp_course <- data.frame(details=tolower(temp_details)) %>% mutate(course=courses$course_name[i])
  
  #Add courses fairway details to fairway named df. If i = 1, we create fairways df
  ifelse(i==1,fairways <- temp_course, fairways <- rbind(fairways,temp_course))
}

#Write the data in a csv-file so we don't have to read it again unless we want to.
write.csv2(fairways,"Fairways.csv",row.names = FALSE)





#######################################################################################################
########################    FETCHING DATA ENDS   ######################################################
#######################################################################################################





#######################################################################################################
########################       ADDITIONAL VARIABLES STARTS       ######################################
#######################################################################################################


# 1) Discgolf hole can in general be eather straight, turn to left or turn to right
# 2) Hole can eather be on flat ground, go downhill or uphill
# 3) Hole can have some obstacles in the middle of the court, usually trees, that you should avoid hitting. There can also be
#     mandos that are marked spots that you should bypass
# 4) If the basket is located on top of hill or on top of rock, there is risk of 'rolling' which means that disc starts rolling
#     after it lands because of hard ground or downhill
# 5) There can also be marked "Out-of-Bounds" areas which you should avoid. These are usually written as OB. It is good to remember
#     that b is very rare letter in finnish


#Fairway number
fairways$fairway_number <- suppressWarnings(as.numeric(gsub('väylä ([0-9]{1,2}).*','\\1',fairways$details)))
table(fairways$fairway_number)

#Fairway length
fairways$distance <- suppressWarnings(as.numeric(gsub('.*pituus ([0-9]{2,3}) metriä.*','\\1',fairways$details)))
table(fairways$distance)

#Fairway name based on course name and fairway number. Remove also space from course name
fairways$fairway <- paste(gsub(" ","_",fairways$course),fairways$fairway_number,sep="")



#Lets create variable straight which will tell, does the discription include word suora/straight in some format
data$straight <- ifelse(grepl("suora",data$details),1,0)
table(data$straight)

#Lets see how many of the courses have curve / kaartoa.
data$curve <- ifelse(grepl("kaarta|kaartu|kaarto|mutka",data$details),1,0)
table(data$curve)

## REMEMBER 'korkeus ero' and minus and plus sign >> could be used to measure down/uphill

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


#######################################################################################################
########################       ADDITIONAL VARIABLES ENDS      #########################################
#######################################################################################################





#######################################################################################################
########################         ANALYSIS STARTS        ###############################################
#######################################################################################################



########################################################
###########   DESCRIPTION BASED SIMILARITY   ###########
########################################################


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


