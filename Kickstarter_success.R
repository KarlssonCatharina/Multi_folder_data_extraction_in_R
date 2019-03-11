#kickstarter project code file for the data incubator semi-final

#set directory to the one that contains all of the kickstarter folders (Kickstarter) which can be downloaded from here: https://webrobots.io/kickstarter-datasets/

#list all folders in current directory
folders <- list.dirs( full.names = TRUE, recursive = TRUE)

#create a new blank data frame
variables = 16
output <- data.frame(matrix(ncol=variables, nrow=0))

i=1
j = 1
#write function to tie together data from all of the kickstarter files
for (i in 2:length(folders)){
  #list all files in folder
  foldername=folders[i]
  files=as.data.frame(list.files(foldername,recursive=F,full.names=T,pattern=".csv"))
  colnames(files)[1] <- "path"
  for (j in 1:length(files$path)){
    k <- read.csv(paste(files[j,]), head = T)
    #change two columns to character to allow information extraction
    k$source_url <- as.character(k$source_url)
    k$location <- as.character(k$location)
    #extract the category information from the text in one of the columns
    k$category = unlist(strsplit(k$source_url, split='/', fixed=TRUE))[6]
    k$category = unlist(strsplit(k$category, split='?', fixed=TRUE))[1]
    k$folderfile <- paste(files[j,]) 
    #keep columns that are not annoying
    keeps <- c("id","goal","pledged","state","country","currency","deadline","state_changed_at","created_at","launched_at","backers_count","usd_pledged","folderfile","category","sub-category")
    k <- k[ , (names(k) %in% keeps)]
    output <- rbind (output,k)
  }
}

#write out file
write.csv(output,"kickstarter_full.csv")
output$category <- as.factor(output$category)

#if files already rendered then read in the full file
output <- read.csv("kickstarter_full.csv",head = T)

#rename failed levels in category
levels(output$category)[levels(output$category) == "art?ref=discover_index"] <-"art"
levels(output$category)[levels(output$category) == "crafts?ref=discover_index"] <-"crafts"
levels(output$category)[levels(output$category) == "dance?ref=discover_index"] <-"dance"
levels(output$category)[levels(output$category) == "design?ref=discover_index"] <-"design"
levels(output$category)[levels(output$category) == "fashion?ref=discover_index"] <-"fashion"
levels(output$category)[levels(output$category) == "film%20&%20video"] <-"film"
levels(output$category)[levels(output$category) == "film%20&%20video?ref=discover_index"] <-"film"
levels(output$category)[levels(output$category) == "journalism?ref=discover_index"] <-"journalism"
levels(output$category)[levels(output$category) == "photography?ref=discover_index"] <-"photography"
levels(output$category)[levels(output$category) == "publishing?ref=discover_index"] <-"publishing"
levels(output$category)[levels(output$category) == "theater?ref=discover_index"] <-"theater"
levels(output$category)[levels(output$category) == "games?ref=discover_index"] <-"games"
output$category <- droplevels(output$category)

########################################
#create new summarized data set
library(dplyr)


##file is too large for vis of raw data, so calculate 
summary_kick <- output %>%
  group_by(country,state,category) %>%
  summarize (n = n(), mean_pledged = mean(usd_pledged, na.rm = TRUE),mean_goal = mean(goal),mean_backers= mean(backers_count))%>%
  mutate(freq = n / sum(n))

summary_kick <- as.data.frame(summary_kick)
summary_kick$prop_back <- summary_kick$mean_pledged/summary_kick$mean_backers
summary_kick$pledge_goal <- summary_kick$mean_pledged/summary_kick$mean_goal

#load libraries for visuals
library(ggplot2)
library(googleVis)
library(plotly)

#enter plotly user name and key if it is not in the settings
Sys.setenv("plotly_username"="UserName") #these need to be replaced with the users plotly name
Sys.setenv("plotly_api_key"="********************") #this needs to be replaced with the users API key


#create a bubble plot with countries and the average goal amount relative to the mean number of backers, goal amount is related to size of the circles
#indicating that projects with higher pledge amounts get fewer backers. 
f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
y1 <- list(title = "Mean goal amount",titlefont = f)
x1 <- list(title = "Mean number of backers",titlefont = f)

p1 <- plot_ly(summary_kick, x = summary_kick$mean_backers, y = summary_kick$mean_goal, text = paste("Country: ", summary_kick$country,"Category:", summary_kick$category),mode = "markers", color = summary_kick$category, size = summary_kick$mean_goal) %>% layout(xaxis = x1, yaxis = y1)

chart_link1 = api_create(p1, filename="Mean number of backers and goal amount")
chart_link1

#create a bubble plot with countries and the average pledge amount relative to the mean number of backers
#Indicating interesting trends relative to countries and the types of projects that are backed. For example Honk Kong is likley to pledge higher amounts to art related projects such as theater and music and on average have a higher pledge than other countries
y2 <- list(title = "Mean pledge amount per project",titlefont = f)
x2 <- list(title = "Mean number of backers",titlefont = f)

p2 <- plot_ly(summary_kick, y = summary_kick$mean_pledge, x = summary_kick$mean_backers, text = paste("Country: ", summary_kick$country,"Category:", summary_kick$category),mode = "markers", color = summary_kick$category, size = summary_kick$mean_pledged) %>%
  layout(xaxis = x2, yaxis = y2)

chart_link2 = api_create(p2, filename="Mean pledge amount against mean number of backers per project")
chart_link2

y5 <- list(title = "Mean pledge amount per backer (USD)",titlefont = f)
x5 <- list(title = "Mean number of backers",titlefont = f)

p5 <- plot_ly(summary_kick, y = summary_kick$prop_back, x = summary_kick$mean_backers, text = paste("Country: ", summary_kick$country,"Category:", summary_kick$category),mode = "markers", color = summary_kick$category, size = summary_kick$mean_pledged) %>%
  layout(xaxis = x5, yaxis = y5)

chart_link5 = api_create(p5, filename="Mean pledge per backer amount against mean number of backers")
chart_link5

#investigate failure and success rates
y3 <- list(title = "Number of Projects",titlefont = f)
x3 <- list(title = "State of Project",titlefont = f)

p3 <- plot_ly(summary_kick, y = summary_kick$n, x = summary_kick$state,mode = "markers", color = summary_kick$category) %>% layout(xaxis = x3, yaxis = y3)

chart_link3 = api_create(p3, filename="failur_and_success_in_country")
chart_link3

#investigate failure and success rates related to country
summary_kick_2 <- output %>%
  group_by(country,state) %>%
  summarize (n = n(), mean_pledged = mean(pledged, na.rm = TRUE),mean_goal = mean(goal),mean_backers= mean(backers_count))%>%
  mutate(freq = n / sum(n))

y4 <- list(title = "Fraction of Projects in Category",titlefont = f)
x4 <- list(title = "Country",titlefont = f)

p4 <- plot_ly(summary_kick_2, y = summary_kick_2$freq, x = summary_kick_2$country, type = 'bar', color = summary_kick_2$state, name = ~ state) %>% layout(xaxis = x4, yaxis = y4,barmode = 'stack')

chart_link4 = api_create(p4, filename="Fraction_of_project_in_each_category")
chart_link4










