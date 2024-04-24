#Seyi Aro

#installing packages
install.packages("dplyr")
library(dplyr)
library(magrittr)
library(stringr)

getwd()
myfile1 <- "/Users/seyi/CIS 663/MyProj/population.csv"
population <- read.csv(file = myfile1, header = FALSE, sep = ',')
population <- population[4:55,]
colnames(population) <- population[1,]
population <- population[-1,]
rownames(population) <- NULL
colnames(population)[3] <- "year_21"
colnames(population)[4] <- "year_22"
population$GeoName <- gsub(pattern ="[\\* ]$", replacement = "", x = population$GeoName)
population$GeoName <- gsub(pattern ="[\\ ]$", replacement = "", x = population$GeoName)

population <- mutate(population, popu_diff = year_22 - year_21)

#income per capita data
myfile2 <- "/Users/seyi/CIS 663/MyProj/percapita.csv"
percapita <- read.csv(file = myfile2, header = FALSE, sep = ',')
percapita <- percapita[4:55,]
colnames(percapita) <- percapita[1,]
percapita <- percapita[-1,]
rownames(percapita) <- NULL
colnames(percapita)[3] <- "perc_21"
colnames(percapita)[4] <- "perc_22"
percapita <- subset(percapita, select = -c(perc_22))
percapita$GeoName <- gsub(pattern ="[\\*]$", replacement = "", x = percapita$GeoName)
percapita$GeoName <- gsub(pattern ="[\\ ]$", replacement = "", x = percapita$GeoName)

#employment data
myfile3 <- "/Users/seyi/CIS 663/MyProj/employment.csv"
employment <- read.csv(file = myfile3, header = FALSE, sep = ',')
employment <- employment[4:55,]
colnames(employment) <- employment[1,]
employment <- employment[-1,]
rownames(employment) <- NULL
colnames(employment)[3] <- "emp_21"
colnames(employment)[4] <- "emp_22"
employment <- subset(employment, select = -c(emp_22))

#personal income data
myfile4 <- "/Users/seyi/CIS 663/MyProj/personalincome.csv"
personalincome <- read.csv(file = myfile4, header = FALSE, sep = ',')
personalincome <- personalincome[4:55,]
colnames(personalincome) <- personalincome[1,]
personalincome <- personalincome[-1,]
rownames(personalincome) <- NULL
colnames(personalincome)[3] <- "perinc_21"
colnames(personalincome)[4] <- "perinc_22"
personalincome <- subset(personalincome, select = -c(perinc_22))
personalincome$GeoName <- gsub(pattern ="[\\*]$", replacement = "", x = personalincome$GeoName)
personalincome$GeoName <- gsub(pattern ="[\\ ]$", replacement = "", x = personalincome$GeoName)


#Combine economy performance data frames into one
economy<-left_join(population, employment, by = c("GeoFips", "GeoName")) %>%
  left_join(., percapita, by = c("GeoFips","GeoName")) %>%
  left_join(., personalincome, by = c("GeoFips","GeoName"))

#Cleaning economy table
economy$GeoFips <- str_replace(economy$GeoFips, "0{3}$" ,"")

#cost of living
myfile5 <- "/Users/seyi/CIS 663/MyProj/Cost_of_living_Missouri_Economic_Research_and_Information_Center.csv"
cost <- read.csv(file = myfile5, header = TRUE, sep = ',')
cost <- cost[-c(53:56), ]
cost <- cost[-c(27), ]
rownames(cost) <- NULL
i_cost <- cost[,c(1:3)]

#state details
myfile6 <- "/Users/seyi/CIS 663/MyProj/us-state-ansi-fips.csv"
states <- read.csv(file = myfile6, header = TRUE, sep = ',')
states$stusps <- gsub(pattern ="\\s", replacement = "", x = states$stusps)
states$stusps <- sort(states$stusps, decreasing = FALSE)

#Stardand of living merged with state details
i_cost <- i_cost %>% arrange(State)
states <- states %>% arrange(stusps)
livingcost <- merge(states, i_cost, by.x = "stusps", by.y = "State")

#Merging economic performance and standard of living details
whole_tb <- merge(economy, livingcost, by.x = "GeoName", by.y = "stname")
whole_tb <- whole_tb[, c(2,1,9,12,11,3,4,5,6,7,8)]
colnames(whole_tb)[4] <- "col_index"
colnames(whole_tb)[5] <- "col_rank"


#Analysis

sum(economy$popu_diff <= 0)
sum(economy$popu_diff >= 0)

whole_tb[whole_tb$popu_diff < 0, ]
economy[economy$popu_diff > 0, ]

whole_tb %>% filter(popu_diff<0) %>% select(GeoName, col_index) %>% summarise(mean(col_index))
whole_tb %>% filter(popu_diff>0) %>% select(GeoName, col_index) %>% summarise(mean(col_index))

ggplot(data = whole_tb, aes(x= emp_21)) + geom_histogram()
ggplot(data = whole_tb, aes(x = perc_21)) + geom_histogram()
ggplot(data = whole_tb, aes(x = perinc_21)) + geom_histogram()
