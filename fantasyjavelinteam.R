#Ethan McDonald
#Scouting and Drafting a fantasy athletics (Womens Javelin) team


# Load the tidyverse package
library(tidyverse)

# Import data and select event
data <- read_csv("datasets/athletics.csv")

# Selecting the results of interest: Womens javelin
javelin <- data %>%
  filter(Male_Female == "Female" & Event == "Javelin") %>%
  select(-Male_Female, -Event) 

# Snapshot of your event 
summary(javelin)
head(javelin)




# Assign the tidy data to javelin_long, wide to long
javelin_long <- javelin %>%
  gather(Flight1:Flight6, key = "Flight", value="Distance")

# Make Flight a numeric
javelin_long$Flight = as.numeric(gsub("Flight", "", javelin_long$Flight))

# Examine the first 6 rows
head(javelin_long)




#Finding number of successful throws, sum of total distance and sd
javelin_totals <- javelin_long %>% 
  filter(Distance > 0) %>%
  group_by(Athlete, EventID) %>%
  summarize(TotalDistance = sum(Distance), StandardDev = round(sd(Distance),3), Success = n())

javelin_totals[60:70,]



#Finding clutch performers, who does best in different situations 
javelin <- javelin %>%
  mutate(early = Flight1+Flight2+Flight3, late = Flight4+Flight5+Flight6, diff = late - early) 

tail(javelin, 10)



#Join the two dataframes together and select important columns
javelin_totals <- javelin_totals %>%
  left_join(javelin, by=c("EventID", "Athlete")) %>%
  select(1, 3:5, 14)

head(javelin_totals, 10)



#Normalize the data 
norm <- function(result) {
  (result - min(result)) / (max(result) - min(result))
}
aggstats <- c("TotalDistance", "StandardDev", "Success", "diff")
javelin_norm <- javelin_totals %>%
  ungroup() %>%
  mutate_at(aggstats, norm) %>%
  group_by(Athlete) %>%
  summarize_all(mean)
head(javelin_norm)



#Apply a weight to the different variables based on importance
#Sort them by totalscore and select the top 5 for your team
weights <- c(2.1, 4.9, .5, 2.5)
javelin_team <- javelin_norm %>%
  mutate(TotalScore = TotalDistance * weights[1] + StandardDev*weights[2] + Success*weights[3] + diff*weights[4]) %>%
  arrange(desc(TotalScore)) %>%
  slice(1:5) %>%
  select(Athlete, TotalScore)

javelin_team



#Getting to know your selected players
#Selecting your players from javelin totals data set
team_stats <- javelin_totals %>% 
  filter(Athlete %in% javelin_team$Athlete) %>% 
  summarize_all(mean) 
team_stats
#Find max and mean of each numeric column in javelin totals using sapply, do.call on cbind combines max and avg lists
pool_stats <- data.frame(do.call('cbind', sapply(javelin_totals, function(x) if(is.numeric(x)) c(max(x), mean(x))))) 
pool_stats$MaxAve <- c("Maximum", "Average") #provides the maximum and average for each statistic for the whole dataset to compare to the selected team
pool_stats <- pool_stats %>%
  gather(key="Statistic", value="Aggregate", -MaxAve)



#Present findings
p <- team_stats %>%
  gather(key="Statistic", value="Aggregate", -Athlete) %>%
  ggplot(aes(x=Athlete, y=Aggregate, fill=Athlete)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(vars(Statistic), scales="free_y") +
  geom_hline(data=pool_stats, aes(yintercept=Aggregate, group=Statistic, color=MaxAve), size=1) +
  labs(title="AnyTown Athletic Club: Women's Javelin", color="Athlete pool maximum / average") +
  scale_fill_hue(l=70) +
  scale_color_hue(l=20) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

p


#Put the team to the test
home <- c(1,3,5)  #select our players
away <- sample(1:nrow(javelin_totals), 3, replace=FALSE) #randomly choose 3 opponents

HomeTeam <- round(sum(team_stats$TotalDistance[home]),2)
AwayTeam <- round(sum(javelin_totals$TotalDistance[away]),2)

print(paste0("Javelin match, Final Score: ", HomeTeam, " - ", AwayTeam))
ifelse(HomeTeam > AwayTeam, print("Win!"), print("Sometimes you just have to take the L."))