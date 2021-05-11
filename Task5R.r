library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
library(corrr)
library(treemap)
library(treemapify)
library(corrplot)


matches <- read.csv("C:\\Users\\ADMIN\\OneDrive\\Desktop\\Internship Tasks\\matches.csv",stringsAsFactors = FALSE)
head(matches)
tail(matches)

deliveries <- read.csv("C:\\Users\\ADMIN\\OneDrive\\Desktop\\Internship Tasks\\deliveries.csv",stringsAsFactors = FALSE)
head(deliveries)
tail(deliveries)

summary(matches)
summary(deliveries)

matches_per_season <- table(matches$season)
matches_per_season
barplot(matches_per_season,main = "Total matches played each Season", xlab = 'Season' , ylab = 'No. of Matches', col = 'midnightblue',las=2, ylim = c(0,80))

matches_by_venue = matches %>%
                   group_by(venue) %>%
                   summarize(Total_matches = n()) %>%
                   arrange(desc(Total_matches))
matches_by_venue

ggplot(matches_by_venue,aes(Total_matches, venue, colour = Total_matches)) + geom_count() + 
ggtitle(" No. of Matches played in Stadium")


wins_by_team <- matches %>%
                group_by(winner) %>%
                summarize(Total_wins = n()) %>%
                arrange(desc(Total_wins))
wins_by_team

 ggplot(wins_by_team , aes(x=winner,y=Total_wins,fill=winner)) +
         geom_col()+
          ggtitle("Teams with total number of wins") + 
          coord_flip() + 
        theme(legend.position = "None", axis.text.y = element_text(size=6)) +
        geom_text(aes(label = Total_wins), hjust = 1.25)

wins_by_max_runs <- matches %>% 
                select(winner, win_by_runs) %>%
                group_by(winner) %>%
                summarize(Total_runs = sum(win_by_runs)) %>%
                arrange(desc(Total_runs))
wins_by_max_runs

 ggplot(wins_by_max_runs , aes(x=reorder(winner,-Total_runs),y =Total_runs ,fill= winner)) +
         geom_col()+
          ggtitle("Teams won by maximum runs") + 
          coord_flip() + 
        theme(legend.position = "None", axis.text.y = element_text(size=6)) +
        geom_text(aes(label = Total_runs), hjust = 1.25)

wins_by_max_wickets <- matches %>% 
                select(winner, win_by_wickets) %>%
                group_by(winner) %>%
                summarize(Total_wickets = sum(win_by_wickets)) %>%
                arrange(desc(Total_wickets))
wins_by_max_wickets

 ggplot(wins_by_max_wickets , aes(x=winner,y=Total_wickets ,fill= winner)) +
         geom_col()+
          ggtitle("Teams won by maximum wickets") + 
          coord_flip() + 
        theme(legend.position = "None", axis.text.y = element_text(size=6)) +
        geom_text(aes(label = Total_wickets), hjust = 1.25)

batsman_max_runs_per_season = deliveries %>%
                                left_join(matches , by = c("match_id"="id")) %>%
                                group_by(season , batsman) %>%
                                summarize(total_runs= n()) %>% 
                                arrange(season , desc(total_runs)) %>%
                                filter(total_runs == max(total_runs))
batsman_max_runs_per_season

batsman_with_most_runs <- deliveries %>% 
                select(batsman, batsman_runs) %>%
                group_by(batsman) %>%
                summarize(Total_runs = sum(batsman_runs)) %>%
                arrange(desc(Total_runs))
batsman_with_most_runs



bowler_max_wickets_per_season = deliveries %>%
                                left_join(matches , by = c("match_id"="id")) %>%
                                filter(dismissal_kind != "run out") %>%
                                group_by(season , bowler) %>%
                                summarize(wickets = n()) %>% 
                                arrange(season , desc(wickets)) %>%
                                filter(wickets == max(wickets))
bowler_max_wickets_per_season

bowlers_with_most_wickets <- deliveries %>%
                             filter(dismissal_kind != "run out") %>%
                                group_by( bowler) %>%
                                summarize(wickets = n()) %>% 
                                arrange( desc(wickets))
                                
bowlers_with_most_wickets 

#Ho: Toss winning has an impact on winning the game.
y=0
n=0

for(i in seq(1, nrow(matches)))
{
  if(matches$toss_winner[i] == matches$winner[i])
    y=y+1
  else
    n=n+1
}
if(y>= n)
{
  print(paste("Yes,Toss-winning has an impact of winning a game"))
  print(paste("Matches won by toss winners are:",y, "& Total matches:", nrow(matches)))
}
winning = c(y,n)
teams = c("toss_win&game_win", "toss_win&game_lost")
df = data.frame(teams,winning,stringsAsFactors = FALSE)

ggplot(df)+geom_bar(aes(teams,winning,fill=teams),stat = "identity")

# Ho: Batting first helps to win the match.

toss=matches[matches$toss_decision=="bat",]
bf=0
bl=0
i=0
for(i in seq(1,nrow(toss)))
{
  if(as.factor(toss$toss_winner[i]==as.character(toss$winner[i])))
  {
    bf=bf+1
  }
  else
  {
    bl=bl+1
  }
}

toss1=matches[matches$toss_decision=="field",]
fw=0
fl=0
j=0
for(j in seq(1,nrow(toss1)))
{
  if(as.factor(toss1$toss_winner[j]==as.character(toss1$winner[j])))
  {
    fw=fw+1
  }
  else
  {
    fl=fl+1
  }
}
toss_decision = data.frame("Bat first or second"=c("Batting first","Fielding first"),"count"=c(bf,fw))
tibble(toss_decision)

matches%>%filter(city=="Mumbai"|city=="Bangalore"|city=="Delhi"|city=="kolkata"|city=="jaipur"|city=="Hydrabad"|city=="Chandigarh"|city=="pune")%>% ggplot()+
  geom_bar(aes(x=toss_decision,fill=toss_winner))+facet_wrap(~city)

matches %>%
  select(season, id, winner)%>%
  group_by(season)%>%
  slice(which.max(id))%>%
  select(season,winner)

ggplot(matches,aes(win_by_runs, venue, colour = winner)) + geom_count() + 
ggtitle("Best Stadium for winning by runs")


ggplot(matches,aes(win_by_runs, winner, colour = winner)) + geom_point() + 
ggtitle("Best Defending Team") 

deliveries %>% 
  group_by(batsman) %>%
  filter(batsman_runs == 6) %>%
  dplyr:: summarize(sixes = n()) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(batsman, sixes), y = sixes, fill= batsman))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("players")+
  ggtitle("Top 10 batsman with highest number of 6's in ipl")+
geom_text(aes(label = sixes), hjust = 1.25)

matches%>%
  group_by(player_of_match)%>%
  dplyr::summarise(awards =n())%>%
  top_n(10)%>%
  ggplot(aes(x = reorder(player_of_match, awards), y = awards ,fill= player_of_match))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("players") +
  geom_text(aes(label = awards), hjust = 1.25)
