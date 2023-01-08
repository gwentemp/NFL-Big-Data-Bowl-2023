#Gwen Templeton's NFL Big Data Bowl
#load in library
library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)
library(dplyr)
library(caret)
#bring in the data
pff <- read_csv("nfl-big-data-bowl-2023/pffScoutingData.csv")
players <- read_csv("nfl-big-data-bowl-2023/players.csv")
plays <- read_csv("nfl-big-data-bowl-2023/plays.csv")
week<-seq(1)
df_tracking<-data.frame()
for(w in week){
  df_tracking_temp <- read_csv(paste0("nfl-big-data-bowl-2023/tracking/week",w,".csv"), col_types = cols())
  df_tracking<-bind_rows(df_tracking_temp, df_tracking) }
df_readRPlays <- read_csv("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_2021.csv",col_types=cols_only(old_game_id='i', play_id ='i', xpass='d'))
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

# extracting what we need from the players data set 
players1<-players %>% 
  select(nflId, displayName)

#merging all the data together for analysis
totalt<-merge(plays,df_tracking,by=c("gameId","playId"))
totalf<- merge(totalt,pff,all.x=TRUE, by=c("gameId","playId","nflId"))
total_players<-merge(totalf,players1,all.x=TRUE, by="nflId")

#creating nappers which creates new columns of the current frameId -5 and snap column representing the 6th frame
#filter it out so we don't get presnap data
nappers <- total_players %>%
  mutate(fm5=frameId-5) %>% mutate(snap=6) %>%
  filter(fm5>=4)

# creating the ability to get the orientation and direction from 5 previous frames so we can do our comparison in player movement
test_lag<- nappers %>%
 group_by(gameId, playId,nflId) %>%
 mutate(lag_frame=lag(o,5),lag_dir=lag(dir,5))


# create new dataframe that has change in orientation and direction, which takes the current one and substract the o or dir from the 5th previous frame
newtestsnap<- test_lag %>% mutate(changeo=o-lag_frame,changedir=dir-lag_dir)
# creating spinmove category classification by using change in o and dir 
newtestsnap$spinmove<-ifelse(newtestsnap$changeo  >= 180 & newtestsnap$changedir < 180,1,0)
#filtering our data to just the passrushers
testsnap<- newtestsnap %>% select(gameId, playId, frameId, time,team, nflId,pff_positionLinedUp,displayName, quarter, down, yardsToGo, preSnapHomeScore,preSnapVisitorScore,offenseFormation, personnelO, personnelD,pff_passCoverage, pff_playAction, passResult, playDirection,event, fm5, snap,lag_frame,o,changeo,spinmove,x, y, dis, dir,changedir,pff_sack, pff_hit, pff_hurry) %>%
  filter(pff_positionLinedUp==c("RE", "REO","DLT","DRT", "ROLB", "LE", "LEO","LOLB"))
#adding a pressure summary score adding up the pff data by row
testsnap<-testsnap %>% 
  group_by(gameId, playId,nflId) %>%
  mutate(total_pressure= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))
#adding a column that gives score difference in the game. 
testsnap<-testsnap %>% 
  group_by(gameId, playId,nflId) %>%
  mutate(scorediff= ifelse(preSnapHomeScore > preSnapVisitorScore,2,0)+ifelse(preSnapHomeScore==preSnapVisitorScore,0,0)+ifelse(preSnapVisitorScore>preSnapHomeScore, 1, 0))


# spinrate pressure score vs other moves pressure score
spin<-testsnap %>%
  filter(spinmove==1) %>%
  mutate(score= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))

sum((spin$score)/nrow(spin))

not<-testsnap %>%
  filter(spinmove==0) %>%
  mutate(score= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))

sum((not$score)/nrow(not))

# spin move rate by player 
pla<-testsnap %>%
  group_by(displayName) %>%
  summarize(spin_rate= mean(spinmove))
 view(pla)

#amount of spin moves by player
playerspin<-testsnap %>% 
  group_by(displayName) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(playerspin)

# amount of other moves

playernotspin<-testsnap %>% 
  group_by(displayName) %>%
  filter(spinmove==0) %>%
  summarize(count=n())
view(playernotspin)



sum(playerspin/nrow(total_players))

#number of spin move pressure score by player
playscore <- testsnap %>% 
  group_by(displayName) %>%
  summarize(total_score=sum(score))
view(playscore)

#player spin move pressure effectivness

effectivespin<- testsnap %>% 
  group_by(displayName)%>%
  filter(spinmove==1) %>%
  summarize(total_score=sum(sum(total_pressure)/sum(spinmove)))

#Team count spin moves
teamspin<-testsnap %>% 
  group_by(team) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(teamspin)


#
spin<-testsnap %>%
  filter(spinmove==1) %>%
  mutate(score= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))

sum((spin$score)/nrow(spin))

not<-testsnap %>%
  filter(spinmove==0) %>%
  mutate(score= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))

sum((not$score)/nrow(not))

#total score pressure by player
playerspressurescore<- testplay %>%
  group_by(displayName) %>%
  summarize(total_score = sum(score))

#total team pressures from spin moves
teampressurescore<- testplay %>%
  group_by(team) %>%
  summarize(total_score = sum(score))

#total team pressures from non spinmoves
teamnonpressurescore<- not %>%
  group_by(team) %>%
  summarize(total_score = sum(score))


#Pressure = Down + Yards To Go + Spin_Move
test1 <- glm(total_pressure ~ spinmove +down + yardsToGo,data=testsnap, family="binomial")
test <- glm(total_pressure ~ spinmove +down + yardsToGo+ pff_positionLinedUp+pff_passCoverage,data=testsnap, family="binomial")
cor.test(testsnap$down,testsnap$yardsToGo)

split <- createDataPartition(testsnap$total_pressure, p = 0.7, list = FALSE)
train <-testsnap[split, ]
test <- testsnap[-split, ]
model<- glm(total_pressure ~ spinmove +down + yardsToGo ,data=train, family="binomial")
predictions <- predict(model, test, type = "response")
cats.pred = rep(1, dim(train)[1])
cats.pred[predictions > .5] = 0
table(cats.pred, train$total_pressure)
mean(cats.pred == train$total_pressure)
1-mean(cats.pred == train$total_pressure)
print(accuracy)
#results 
summary(test)
# as the down increases from 1 to 4 the odds of pressure increases
# as yards to go increases the odds of a pressure increases

#spin moves by pass coverage

spindcov <- testsnap %>% 
  group_by(pff_passCoverage) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(spindcov)


nospinpdcov <- testsnap %>% 
  group_by(pff_passCoverage) %>%
  filter(spinmove==0) %>%
  summarize(count=n())
view(nospinpdcov )

#spin moves by offesnive formation
oplaytype <- testsnap %>% 
  group_by(offenseFormation) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(oplaytype)
#spinmoves by down
spinbydown <- testsnap %>% 
  group_by(down) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(spinbydown)
#spinmoves by yards to go
spinbyytg <- testsnap %>% 
  group_by(yardsToGo) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(spinbyytg)

# spin moves by score
homescoretype <- testsnap %>% 
  group_by(scorediff) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(homescoretype)


#counting to make sure all available are accounted for 
spincount<-testsnap %>%
  filter(spinmove==1)
nrow(spincount)
notspin<-testsnap %>%
  filter(spinmove==0) 
sum(is.na(testsnap$spinmove))


