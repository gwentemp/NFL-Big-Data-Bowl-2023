#Gwen Templeton's NFL Big Data Bowl
# Thanks to help of my mentors Ethan Douglas & Matthew Reyers

# Exploring spin moves by pass rushers
# Limitations 
#label of what makes a spin move maybe off and misclassify player movement
# lack of film to confirm spin move occurrence
# players with with loop around edge rushes or spins caused by running into a teammate
# QB roll out plays
#
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
week<-seq(1,8)
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
newtestsnap<- test_lag %>% mutate(changeo=abs(o-lag_frame),changedir=dir-lag_dir)
# creating spinmove category classification by using change in o and dir 
# orientation is measured in degrees and is a circle so 10 degrees and 360 degrees is only 20 degree change.So that is kept in mind by max change in orientation of 300. In order for a player to do a complete spin a player shoulders and body
#will change orientation at least over 250 degrees to start making its way around a circle. So we will go little over with 260 in order to make sure spin is done, 180 could be an indicator but also doesn't fully tell if a player went all away with their spin or if they stopped. 
#In order to try to take in account defender running past a QB and turning back into them like a spin, a limit is placed on direction to less then 90 to help combate those incidents
newtestsnap$spinmove<-ifelse(newtestsnap$changeo  >= 260 & newtestsnap$changeo <=300 & newtestsnap$changedir <= 90,1,0)

#filtering our data to just the passrushers
testsnap<- newtestsnap %>% select(gameId, playId, frameId, time,team, nflId,pff_positionLinedUp,displayName, quarter, down, yardsToGo, preSnapHomeScore,preSnapVisitorScore,offenseFormation, personnelO, personnelD,pff_passCoverage, pff_playAction, passResult, playDirection,event, fm5, snap,lag_frame,o,changeo,spinmove,x, y, dis, dir,changedir, a,pff_sack, pff_hit, pff_hurry) %>%
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
# [1] 0.1466667
not<-testsnap %>%
  filter(spinmove==0) %>%
  mutate(score= ifelse(pff_hit==1,1,0) +
           ifelse(pff_hurry==1, 1,0) +
           ifelse(pff_sack==1,1,0))

sum((not$score)/nrow(not))

#0.1345395 

#from here we can see spin moves have slightly more effective in causing pressure then other moves.

# spin move rate by player 
pla<-testsnap %>%
  group_by(displayName) %>%
  summarize(spin_rate= mean(spinmove)) %>%
arrange(desc(spin_rate))
 view(pla)

#amount of spin moves by player
playerspin<-testsnap %>% 
  group_by(displayName) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  head(20)


# top 20 players in spin moves week 1 through 8 2021 season

# 1 Yannick Ngakoue     38
#2 Azeez Ojulari       36
#3 Trey Hendrickson    34
#4 Emmanuel Ogbah      33
#5 Cameron Jordan      32
#6 Sam Hubbard         27
#7 T.J. Watt           27
#8 Jadeveon Clowney    26
#9 Danielle Hunter     25
#10 Josh Sweat          25
#11 Charles Harris      24
#12 Whitney Mercilus    24
#13 Chandler Jones      23
#14 Dante Fowler        23
#15 Montez Sweat        23
#16 Rasheem Green       23
#17 Shaquil Barrett     23
#18 Tyquan Lewis        23
#19 Myles Garrett       22
#20 Brian Burns         21
view(playerspin)

  

#total score pressure by player
playerspressurescore<- testsnap %>%
  group_by(displayName) %>%
  filter(spinmove==1) %>%
  summarize(total_score = sum(total_pressure)) %>%
arrange(desc(total_score)) %>%
  head(20)

# Top 20 players in applying pressure from spin moves
#1 Charles Harris               10
#2 Trey Hendrickson              8
#3 Yannick Ngakoue               7
#4 Cameron Jordan                6
#5 Kenny Clark                   6
#6 Marcus Davenport              6
#7 Azeez Ojulari                 5
#8 Everson Griffen               5
#9 Haason Reddick                5
#10 Jadeveon Clowney             5
#11 John Franklin-Myers          5
#12 Jonathan Garvin              5
#13 Josh Allen                   5
##14 Khalil Mack                 5
#15 Myles Garrett                5
#16 Rashan Gary                  5
#17 T.J. Watt                    5
#18 Terrell Lewis                5
#19 Bryce Huff                   4
#20 Bud Dupree                   4
view(playerspressurescore)
#player spin move pressure effectiveness

effectivespin<- testsnap %>% 
  group_by(displayName)%>%
  filter(spinmove==1) %>%
  summarize(total_score=sum(sum(total_pressure)/sum(spinmove))) %>%
  arrange(desc(total_score))

view(effectivespin)

#Team count spin moves
teamspin<-testsnap %>% 
  group_by(team) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(teamspin)

# graph for spin moves by team
testsnap %>%
  
  group_by(team) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(team), count))+
  
  geom_bar(fill = "red", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Team total spin moves") +
  
  xlab("Team") +
  
  ggtitle("Spin moves by team")




#total team pressures from spin moves
teampressurescore<- testsnap %>%
  group_by(team) %>%
  filter(spinmove==1) %>%
  summarize(total_score = sum(total_pressure))
view(teampressurescore)

testsnap %>%
  
  group_by(team) %>%
  filter(spinmove==1) %>%

  summarize(total_score = sum(total_pressure))  %>%
  
  ggplot(aes(as.character(team), total_score))+
  
  geom_bar(fill = "lightblue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Team total pressure scores") +
  
  xlab("Team") +
  
  ggtitle("Spin move pressure by team")
#team spin move pressure effectiveness
effectivespinteam<- testsnap %>% 
  group_by(team)%>%
  filter(spinmove==1) %>%
  summarize(total_score=sum(sum(total_pressure)/sum(spinmove)))
view(effectivespinteam)

#Pressure = Down + Yards To Go + Spin_Move
model1 <- glm(total_pressure ~ spinmove +down + yardsToGo+scorediff,data=testsnap, family="binomial")
summary(model1)
#results
# as the down increases from 1 to 4 the odds of pressure increases
# spin move is not a significant variable for increasing chance of pressure

model2<- glm(total_pressure ~ spinmove +down + yardsToGo+ pff_positionLinedUp+pff_passCoverage,data=testsnap, family="binomial")
cor.test(testsnap$down,testsnap$yardsToGo)

#results 
summary(model2)
# as the down increases from 1 to 4 the odds of pressure increases
# spin move is not a significant variable for increasing chance of pressure

#spin moves by pass coverage

spindcov <- testsnap %>% 
  group_by(pff_passCoverage) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(spindcov)


testsnap %>%
  
  group_by(pff_passCoverage) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(pff_passCoverage), count))+
  
  geom_bar(fill = "blue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves") +
  
  xlab("pass coverage") +
  ggtitle("Spin moves by pass coverage")

# coverage 3 and 1 the most common for spin moves. 


#spin moves by offensive formation
oplaytype <- testsnap %>% 
  group_by(offenseFormation) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(oplaytype)

#spin moves by offensive formation

testsnap %>%
  
  group_by(offenseFormation) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(offenseFormation), count))+
  
  geom_bar(fill = "lightblue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves per offensive formation") +
  
  xlab("Offensive formation") +
  
  ggtitle("Spin moves by offense formation")

# results shotgun formations tend to lead to more spin moves then other formations as well as being the most common offense formation 
# This may be because shotgun tends to be more passing and QB being further away creates more time for spin moves to develop.


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

testsnap %>%
  
  group_by(down) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(down), count))+
  
  geom_bar(fill = "lightblue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves") +
  
  xlab("Down") +
  
  ggtitle("Spin moves by Down")

# first down the most common down for spinmove in passing situations while second and third are close to same amount. 

# spin moves by score #0=tie 1= visiting team is winning, 2=home team is winning
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


testsnap %>%
  
  group_by(down) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(down), count))+
  
  geom_bar(fill = "lightblue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves") +
  
  xlab("Down") +
  
  ggtitle("Spin moves by Down")

# first down the most common down for spinmove in passing situations while second and third are close to same amount. 


# Pass Results
testsnap %>%
  
  group_by(passResult) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(passResult), count))+
  
  geom_bar(fill = "green", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves") +
  
  xlab("passResult") +
  
  ggtitle("Spin moves by pass result")

passresult<-testsnap %>%
  
  group_by(passResult) %>%
  filter(spinmove==1) %>%
  summarize(count=n())
view(passresult)

#pass results combined with spin moves that though there is more completed passes then other outcomes, 45% passes were completed vs 55% other 4 more defensive positive outcomes

25 +66 + 267+ 277
#[1] 1435
 1190/(1190+1435)
#[1] 0.4533333

 
# Spin move frequency by player position
testsnap %>%
  
  group_by(pff_positionLinedUp) %>%
  filter(spinmove==1) %>%
  summarize(count=n()) %>%
  
  ggplot(aes(as.character(pff_positionLinedUp), count))+
  
  geom_bar(fill = "lightblue", stat = "identity") +
  
  theme_bw()  +
  
  theme(text = element_text(size = 8)) +
  
  ylab("Total spin moves") +
  
  xlab("player position") +
  ggtitle("Spin moves by player position")








#counting to make sure all available are accounted for 
spincount<-testsnap %>%
  filter(spinmove==1)
nrow(spincount)
notspin<-testsnap %>%
  filter(spinmove==0) 
sum(is.na(testsnap$spinmove))

