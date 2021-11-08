
data<-read.csv('./shots_data.csv')
# find distance to hoop
data$dist<-sqrt(data$x^2 + data$y^2)
# classify each shot as a 2, c3, or nc3
data$shot<-factor(ifelse(data$y<=7.8 & abs(data$x)>22, "C3",
                         ifelse(data$y>7.8 & data$dist>23.75, "NC3",
                                "2PT")))

######## shot distribution

# get team A's distribution of shots
teamA<-data[data$team=="Team A",]
distA<-round(table(teamA$shot)/nrow(teamA), 3)

# get team B's distribution of shots
teamB<-data[data$team=="Team B",]
distB<-round(table(teamB$shot)/nrow(teamB), 3)

######## eFG%

# get the number of made and attempted shots for each team and shot zone
data_grouped<-data%>%
  group_by(team, shot)%>%
  summarize(made=sum(fgmade), attempted=n())%>%
  as.data.frame()

# calculate the eFG%
data_grouped$threes<-ifelse(data_grouped$shot!="2PT", 0, data_grouped$made)
data_grouped$efg<-round((data_grouped$made+0.5*(data_grouped$threes))/data_grouped$attempted, 3)