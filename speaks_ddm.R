library(plyr)
## Read in tournament result data
ddmdata = read.csv2("DDM2014_results.csv")

## data pre-processing
round_avg_vec = as.numeric(gsub(",", ".", substr(ddmdata$Schnitt, 1, 4)))
round_no_vec = as.numeric(substr(ddmdata$Debatte,2,2))
rounds = unique(round_no_vec)
room_no_vec = as.numeric(substr(ddmdata$Debatte,6,8))


## extract vectors of positions in debates & speaks
team_positions = c("ER", "EO", "SR", "SO")
pos_vecs = NULL
speaks_vecs = NULL
teams_vecs = NULL
for (pos in team_positions) {
  tmp_vec = regexpr("#([[:digit:]]{1})",ddmdata[[pos]])
  print(tmp_vec)
  pos_vecs[[pos]] = as.numeric(substr(ddmdata[[pos]], tmp_vec+1, tmp_vec+1))
  teams_vecs[[pos]] = substr( ddmdata[[pos]], 1, tmp_vec-3)
  speaks_vecs[[pos]] = as.numeric( substr( 
    unlist(lapply( strsplit(
      as.character(ddmdata[[pos]])," ") , tail,1)), 1, 3))
}

pos_matrix = matrix(unlist(pos_vecs), 
                    length(team_positions), length(pos_vecs[[1]]), T)


# Team statistics
team_names = unique(unlist(teams_vecs))
nTeams = length(team_names)
nRounds = length(unique(round_no_vec))
#team_rdpts_matrix = matrix( rep(-1, nTeams*nRounds), nTeams, nRounds )

teams = unlist(teams_vecs) # long of all teams
points = 4 - unlist(pos_vecs) # long of all teampoints
speaks = unlist(speaks_vecs) # long of all speaks
rounds = rep(round_no_vec,4)
rooms = rep(room_no_vec,4)

data = data.frame(speaks, teams, rounds, rooms, points)[order(rounds, teams),]

## Calculate Buchholz of other teams
# Calculate current team points of each team.
data$current_points = 0
for (round in 2:nRounds) {
  past_data = data[data$round < round,]
  current_points = aggregate(past_data$points, by = list(past_data$teams), FUN = "sum")
  data[data$round == round,]$current_points  = current_points[,2]
}
# Calculate current points of opposing teams
room_points = aggregate(current_points ~ rounds + rooms, data = data, FUN = "sum") 
room_points = rename(room_points, c("current_points"="room_points"))
data = merge(data, room_points)
data$opposing_points = (data$room_points - data$current_points)/ (data$rounds - 1) / 3
data$avg_points = data$current_points / (data$rounds - 1)
data$diff_points = data$avg_points - data$opposing_points
data$avg_room_points = (data$room_points)/ (data$rounds - 1) / 4
# Create data set without Springer
data_without_springer = data[data$teams != "Berliner Springer",]
data_without_springer = data_without_springer[data_without_springer$rounds != 1,]

## Run several regressions
ols = lm(speaks ~ opposing_points + avg_points, data=data)
ols = lm(speaks ~ diff_points, data=data)

ols = lm(speaks ~ opposing_points + avg_points, data=data_without_springer)
ols = lm(speaks ~ diff_points, data=data_without_springer)
ols = lm(speaks ~ opposing_points + avg_points + opposing_points*avg_points, data=data_without_springer)
ols = lm(speaks ~ diff_points + avg_points, data=data_without_springer)

ols = lm(points ~ opposing_points + teams, data=data_without_springer)
ols = lm(points ~ opposing_points + avg_points + teams, data=data_without_springer)
ols = lm(speaks ~ opposing_points + teams, data=data_without_springer)
ols = lm(speaks ~ opposing_points + avg_points + teams, data=data_without_springer)
ols = lm(speaks ~ avg_points + teams, data=data_without_springer)

ols = lm(speaks ~ avg_room_points + teams, data=data_without_springer)

# ToDo: Rounds as categorical variable??
ols = lm(speaks ~ opposing_points + avg_points + teams + rounds, data=data_without_springer)


summary(ols)


plot(data_without_springer$speaks, data_without_springer$avg_room_points)

# ToDo: First difference room points for each team, Clustered SE?