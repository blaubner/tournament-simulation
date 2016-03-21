# Tournament simulation script
# Buchholz system evaluation
# Bastian Laubner, 2015-12-10


### HOW TO USE
# Set the input variables at the top and source the file. It is important you source the file again after changing
# any input variables.
# The function test_function at the end has some lines of code that make sense to execute manually afterwards

library(Kendall)
library(parallel)
closeAllConnections()
cl = makeCluster(3)  # this speeds up the repeated simulation of tournaments

# input variables
team_spread = 3  # std deviation for team strength, centered at 150 points - 4.79 is the mean of stdevs of team speaks at DDM 2014
tournament_spread = 2*team_spread  # std deviation of team strength distribution across tournament - 4.72 is the stdev of mean team speaks at DDM 2014
nRooms = 10  # DDM 2014: 22
nRounds = 5  # DDM 2014: 7

nSims = 1e3  # number of tournaments to simulate
size_of_break = 8  # DDM 2014: 16

# automatically set - don't edit
nTeams = 4*nRooms



### Debate Round Model
# random_round gives the team points for the whole field simultaneously
# team_strengths: vector of team's average strengths (in order of team IDs)
# draw: vector of rooms that teams are in (4 teams per room)
# returns the points made by each team in this round (3, 2, 1, or 0)
random_round = function( team_strengths, draw ) {
  team_performance = rnorm(nTeams, team_strengths, team_spread)  # get random round performance for each team based on their individual strength
  team_points = order( order( draw, team_performance ) ) - 4*(draw-1) - 1  # calculates the team points for each team in its room
  return(team_points)
}


### Round Draw Model
## Draw Round 1
# a draw is given by assigning a number to each team s.t. the teams with the same numbers debate
# against each other, i.e. each number appears exactly 4 times
draw1 = function() {
  # first round is drawn at random
  draw = sample( rep(1:nRooms, 4), 4*nRooms )
  return(draw)
}

## Draw Rounds >= 2
# later rounds are drawn based on power-pairing
room_vec = rep( 1:nRooms, rep(4, nRooms) )  # gives 1 1 1 1 2 2 2 2 3 3 3 3 4 ...
draw2 = function( round_results, round_rooms ) {
  team_points = rowSums(round_results)
  
  # here we just sort the teams and group them into contiguous groups of 4 (without regard for 
  # the details of pull-ups - this can be improved!)
  #ranking = order(order(team_points, decreasing = TRUE))  # gives for each team the current tab position
  
  # here's the version with pull-ups (based on partial Buchholz points)
  partial_Buchholz_points = get_Buchholz_tab(round_rooms, round_results)
  ranking = order(order(team_points, partial_Buchholz_points, decreasing = TRUE))  # gives for each team the current tab position
  
  draw = room_vec[ranking]
  return(draw)
}

## Generic draw function - for disambiguation
get_draw = function( round_results, round_rooms ) {
  if (ncol(round_results) == 0) {
    return( draw1() )
  } else {
    return( draw2(round_results, round_rooms) )
  }
}

### Tournament Simulation
run_tournament = function( team_strengths ) {
  # result data frames
  round_results = data.frame()  # the team points for each team in each round
  round_rooms = data.frame()  # the rooms that each team debated in for each round
  
  for (round in 1:nRounds) {
    draw = get_draw( round_results, round_rooms )
    team_points = random_round(team_strengths, draw)
    
    # save results of round
    round_results = data.frame(c( round_results, data.frame(team_points) ))
    colnames(round_results)[round] = paste0('Points_', round)
    
    round_rooms = data.frame(c( round_rooms, data.frame(draw) ))
    colnames(round_rooms)[round] = paste0('Room_', round)
  }
  
  # put all results together incl. Buchholz points
  tab = cbind( data.frame(ID = 1:nTeams, strength = team_strengths), 
               round_rooms,
               round_results, 
               'Total_points' = rowSums(round_results),
               'Buchholz_points' = get_Buchholz_tab(round_rooms, round_results))
  return(tab)
}

# draws team strengths at random and then simulates the tournament
random_tournament = function() {
  # draw random team strengths
  team_strengths = rnorm(nTeams, 150, tournament_spread)
  return( run_tournament(team_strengths) )
}


## Buchholz points calculation
get_Buchholz_tab = function(round_rooms, round_results) {
  stopifnot( ncol(round_rooms) == ncol(round_results) )  # sanity check
  total_points = rowSums(round_results)
  buchholz_points = rep(0, length(total_points))
  for (round in 1:ncol(round_results)) {
    room_points = aggregate(total_points, by = list(round_rooms[,round]), FUN = "sum")  # sum of final team points for each room
    buchholz_points = buchholz_points + room_points[round_rooms[,round], 2]  # add room points for each team
  }
  return(buchholz_points)
}


### Simulation of many tournaments
## Statistics
# Kendall's tau is a measure of discriminatory power of the tournament system (i.e. how good the ranks/team points/
# Buchholz points are at ranking the teams in the order of their pre-defined strength)
# clashes_at_18 only makes sense for old WUDC tournaments (i.e. 9 rounds, 32 teams break)
# percent_rank_equality measures how often teams are not distinguished in the tab (i.e. team points and Buchholz points are equal)
# n_top_teams_in_break measures how many of the top teams make the break, where the top teams are those (size-of_break/2) teams with the largest pre-defined strengths
extract_stats = function( tab ) {
  ordered_tab = tab[order(tab$Total_points, tab$Buchholz_points, decreasing = TRUE), ]
  ordered_tab = data.frame(ordered_tab, Rank = 1:nTeams)
  stats = list()
  stats$tau_strength_rank = Kendall(ordered_tab$strength, -ordered_tab$Rank)$tau[1]
  stats$tau_strength_points = Kendall(ordered_tab$strength, ordered_tab$Total_points)$tau[1]
  stats$tau_strength_buchholz = Kendall(ordered_tab$strength, ordered_tab$Buchholz_points)$tau[1]
  clashes = aggregate( ID ~ Total_points + Buchholz_points, data = tab, FUN = "length" )
  clashes_at_18 = clashes[clashes$Total_points == 18, ]
  stats$percent_rank_equality = (nrow(tab) - nrow(clashes)) / nrow(tab)
  stats$clashes_at_18 = sum(clashes_at_18$ID) - nrow(clashes_at_18) 
  stats$clash_at_break = ( (ordered_tab$Total_points[size_of_break] == ordered_tab$Total_points[size_of_break+1]) &&
                           (ordered_tab$Buchholz_points[size_of_break] == ordered_tab$Buchholz_points[size_of_break+1]) )
  top_teams = order(tab$strength, decreasing = TRUE)[1:(size_of_break/2)]  # top teams correspond to half size of break
  stats$n_top_teams_in_break = sum(top_teams %in% ordered_tab$ID[1:size_of_break])
  stats$stdev_team_points = sd(tab$Total_points)
  return(stats)
}

## Simulation function sets up the parallel computation and returns the vector of stats
sim_stats = function() {
  simfn = function(...) {
    tab = random_tournament()
    return( extract_stats(tab) )
  }
  clusterExport(cl, list("simfn"), envir = environment())
  clusterExport(cl, as.list(ls(envir = globalenv())))
  clusterCall(cl, function() {library("Kendall")})
  sim_results = parLapply(cl, 1:nSims, simfn)  # paralle execution of nSims random tournament simulations
  return( sim_results )
}

## Calculates the mean of all statistics returned by the simulation
extract_results = function(res) {
  statnames = names(res[[1]])
  statresults = list()
  for (name in statnames) {
    statvec = sapply(res, function(x) {x[[name]]})
    statresults[name] = mean(statvec)
  }
  return(statresults)
}


### Commands to run
testfunctions = function() {
  # plot team strength distribution function
  x = seq(120,180,length=1000)
  y = dnorm(x, mean=150, sd = tournament_spread)
  plot(x, y, type = 'l')
  
  # run a random tournament once
  tab = random_tournament()
  ordered_tab = tab[order(tab$Total_points, tab$Buchholz_points, decreasing = TRUE), ]
  ordered_tab = data.frame(ordered_tab, Rank = 1:nTeams)
  extract_stats(tab)
  
  # plot the bucket sizes for each amount of team points in simulated tab
  simtab_teampoints_dist = aggregate(tab$ID, by = list(tab$Total_points), FUN = "length")
  plot(simtab_teampoints_dist)
  
  # RUN THE SIMULATION
  sim_results = sim_stats()
  mean_stats = extract_results(sim_results)
}


## NOT USED
# test_duel: get percentages of wins and losses for two teams pitched against each other
test_duel = function( strength1, strength2 ) {
  p1 = pnorm(0, strength2-strength1, sqrt(2)*team_spread)
  c(p1, 1-p1)
}
