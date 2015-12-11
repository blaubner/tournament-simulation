# Tournament simulation script
# Buchholz system evaluation
# Bastian Laubner, 2015-12-10


## debate model
# input variables
team_spread = 5  # std deviation for team strength, centered at 150 points
tournament_spread = sqrt(2) * team_spread  # std deviation of team strength distribution across tournament
nRooms = 100
nTeams = 4*nRooms
nRounds = 9


## team data -- drawn at random for each tournament later
#team_strengths = rnorm(nTeams, 150, tournament_spread)
#team_tab = data.frame(ID = 1:nTeams, strength = team_strengths)



# random_round gives the team points for the whole field simultaneously
random_round = function( team_strengths, draw ) {
  team_performance = rnorm(nTeams, team_strengths, team_spread)
  team_points = order( order( draw, team_performance ) ) - 4*(draw-1) - 1
  return(team_points)
}


### Round model
## Draw Round 1
# a draw is given by assigning a number to each team s.t. the teams with the same numbers debate
# against each other, i.e. each number appears exactly 4 times
draw1 = function() {
  # first round is drawn at random
  draw = sample( rep(1:nRooms, 4), 4*nRooms )
  return(draw)
}

## Draw Rounds >= 2
room_vec = rep( 1:nRooms, rep(4, nRooms) )  # gives 1 1 1 1 2 2 2 2 3 3 3 3 4 ...
draw2 = function( team_points ) {
  # later rounds are drawn based on power-pairing
  # here we just sort the teams and group them into contiguous groups of 4 (without regard for 
  # the details of pull-ups - this can be improved!)
  ranking = order(order(team_points, decreasing = TRUE))  # gives for each team the current tab position
  draw = room_vec[ranking]
  return(draw)
}

## Generic draw function - for disambiguation
get_draw = function( round_results ) {
  if (ncol(round_results) == 0) {
    return( draw1() )
  } else {
    return( draw2(rowSums(round_results)) )
  }
}

## Round simulation
run_tournament = function( team_strengths ) {
  round_results = data.frame()
  round_rooms = data.frame()  # this is for later retracing the tournament - can be left out
  for (round in 1:nRounds) {
    draw = get_draw( round_results )
    team_points = random_round(team_strengths, draw)
    
    round_results = data.frame(c( round_results, data.frame(team_points) ))
    colnames(round_results)[round] = paste0('Points_', round)
    
    round_rooms = data.frame(c( round_rooms, data.frame(draw) ))
    colnames(round_rooms)[round] = paste0('Room_', round)
  }
  #team_tab = data.frame(ID = 1:nTeams, strength = team_strengths)
  
  tab = cbind( data.frame(ID = 1:nTeams, strength = team_strengths), 
               round_rooms,
               round_results, 
               'Total_points' = rowSums(round_results),
               'Buchholz_points' = get_Buchholz_tab(round_rooms, round_results))
  return(tab)
}

random_tournament = function() {
  # draw random team strengths
  team_strengths = rnorm(nTeams, 150, tournament_spread)
  run_tournament(team_strengths)
}


## Buchholz points calculation
get_Buchholz_tab = function(round_rooms, round_results) {
  stopifnot( ncol(round_rooms) == ncol(round_results) )
  total_points = rowSums(round_results)
  buchholz_points = rep(0, length(total_points))
  for (round in 1:ncol(round_results)) {
    room_points = aggregate(total_points, by = list(round_rooms[,round]), FUN = "sum")
    buchholz_points = buchholz_points + room_points[round_rooms[,round], 2]
  }
  return(buchholz_points)
}


## simulation
nSims = 1e3
size_of_break = 32
library(parallel)
library(Kendall)

extract_stats = function( tab ) {
  ordered_tab = tab[order(tab$Total_points, tab$Buchholz_points, decreasing = TRUE), ]
  ordered_tab = data.frame(ordered_tab, Rank = 1:nTeams)
  stats = list()
  stats$tau_strength_rank = Kendall(ordered_tab$strength, -ordered_tab$Rank)$tau[1]
  stats$tau_strength_points = Kendall(ordered_tab$strength, ordered_tab$Total_points)$tau[1]
  stats$tau_strength_buchholz = Kendall(ordered_tab$strength, ordered_tab$Buchholz_points)$tau[1]
  clashes = aggregate( ID ~ Total_points + Buchholz_points, data = tab, FUN = "length" )
  assign("clashes", clashes, envir = globalenv())
  clashes_at_18 = clashes[clashes$Total_points == 18, ]
  stats$percent_rank_equality = (nrow(tab) - nrow(clashes)) / nrow(tab)
  stats$clashes_at_18 = sum(clashes_at_18$ID) - nrow(clashes_at_18) 
  stats$clash_at_break = ( (ordered_tab$Total_points[size_of_break] == ordered_tab$Total_points[size_of_break+1]) &&
                           (ordered_tab$Buchholz_points[size_of_break] == ordered_tab$Buchholz_points[size_of_break+1]) )
  return(stats)
}

closeAllConnections()
cl = makeCluster(3)
sim_stats = function() {
  simfn = function(x) {
    tab = random_tournament()
    return( extract_stats(tab) )
  }
  clusterExport(cl, list("simfn"), envir = environment())
  clusterExport(cl, as.list(ls(envir = globalenv())))
  clusterCall(cl, function() {library("Kendall")})
  sim_results = parLapply(cl, 1:nSims, simfn)
  return( sim_results )
}

sim_results = sim_stats()

extract_results = function(res) {
  statnames = names(res[[1]])
  statresults = list()
  for (name in statnames) {
    statvec = sapply(res, function(x) {x[[name]]})
    print(statvec[1:10])
    statresults[name] = mean(statvec)
  }
  return(statresults)
}

mean_results = extract_results(sim_results)


testfunctions = function() {
  x = seq(100,200,length=1000)
  y = dnorm(x, mean=150, sd = sqrt(2)*team_spread)
  plot(x, y, type = 'l')
  
  x = seq(-3,5,length=100)
  y = dnorm(x, mean = 1)
  plot(x, y, type = 'l')
  
  tab = random_tournament()
  ordered_tab = tab[order(tab$Total_points, tab$Buchholz_points, decreasing = TRUE), ]
  ordered_tab = data.frame(ordered_tab, Rank = 1:nTeams)
  
}

# NOT USED
# random_debate gives a random team ranking (3, 2, 1, 0 points) based on the input teams' strengths
random_debate = function( team_strengths ) {
  random_strengths = rnorm(4, team_strengths, team_spread)
  team_points = 4 - order(order(random_strengths, decreasing = TRUE))
  return(team_points)
}

# NOT USED
# test_duel: get percentages of wins and losses for two teams pitched against each other
test_duel = function( strength1, strength2 ) {
  p1 = pnorm(0, strength2-strength1, sqrt(2)*team_spread)
  c(p1, 1-p1)
}
