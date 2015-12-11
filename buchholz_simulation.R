# Tournament simulation script
# Buchholz system evaluation
# Bastian Laubner, 2015-12-10


## debate model
# constants
team_spread = 10  # std deviation for team strength, centered at 150 points

# random_debate gives a random team ranking (3, 2, 1, 0 points) based on the input teams' strengths
random_debate = function( team_strengths ) {
  random_strengths = rnorm(4, team_strengths, team_spread)
  team_points = 4 - order(order(random_strengths, decreasing = TRUE))
  return(team_points)
}

# test_duel: get percentages of wins and losses for two teams pitched against each other
test_duel = function( strength1, strength2 ) {
  p1 = pnorm(0, strength2-strength1, sqrt(2)*team_spread)
  c(p1, 1-p1)
}



x = seq(0,300,length=1000)
y = dnorm(x, mean=150, sd = 10)
plot(x, y, type = 'l')

x = seq(-3,5,length=100)
y = dnorm(x, mean = 1)
plot(x, y, type = 'l')