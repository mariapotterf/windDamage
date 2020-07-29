
# Create simple function to handle multiple decisions
# Learn R: Create control structure
# https://www.dataquest.io/blog/control-structures-in-r-using-loops-and-if-else-statements/

team_A <- 1
team_B <- 3

if (team_A > team_B){
  print ("Team A wins!")
} else {
  print ("Team B wins")
}


matches <- list(c(2,1),c(5,2),c(6,3))

teams = c("team_A", "team_B")
total_goals <- c()

for (value in teams){
  print(value)
}




for (match in matches){
  print(match)
  print(sum(match))
}


for (match in matches){
  total_goals <- c(total_goals, sum(match))
}

matches <- list(c(1,2),c(5,2),c(6,3))

for (match in matches) {
  if (match[1] > match[2]) {
    print("Win")
    break                    # Break the code 
  } else {
    print ("Lose")
  }
}
