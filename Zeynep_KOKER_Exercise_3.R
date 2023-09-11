
Dice_function = function(number_of_games,most_times_value,number_of_times_get_spesific_number_to_win){
  probability_of_spesific_number = 1/(6^number_of_times_get_spesific_number_to_win)
  game= rbinom(number_of_games,most_times_value,probability_of_spesific_number)
  results_game = matrix(0,nrow=number_of_games, ncol=1)
  cumulative_sum_game = matrix(0,nrow=number_of_games, ncol=1)
  cumulative_sum_game_value = 0
  probability_game = matrix(0,nrow=number_of_games, ncol=2)
  for (i in 1:number_of_games){
    probability_game[i,1] = i
    if (game[i] >0){
      results_game[i]=1
      cumulative_sum_game_value = cumulative_sum_game_value + 1
      probability_game[i,2] = cumulative_sum_game_value/i
    }
    else {
      results_game[i]=0
      probability_game[i,2] = cumulative_sum_game_value/i
    }
  }
  return(probability_game)
}

par(mfrow=c(2,1))
for (i in c(450,10000)){
  # Calling a Dice function for two types of game
  probability_game_1 = Dice_function(i,4,1)
  probability_game_2 = Dice_function(i,24,2)
  plot(probability_game_1[,1],probability_game_1[,2], type="l", lwd=2, xlab="Number of games", ylab="Proportion of wins", col="#0099FF", xlim=c(0,length(probability_game_1[,1])), ylim=c(0,1))
  lines(probability_game_2[,1],probability_game_2[,2], type="l", lwd=2, xlab="Number of games", ylab="Proportion of wins", col="#FF0000", xlim=c(0,length(probability_game_2[,1])), ylim=c(0,1))
  abline(h=0.5, col="black", lwd=1, lty=2)
  legend("topright","inset", legend=c("One die","Two dice"), col=c("#0099FF","#FF0000"), lty=1:1, cex=0.8, bty = "n")
}




