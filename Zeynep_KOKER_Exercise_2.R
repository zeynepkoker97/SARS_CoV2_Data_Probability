## Coin tossing
# Tails = less then 0.5
Number_of_tossing_function = function(number_of_tossing){
  number_of_heads = 0
  number_of_tails = 0
  proportion_of_head = 0
  toss_number=matrix(0, nrow=number_of_tossing, ncol=2)
  toss_results=matrix(0, nrow=number_of_tossing, ncol=2)
  proportion_results=matrix(0, nrow=number_of_tossing, ncol=2)
  for (i in 1:number_of_tossing)
  {
    toss_number[i,1] = i
    toss_number[i,2] = runif(1, min=0, max=1)
    toss_results[i,1] = i
    toss_results[i,2] = toss_number[i,2]
    proportion_results[i,1] = i
    if (toss_number[i,2] > 0.5){
      toss_results[i,2] = 1
      number_of_heads = number_of_heads + 1
    } 
    else {
      number_of_tails = number_of_tails + 1
      toss_results[i,2] = 0
      }
    proportion_of_head = number_of_heads/i
    proportion_results[i,2] = proportion_of_head
  }
  all_result = list(proportion_results,number_of_heads,number_of_tails)
  return(all_result)
}

par(mfrow=c(2,3))

all_experiment_array = c(10,25,50,100,250,1000)

for (i in all_experiment_array) {
  heads = 0
  tails = 0
  # Calling Number of Tossing function with multiple number
  all_values = Number_of_tossing_function(i)
  values = all_values[[1]]
  heads = all_values[[2]]
  tails = all_values[[3]]
  plot(values[,1],values[,2],xlab="Number of coin tosses", ylab="Proportion of heads", col="#339000", xlim=c(0,length(values[,1])), ylim=c(0,1), pch=16)
  abline(h=0.5, col="red", lwd=1, lty=2)
  text(length(values[,1])-(length(values[,1])/4),0.9, paste0("Heads=", heads),cex=0.9)
  text(length(values[,1])-(length(values[,1])/4),0.8, paste0("Tails=", tails), cex=0.9)
}
  
