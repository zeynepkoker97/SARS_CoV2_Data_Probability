
# Example Data Plot includes Lebanon data

Covid19_function = function(starting_date,end_date,country_name,population_of_country_in_million){
  data = read.csv(file="WHO-COVID-19-global-data.csv", head=FALSE, col.names = c('date','Country_Code','Country','WHO_region','New_cases','Cumulative_cases','New_deaths','Cumulative_deaths'), sep=",")
  starting_date_row = as.numeric(rownames(data[data$Country==country_name & data$date==starting_date,]))
  end_date_row = as.numeric(rownames(data[data$Country==country_name & data$date==end_date,]))
  number_of_days = end_date_row - starting_date_row
  country_data = matrix(0,nrow=number_of_days, ncol=4)
  for (i in 1:(number_of_days)){
    x = i + starting_date_row
    country_data[i,1] = i
    country_data[i,2] = as.numeric(data[x,7])/population_of_country_in_million
    country_data[i,3] = data[x,7]
    country_data[i,4] = data[x,1]
  }
  return(country_data)
}

# Calling function for plot

# Function inputs: Starting_date, End_date, Country_name, Population_of_country_in_million
# Population_of_country_in_million data from (https://www.worldometers.info/world-population)
mongolia = Covid19_function("2020-12-28","2022-01-09","Mongolia",3.432353)
usa = Covid19_function("2020-12-28","2022-01-09","United States of America",336.511170)
lebanon = Covid19_function("2020-12-28","2022-01-09","Lebanon",6.741067)

plot(mongolia[,1],mongolia[,2], type="l", lwd=2, xlab="Days from 2020-12-28 to 2022-01-09", ylab="Covid19 deaths per million", col="#0099FF", xlim=c(0,400), ylim=c(0,55))
lines(usa[,1],usa[,2], type="l", lwd=2, col="#FF0000")
lines(lebanon[,1],lebanon[,2], type="l", lwd=2, col="#339000")
legend("topright","inset", legend=c("Mongolia","USA","Lebanon"), col=c("#0099FF","#FF0000","#339000"), lty=1:1, cex=0.8, bty = "n")

# Data Analysis
Comparison_with_Poisson = function(starting_date,end_date,country_name,number_of_simulated_weeks,number_of_random_selected_week_from_actual_data){
  data = read.csv(file="WHO-COVID-19-global-data.csv", head=FALSE, col.names = c('date','Country_Code','Country','WHO_region','New_cases','Cumulative_cases','New_deaths','Cumulative_deaths'), sep=",")
  starting_date_row = as.numeric(rownames(data[data$Country==country_name & data$date==starting_date,]))
  end_date_row = as.numeric(rownames(data[data$Country==country_name & data$date==end_date,]))
  number_of_days = end_date_row - starting_date_row
  country_data = matrix(0,nrow=number_of_days, ncol=3)
  for (i in 1:(number_of_days)){
    x = i + starting_date_row
    country_data[i,1] = i
    country_data[i,2] = data[x,7]
    country_data[i,3] = data[x,1]
  }
  
  total_week_in_data = floor(nrow(country_data)/7)
  
  actual_data = matrix(0,nrow=(number_of_random_selected_week_from_actual_data*7), ncol=6)
  actual_data_simplified = matrix(0,nrow=(number_of_random_selected_week_from_actual_data), ncol=6)
  colnames(actual_data_simplified) = c("Dates of Week", "New Deaths from Data", "Mean of Week", "Variance of Week", "Probability", "Final Decision")
  simulated_week = matrix(0,nrow=(number_of_simulated_weeks*7), ncol=2)
  
  start = 1
  for (x in 1:number_of_random_selected_week_from_actual_data){
    count_of_lower_variance = 0
    actual_data_temp_var = 0
    simulated_temp_var = 0
    
    random_i = floor(runif(1, min=1, max=total_week_in_data-6))
    starting_week = random_i
    end_week = random_i+6
    end = start+6
    actual_data[(start:end),1] = x
    actual_data[(start:end),2] = starting_week:end_week
    actual_data[(start:end),3] = country_data[starting_week:end_week,3]
    actual_data[(start:end),4] = country_data[starting_week:end_week,2]
    actual_data[(start:end),5] = round(mean(as.numeric(country_data[starting_week:end_week,2])),3)
    actual_data[(start:end),6] = round(var(as.numeric(country_data[starting_week:end_week,2])),3)
    
    actual_data_simplified[x,1] = paste(country_data[starting_week,3], '/'  ,country_data[end_week,3])
    deaths_str <- ''
    for (i in starting_week:end_week) {
      deaths_str = paste(deaths_str, country_data[i,2], ',')
    }
    deaths_str = substring(deaths_str,1, nchar(deaths_str)-1)
    actual_data_simplified[x,2] = deaths_str
    actual_data_simplified[x,3] = round(mean(as.numeric(country_data[starting_week:end_week,2])),3)
    actual_data_simplified[x,4] = round(var(as.numeric(country_data[starting_week:end_week,2])),3)
    actual_data_temp_var = round(var(as.numeric(country_data[starting_week:end_week,2])),3)
    
    start_z = 1
    for (z in 1:number_of_simulated_weeks){
      end_z = start_z + 6
      simulated_week[(start_z:end_z),1] = rpois(n=7,lambda=mean(as.numeric(actual_data_simplified[x,3])))[1:7]
      simulated_week[(start_z:end_z),2] = round(var(simulated_week[(start_z:end_z),1]),3)
      simulated_temp_var = round(var(simulated_week[(start_z:end_z),1]),3)
      if (actual_data_temp_var > simulated_temp_var) {
        count_of_lower_variance = count_of_lower_variance + 1
    }
    start_z = end_z+1
    }
    actual_data_simplified[x,5] = paste((count_of_lower_variance/1000)*100,'%')
    if ((count_of_lower_variance/1000)*100 < 5){
      actual_data_simplified[x,6] = "Excellent healthcare or data tempering?"
    } else {
      actual_data_simplified[x,6] = "The deadly effect of SARS Cov 2 seems in the data"
    }
    start = end +1
  }
  all_results = list(actual_data_simplified,actual_data,simulated_week)
  return(all_results)
}

# Calling a function for Possibility
# Check X_simplified data for results 
lebanon_specific = Comparison_with_Poisson("2020-12-28","2022-01-09",'Lebanon',1000,10)
lebanon_specific_simplified = lebanon_specific[[1]]
# lebanon_specific_actual = lebanon_specific[[2]]
# lebanon_specific_simulated_week = lebanon_specific[[3]]

lebanon_all = Comparison_with_Poisson("2020-01-03","2023-04-19",'Lebanon',1000,10)
lebanon_all_simplified = lebanon_all[[1]]
# lebanon_all_actual = lebanon_all[[2]]
# lebanon_all_simulated_week = lebanon_all[[3]]

usa_specific = Comparison_with_Poisson("2020-12-28","2022-01-09",'United States of America',1000,10)
usa_specific_simplified = usa_specific[[1]]
# usa_specific_actual = usa_specific[[2]]
# usa_specific_simulated_week = usa_specific[[3]]

