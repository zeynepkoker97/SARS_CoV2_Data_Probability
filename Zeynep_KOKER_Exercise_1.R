## Project - Exercise 1
BMM422_Data_raw <- read.csv(file="BMM422_Data.csv", head=TRUE, sep=";")
BMM422_Data = BMM422_Data_raw[-1,]

# Example Plot
plot(BMM422_Data$away,BMM422_Data$away.1, main="Example Plot for Exercise 1", xlab="x-axis title", ylab="y-axis title", col="blue", pch=16)
sd_example = sd(as.numeric(BMM422_Data$away))
avg_example = mean(as.numeric(BMM422_Data$away))
cor_example = cor(as.numeric(BMM422_Data$away),as.numeric(BMM422_Data$away.1))
text(75,97, paste0("Mean=", round(avg_example,2)),cex=0.85)
text(75,87, paste0("SD=", round(sd_example,2)), cex=0.85)
text(75,77, paste0("Corr=", round(cor_example,3)), cex=0.85)

length_csv_data = ncol(BMM422_Data)/2
columnNames = colnames(BMM422_Data)
colors_array=c("blue","#0099FF","#FF0000","#FF6633","#FF9933","#FFCC33","#FFFF33","#99CC33","#669933","#33CC00","#3399CC","#00CCFF","#66CCFF")

par(mfrow=c(3,4))
for (i in 2: length_csv_data)
{
  x_data = i*2-1
  y_data = i*2
  sd = sd(as.numeric(BMM422_Data[,x_data]))
  avg = mean(as.numeric(BMM422_Data[,x_data]))
  cor = cor(as.numeric(BMM422_Data[,x_data]),as.numeric(BMM422_Data[,y_data]))
  plot(BMM422_Data[,x_data],BMM422_Data[,y_data], main= paste("Plot for",columnNames[x_data]), xlab=columnNames[x_data], ylab=columnNames[y_data], col=colors_array[i], xlim=c(0,100), ylim=c(0,100), pch=16)
  text(75,97, paste0("Mean=", round(avg,2)),cex=0.85)
  text(75,87, paste0("SD=", round(sd,2)), cex=0.85)
  text(75,77, paste0("Corr=", round(cor,3)), cex=0.85)
}

