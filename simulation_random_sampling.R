# Note: 
# First person = Traditional survey approach
# Third person = social-circle approach
library(matrixStats)
library(ggplot2)

n <- 1000
iterations <- 500 # 1000
number_of_samples <- 100 # 500

master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))

set.seed(101)
num_neighbours <- 1
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_standard <- append(temp_standard, mean(my_sample$pref, na.rm = TRUE))
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_first_person[,i] <- abs(accuracy_df$first_person_est - true_prop)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- data.frame("sample_size" = (1:100)/100,
                    "first_person_acc" = rowMeans(master_first_person),
                    "first_person_acc_sd" = rowSds(as.matrix(master_first_person)),
                    "third_person_acc_1" = rowMeans(master_third_person),
                    "third_person_acc_1_sd" = rowSds(as.matrix(master_third_person)))

num_neighbours <- 2
master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- cbind(means,
               data.frame("third_person_acc_2" = rowMeans(master_third_person),
                          "third_person_acc_2_sd" = rowSds(as.matrix(master_third_person))))

num_neighbours <- 5
master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- cbind(means,
               data.frame("third_person_acc_5" = rowMeans(master_third_person),
                          "third_person_acc_5_sd" = rowSds(as.matrix(master_third_person))))

num_neighbours <- 10
master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- cbind(means,
               data.frame("third_person_acc_10" = rowMeans(master_third_person),
                          "third_person_acc_10_sd" = rowSds(as.matrix(master_third_person))))

num_neighbours <- 50
master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- cbind(means,
               data.frame("third_person_acc_50" = rowMeans(master_third_person),
                          "third_person_acc_50_sd" = rowSds(as.matrix(master_third_person))))

num_neighbours <- 100
master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    neighbours <- df[sample(nrow(df[-j,]), num_neighbours, replace = FALSE), ]
    neighbours <- rbind(neighbours,df[j,])
    df$est[j] <- mean(neighbours$pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    for(l in 1:number_of_samples) {
      my_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE), ]
      temp_third_person <- append(temp_third_person, mean(my_sample$est, na.rm = TRUE))
    }
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(i)
}

means <- cbind(means,
               data.frame("third_person_acc_100" = rowMeans(master_third_person),
                          "third_person_acc_100_sd" = rowSds(as.matrix(master_third_person))))

means$first_person_acc_se <- qt(0.975,df=iterations-1)*means$first_person_acc_sd/sqrt(iterations)
means$third_person_acc_1_se <- qt(0.975,df=iterations-1)*means$third_person_acc_1_sd/sqrt(iterations)
means$third_person_acc_2_se <- qt(0.975,df=iterations-1)*means$third_person_acc_2_sd/sqrt(iterations)
means$third_person_acc_5_se <- qt(0.975,df=iterations-1)*means$third_person_acc_5_sd/sqrt(iterations)
means$third_person_acc_10_se <- qt(0.975,df=iterations-1)*means$third_person_acc_10_sd/sqrt(iterations)
means$third_person_acc_50_se <- qt(0.975,df=iterations-1)*means$third_person_acc_50_sd/sqrt(iterations)
means$third_person_acc_100_se <- qt(0.975,df=iterations-1)*means$third_person_acc_100_sd/sqrt(iterations)

write.csv(means,"means-new.csv")

ggplot(means, aes(x=sample_size*100)) + 
  geom_line(aes(y = first_person_acc*100, color = "Traditional survey"), size = 1.2) + 
  geom_line(aes(y = third_person_acc_1*100, color = "Social-circle: 1 neighbour"), size = 1.2) +
  geom_line(aes(y = third_person_acc_2*100, color = "Social-circle: 2 neighbours"), size = 1.2) +
  geom_line(aes(y = third_person_acc_5*100, color = "Social-circle: 5 neighbours"), size = 1.2) +
  geom_line(aes(y = third_person_acc_10*100, color = "Social-circle: 10 neighbours"), size = 1.2) +
  geom_line(aes(y = third_person_acc_50*100, color = "Social-circle: 50 neighbours"), size = 1.2) +
  geom_line(aes(y = third_person_acc_100*100, color = "Social-circle: 100 neighbours"), size = 1.2) +
  geom_ribbon(aes(ymin=first_person_acc*100 - first_person_acc_se*100, ymax=first_person_acc*100 + first_person_acc_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_1*100 - third_person_acc_1_se*100, ymax=third_person_acc_1*100 + third_person_acc_1_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_2*100 - third_person_acc_2_se*100, ymax=third_person_acc_2*100 + third_person_acc_2_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_5*100 - third_person_acc_5_se*100, ymax=third_person_acc_5*100 + third_person_acc_5_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_10*100 - third_person_acc_10_se*100, ymax=third_person_acc_10*100 + third_person_acc_10_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_50*100 - third_person_acc_50_se*100, ymax=third_person_acc_50*100 + third_person_acc_50_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_100*100 - third_person_acc_100_se*100, ymax=third_person_acc_100*100 + third_person_acc_100_se*100),alpha=0.3) +
  labs(x = "Sample size (percent)",
       y = "Error (percentage points)",
       color = "") +
  scale_color_manual(breaks = c("Traditional survey","Social-circle: 1 neighbour","Social-circle: 2 neighbours","Social-circle: 5 neighbours", "Social-circle: 10 neighbours","Social-circle: 50 neighbours","Social-circle: 100 neighbours"), values=c("black","red", "blue","green","orange","purple","rosybrown"))
