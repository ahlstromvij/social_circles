# Note: 
# First person = Traditional survey approach
# Third person = social-circle approach
library(matrixStats)
library(ggplot2)

# 90% homophily
n <- 1000 
iterations <- 500 
number_of_samples <- 100 
discernment <- 0.51
bubble <- 0.90

master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))

set.seed(101)
num_neighbours <- 1
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
                                 
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA,
                            "third_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    # generate biased sample
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_standard <- append(temp_standard, mean(biased_sample$pref, na.rm = TRUE))
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_first_person[,i] <- abs(accuracy_df$first_person_est - true_prop)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
}

means <- cbind(means,
               data.frame("third_person_acc_100" = rowMeans(master_third_person),
                          "third_person_acc_100_sd" = rowSds(as.matrix(master_third_person))))

write.csv(means,"means_birds_90-new.csv")
# means <- read.csv("means_birds_90.csv")

means$first_person_acc_se <- qt(0.975,df=iterations-1)*means$first_person_acc_sd/sqrt(iterations)
means$third_person_acc_1_se <- qt(0.975,df=iterations-1)*means$third_person_acc_1_sd/sqrt(iterations)
means$third_person_acc_2_se <- qt(0.975,df=iterations-1)*means$third_person_acc_2_sd/sqrt(iterations)
means$third_person_acc_5_se <- qt(0.975,df=iterations-1)*means$third_person_acc_5_sd/sqrt(iterations)
means$third_person_acc_10_se <- qt(0.975,df=iterations-1)*means$third_person_acc_10_sd/sqrt(iterations)
means$third_person_acc_50_se <- qt(0.975,df=iterations-1)*means$third_person_acc_50_sd/sqrt(iterations)
means$third_person_acc_100_se <- qt(0.975,df=iterations-1)*means$third_person_acc_100_sd/sqrt(iterations)

ggplot(means[1:20,], aes(x=sample_size*100)) + 
  geom_line(aes(y = first_person_acc*100, color = "Traditional survey"), size = 1.2) + 
  #geom_line(aes(y = third_person_acc_1*100, color = "Social-circle: 1 neighbour"), size = 1.2) +
  geom_line(aes(y = third_person_acc_2*100, color = "Social-circle: 2 neighbours"), size = 1.2) +
  #geom_line(aes(y = third_person_acc_5*100, color = "Social-circle: 5 neighbours"), size = 1.2) +
  #geom_line(aes(y = third_person_acc_10*100, color = "Social-circle: 10 neighbours"), size = 1.2) +
  #geom_line(aes(y = third_person_acc_50*100, color = "Social-circle: 50 neighbours"), size = 1.2) +
  #geom_line(aes(y = third_person_acc_100*100, color = "Social-circle: 100 neighbours"), size = 1.2) +
  geom_ribbon(aes(ymin=first_person_acc*100 - first_person_acc_se*100, ymax=first_person_acc*100 + first_person_acc_se*100),alpha=0.3) +
  #geom_ribbon(aes(ymin=third_person_acc_1*100 - third_person_acc_1_se*100, ymax=third_person_acc_1*100 + third_person_acc_1_se*100),alpha=0.3) +
  geom_ribbon(aes(ymin=third_person_acc_2*100 - third_person_acc_2_se*100, ymax=third_person_acc_2*100 + third_person_acc_2_se*100),alpha=0.3) +
  #geom_ribbon(aes(ymin=third_person_acc_5*100 - third_person_acc_5_se*100, ymax=third_person_acc_5*100 + third_person_acc_5_se*100),alpha=0.3) +
  #geom_ribbon(aes(ymin=third_person_acc_10*100 - third_person_acc_10_se*100, ymax=third_person_acc_10*100 + third_person_acc_10_se*100),alpha=0.3) +
  #geom_ribbon(aes(ymin=third_person_acc_50*100 - third_person_acc_50_se*100, ymax=third_person_acc_50*100 + third_person_acc_50_se*100),alpha=0.3) +
  #geom_ribbon(aes(ymin=third_person_acc_100*100 - third_person_acc_100_se*100, ymax=third_person_acc_100*100 + third_person_acc_100_se*100),alpha=0.3) +
  labs(x = "Sample size (percent)",
       y = "Error (percentage points)",
       color = "") +
  scale_color_manual(breaks = c("Traditional survey","Social-circle: 1 neighbour","Social-circle: 2 neighbours","Social-circle: 5 neighbours", "Social-circle: 10 neighbours","Social-circle: 50 neighbours","Social-circle: 100 neighbours"), values=c("black","red", "blue","green","orange","purple","rosybrown"))

# 99% homophily
n <- 1000 
iterations <- 500 
number_of_samples <- 100 
discernment <- 0.51
bubble <- 0.99

master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))

set.seed(101)
num_neighbours <- 1
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA,
                            "third_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    # generate biased sample
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_standard <- append(temp_standard, mean(biased_sample$pref, na.rm = TRUE))
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_first_person[,i] <- abs(accuracy_df$first_person_est - true_prop)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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

write.csv(means,"means_birds_99-new.csv")
# means <- read.csv("means_birds_99.csv")

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

# 75% homophily
n <- 1000 
iterations <- 500 
number_of_samples <- 100 
discernment <- 0.51
bubble <- 0.75

master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))

set.seed(101)
num_neighbours <- 1
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA,
                            "third_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    # generate biased sample
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_standard <- append(temp_standard, mean(biased_sample$pref, na.rm = TRUE))
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_first_person[,i] <- abs(accuracy_df$first_person_est - true_prop)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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

write.csv(means,"means_birds_75-new.csv")
# means <- read.csv("means_birds_75.csv")

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

# 50% homophily
n <- 1000 
iterations <- 500 
number_of_samples <- 100 
discernment <- 0.51
bubble <- 0.50

master_first_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))
master_third_person <- data.frame(matrix(NA, nrow = 100, ncol = iterations))

set.seed(101)
num_neighbours <- 1
for(i in 1:iterations) {
  prop <- sample(1:100,1)/100
  df <- data.frame("pref" = sample(0:1, size = n, replace = TRUE, prob = c(prop,1-prop)))
  df$est <- NA
  
  for(j in 1:length(df$pref)) {
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA,
                            "third_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    # generate biased sample
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_standard <- append(temp_standard, mean(biased_sample$pref, na.rm = TRUE))
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_first_person[,i] <- abs(accuracy_df$first_person_est - true_prop)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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
    # homophily
    pref_of_j <- df$pref[j]
    df$prob[df$pref==pref_of_j] <- bubble
    df$prob[df$pref!=pref_of_j] <- 1-bubble
    neighbours_indices <- sample(c(rownames(df[df$pref==pref_of_j,]),
                                   rownames(df[df$pref!=pref_of_j,])), 
                                 num_neighbours, 
                                 replace = FALSE,
                                 prob = c(df$prob[df$pref==pref_of_j],
                                          df$prob[df$pref!=pref_of_j]))
    
    neighbours <- df[neighbours_indices,]
    
    #egocentric bias
    neighbours$perceived_pref <- NA
    for(m in 1:length(neighbours$pref)) {
      neighbours$perceived_pref[m] <- sample(c(neighbours$pref[m],df$pref[j]), 1, prob = c(discernment, 1-discernment))
    }
    neighbours <- rbind(neighbours,cbind(df[j,],"perceived_pref"=df[j,]$pref))
    df$est[j] <- mean(neighbours$perceived_pref)
  }
  
  accuracy_df <- data.frame("sample_size" = 1:100,
                            "first_person_est" = NA)
  accuracy_df$sample_size <- accuracy_df$sample_size/100
  
  for(k in 1:length(accuracy_df$sample_size)) {
    temp_standard <- NA
    temp_third_person <- NA
    bias <- sample(1:99,1,replace=TRUE)/100
    df$bias <- NA
    df$bias[df$pref==0] <- bias
    df$bias[df$pref==1] <- 1-bias
    for(l in 1:number_of_samples) {
      biased_sample <- df[sample(nrow(df), length(df$pref)*accuracy_df$sample_size[k], replace = FALSE, prob = df$bias), ]
      temp_third_person <- append(temp_third_person, mean(biased_sample$est, na.rm = TRUE))
    }
    accuracy_df$first_person_est[k] <- mean(temp_standard, na.rm = TRUE)
    accuracy_df$third_person_est[k] <- mean(temp_third_person, na.rm = TRUE)
  }
  
  true_prop <- mean(df$pref)
  master_third_person[,i] <- abs(accuracy_df$third_person_est - true_prop)
  
  print(paste("Iteration",i,"of",iterations,"for",num_neighbours,"neighbour(s)"))
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

write.csv(means,"means_birds_50-new.csv")
# means <- read.csv("means_birds_50.csv")

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
