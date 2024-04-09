library(tidyverse)
library(tidycensus)
require(plotrix)

my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"

#read in variable info
pums_raw <- rvest::read_html("https://api.census.gov/data/2022/acs/acs1/pums/variables.html")
tab <- rvest::html_table(pums_raw)[[1]][-1, ]
head(tab)


my_var <- "FFSP"
data_micro <- get_pums(
  variables = my_var, 
  variables_filter = list(AGEP = 18:30),
  state = "PA",
  recode = TRUE,
  year = 2022,
#  show_call = TRUE,
  
  survey = "acs1",
  key = my_key
)
data_micro$FFSP <- as.numeric(data_micro$FFSP)
data_micro$AGEP <- as.numeric(data_micro$AGEP)

n <- 300
index <- sample(1:nrow(data_micro), size = n, replace = FALSE)
my_sub <- data_micro[index, c("FFSP", "AGEP")]

#find parameteric CI
p_hat <- my_sub$FFSP |> mean()
y <- my_sub$FFSP |> sum()

basicCI <- function(y, n, alpha = 0.05){
  p_hat <- y/n
  c(p_hat - qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n), p_hat + qnorm(1-alpha/2) * sqrt(p_hat*(1-p_hat)/n))  
}
basicCI(y, n)

#or do the fancier score interval
#find Score
scoreCI <- function(y, n, alpha = 0.05){
  c((y/n+qnorm(1-alpha/2)^2/(2*n)-qnorm(1-alpha/2)*sqrt((y/n*(1-y/n)+qnorm(1-alpha/2)^2/(4*n))/n))/(1+qnorm(1-alpha/2)^2/n), (y/n+qnorm(1-alpha/2)^2/(2*n)+qnorm(1-alpha/2)*sqrt((y/n*(1-y/n)+qnorm(1-alpha/2)^2/(4*n))/n))/(1+qnorm(1-alpha/2)^2/n))
}

scoreCI(y, n)

#find bootstrap interval
#both bootstrap intervals at once
bootInt <- function(y, n, alpha = 0.05, B = 500){
    bootMLEs <- rbinom(n = B, size = n, prob = y/n)/n
    return(quantile(bootMLEs, c(alpha/2, 1-alpha/2)))
  }
bootInt(y, n)


mycolor <- function(endpoints, par) {
  if (par < endpoints[1]) 
    "Red"  # if the mean is below the left endpoint of the confidence interval
  else if (par > endpoints[2]) 
    "Red"  # if the mean is above the right endpoint of the confidence interval
  else "Black"  # if the mean lies between the endpoints
}

basic <- basicCI(y, n)
CIs <- data.frame(lower = basic[1], upper = basic[2])

true_p <- data_micro$FFSP |> as.numeric() |> mean()
CIs$col <- apply(FUN = mycolor, X = CIs, MARGIN = 1, par = true_p)

plotCI(x = 1:nrow(CIs), 
       y = (CIs[, 1] + CIs[, 2])/2,
       li = CIs$lower, 
       ui = CIs$upper,
       col = CIs$col, 
       lwd = 1.5,
       ylim = c(min(CIs[, 1]), max(CIs[, 2])),
       ylab = "Intervals",
       xlab = "Sampled Data Set",
       main = "Visualization of CIs")
#draw a line for true mean
abline(h = true_p, lwd = 2)







add_ci <- function(CIdf = NULL, y, n, type = 'basic', alpha = 0.05, B = 500){
  if(is.null(CIdf)){
    if(type == "basic"){
      ci_values <- basicCI(y, n, alpha)
      CIdf <- data.frame(lower = ci_values[1], upper = ci_values[2])
    } else if(type == "score"){
      ci_values <- scoreCI(y, n, alpha)
      CIdf <- data.frame(lower = ci_values[1], upper = ci_values[2])
    } else if(type == "bootstrap"){
      ci_values <- bootInt(y, n, alpha, B)
      CIdf <- data.frame(lower = ci_values[1], upper = ci_values[2])
    } 
  } else {
    if(type == "basic"){
      ci_values <- basicCI(y, n, alpha)
      CIdf <- rbind(CIdf, data.frame(lower = ci_values[1], upper = ci_values[2]))
    } else if(type == "score"){
      ci_values <- scoreCI(y, n, alpha)
      CIdf <- rbind(CIdf, data.frame(lower = ci_values[1], upper = ci_values[2]))
    } else if(type == "bootstrap"){
      ci_values <- bootInt(y, n, alpha, B)
      CIdf <- rbind(CIdf, data.frame(lower = ci_values[1], upper = ci_values[2]))
    } 
  }
  return(CIdf)
}

add_color <- function(CIdf, truth){
  CIdf$col <- apply(FUN = mycolor, X = CIdf, MARGIN = 1, par = truth)
}


get_sample <- function(var, ages, state, year){
  data_micro <- get_pums(
    variables = var, 
    variables_filter = list(AGEP = ages),
    state = state,
    recode = TRUE,
    year = year,
    survey = "acs1",
    key = my_key
  )
  data_micro[, 5] <- as.numeric(pull(data_micro, 5))
  data_micro[, 6] <- as.numeric(pull(data_micro, 6))
  return(data_micro)
}

get_subset <- function(sample_data, n = 300){
  index <- sample(1:nrow(sample_data), size = n, replace = FALSE)
  sub <- sample_data[index, 5:6]
  truth <- pull(sample_data, 5) |> mean()
  y <- sub[, 1] |> sum()
  return(data.frame(y = y, n = n, truth = truth))
}

#now let's call them and see
full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(y = sam$y, n = sam$n)

#get another sample and add the CI
full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

full_sam <- get_sample("FFSP", ages = 18:30, state = "PA", year = 2022)
sam <- get_subset(full_sam)
CI_data_frame <- add_ci(CI_data_frame, y = sam$y, n = sam$n)

#add color column
CI_data_frame$col <- add_color(CI_data_frame, sam$truth)


#now plot
plot_CI <- function(CIdf, truth){
  plotCI(x = 1:nrow(CIdf), 
       y = (CIdf[, 1] + CIdf[, 2])/2,
       li = CIdf$lower, 
       ui = CIdf$upper,
       col = CIdf$col, 
       lwd = 1.5,
       ylim = c(min(CIdf[, 1]), max(CIdf[, 2])),
       ylab = "Intervals",
       xlab = "Sampled Data Set",
       main = "Visualization of CIs")
  #draw a line for true mean
  abline(h = truth, lwd = 2)
}

plot_CI(CI_data_frame, sam$truth)




