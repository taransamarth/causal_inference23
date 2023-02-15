library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Part 1

pop <- read_dta("/Users/taransamarth/Downloads/pop.dta")

x_hist <- pop %>% ggplot(aes(x = X)) + geom_histogram()
y_hist <- pop %>% ggplot(aes(x = Y)) + geom_histogram()

ggsave("hw1_hists.pdf", width = 5, height = 7, units = "in",
       arrangeGrob(x_hist, y_hist, ncol = 2))

# Part 2

# 1k replicates of SRS w/o rep 10 50 250 500, compute means for each rep/size, 
# graphs with normal densities over histograms for each N

n <- 1000
size <- c(10, 50, 250, 500)

plot.list <- list()

for (s in size) {
  means <- c()
  for (i in 1:n) {
    samp <- sample(pop$X, s, replace = FALSE)
    m <- mean(pop$X[samp])
    means <- c(means, m)
  }
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    mean = means
  ) %>% ggplot(aes(x = mean)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Means with ", s, " samples")) +
    stat_function(fun = dnorm, n = 101, args = list(mean = mean(pop$X), sd = sqrt(var(pop$X)/s)))))
}

ggsave("hw1_Xconv.pdf", plot(arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2)),
       width = 5, height = 7, units = "in")

plot.list <- list()

for (s in size) {
  means <- c()
  for (i in 1:n) {
    samp <- sample(pop$Y, s, replace = FALSE)
    m <- mean(pop$Y[samp])
    means <- c(means, m)
  }
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    mean = means
  ) %>% ggplot(aes(x = mean)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Means with ", s, " samples")) +
    stat_function(fun = dnorm, n = 101, args = list(mean = mean(pop$Y), sd = sqrt(var(pop$Y)/s)))))
}

ggsave("hw1_Yconv.pdf", plot(arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2)),
       width = 5, height = 7, units = "in")

# Part 3

n <- 1000
size <- c(10, 50, 250, 500)

plot.list <- list()

for (s in size) {
  diff_means <- c()
  for (i in 1:n) {
    samp <- sample(pop$index, s*2, replace = FALSE)
    treat <- sample(samp, s, replace = FALSE)
    control <- samp[samp != treat]
    diff_means <- c(diff_means, mean(pop$X[treat]) - mean(pop$Y[control]))
  }
  plot.list <- append(plot.list, list(data.frame(
    i = c(1:n),
    diff = diff_means
  ) %>% ggplot(aes(x = diff)) + geom_histogram(aes(y=..density..)) +
    xlab(paste0("Diff. of means with ", s, " samples")) +
    stat_function(fun = dnorm, n = 100, args = list(mean = mean(pop$X) - mean(pop$Y), 
                                                    sd = sqrt((var(pop$X)/s) + 
                                                                (var(pop$Y)/s))))))
}

ggsave("hw1_diffmeans.pdf", plot(arrangeGrob(grobs = plot.list, ncol = 2, nrow = 2)),
       width = 5, height = 7, units = "in")
