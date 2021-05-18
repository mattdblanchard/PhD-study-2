# library("tuneR")
# library("fftw")
# library("rgl")
# library("rpanel")
# library("seewave")

library("ggplot2")
library("tidyverse")
library("psych")

# seewave vingettes
# vignette("seewave_IO", package="seewave")
# vignette("seewave_analysis", package="seewave")

# can also play with tico file (small quick to load)
# data("tico")
# # oscillo(tico)
# 
# 
# # test
# d <- readWave("/Users/Matty/Desktop/test.wav")
# 
# # if saved in data folder of working directory
# # data("d")
# 
# x <- timer(d, f = 44100, threshold = 5, msmooth = c(5000, 0))
# 
# # sum length of talking in seconds
# sum(x$s)
# 
# # number of talking turns
# length(x$s)
# 
# 
# # also need to calculate distribution of speaking turns (look at both team members audio together)
# 
# 
# 
# d <- readWave("/Users/Matty/Desktop/17100609_d2.wav")
# 
# # to save as text file
# # export(d, f = 44100, header = FALSE, filename = "17100609_d2")
# 
# 
# data("17100609_d2")
# 
# oscillo(d)
# 


# Working with text file
d <- readLines("data/Audio100HZ.txt")


# convert character string to data frame
x <- as.data.frame(strsplit(d, split="\t"))

# rename column
colnames(x) <- c("Hz")


# write dataframe as csv
# x %>% write.csv("audio100hz.csv")

# x %>% filter(freq > 1)

# add time column (100th of a second increments)
x <- x %>% mutate(
  time = as.numeric(row(x) * .01),
  freq = as.numeric(freq))

index <- rep(1:24160, 25)
index <- as.data.frame(index[order(index)])
 
# rename column
colnames(index) <- c("chunk")
 
# index <- index %>% mutate(
#   time = row(index) * .01)
# 
# 
# describe(y)
# 
x <- x %>% left_join(index)

# doesn't work (also try cbind)
crossing(x, index)

# use rolling window?

x %>% 
  ggplot(aes(x = time, y = freq)) +
           geom_line()


# n <- 100
# 
# y <- as.data.frame(rowMeans(x[seq(1, nrow(x), n),]))
# 
# apply(x[seq(1, nrow(x), n),], 1, mean)

y <- as.data.frame(colMeans(matrix(x$freq, nrow=100)))

# rename column
colnames(y) <- c("freq")

# add time column so can plot
y <- y %>% mutate(
  time = as.numeric(row(y) * 1))

one <- x %>% filter(time <= 60)

sum(one$freq >= 20)

sum(x$freq >= 20)/100

one %>%
  ggplot(aes(x = time, y = freq)) +
  geom_line()


one %>% filter(freq >= 30) %>%
ggplot(aes(x = time, y = freq)) +
  geom_line()

