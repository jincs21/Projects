df = read.csv("/Users/LT/Downloads/EWCS_2016.csv")
df <- df %>%
  rename(Gender = Q2a,
         Age = Q2b)
df[df$Gender == 1,]$Gender <- "M"
df[df$Gender == 2,]$Gender = "F"
View(df)
df[df == -999] <- NA
summary(df)
index <- 1:ncol(df)
df[3:11] <- lapply(df[3:11], as.factor)
str(df)