
# EXERCISE 1.1

colors <- factor(c("Red", "Blue", "Orange", "Yellow"))

pattern <- list(sample(colors, 500, replace = TRUE))
pattern1 <-data.frame(pattern)
pattern2 <- as.matrix(pattern)

colors1 <- as.list(colors)


# EXERCISE 1.2

data(cars)

cars2 <- data.frame(cars)

cars2$speed <- set_units(cars2$speed,m/s)
cars2$distance <- set_units(cars2$dist,m)

plot(x = cars$speed, y = cars$dist, xlab = "speed", ylab = "distance")

abline(lm(speed~dist, data = cars2))


# EXERCISE 1.3

age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)

mean_age <- mean(age)
median_age <- median(age)

mode_age <- mode(age)

midrange_age <- (max(age) - min(age)) / 2

first_quartile <- quantile(age, 0.25)
third_quartile <- quantile(age, 0.75)

five_number_summary <- fivenum(age)

boxplot(cars$speed~cars$dist, xlab = "speed", ylab = "distance")


# EXERCISE 1.4

hospital_data <- data.frame(
    age = c(23, 23, 27, 27, 39, 41, 47, 49, 50, 52, 54, 54, 56, 57, 58, 58, 60, 61),
    fat = c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4, 27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2, 34.1, 32.9, 41.2, 35.7),
    stringsAsFactors = TRUE
)

mean_adult_age <- mean(hospital_data$age)
median_adult_age <- median(hospital_data$age)
standard_deviation_adult_age <- sd(hospital_data$age)

mean_adult_fat <- mean(hospital_data$far)
median_adult_fat <- median(hospital_data$far)
standard_deviation_adult_fat <- sd(hospital_data$fat)

boxplot(hospital_data$age~hospital_data$fat, xlab = "age", ylab = "fat")

plot(x = hospital_data$age, y = hospital_data$fat, xlab = "age", ylab = "fat")

correlation <- cor(hospital_data$age,hospital_data$fat)


# EXERCISE 1.5


# EXERCISE 1.6

pres.age <- c(57, 61, 57, 57, 58, 57, 61, 54, 68, 51, 49, 64, 50, 48, 65, 52, 56, 46, 54, 49, 51, 47, 55, 55, 54, 42, 51, 56, 55, 51, 54, 51, 60, 61, 43, 55, 56, 61, 52, 69, 64, 46, 54, 47)

hist(pres.age)

    #outliers

boxplot(pres.age)


# EXERCISE 1.7


# EXERCISE 1.8


# EXERCISE 1.9

queen <- data.frame(
  year = c(1994, 1995, 1996, 1997, 1998),
  passengers = c(34567, 34678, 36789, 38102, 39024),
  stringsAsFactors = FALSE
)

hist(queen$passengers,
     xlab = "Passengers",
     freq = TRUE,
     probability = FALSE,
     border = "blue",
     col="green",
     breaks = 5)

hist(queen$passengers,
     xlab = "Passengers",
     freq = NULL,
     probability = TRUE,
     border = "green",
     col="red")




# EXERCISE 1.10