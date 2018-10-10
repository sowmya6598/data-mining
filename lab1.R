
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


# EXERCISE 1.7


# EXERCISE 1.8


# EXERCISE 1.9


# EXERCISE 1.10