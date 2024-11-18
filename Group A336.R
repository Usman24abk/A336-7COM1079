# Load necessary libraries
library("scales")
library("tidyverse")
library("readxl")  # Library for read_excel function

# Load the data set from an Excel file
film_video_games <- read_excel("film video games.xlsx")


# Remove rows with any NA values
film_video_games_clean <- film_video_games %>%
  drop_na() %>%


# Plotting the histogram
hist(film_video_games_clean$`Worldwide Box Office`,
     main = "Worldwide Box Office",
     xlab = "Worldwide Box Office (Million $)",
     col = "azure",
     probability = FALSE,)  # Frequency scale

 
# Define the x-axis values for the curve
x <- seq(min(film_video_games_clean$`Worldwide Box Office`),
         max(film_video_games_clean$`Worldwide Box Office`),
         length.out = 1000)


# Calculate the mean and standard deviation of the Worldwide Box Office
mn <- mean(film_video_games_clean$`Worldwide Box Office`)
stdDev <- sd(film_video_games_clean$`Worldwide Box Office`)


# Generate the normal distribution curve
yn <- dnorm(x, mean = mn, sd = stdDev)


# Scale the curve to match the histogram's frequency
bin.size <- diff(hist(film_video_games_clean$`Worldwide Box Office`, plot = FALSE)$breaks[1:2])
yn <- yn * bin.size * length(film_video_games_clean$`Worldwide Box Office`)


# Overlay the normal curve on the histogram
lines(x, yn, col = "blue", lwd = 2)


# Scatter plot
plot(film_video_games_clean$`Rotten Tomatoes`, 
     film_video_games_clean$`Worldwide Box Office`,
     main = "Rotten Tomatoes vs Worldwide Box Office",
     xlab = "Rotten Tomatoes (%)", 
     ylab = "Worldwide Box Office (Million $)",
     pch = 19, 
     col = "black",
     xlim = c(0, 85),  # Adjust x-axis limits
     ylim = c(0, max(film_video_games_clean$`Worldwide Box Office`)))  # Adjust y-axis limits

# Add grid lines to the plot
grid()


# Fit a linear regression model
model <- lm(`Worldwide Box Office` ~ `Rotten Tomatoes`, data = film_video_games_clean)

# Add the regression line to the scatter plot
abline(model, col = "blue",lwd = 2)


