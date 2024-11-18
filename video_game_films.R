# Loading necessary Libraries
library("tidyverse")
library("scales")

# Removing the rows containing NA values
video_game_films_cleaned <- na.omit(video_game_films)

# Saving the cleaned data with the new file name
write_xlsx(video_game_films_cleaned, "video_film_games_cleaned.xlsx")

# Plotting the histogram
hist(video_game_films_cleaned$`Worldwide Box Office`,
     main = "Worldwide Box Office",
     xlab = "Worldwide Box Office (Million $)",
     col  = "azure",
     Probability = FALSE) #Frequency Scale

# Define the x-axis values for the curve
x <- seq(min(video_game_films_cleaned$`Worldwide Box Office`),
         max(video_game_films_cleaned$`Worldwide Box Office`),
         length.out = 1000)


# Calculate the mean and standard deviation of the Worldwide Box Office
mn <- mean(video_game_films_cleaned$`Worldwide Box Office`)
stdDev <- sd(video_game_films_cleaned$`Worldwide Box Office`)


# Generate the normal distribution curve
yn <- dnorm(x, mean = mn, sd = stdDev)

# Scale the curve to match the histogram's frequency
bin.size <- diff(hist(video_game_films_cleaned$`Worldwide Box Office`, plot = FALSE)$breaks[1:2])
yn <- yn * bin.size * length(video_game_films_cleaned$`Worldwide Box Office`)

# Overlay the normal curve on the histogram
lines(x, yn, col = "blue", lwd = 2)