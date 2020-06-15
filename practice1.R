install.packages("dplyr") # once per machine
library("dplyr")          # in each relevant script

# Install the `pscl` package to use the `presidentialElections` data frame
install.packages("pscl") # once per machine
library("pscl")          # in each relevant script

# You should now be able to interact with the data set
View(presidentialElections)

# Select `year` and `demVotes` (percentage of vote won by the Democrat)
# from the `presidentialElections` data frame
votes <- select(presidentialElections, year, demVote)
head(votes)

# Select all rows from the 2008 election
votes_2008 <- filter(presidentialElections, year == 2008)
head(votes_2008)


# Extract the row(s) for the state of Colorado in 2008
# Arguments are on separate lines for readability
votes_colorado_2008 <- filter(
  presidentialElections,
  year == 2008,
  state == "Colorado"
)
head(votes_colorado_2008)

# Add an `other_parties_vote` column that is the percentage of votes
# for other parties
# Also add an `abs_vote_difference` column of the absolute difference
# between percentages
# Note you can use columns as you create them!
presidentialElections <- mutate(
  presidentialElections,
  other_parties_vote = 100 - demVote, # other parties is 100% - Democrat %
  abs_vote_difference = abs(demVote - other_parties_vote)
)
head(presidentialElections)

# Arrange rows in decreasing order by `year`, then by `demVote`
# within each `year`
presidentialElections <- arrange(presidentialElections, -year, demVote)
head(presidentialElections)

# Compute summary statistics for the `presidentialElections` data frame
average_votes <- summarize(
  presidentialElections,
  mean_dem_vote = mean(demVote),
  mean_other_parties = mean(other_parties_vote)
)
average_votes

# A function that returns the value in a vector furthest from 50
furthest_from_50 <- function(vec) {
  # Subtract 50 from each value
  adjusted_values <- vec - 50
  
  # Return the element with the largest absolute difference from 50
  vec[ abs(adjusted_values) == max(abs(adjusted_values))]
}

# Summarize the data frame, generating a column `biggest_landslide`
# that stores the value furthest from 50%
summarize(
  presidentialElections,
  biggest_landslide = furthest_from_50(demVote)
)
