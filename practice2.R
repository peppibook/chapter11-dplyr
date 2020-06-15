#
# Q : Which state had the highest percentage of votes for the Democratic Party candidate (Barack Obama) in 2008?
#
# 1. Filter down the data set to only observations from 2008.
# 2. Of the percentages in 2008, filter down to the one with the highest percentage of votes for a Democrat.
# 3. Select the name of the state that meets the above criteria.


# Use a sequence of steps to find the state with the highest 2008
# `demVote` percentage

# 1. Filter down to only 2008 votes
votes_2008 <- filter(presidentialElections, year == 2008)
votes_2008

# 2. Filter down to the state with the highest `demVote`
most_dem_votes <- filter(votes_2008, demVote == max(demVote))
most_dem_votes

# 3. Select name of the state
most_dem_state <- select(most_dem_votes, state)
most_dem_state

# Use nested functions to find the state with the highest 2008
# `demVote` percentage
most_dem_state <- select(    # 3. Select name of the state
  filter(                   # 2. Filter down to the highest `demVote`
    filter(                  # 1. Filter down to only 2008 votes
      presidentialElections, # arguments for the Step 1 `filter`
      year == 2008
    ),
    demVote == max(demVote)  # second argument for the Step 2 `filter`
  ),
  state                      # second argument for the Step 3 `select`
  )
most_dem_state

# Ask the same question of our data using the pipe operator
most_dem_state <- presidentialElections %>% # data frame to start with
  filter(year == 2008) %>% # 1. Filter down to only 2008 votes
  filter(demVote == max(demVote)) %>% # 2. Filter down to the highest `demVote`
  select(state) # 3. Select name of the state
most_dem_state

# Group observations by state
grouped <- group_by(presidentialElections, state)
grouped
View(grouped)

# Compute summary statistics by state: average percentages across the years
state_voting_summary <- presidentialElections %>%
  group_by(state) %>%
  summarize(
    mean_dem_vote = mean(demVote),
    mean_other_parties = mean(other_parties_vote)
  )
state_voting_summary

setwd("sources")
donations <- read.csv("Donations.csv", stringsAsFactors = FALSE)
donations

donors <- read.csv("Donors.csv", stringsAsFactors = FALSE)

# Combine (join) donations and donors data frames by their shared column
# ("donor_name")
combined_data <- left_join(donations, donors, by = "donor_name")
combined_data

# Combine (join) donations and donors data frames (see Figure 11.11)
combined_data <- left_join(donors, donations, by = "donor_name")
combined_data
