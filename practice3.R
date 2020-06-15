# Load the `nycflights13` package to access the `flights` data frame
install.packages("nycflights13") # once per machine
library("nycflights13")          # in each relevant script

# Getting to know the `flights` data set
?flights          # read the available documentation
dim(flights)      # check the number of rows/columns
colnames(flights) # inspect the column names
View(flights)     # look at the data frame in the RStudio Viewer

#
# Q : Which airline has the highest number of delayed departures?
#
# 1. Since you want to consider all the flights from a particular airline (based on the carrier feature), 
# you will first want to group the data by that feature.
# 2. You need to figure out the largest number of delayed departures (based on the dep_delay feature)—
# which means you need to find the flights that were delayed (filtering for them).
# 3. You can take the found flights and aggregate them into a count (summarize the different groups).
# 4. You will then need to find which group has the highest count (filtering).
# 5. Finally, you can choose (select) the airline of that group.

# Identify the airline (`carrier`) that has the highest number of
# delayed flights
has_most_delays <- flights %>%            # start with the flights
  group_by(carrier) %>%                   # group by airline (carrier)
  filter(dep_delay > 0) %>%               # find only the delays
  summarize(num_delay = n()) %>%          # count the observations
  filter(num_delay == max(num_delay)) %>% # find most delayed
  select(carrier)                         # select the airline
has_most_delays

head(airlines)

# Get name of the most delayed carrier
most_delayed_name <- has_most_delays %>%  # start with the previous answer
  left_join(airlines, by = "carrier") %>% # join on airline ID
  select(name)                            # select the airline name
most_delayed_name
print(most_delayed_name$name)


#
# 2.On average, to which airport do flights arrive most early?
#
# Calculate the average arrival delay (`arr_delay`) for each destination
# (`dest`)
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay)) # compute mean delay
head(most_early)#

# Compute the average delay by destination airport, omitting NA results
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) # compute mean delay
head(most_early)

# Identify the destination where flights, on average, arrive most early
most_early <- flights %>%
  group_by(dest) %>% # group by destination
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # compute mean delay
  filter(delay == min(delay, na.rm = TRUE)) %>% # filter for least delayed
  select(dest, delay) %>% # select the destination (and delay to store it)
  left_join(airports, by = c("dest" = "faa")) %>% # join on `airports` data
  select(dest, name, delay) # select output variables of interest

print(most_early)
# A tibble: 1 x 3
#  dest  name       delay
#  <chr> <chr>      <dbl>
#1 LEX   Blue Grass   -22

# Q : In which month do flights tend to have the longest delays?
#
# Identify the month in which flights tend to have the longest delays
flights %>%
  group_by(month) %>% # group by selected feature
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% # summarize delays
  filter(delay == max(delay)) %>% # filter for the record of interest
  select(month) %>% # select the column that answers the question
  print() # print the tibble out directly
# A tibble: 1 x 1
#  month
#  <int>
#1   7

# Compute delay by month, adding month names for visual display
# Note, `month.name` is a variable built into R
delay_by_month <- flights %>%
  group_by(month) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%
  select(delay) %>%
  mutate(month = month.name)

install.packages("plotly") # once per machine
library("plotly")          # in each relevant script

# Create a plot using the ggplot2 package (described in Chapter 17)
ggplot(data = delay_by_month) +
  geom_point(
    mapping = aes(x = delay, y = month),
    color = "blue",
    alpha = .4,
    size = 3
  ) +
  geom_vline(xintercept = 0, size = .25) +
  xlim(c(-20, 20)) +
  scale_y_discrete(limits = rev(month.name)) +
  labs(title = "Average Delay by Month", y = "", x = "Delay (minutes)")



