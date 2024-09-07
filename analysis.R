rm(list = ls())

library("tidyverse")

data = read_csv("./data/data.csv")


## lets first check use of AI tools

data$is_ai = TRUE
traditional_search = c("Google", "Bing", "Yahoo", "DuckDuckGo")
other_ai = "gemini"

data$is_ai[data$which_tool %in% traditional_search] = FALSE
data$is_ai[data$which_tool == "Other"] = NA
data$is_ai[data$which_tool == "Other (please specify)" & data$other_text != "gemini"] = FALSE


model = glm(is_ai ~ gender + age + region + device + household_income, family="binomial", data = subset(data, !is.na(data$is_ai)))
summary(model)


model = glm(is_ai ~ gender + age + region, family="binomial", data = subset(data, !is.na(data$is_ai)))
summary(model)

library("rpart")

tree_model <- rpart(
  is_ai ~ gender + age + region + device + household_income,
  data = subset(data, !is.na(data$is_ai)),
  control = rpart.control(cp = 0.001, minbucket = 10, minsplit = 20),
  method = "class"
)
summary(tree_model)

plot(tree_model)
text(tree_model)
print(tree_model)

# If you are from regions (east_north_central,east_south_central,mountain,new_england,west_north_central,west_south_central)
# you dont use AI tools as often
library("maps")

state_regions <- data.frame(
  state = tolower(state.name),
  region = c(
    "south_atlantic",      # Alabama
    "pacific",             # Alaska
    "mountain",            # Arizona
    "west_south_central",  # Arkansas
    "pacific",             # California
    "mountain",            # Colorado
    "new_england",         # Connecticut
    "south_atlantic",      # Delaware
    "south_atlantic",      # Florida
    "south_atlantic",      # Georgia
    "pacific",             # Hawaii
    "mountain",            # Idaho
    "east_north_central",  # Illinois
    "east_north_central",  # Indiana
    "west_north_central",  # Iowa
    "west_north_central",  # Kansas
    "east_south_central",  # Kentucky
    "west_south_central",  # Louisiana
    "new_england",         # Maine
    "south_atlantic",      # Maryland
    "new_england",         # Massachusetts
    "east_north_central",  # Michigan
    "west_north_central",  # Minnesota
    "east_south_central",  # Mississippi
    "west_north_central",  # Missouri
    "mountain",            # Montana
    "west_north_central",  # Nebraska
    "mountain",            # Nevada
    "new_england",         # New Hampshire
    "middle_atlantic",     # New Jersey
    "mountain",            # New Mexico
    "middle_atlantic",     # New York
    "south_atlantic",      # North Carolina
    "west_north_central",  # North Dakota
    "east_north_central",  # Ohio
    "west_south_central",  # Oklahoma
    "pacific",             # Oregon
    "middle_atlantic",     # Pennsylvania
    "new_england",         # Rhode Island
    "south_atlantic",      # South Carolina
    "west_north_central",  # South Dakota
    "east_south_central",  # Tennessee
    "west_south_central",  # Texas
    "mountain",            # Utah
    "new_england",         # Vermont
    "south_atlantic",      # Virginia
    "pacific",             # Washington
    "south_atlantic",      # West Virginia
    "east_north_central",  # Wisconsin
    "mountain"             # Wyoming
  )
)


# Get US map data
us_map <- map_data("state")

# Join the map data with our region data
us_map <- left_join(us_map, state_regions, by = c("region" = "state"))

custom_palette <- c(
  "new_england" = "#E41A1C",
  "middle_atlantic" = "#377EB8",
  "east_north_central" = "#4DAF4A",
  "west_north_central" = "#984EA3",
  "south_atlantic" = "#FF7F00",
  "east_south_central" = "#FFFF33",
  "west_south_central" = "#A65628",
  "mountain" = "#F781BF",
  "pacific" = "#999999"
)

# Create the plot
ggplot(data = us_map, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = custom_palette, name = "Region") +
  theme_void() +
  labs(title = "US Regions Map")


## Lets look at (east_north_central,east_south_central,mountain,new_england,west_north_central,west_south_central)

non_ai_regions = c(
  "east_north_central",
  "east_south_central",
  "mountain",
  "new_england",
  "west_north_central",
  "west_south_central"
)


custom_palette <- c(
  "new_england" = "#E41A1C",
  "east_north_central" = "#E41A1C",
  "west_north_central" = "#E41A1C",
  "east_south_central" = "#E41A1C",
  "west_south_central" = "#E41A1C",
  "mountain" = "#E41A1C",
  "pacific" = "#999999",
  "middle_atlantic" = "#999999",
  "south_atlantic" = "#999999"
  
)

# Create the plot
ggplot(data = us_map, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = custom_palette, name = "Region") +
  theme_void() +
  labs(title = "US Regions Map")

data %>%
  filter(!is.na(is_ai)) %>%
  mutate(is_ai_region = !(region %in% non_ai_regions)) %>%
  group_by(is_ai_region) %>%
  summarize(p_ai = mean(is_ai))


data %>%
  filter(!is.na(is_ai)) %>%
  mutate(is_ai_region = !(region %in% non_ai_regions)) %>%
  group_by(is_ai_region, gender) %>%
  summarize(p_ai = mean(is_ai))


data %>%
  filter(!is.na(is_ai)) %>%
  mutate(is_ai_region = !(region %in% non_ai_regions)) %>%
  group_by(gender) %>%
  summarize(p_ai = mean(is_ai))


data %>%
  filter(!is.na(is_ai)) %>%
  group_by(region) %>%
  summarize(p_ai = mean(is_ai))


data %>%
  filter(!is.na(is_ai)) %>%
  group_by(over_60) %>%
  summarize(p_ai = mean(is_ai), n=n())


data %>%
  filter(!is.na(is_ai)) %>%
  group_by(is_ai_region) %>%
  summarize(p_ai = mean(is_ai), n=n())


data$income_range = "low"
mid_income = c("25000_to_49999", "50000_to_74999", "75000_to_99999")
data$income_range[data$household_income %in% mid_income] = "mid"
high_income = c("100000_to_124999", "125000_to_149999", "150000_to_174999", "175000_to_199999", "200000_and_up")
data$income_range[data$household_income %in% high_income] = "high"


data$over_60 = FALSE
data$over_60[data$age == "60_above"] = TRUE

x = data %>%
  filter(!is.na(is_ai)) %>%
  mutate(is_ai_region = !(region %in% non_ai_regions)) %>%
  group_by(is_ai_region, over_60, income_range, gender) %>%
  summarize(p_ai = mean(is_ai), n=n())

data$is_ai_region = !(data$region %in% non_ai_regions)


model = glm(is_ai ~ gender + age + region + device + household_income, family="binomial", data = subset(data, !is.na(data$is_ai)))
summary(model)


model = glm(is_ai ~ gender + over_60 + is_ai_region + income_range, family="binomial", data = subset(data, !is.na(data$is_ai)))
summary(model)

library("rpart")

tree_model <- rpart(
  is_ai ~ gender + over_60 + is_ai_region + income_range,
  data = subset(data, !is.na(data$is_ai)),
  control = rpart.control(cp = 0.000000000000000001),
  method = "class"
)
summary(tree_model)

plot(tree_model)
text(tree_model)
print(tree_model)




#### properly weight the cost matrix
data = subset(data, !is.na(data$is_ai))
cost_true = length(data$is_ai)/sum(data$is_ai)
cost_false = length(data$is_ai)/sum(!data$is_ai)

cost_matrix = matrix(1, nrow = 2, ncol = 2)
diag(cost_matrix) <- 0
cost_matrix[2,1] = cost_true
cost_matrix[1,2] = cost_false


tree_model <- rpart(
  is_ai ~ gender + age + region + device + household_income,
  data = data,
  parms=list(loss=cost_matrix),
  control = rpart.control(cp = 0.01, minbucket = 10, minsplit = 20),
  method = "class"
)
summary(tree_model)

plot(tree_model)
text(tree_model)
print(tree_model)



tree_model <- rpart(
  is_ai ~ gender + over_60 + is_ai_region + income_range,
  data = data,
  parms=list(loss=cost_matrix),
  control = rpart.control(cp = 0.01, minbucket = 10, minsplit = 20),
  method = "class"
)
summary(tree_model)

plot(tree_model)
text(tree_model)
print(tree_model)


library(FSelector)


