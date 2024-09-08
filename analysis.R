rm(list = ls())

library("tidyverse")
library("cowplot")
data = read_csv("./data/data.csv")


## Exploratory analysis to see what is important and what is not

data$uses_ai = TRUE
traditional_search = c("Google", "Bing", "Yahoo", "DuckDuckGo")
other_ai = "gemini"

data$uses_ai[data$which_tool %in% traditional_search] = FALSE
data$uses_ai[data$which_tool == "Other"] = NA
data$uses_ai[data$which_tool == "Other (please specify)" & data$other_text != "gemini"] = FALSE

data = subset(data, !is.na(data$uses_ai))

## Section 1: Check which variables are important
model = glm(
  uses_ai ~ gender + age + region + device + household_income,
  family="binomial",
  data = data
)
summary(model)

### GLM shows that gender, age, and region are important. Though there are too many splits
### Lets use Decision trees to see which splits are important

library("rpart")

cost_true = length(data$uses_ai)/sum(data$uses_ai)
cost_false = length(data$uses_ai)/sum(!data$uses_ai)

cost_matrix = matrix(1, nrow = 2, ncol = 2)
diag(cost_matrix) <- 0
cost_matrix[2,1] = cost_true
cost_matrix[1,2] = cost_false


tree_model <- rpart(
  uses_ai ~ gender + age + region + device + household_income,
  data = data,
  parms=list(loss=cost_matrix),
  control = rpart.control(cp = 0.01, minbucket = 20, minsplit = 40),
  method = "class"
)

plot(tree_model)
text(tree_model)
print(tree_model) 

### Region is very important
### Income: people with income between 25k-99k dont use AI .. low income (students?) and high income use it
### Lets simplify these two and try again

non_ai_regions = c(
  "east_north_central",
  "east_south_central",
  "mountain",
  "new_england",
  "west_north_central",
  "west_south_central"
)

data$is_ai_region = !(data$region %in% non_ai_regions)

data$income_range = "low"
mid_income = c("25000_to_49999", "50000_to_74999", "75000_to_99999")
data$income_range[data$household_income %in% mid_income] = "mid"
high_income = c("100000_to_124999", "125000_to_149999", "150000_to_174999", "175000_to_199999", "200000_and_up")
data$income_range[data$household_income %in% high_income] = "high"

tree_model <- rpart(
  uses_ai ~ gender + age + is_ai_region + device + income_range,
  data = data,
  parms=list(loss=cost_matrix),
  control = rpart.control(cp = 0.01, minbucket = 20, minsplit = 40),
  method = "class"
)

plot(tree_model)
text(tree_model)
print(tree_model) 

### If you are not in AI region 5% uses the tools, in AI region 22%
## Income, then age, gender, device

### Lets use GLM to see which ones are statistically significant

model = glm(
  uses_ai ~ gender + age + is_ai_region + device + income_range,
  family="binomial",
  data = data
)
summary(model)

# device is not significant. Gender, age (over 60), region, income are significant use those

data$age_over_60 = FALSE
data$age_over_60[data$age == "60_above"] = TRUE

tree_model <- rpart(
  uses_ai ~ gender + age_over_60 + is_ai_region + income_range,
  data = data,
  parms=list(loss=cost_matrix),
  control = rpart.control(cp = 0.01, minbucket = 20, minsplit = 40),
  method = "class"
)

plot(tree_model)
text(tree_model)
print(tree_model) 

## Leaves
## Non AI region: 4.7% AI tool use
## AI region, mid income, female 6.5%
## AI region, mid income, male 18.7%
## AI region High/low income under 60: 31%
## AI region High/low income over 60: 10%

# Lets see the AI and non ai regions on a map
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


## Q1) How prevalent are these tools in early adopters (vs late adopters)

plot_data = data %>%
  filter(!is.na(uses_ai)) %>%
  group_by(is_ai_region) %>%
  summarize(
    p_ai_tools = mean(uses_ai) * 100,
    n = n(),
    n_ai = sum(uses_ai),
    p_low = binom.test(n_ai, n)$conf.int[1] * 100,
    p_high = binom.test(n_ai, n)$conf.int[2] * 100
  )

# 1. Create the bar plot with confidence intervals
bar_plot <- ggplot(plot_data, aes(x = is_ai_region, y = p_ai_tools, fill = is_ai_region)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = p_low, ymax = p_high), width = 0.2) +
  scale_fill_manual(values = custom_palette) +
  labs(x = "Region", y = "Percent Using AI Tools", title = "AI Tool Usage by Region") +
  theme_minimal() +
  ylim(0, 30) +
  theme(legend.position = "none") + # No legend needed for the bars
  coord_flip()  # Make the bar plot horizontal
print(bar_plot)

# 2. Create the small map plot
map_plot <- ggplot(data = us_map, aes(x = long, y = lat, group = group, fill = region.y)) +
  geom_polygon(color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = custom_palette, name = "Region") +
  theme_void() +
  labs(title = "US Regions Map") +
  theme(legend.position = "none")  # No legend for the map


## Q3) ChatGPT vs Other AI tools what are the biggest differences in who chooses what?

ai_users = data %>%
  filter(uses_ai) %>%
  mutate(is_chat_gpt_user = which_tool == "ChatGPT")

tree_model <- rpart(
  is_chat_gpt_user ~ gender + age_over_60 + is_ai_region + income_range,
  data = ai_users,
  control = rpart.control(cp = 0.00000001),
  method = "class"
) # lol no splits


ai_users %>%
  filter(is_ai_region) %>%
  filter(!age_over_60) %>%
  filter(!(income_range == "mid")) %>%
  group_by(gender) %>%
  summarise(chat_gpt_rate = mean(is_chat_gpt_user))

model = glm(
  is_chat_gpt_user ~ gender + age + is_ai_region + device + income_range,
  family="binomial",
  data = ai_users
)
summary(model)

# chat GPT is king just do early adopters vs not

## Q4) Where is the gender gap the largest?
gender_gap = data %>%
  group_by(age_over_60, is_ai_region, income_range, gender) %>% 
  summarise(usage_rate = mean(uses_ai)) %>% 
  spread(gender, usage_rate) %>%
  mutate(gender_gap = male/female) %>%
  arrange(desc(gender_gap))

# Non adopters: gender gap is not there no one is adopting
# adopters: low income group best, mid income worst

## 
          