
donor <- read.csv(file.choose())

donors <- donors %>%
  mutate(across(starts_with("CY."), ~ as.numeric(gsub("[$,]", "", .))))

# Create a total donation column
donors <- donors %>%
  mutate(Total_Donations = CY.20 + CY.21 + CY.22 + CY.23 + CY.24)

# Calculate donation growth (2024 compared to 2020)
donors <- donors %>%
  mutate(Donation_Growth = (CY.24 - CY.20) / CY.20)



# Reshape data from wide to long format
donors_long <- donors %>%
  select(CnBio_ID, CY.20, CY.21, CY.22, CY.23) %>% 
  pivot_longer(cols = starts_with("CY."),
               names_to = "Year",
               names_prefix = "CY.",
               values_to = "Donation") %>%
  mutate(Year = as.numeric(Year) )  # Convert to full year (20 -> 2020, etc.)

# Function to fit a linear model and extract the slope
get_slope <- function(data) {
  model <- lm(Donation ~ Year, data = data)
  slope <- coef(model)["Year"]
  return(slope)
}

# Apply the function to each donor
donor_slopes <- donors_long %>%
  group_by(CnBio_ID) %>%  # Replace with the actual donor ID column name
  nest() %>%
  mutate(Slope = map_dbl(data, get_slope)) %>%
  select(CnBio_ID, Slope)


donors <- merge(donors, donor_slopes, by = "CnBio_ID")

# Categorize donors based on donation patterns
donors <- donors %>%
  mutate(Donation_Pattern = case_when(
    Slope > 0 ~ "Increasing",
    Slope < 0 ~ "Decreasing",
    Slope == 0 ~ "Consistent"
  ))


write.csv(donors, "/Users/000766412/Library/CloudStorage/OneDrive-DrakeUniversity/Documents/DMARC non-research/all_donors.csv")

