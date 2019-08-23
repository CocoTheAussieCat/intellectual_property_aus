library(jsonlite)
library(httr)
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(RColorBrewer)


### Import data from data.gov.au ----------------------------------------------
## Run only when .csv are updated
patent_df <- read.csv(here::here("data", "ipgod101.csv"), stringsAsFactors = F)
applicant_df <- read.csv(here::here("data", "ipgod102.csv"), stringsAsFactors = F)
attorney_df <- read.csv(here::here("data", "ipgod106.csv"), stringsAsFactors = F)
patent_info_df <- read.csv(here::here("data", "ipgod107.csv"), stringsAsFactors = F)

# Convert australian_appl_no to characters in attorney_df and patent_info_df
attorney_df$australian_appl_no <- as.character(attorney_df$australian_appl_no)
patent_info_df$australian_appl_no <- as.character(patent_info_df$australian_appl_no)

# Convert filing_date and sealing_date into date data type
patent_info_df$filing_date <- as.Date(patent_info_df$filing_date, "%Y-%m-%d")
patent_info_df$sealing_date <- as.Date(patent_info_df$sealing_date, "%Y-%m-%d")

# Convert firm_grp into factor in attorney_df
attorney_df$firm_grp <- as.factor(attorney_df$firm_grp)

# Select filing and sealing dates from patent_info_df
patent_dates_df <- patent_info_df %>% 
  select(australian_appl_no, filing_date, sealing_date)

# Join attorney_df to patent_df using australian_appl_no as primary key
patent_attorney_df <- inner_join(patent_df, attorney_df, by = "australian_appl_no")

# Join patent_dates to patent_attorney_df using australian_appl_no as primary key
patent_attorney_df <- inner_join(patent_attorney_df, patent_dates_df, 
                                 by = "australian_appl_no")

# Add half year variable where Jan-Jun = 1H; Jul-Dec = 2H
patent_attorney_df <- patent_attorney_df %>% 
  mutate(filing_half_year = 
           if_else(
             month(patent_attorney_df$filing_date) <= 6, 
             paste0(year(patent_attorney_df$filing_date), "-01-01"), 
             paste0(year(patent_attorney_df$filing_date), "-07-01")
             )
         ) %>%
  mutate(sealing_half_year = 
           if_else(
             month(patent_attorney_df$sealing_date) <= 6, 
             paste0(year(patent_attorney_df$sealing_date), "-01-01"), 
             paste0(year(patent_attorney_df$sealing_date), "-07-01")
           )
         ) %>% 
  mutate(filing_hy = 
            if_else(
              month(patent_attorney_df$filing_date) <= 6, "1H", "2H"
            )
   ) %>%
  mutate(sealing_hy = 
              if_else(
                month(patent_attorney_df$sealing_date) <= 6, "1H", "2H"
              )
  )

# Converting filing_half_year and sealing_half_year to date data format for plots
patent_attorney_df$filing_half_year <- 
  as.Date(patent_attorney_df$filing_half_year, "%Y-%m-%d")
patent_attorney_df$sealing_half_year <- 
  as.Date(patent_attorney_df$sealing_half_year, "%Y-%m-%d")

# Change "" in firm_grp to "Other"
levels(patent_attorney_df$firm_grp)[1] <- "Other"
levels(patent_attorney_df$firm_grp)[3] <- "Privately held"
patent_attorney_df$firm_grp <- ordered(patent_attorney_df$firm_grp, levels = c("Other", "Privately held", "XIP", "QIP", "IPH"))

levels(patent_attorney_df$firm_grp)

saveRDS(patent_attorney_df, here::here("ip_app", "patent_attorney_df.rds"))


### Create IP share bar chart -------------------------------------------------

firm_df <- patent_attorney_df %>% 
  group_by(firm_grp, filing_half_year, filing_hy) %>% 
  summarise(no_applications = n())

total_app_df <- firm_df %>% 
  group_by(filing_half_year) %>% 
  summarise(total_applications = sum(no_applications)) %>% 
  ungroup(filing_half_year)

firm_df <-  firm_df %>% 
  left_join(total_app_df, by = "filing_half_year") %>% 
  mutate(pct_total = no_applications/total_applications) %>% 
  select(firm_grp, filing_half_year, filing_hy, no_applications, 
         total_applications, pct_total)

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2018-07-01")

plot_df <- firm_df %>% 
  filter(filing_half_year >= start_date &
           filing_half_year <= end_date) %>% 
  mutate(hy_label = paste0(year(filing_half_year), "-", filing_hy))
 
plot_df %>% 
  ggplot() +
  geom_col(aes(x = hy_label, y = pct_total, fill = firm_grp)) +
  geom_label(aes(x = hy_label, y = pct_total, label = round(pct_total*100, 0)), 
             label.size = 0) +
  theme_minimal() +
  labs(title = "Percent of patent applications by firm", x = "", y = "", caption = "Source: data.gov.au") +
  scale_color_brewer(palette = "Spectral") +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_y_continuous(labels = percent_format(accuracy = 1, big.mark = ",")) +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) +
  theme(plot.title = element_text(face = "bold", size = 16)) +
  theme(axis.text.x = element_text(angle = 90))

# Create list of brands included in each firm_grp
firm_list <- patent_attorney_df %>% 
  filter(firm_grp != "Other" & firm_grp != "PVT") %>% 
  group_by(cleanname, firm_grp) %>% 
  summarise(total_applications = n()) %>% 
  arrange(firm_grp, cleanname)