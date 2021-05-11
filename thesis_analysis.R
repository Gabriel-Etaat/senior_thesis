# File:      thesis_analysis.R
# Author:    Gabriel Etaat
# Created:   01 March 2020
# Last Edit: 20 April 2020
# Purpose:   To subset formatted data and employ regression analysis

# ------------------------- #
#                           #
#           Setup           #
#                           #
# ------------------------- #

# clear memory
rm(list=ls())

# directory
my_dir  <- "C:/Users/Etaat/Downloads/senior_thesis"
setwd(my_dir)

# libraries
library(plm)
library(tidyverse)
library(gtools)
library(ggpubr)
library(stargazer)

#data
master_df <- read.csv('master_df.csv')

# ---------------------- #
#                        #
#        Chapter 4       #
#                        #
# ---------------------- #

# ----- Subsets & Vectors----- #

cali_df <- master_df %>%
  filter(county_code == "6000")
#master_df but only for California

county_df_all <- master_df %>%
  filter(county_code != "6000") 
#master_df but w/o California

county_df <- county_df_all %>%
  subset(total_mf_sf_perm > 0) %>%
  mutate(
    over_40 = ifelse(per_cap_inc >= 40000, 1, 0),
    after_fire= ifelse(((year_1 == 1 | year_2 == 1 | year_3 == 1 | year_2 == 1 |
                       year_3 == 1 | year_4 == 1 | year_5 == 1 | year_6 == 1 |
                       year_7 == 1) != 0), 1, 0)
  )
#master_df but w/o California, elimiating 0 permit county/years for log analysis,
#binary per_cap variable at $40,000 since mean/median is in $36000-$39000 range,
#indicator for any of a range of years after a fire (1-7)

county_sf_df <- county_df %>%
  subset(total_sf_perm > 0)
county_mf_df <- county_df %>%
  subset(total_mf_perm > 0)

county_sum_df <- county_df %>%
  group_by(county, after_fire) %>%
  summarise(post_fire_permits = sum(total_mf_sf_perm))
county_sum_df <- left_join(county_sum_df, county_df, by = c("county", "after_fire"))
#Differentiate permits distributed either w/in or not w/in the fire recovery period,
#then left-join back the data

county_df_indic <- county_df %>%
  group_by(county) %>%
  mutate(fire_county = ifelse(any(struc_dest != 0), 1, 0)) 
#binary variable for whether a county is a "fire" county

fire_county_df <- county_df_all %>%
  mutate(highlight=ifelse(struc_dest != 0, T, F)) %>%
  group_by(county) %>%
  filter(any(struc_dest != 0))
#master_df but only for counties with fires observed

fire_counties_vec <- master_df %>%
  filter(struc_dest != 0) %>%
  select(county) %>%
  distinct()
#vector of the 13 distinct primary counties impacted by the observed fires

lead_lag_perm <- master_df %>%
  subset(year_b4 == 1 | 
           year_0 == 1 |
           year_1 == 1 |
           year_2 == 1
  ) %>%
  select(fire_name, year, county, struc_dest, total_mf_sf_perm)
#permits only in year range around fire

firec_labs <- c('Fire', 'No Fire')
names(firec_labs) <- c(0, 1)
#labels for figure 4

# ------ Chapter 4 Plots/Tables ----- #

calpermtrnds_graph <- ggplot(cali_df, aes(year, total_mf_sf_perm)) +
  geom_point(color='blue') +
  geom_smooth(method='loess', se = F) +
  ylim(c(0, 225000)) +
  labs(title='Figure 1: California Housing Permits Issued Over Time', y='Total Permits Issued',
       x='Year', caption='Source: U.S. Department of Housing and Urban Development') + 
  theme_bw()
#figure 1, 8x6

calpermtrnds_table <- cali_df %>%
  select(year, total_mf_sf_perm, total_sf_mf_exist, population) %>%
  slice(which(row_number() %% 5 == 1))

calhouse_1 <- ggplot(cali_df, aes(x = year)) +
  geom_line(aes(y = exist_gr, color = 'blue'), size = 1.5) +
  geom_line(aes(y = pop_gr, color = 'black'), size = 1.5) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = c('blue', 'black'), labels = c('Existing Housing', 'Population')) +
  labs(title='Figure 2: California Population, Existing Housing, and Housing Permits Growth Rates 
       Over Time', y = 'Growth Rate (%)') 

calhouse_2 <- ggplot(cali_df, aes(x = year)) +
  geom_line(aes(y = permit_gr, color = 'red'), size = 1.5) +
  scale_color_manual(values = 'red', labels = 'Housing Permits') +
  labs(caption='Sources: U.S. Department of Housing and Urban Development, California State Association
       of Counties & U.S. Bureau of Economic Analysis', x = 'Year',
       y = 'Growth Rate (%)') 

calhouse_pop <- ggarrange(calhouse_1, calhouse_2, ncol = 1, nrow = 2) 
#figure 2, 7.5x4.5

fire_county_permits <- ggplot(fire_county_df, aes(year, total_mf_sf_perm)) + 
  geom_point(aes(color=fire_county_df$highlight)) + 
  scale_color_manual(values = c('#595959', 'red')) +
  geom_smooth(method='loess', se = F) + 
  facet_wrap( ~ county, scales = 'free_y', ncol = 3) +
  theme(legend.position = 'bottom') +
  labs(title='Figure 3: Housing Permits Issued For Observed "Fire" Counties Over Time', y='Total Permits Issued', 
       x='Year', caption='Source: U.S. Department of Housing and Urban Development', color = "Fire Year") 
#figure 3, 8x8

nofire_county_permits <- ggplot(county_df_indic, aes(year, total_mf_sf_perm)) +
  geom_smooth(method='loess', se = F) + 
  facet_wrap( ~ fire_county, labeller = labeller(fire_county = firec_labs)) +
  labs(title='Figure 4: Housing Permits Issued For Observed "Fire" vs. "No Fire" Counties Over Time', 
       y='Total Permits Issued',x='Year', caption='Source: U.S. Department of Housing and Urban Development') +
  theme_bw()
#figure 4, 8x6

# ---------------------- #
#                        #
#        Chapter 5       #
#                        #
# ---------------------- #


# ----- Regressions -----#



model_1 <- plm(log(total_mf_sf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#primary model

model_test <- plm(log(total_mf_sf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1),
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#test model, comparison

model_2 <- plm(log(total_mf_sf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 struc_dest +
                 lead(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40 +
                 struc_dest*over_40 +
                 lead(struc_dest, 1)*over_40,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#original main model, with lead(1) and year of

model_3 <- plm(log(total_mf_sf_perm) ~
                  lag(struc_dest, 7) +
                  lag(struc_dest, 6) +
                  lag(struc_dest, 5) +
                  lag(struc_dest, 4) +
                  lag(struc_dest, 3) +
                  lag(struc_dest, 2) +
                  lag(struc_dest, 1) +
                  per_cap_inc,
                data = subset(county_df, over_40 == 0),
                index = c("county", "year"),
                model = "within",
                effect = "twoways"
)
model_4 <- plm(log(total_mf_sf_perm) ~
                  lag(struc_dest, 7) +
                  lag(struc_dest, 6) +
                  lag(struc_dest, 5) +
                  lag(struc_dest, 4) +
                  lag(struc_dest, 3) +
                  lag(struc_dest, 2) +
                  lag(struc_dest, 1) +
                  per_cap_inc,
                data = subset(county_df, over_40 == 1),
                index = c("county", "year"),
                model = "within",
                effect = "twoways"
)
#over_40 yes no comparison

model_5 <- plm(log(total_mf_sf_perm) ~
                  lead(struc_dest, 7) +
                  lead(struc_dest, 6) +
                  lead(struc_dest, 5) +
                  lead(struc_dest, 4) +
                  lead(struc_dest, 3) +
                  lead(struc_dest, 2) +
                  lead(struc_dest, 1) +
                  struc_dest,           
                data = county_df,
                index = c("county", "year"),
                model = "within",
                effect = "twoways"
)
#follow up years, nothing else

model_6 <- plm(log(total_mf_sf_perm) ~
                 lead(struc_dest, 7) +
                 lead(struc_dest, 6) +
                 lead(struc_dest, 5) +
                 lead(struc_dest, 4) +
                 lead(struc_dest, 3) +
                 lead(struc_dest, 2) +
                 lead(struc_dest, 1) +
                 struc_dest +
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#follow up years, with model_1***

model_7 <- plm(log(total_sf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40,
               data = county_sf_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
model_8 <- plm(log(total_mf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40,
               data = county_mf_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#sf/mf comparison

model_9 <- plm(log(total_mf_sf_perm) ~
                  lag(struc_dest, 7) +
                  lag(struc_dest, 6) +
                  lag(struc_dest, 5) +
                  lag(struc_dest, 4) +
                  lag(struc_dest, 3) +
                  lag(struc_dest, 2) +
                  lag(struc_dest, 1) +
                  per_cap_inc,
                data = subset(county_df, over_40 == 1),
                index = c("county", "year"),
                model = "within",
                effect = "twoways"
)
model_10 <- plm(log(total_mf_sf_perm) ~
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 per_cap_inc +
                 lag(struc_dest, 7)*per_cap_inc +
                 lag(struc_dest, 6)*per_cap_inc +
                 lag(struc_dest, 5)*per_cap_inc +
                 lag(struc_dest, 4)*per_cap_inc +
                 lag(struc_dest, 3)*per_cap_inc +
                 lag(struc_dest, 2)*per_cap_inc +
                 lag(struc_dest, 1)*per_cap_inc,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#models 9 and 10 use per_cap_inc, not binary...why no significance here?

model_11 <- plm(log(total_mf_sf_perm) ~
                 lag(year_0, 7) +
                 lag(year_0, 6) +
                 lag(year_0, 5) +
                 lag(year_0, 4) +
                 lag(year_0, 3) +
                 lag(year_0, 2) +
                 lag(year_0, 1) +
                 over_40 +
                 lag(year_0, 7)*over_40 +
                 lag(year_0, 6)*over_40 +
                 lag(year_0, 5)*over_40 +
                 lag(year_0, 4)*over_40 +
                 lag(year_0, 3)*over_40 +
                 lag(year_0, 2)*over_40 +
                 lag(year_0, 1)*over_40,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#bin-bin experiment

model_12 <- plm(log(total_mf_sf_perm) ~
                  lag(struc_dest, 7) +
                  lag(struc_dest, 6) +
                  lag(struc_dest, 5) +
                  lag(struc_dest, 4) +
                  lag(struc_dest, 3) +
                  lag(struc_dest, 2) +
                  lag(struc_dest, 1) +
                  struc_dest +
                  lead(struc_dest, 1) +
                  over_40 +
                  lag(struc_dest, 7)*over_40 +
                  lag(struc_dest, 6)*over_40 +
                  lag(struc_dest, 5)*over_40 +
                  lag(struc_dest, 4)*over_40 +
                  lag(struc_dest, 3)*over_40 +
                  lag(struc_dest, 2)*over_40 +
                  lag(struc_dest, 1)*over_40 +
                  struc_dest*over_40,
                data = county_df,
                index = c("county", "year"),
                model = "within",
                effect = "twoways"
)
#proving insignificance of year before coefficient from model_2

model_13 <- plm(log(total_mf_sf_perm) ~
                 lag(struc_dest, 12) +
                 lag(struc_dest, 11) + 
                 lag(struc_dest, 10) +
                 lag(struc_dest, 9) +
                 lag(struc_dest, 8) +
                 lag(struc_dest, 7) +
                 lag(struc_dest, 6) +
                 lag(struc_dest, 5) +
                 lag(struc_dest, 4) +
                 lag(struc_dest, 3) +
                 lag(struc_dest, 2) +
                 lag(struc_dest, 1) +
                 over_40 +
                 lag(struc_dest, 12)*over_40 +
                 lag(struc_dest, 11)*over_40 +
                 lag(struc_dest, 10)*over_40 +
                 lag(struc_dest, 9)*over_40 +
                 lag(struc_dest, 8)*over_40 +
                 lag(struc_dest, 7)*over_40 +
                 lag(struc_dest, 6)*over_40 +
                 lag(struc_dest, 5)*over_40 +
                 lag(struc_dest, 4)*over_40 +
                 lag(struc_dest, 3)*over_40 +
                 lag(struc_dest, 2)*over_40 +
                 lag(struc_dest, 1)*over_40,
               data = county_df,
               index = c("county", "year"),
               model = "within",
               effect = "twoways"
)
#more years than model_7


# ----- Further Analysis ----- #

avg_fire_county <- county_df_all %>%
  filter(county_code == 6005 | county_code == 6007 | county_code == 6017 |
         county_code == 6033 | county_code == 6037 | county_code == 6045 |
         county_code == 6055 | county_code == 6071 | county_code == 6073 |
         county_code == 6089 | county_code == 6097 | county_code == 6111 | 
         county_code == 6115) %>%
  group_by(year) %>%
  summarize(
    total_mf_sf_perm = mean(total_mf_sf_perm),
    total_sf_perm = mean(total_sf_perm),
    total_mf_perm = mean(total_mf_perm),
    total_sf_mf_exist = mean(total_sf_mf_exist),
    per_cap_inc = mean(per_cap_inc)
    )

# ----- Tables ----- #

table_3 <- stargazer(model_1, model_test, title = 'Table 3: Primary Model With and Without Income Indicator',
                     type = 'html', align = T, digits = 8, out = 'table_3.html')
table_4 <- stargazer(model_5, title = 'Table 4: Fire Impacts on Housing During and Before Its Occurance',
                     type = 'html', align = T, digits = 8, out = 'table_4.html')
table_5 <- stargazer(model_7, model_8, title = 'Table 5: Primary Model Assessment with Single-Family and Multi-Family Permit Comparison',
                     type = 'html', align = T, digits = 8, out = 'table_5.html')
table_6 <- stargazer(model_1, model_13, title = 'Table 6: Primary Model Comparison to 12-Year Equivalent',
                     type = 'html', align = T, digits = 8, out = 'table_6.html')
table_7 <- stargazer(as.data.frame(avg_fire_county), title = 'Table 7: "Average Fire County" Scenario',
                     type = 'html', align = T, out = 'table_7.html')
