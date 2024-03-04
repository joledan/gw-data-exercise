# title: gw_exercise.R
# by: jan oledan
# description: load deals data, generate visualizations on 
#               financiers of agribusiness 
# date: march 4, 2024

##### load packages #####
rm(list=ls())

# Package names
packages <- c("tidyverse", "camcorder", 
              "janitor", "ggtext", "showtext",
              "gghighlight")

# load packages
lapply(packages, library, character.only = TRUE)

# load font 
font_add_google("Noto Sans")
showtext_opts(dpi = 300)
showtext_auto()

##### set up main directory #####
main <- #<LINK TO MAIN FOLDER HERE>
plots <- paste(main, "plots", sep = "/")

# remove scientific notation
options(scipen = 999)


##### define theme for plots #####
# do this so we don't have to repeat things when making plots 
th <- theme_minimal(base_family = "Noto Sans") + 
  theme(panel.border = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
        plot.title = element_markdown(face = "bold", # plot title, subtitle, caption text size and margins
                                      size = 12,
                                      margin = margin(t=0, r=0, b=2, l=0, "pt")),
        plot.subtitle = element_markdown(size = 8,
                                         margin = margin(t=0,r=0,b=4,l=0, "pt"),
                                         lineheight = 1.2),
        plot.caption = element_markdown(hjust = 0,
                                        size = 6,
                                        color = "#999999",
                                        margin = margin(t=4,r=0,b=0,l=0, "pt")),
        plot.title.position = "plot", # align title text and caption text to plot
        plot.caption.position = "plot") +
  theme(axis.title = element_blank(),
        axis.title.x = element_blank(), # adjust x axis title
        axis.title.y = element_blank(), # adjust y axis title
        axis.text.x = element_text(size = 8, # make axis text (labels) size 8
                                   colour = "#000000"),
        axis.text.y = element_text(size = 8,
                                   colour = "#000000",
                                   vjust = 0.5,
                                   hjust = 0),
        axis.ticks.length.x = unit(4, "pt"), # define tick length and colour for x-axis
        axis.ticks.x = element_line(colour="#000000"),
        axis.ticks.y = element_blank(), # remove y axis ticks
        axis.line.x = element_line(colour = "#000000", 
                                   lineend = "square"), # adjust x axis line
        panel.grid.major.x = element_blank(), # remove major x lines
        panel.grid.minor.x = element_blank(), # remove minor x lines
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") # remove legend



### intialize camcorder to set up and output rectangle plots ###
camcorder::gg_record(
  dir = plots,
  device = "png",
  scale = 1,
  width = 1800,
  height = 1050,
  units = "px",
  dpi = 300,
  bg = "white"
)

# load data frame
# make profit variable based on segment_adjuster*income
# remove income variables that are NaN (not a number) - make note of this
deals <- paste(main, "deals.csv", sep = "/")

# read data
# construct profit
  # segment_adjuster: the proportion of the recipient's income that is 
                      # estimated to be derived from agribusiness (/1)
  # value: the value of the deal in USD
  # income: the income derived from the deal in USD
# make profit = segment_adjuster*income, i.e. the profit a firm (recipient)
  # makes from agribusiness from the deal (estimated)
# note, there are some observations where income == NaN (not a number), drop these for now
df <- read_csv(deals) %>%
  mutate(profit = segment_adjuster*income) %>%
  filter(!is.nan(income))

# count number of unique investors
n_distinct(df$investor)
# result: 1528 unique investors funded deforestation-related activities

# count number of investors by region
df %>% 
  select(investor, investor_region) %>%
  unique() %>%
  group_by(investor_region) %>%
  summarize(n = n_distinct(investor))
# result: 
# 1 China             128
# 2 EU & UK           325
# 3 United States    1076


##### how much money goes to each region? #####
# group by investor region, divide total by 1 million
df %>%
  group_by(investor_region) %>%
  summarize(total_profit = sum(profit)/1000000)
# result - EU+UK based banks made 646 million, then China, and US
# 1 China                   554.
# 2 EU & UK                 646.
# 3 United States           538.

df %>% summarize(total_profit =  sum(profit)/1000000)
# total income made is $1.738 billion in deforestation-related activities


##### plot 1 - profit by country #####
# show the breakdown of profit by country 
# calculate total profit by investor country
# rank the profit, and plot top 6 + others
# if rank > 6, group the profit into "Others"
df1 <- df %>%
  group_by(investor_country) %>%
  summarize(total_profit = sum(profit)/1000000) %>%
  ungroup() %>%
  arrange(desc(total_profit)) %>% # arrange the data in descending order based on profit
  mutate(rank = rank(desc(total_profit)), # rank the data in descending order of profit
         investor_country = if_else(rank > 6, "Others", investor_country)) %>%
  group_by(investor_country) %>% # recompute total_profit again, with "Others" group
  summarize(total_profit = sum(total_profit)) %>%
  arrange(-total_profit) %>% # arrange again, and factor the investor countries to plot properly
  mutate(investor_country = factor(investor_country, 
                                   levels = rev(c("China", "United States", "Netherlands",
                                                  "United Kingdom", "France", "Germany",
                                                  "Others")))) 
# result: firms based in China ($553 million), USA ($538 million), Netherlands ($267 million)
# profit the most from agribusiness

##### make plot 1 #####
p1 <- ggplot(data = df1,
             aes(x = total_profit, 
                 y = investor_country)) +
  geom_bar(stat = "identity",
           fill = "#0072B2") +
  th +
  scale_y_discrete(expand = expansion(mult = 0.1,
                                      add = 0)) +
  scale_x_continuous(position = "top",
                     limits = c(0, 555),
                     breaks = seq(0, 555, 100),
                     expand = expansion(mult = 0,
                                        add = 0)) +
  theme(axis.line.x = element_blank(),  # remove x-axis line and ticks
        axis.ticks.length.x = unit(0, "pt"),
        axis.line.y = element_line(colour = 'black', 
                                   linewidth = 0.5, 
                                   linetype = 'solid'),
        panel.grid.major.x = element_line(colour="grey90")) +
  labs(title = "Firms based in China, the US, and the Netherlands profited the most",
       subtitle = "Total profits made from agribusiness deals*, 2015--2020, millions USD")

p1


##### plot 2 - profit by firm #####
# show the breakdown of profit by firm 
# calculate total profit by firm
# rank the profit, and plot top 6 + others
# if rank > 6, group the profit into "Others"
df2 <- df %>%
  group_by(investor) %>%
  summarize(total_profit = sum(profit)/1000000) %>%
  ungroup() %>%
  arrange(desc(total_profit)) %>% # arrange the data in descending order based on profit
  mutate(rank = rank(desc(total_profit))) %>% # rank based on total_profit 
  filter(rank <= 10) %>% # get top 10 firms
  arrange(-total_profit) %>% # arrange again, and factor the investor countries to plot properly
  mutate(investor = stringr::str_wrap(investor, 25), # wrap text to show nicely on plot
         investor = factor(investor, levels = rev(investor)))

##### make plot 2 #####
p2 <- ggplot(data = df2,
             aes(x = total_profit, 
                 y = investor)) +
  geom_bar(stat = "identity",
           fill = "#0072B2") +
  th +
  scale_y_discrete(expand = expansion(mult = 0.1,
                                      add = 0)) +
  scale_x_continuous(position = "top",
                     limits = c(0, 131),
                     breaks = seq(0, 131, 30),
                     expand = expansion(mult = 0,
                                        add = 0)) +
  theme(axis.line.x = element_blank(),  # remove x-axis line and ticks
        axis.ticks.length.x = unit(0, "pt"),
        axis.line.y = element_line(colour = 'black', 
                                   linewidth = 0.5, 
                                   linetype = 'solid'),
        panel.grid.major.x = element_line(colour="grey90")) +
  labs(title = "Among all firms, Dutch bank ABN Amro pocketed over $130 million",
       subtitle = "Top 10 firms, total profits made from agribusiness deals, 2015--2020, millions USD")

p2


##### plot 3 - connections by group over time #####
# among the 20 top deforesting agribusinesses,
# how many banks/institutions do they actually interact and make deals with?
# does this change over time?

# prepare data
# by group, count number of unique investors from entire time period 
# then, by group-year count number of investors (to see if these connections change over time)
# keep and plot the top 5 based on overall total_connections
df3 <- df %>%
  group_by(group) %>%
  mutate(total_connections = n_distinct(investor)) %>%
  group_by(group, year, total_connections) %>%
  summarize(annual_connections = n_distinct(investor)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(total_connections))) %>% # use dense_rank function to get rank of total connections 
  filter(rank <= 5) %>% # keep top 5 groups with the most connections
  arrange(rank, year)

##### make plot 3 #####
# highlight brookfield and cargill
# brookfield = largest number of connections with investors yearly
# cargill = huge spike from 2019 to 2020

# define colours 
highlight_lines <- c("Brookfield Asset Management" = "#E69F00",
                     "Cargill" = "#CC79A7")

# make line chart, highlighting brookfield and cargill
p3 <- ggplot(data = df3,
             aes(x = year, 
                 y = annual_connections,
                 group = group,
                 colour = group)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = highlight_lines) +
  gghighlight(group %in% c("Brookfield Asset Management", "Cargill"),
              unhighlighted_colour = "#CCCCCC",
              use_direct_label = FALSE) +
  th +
  scale_x_continuous(limits = c(2016, 2020),
                     breaks = seq(2016, 2020, 1),
                     expand = expansion(mult = c(0, 0.1),
                                        add = 0)) +
  scale_y_continuous(position = "right",
                     limits = c(0, 800),
                     breaks = seq(0, 800, 200),
                     expand = expansion(mult = 0,
                                        add = 0)) +
  theme(axis.line.x = element_line(colour = 'black', 
                                   linewidth = 0.5, 
                                   linetype = 'solid'),  # remove x-axis line and ticks
        panel.grid.major.y = element_line(colour="grey90"),
        axis.text.y.right = element_text(margin = margin(t = 0, 
                                                         r = 0,
                                                         b = 0, 
                                                         l = -16, 
                                                         unit = "pt"), # l = -12, hjust = 0
                                         hjust = 1,
                                         vjust = -0.5)) +
  labs(title = "<span style='color: #E69F00'><b>Brookfield</b></span> had the largest number of investor connections <br>
      <span style='color: #CC79A7'><b>Cargill</b></span> investor connections increased from 2019 to 2020 ",
       subtitle = "Top 5 deforesting agribusiness groups*, number of unique investor connections, 2015--2020 <br>",
       caption = "*Including Jardine Matheson Group, Itochu, and Salim Group, ranked based on total unique investor connections. <br>
                  Connections are defined based on new deals struck with investors.")

p3

##### end of code #####