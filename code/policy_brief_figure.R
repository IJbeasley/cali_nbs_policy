
# required packages 
{
library(dplyr)
library(ggplot2)
library(here)
library(grid)
library(extrafont) #for access to windows fonts
}  




# load + set up data
{
birth_data <- data.table::fread(here::here("data/20241030_births_final_state_month_sup.csv"))
birth_data2 <- data.table::fread(here::here("data/2025-04_births_provisional_state_month_sup.csv"))
  
birth_data= dplyr::bind_rows(birth_data, birth_data2)  
  
birth_data = birth_data %>% 
  filter(Year >= 1983)

birth_data = birth_data %>% 
  filter(Geography_Type == "Occurrence",
         Strata == "Total Population")
}


# Calculate cumulative births per year
{
birth_data = birth_data %>% 
  group_by(Year) %>% 
  summarise(Total_births = sum(Count))


birth_data = birth_data %>% 
  mutate(cum_births = cumsum(Total_births))
}


# Plotting 
{
p <- birth_data %>% 
  ggplot(aes(x=Year, y = cum_births)) + 
  geom_col(fill = "#14828C") + 
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    limits = c(0, 25 * 10^6),
    expand = c(0.01, 0)
  ) + 
  scale_x_continuous(breaks = seq(1983, max(birth_data$Year), by = 5),
                     expand = c(0.01, 0)
                     )+
  theme_minimal() +
  #ylim(0, 25 * 10^6) +
  labs(y = "Stored Samples (millions)") +
       #title = "Number of Stored Newborn Screening Blood Samples in California Over Time") + 
  theme(
    panel.grid.major = element_blank(),  # remove major grid lines
    panel.grid.minor = element_blank(),  # remove minor grid lines
    panel.border = element_blank(),
    axis.line = element_line(),          # keep axis lines
    axis.ticks = element_line(),
    axis.text = element_text(size = 16, family = "Helvetica", margin = margin(t = 100)),  
    axis.title.x = element_text(size = 18, family = "Helvetica", margin = margin(t = 10)),  # t = top margin
    axis.title.y = element_text(size = 18, family = "Helvetica", margin = margin(r = 10)),   # r = right margin
    # plot.title = element_text(size = 24, face = "bold", 
    #                           hjust = 0, 
    #                           family = "Helvetica") 
    plot.margin = margin(t = 40, r = 10, b = 10, l = 10)  # make room for title
    )

  
png("figures/Cum_Cali_Births.png", units = "in", width = 2*6.5, height = 1.5*4, res = 300)
#ggsave("Cum_Cali_Births.png", units = "in", width = 2*6.5, height = 1.5*4)

grid::grid.newpage()
grid::grid.text("Number of Stored Newborn Screening Blood Samples in California",
          x = unit(0.01, "npc"),
          y = unit(0.98, "npc"),
          just = "left",
          gp = grid::gpar(fontsize = 24, fontface = "bold", fontfamily = "Helvetica"))
grid::grid.draw(ggplotGrob(p))

# Close the device
dev.off()
}

# grid.text("Cumulative Births in California",
#           x = unit(0.01, "npc"),  # Adjust closer to 0 for farther left
#           y = unit(0.98, "npc"),
#           just = "left",
#           gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "Arial"))
# 
# ggsave("Cum_Cali_Births.png", units = "in", width = 2*6.5, height = 1.5*4)

# in California \n since 1983 (in millions)
