


{

nbs = data.table::fread(here::here("data/newborn-screening-disorders-california-2009-2019_final.csv"))

colnames(nbs) <- gsub("[ \n]", "_", colnames(nbs))

#nbs = nbs |> dplyr::select(Disorder_Type, Disorder_Count)

nbs |> head()

}

# nbs = nbs |> dplyr::filter(Disorder_Count != ".") |> 
#    dplyr::mutate(Disorder_Count = as.numeric(Disorder_Count))

# Number of cases identified per year: 


#SCID, severe combined immunodeficiency # reported since march 2013
# ALD

# Recommended Uniform Screening Panel
# 33 core disorders

{
n_counts_avail = nbs$Disorder_Count[10:36] |> as.numeric() |> sum()



nbs = nbs |> 
  add_row(Disorder_Type = "Other",
          Disorder_Count = as.character(5853 - n_counts_avail)) 

} 

{

nbs = nbs |> dplyr::filter(Disorder_Count != ".", 
                           Disorder_Type != "Total") |>
             dplyr::mutate(Disorder_Count = as.numeric(Disorder_Count)) |>
      dplyr::mutate(avg_per_year = Disorder_Count / 10)

}

nbs = nbs |> 
      mutate(Disorder_Type = case_when(grepl( "Cystic", Disorder_Type) ~ "Cystic Fibrosis",
                                       grepl("3MCC", Disorder_Type) ~ "3-MCC Deficiency",
                                       grepl("CAH", Disorder_Type) ~ "Congenital Adrenal Hyperplasia",
                                       grepl("MCAD", Disorder_Type) ~ "MCAD Deficiency",
                                       grepl("ALD carrier", Disorder_Type) ~ "ALD (carrier)",
                                       grepl("PKU", Disorder_Type) ~ "Phenylketonuria",
                                       grepl("PCH", Disorder_Type) ~ "Hypothyroidism",
                                      TRUE ~ Disorder_Type)
      )



nbs = nbs |> mutate(Disorder_Type = reorder(Disorder_Type, -Disorder_Count))

top_nbs = nbs |> 
          dplyr::slice_max(Disorder_Count, n =6)

top_nbs |> 
  ggplot(aes(x = Disorder_Type, y = avg_per_year)) + 
  labs(x = "Disorder", y = "Average Number of Infants Identified \n (per year)") + 
  geom_col() + 
  theme_minimal() +
  coord_flip() + 
  scale_y_continuous(breaks = seq(from = 0, to = 2672/10, by = 50),
                     expand = c(0.01, 0)
  ) + 
  theme(
    panel.grid.major = element_blank(),  # remove major grid lines
    panel.grid.minor = element_blank(),  # remove minor grid lines
    panel.border = element_blank(),
    axis.line = element_line(),          # keep axis lines
    axis.ticks = element_line(),
    axis.text = element_text(size = 16, family = "Helvetica", margin = margin(t = 100)),  
    axis.title.x = element_text(size = 18, family = "Helvetica", margin = margin(t = 10)),  # t = top margin
    axis.title.y = element_text(size = 18, family = "Helvetica", margin = margin(r = 10)),   # r = right margin
  )

