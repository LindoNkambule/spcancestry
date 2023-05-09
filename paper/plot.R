library(ggplot2)
library(dplyr)
library(tidyverse)

# 1. Classification comparison
gnomad <- read.table("gnomad_probs.txt", header=TRUE,
                       sep="\t", row.names=1)

spcancestry <- read.table("spcancestry_probs.txt", header=TRUE,
                          sep="\t", row.names=1)

# it doesn't matter which df we use to subset by TRUE POP as true_pop is the same in gnomad and spcancestry, 
df <- rbind(cbind(gnomad, source = "gnomad"),
            cbind(spcancestry, source = "spcancestry"))

get_pop_counts <- function(full_df, population, prob_col) {
  df_pop <- full_df |> filter(true_pop == population)
  df_gnomad <- df_pop |> filter(source == "gnomad")
  df_spcancestry <- df_pop |> filter(source == "spcancestry")
  
  df_plot <- data.frame(gnomad = df_gnomad[, prob],
                        spcancestry = df_spcancestry[, prob])
  
  # some samples will completely overlap, so points with more samples will be bigger on the plot
  df_count <- df_plot |>
    group_by(gnomad, spcancestry) |>
    summarise(N = n())
  
  df_count$pop <- pop # add this so we can use facet_grid to get a nice plot title in a grey box
  
  return(df_count)
}

combined_df <- data.frame()
# There is only 1 sample in OCE
pops <- c("EUR",  "EAS", "AMR", "CSA", "AFR", "MID")
for (pop in pops){
  prob <- paste0("prob_", pop)
  df_pop_counts <- get_pop_counts(df, pop, prob)
  
  combined_df <- rbind(combined_df, df_pop_counts)
}

ggplot(combined_df, aes(x=gnomad, y=spcancestry, size = N)) +
  geom_point() +
  facet_wrap(. ~ pop, ncol = 3) +
  xlab("gnomAD RF probability") +
  ylab("SPCAncestry stacking probability") +
  theme_bw()

