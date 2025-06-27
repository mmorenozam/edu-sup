library(haven)
library(dplyr)

meme2019_12 <- read_sav("datos/memeldum/enemdu_persona_201912.sav")
meme2019_09 <- read_sav("datos/memeldum/enemdu_personas_2019_09.sav")
meme2019_06 <- read_sav("datos/memeldum/201906_EnemduBDD_15anios.sav")
meme2019_03 <- read_sav("datos/memeldum/201903_EnemduBDD_15anios.sav")

meme2021 <- read_sav("datos/memeldum/BDDenemdu_personas_2021_anual.sav")
meme2022 <- read_sav("datos/memeldum/BDDenemdu_personas_2022_anual.sav")
meme2023 <- read_sav("datos/memeldum/BDDenemdu_personas_2023_anual.sav")
meme2024 <- read_sav("datos/memeldum/BDDenemdu_personas_2024_anual.sav")

explor_function <- function(df, yr){
  df_temp <- df|>
    mutate(indi = substr(p12b, start = 1, stop = 1),
           docs = substr(p12b, start = 1, stop = 3),
           year = yr) |>
    select(indi, docs, year)
  df_temp$obs <- nrow(df_temp)
  return(df_temp)
}

explor_part <- function(df_list, yr){
  out_list <- list()
  obs_vector <- c()
  for (i in 1:length(df_list)){
    df_temp <- df_list[[i]] |>
      mutate(indi = substr(p12b, start = 1, stop = 1),
             docs = substr(p12b, start = 1, stop = 3),
             year = yr) |>
      select(indi, docs, year)
    out_list[[i]] <- df_temp
    obs_vector[i] <- nrow(df_list[[i]])
  }
  out_df <- do.call(rbind, out_list)
  out_df$obs <- sum(obs_vector)
  return(out_df)
}

explor_function(meme2019_03, 2019)



meme_all <- bind_rows(explor_function(meme2022, 2022), 
                      explor_function(meme2023, 2023),
                      explor_function(meme2024, 2024))
meme2021_a <- explor_function(meme2021, 2021)
meme2019 <- explor_part(list(meme2019_03, meme2019_06, meme2019_09, meme2019_12), 2019)

meme_all <- bind_rows(meme2019, meme_all)

meme_all |>
  filter(indi == "6") |>
  group_by(year, obs) |>
  summarise(nummer = n()) |>
  mutate(percen = nummer / obs)

# |>
#   filter(indi %in% c("5", "6"))

meme1 |>
  mutate(indi = substr(p12b, start = 1, stop = 1)) |> 
  group_by(indi) |> 
  summarise(nummer = n(), total = nrow(meme1), percen = nummer/total*100)
