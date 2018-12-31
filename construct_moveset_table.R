library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)


# Read in pdf
dat <- pdf_text("pokemon_pdf.pdf")

# Source names
source("pokemon_names.R")

# Filter for pages that have pokemon
dat <- dat[49:174]

# Split on new lines and convert to dataframe
full_dat <- lapply(dat, function(x){as.data.frame(strsplit(x, "\n"))})

# Filter out all rows that contain Level or Starting Moves
moves_dat <- lapply(full_dat, function(x){
  mutate(x, str_detect(x[,1],
                       paste(c("Level", "Starting Moves"), 
                             collapse = "|")))
})

# Rename to refer to later
moves_dat <- lapply(moves_dat, function(x){
  colnames(x)[1] <- "moves"
  colnames(x)[2] <- "logi"
  filter(x, logi == TRUE)
})


# Convert from lists to df
long_data <- do.call(bind_rows, moves_dat)

# Count by when Starting Moves appears
long_data <- mutate(long_data, cumsum(str_detect(long_data$moves, "Starting Moves")))
colnames(long_data)[3] <- "index"

# Join in table of pokemon
long_data <- left_join(long_data, pokemon, by = "index")

# Use some heuristics to identify incorrect cells. Cells that describe movesets 
# should not contain these words/characters
long_data <- mutate(long_data, str_detect(long_data$moves, "CR"))
long_data <- mutate(long_data, str_detect(long_data$moves, "Level"))
long_data <- mutate(long_data, str_detect(long_data$moves, "|"))
long_data <- mutate(long_data, str_detect(long_data$moves, "Small|Medium|Large"))
colnames(long_data)[5] <- "one"
colnames(long_data)[6] <- "two"
colnames(long_data)[7] <- "three"
colnames(long_data)[8] <- "four"

# Sum to identify cells did not meet all conditons above and filter out. This is the basis 
# of the 'correct' data
base_data <- mutate(long_data, logi = one + two + three + four) %>% 
  filter(logi != "4")

# Filter out all data that met all the heuristics above. Most of this should be 'wrong' data.
enrich <- mutate(long_data, logi = one + two + three + four) %>% 
  filter(logi == "4") %>% 
  select(1, 4)

# Still some data in here that should be part of the main dataset. Going to identify patterns
# one by one that identify whether it should be in the base_data.
# Collect anything with "Starting Moves:"
enrich <- mutate(enrich, str_detect(enrich$moves, "Starting Moves:"))
colnames(enrich)[3] <- "one"

add_start <- enrich %>% 
  filter(one == TRUE) %>% 
  select(-3)

base_data <- bind_rows(base_data, add_start)

# From here it's just a long line of str_detect to identify patterns and then anti_joins
# to filter it out of the diffed dataset
diff <- anti_join(long_data, base_data, by = "moves")

diff <- diff %>% 
  filter(!str_detect(diff$moves, "Level 20")) %>% 
  select(1, 4)

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Flying") & pokemon != "Charmeleon")

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Grass") & pokemon != "Ampharos")

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Water Type"))

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Electric Type"))

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Level 1") & pokemon != "Charmeleon" & pokemon != "Lapras") 

diff2 <- diff2 %>% 
  filter(pokemon != "Ampharos" & pokemon != "Slowking") %>% 
  filter(pokemon != "Smeargle" & pokemon != "Elekid")

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Medium") & pokemon != "Hitmonchan" & pokemon != "Ampharos") 

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(str_detect(diff$moves, "Large") & pokemon != "Charmeleon") 

diff <- anti_join(diff, diff2, by = "moves")

diff2 <- diff %>% 
  filter(pokemon %in% c("Caterpie", "Weedle", NA)) 

# This is now all the data that was incorrectly filtered out.
diff <- anti_join(diff, diff2, by = "moves")

# Bind it back into base dataset and tidy a little
base_data <- bind_rows(base_data, diff) %>% 
  select(pokemon, moves) %>% 
  left_join(pokemon, by = "pokemon") %>% 
  arrange(index) %>% 
  select(index, pokemon, moves)


# Remove bunch of old dfs
rm(enrich)
rm(diff)
rm(diff2)
rm(long_data)
rm(moves_dat)
rm(add_start)
rm(full_dat)
rm(dat)

# Lots of areas where cells have been merged together by various characters. 
# Several where they've been merged on arbitrairy number of spaces. Use the separate command
# over and over to split on the weird delimiters

base_data <- base_data %>% 
  separate(col = "moves", into = c("Level", "Moves"), sep = ":", 
           remove = TRUE, extra = "merge") %>% 
  filter(!is.na(Moves))

base_data <- base_data %>% 
  separate(col = "Moves", into = c("Moves", "extra"), sep = ":",
           extra = "merge") %>% 
  select(-extra)

base_data <- base_data[-c(25, 26, 27), ]

base_data <- base_data %>% 
  separate(col = "Moves", into = c("Moves", "extra"), sep = "                                                    ",
           extra = "merge") %>% 
  select(-extra)

base_data <- base_data %>% 
  separate(col = "Moves", into = c("Moves", "extra"), sep = "                                         ",
           extra = "merge") %>% 
  select(-extra)

base_data <- base_data %>% 
  separate(col = "Moves", into = c("Moves", "extra"), sep = "                                             ",
           extra = "merge") %>% 
  select(-extra)

rownames(base_data) <- 1:nrow(base_data)

base_data <- base_data %>% 
  separate(col = "Moves", into = c("Moves", "extra"), sep = "                           ",
           extra = "merge") %>% 
  select(-extra)


# Remove some mislabels
base_data <- base_data %>% 
  filter(Level != "HM") %>% 
  filter(Level != "Quick Getaway")

# Relabel bad labels
base_data$Level <- trimws(base_data$Level)

base_data$Level <- dplyr::recode(base_data$Level,
                                  `*** Level 17` = "Level 17",
                                  `*** Level 14` = "Level 14",
                                  `***Level 14` = "Level 14",
                                  `***Starting Moves` = "Starting Moves")


write.csv(base_data, "moveset_table.csv", row.names = FALSE)
