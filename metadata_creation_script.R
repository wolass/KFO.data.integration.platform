pacman::p_load(tidyverse)
data <- read_csv("data/apoA1.csv")
write_csv(data [,1:4],"data/apoA1.csv")
data.frame(Variable = names(data),
           Description = c(
             "Sample number made up from either control (c) or test (t) and clone nr",
             "Optical density expressed in relative value to the baseline of 0.54642",
             "logarhitmic transformtion of the optical density value by log(OD)",
             "Calculated concentration of the ApoA1 protein in each sample expressed in ng/L"
           )) %>% write_csv("data/metadata.csv")
