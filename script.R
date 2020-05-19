library(tidyverse)
library(readxl)

#tidying up data

long_root <- read_excel("data/experiment_39.xlsx", sheet="long_root") %>% 
  gather(treatment,longest_root, "0 µM", "5 µM", "10 µM") %>% 
  mutate(longest_root=as.numeric(longest_root))
long_root 

shoot_dw <- read_excel("data/experiment_39.xlsx", sheet="shoot_dw") %>% 
  gather(treatment,shoot_dw, "0 µM", "5 µM", "10 µM") %>% 
  mutate(shoot_dw=as.numeric(shoot_dw))

shoot_dw

root_dw <- read_excel("data/experiment_39.xlsx", sheet="root_dw") %>% 
  gather(treatment,root_dw,"0 µM", "5 µM", "10 µM")%>% 
  mutate(root_dw=as.numeric(root_dw))
root_dw 

root_length <- read_excel("data/experiment_39.xlsx", sheet="root_length") %>% 
  gather(treatment,root_length,"0 µM", "5 µM", "10 µM")%>% 
  mutate(root_length=as.numeric(root_length))
root_length

ex39_combined <- bind_cols(long_root,shoot_dw,root_dw,root_length) 
ex39_combined
view(ex39_combined)

# tidy data to be used for plots and analysis

ex39 <-select(ex39_combined,1,2,3,4,8,12,16) %>% 
  mutate(total_dw=shoot_dw+root_dw)%>% 
  filter(longest_root != "-",shoot_dw !="-", root_dw !="-", root_length!='_', total_dw != '-')
ex39
view (ex39)

