library(tidyverse)
library(readxl)
library(cowplot)

#tidying up data

long_root <- read_excel("Untitled/data/experiment_39.xlsx", sheet="long_root") %>% 
  gather(treatment,longest_root, "0 µM", "5 µM", "10 µM") %>% 
  mutate(longest_root=as.numeric(longest_root))
long_root$treatment <- factor(long_root$treatment,levels = c("0 µM", "5 µM", "10 µM"))
long_root 

shoot_dw <- read_excel("Untitled/data/experiment_39.xlsx", sheet="shoot_dw") %>% 
  gather(treatment,shoot_dw, "0 µM", "5 µM", "10 µM") %>% 
  mutate(shoot_dw=as.numeric(shoot_dw))
shoot_dw$treatment <- factor(shoot_dw$treatment,levels = c("0 µM", "5 µM", "10 µM"))

shoot_dw

root_dw <- read_excel("Untitled/data/experiment_39.xlsx", sheet="root_dw") %>% 
  gather(treatment,root_dw,"0 µM", "5 µM", "10 µM")%>% 
  mutate(root_dw=as.numeric(root_dw))
root_dw$treatment <- factor(root_dw$treatment,levels = c("0 µM", "5 µM", "10 µM"))
root_dw 

root_length <- read_excel("Untitled/data/experiment_39.xlsx", sheet="root_length") %>% 
  gather(treatment,root_length,"0 µM", "5 µM", "10 µM")%>% 
  mutate(root_length=as.numeric(root_length))
root_length$treatment <- factor(root_length$treatment,levels = c("0 µM", "5 µM", "10 µM"))
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

# show the raw data using boxplots

# length of longest root

long_root<- ggplot(data=ex39, aes(x=genotype, y=longest_root, colour=treatment))+geom_boxplot()+facet_wrap(~treatment)

long_root

long_root_final <- long_root+
  labs(title="Length of the longest root", x= "Line name", y="root length (mm)")+
  theme_bw()+
  theme (axis.text = element_text(size = 6, angle = 90),
         axis.title = element_text(size = 8),
         plot.title = element_text(size = 12),
         legend.title = element_text(size = 10),
         strip.text = element_text(size = 6))

long_root_final

# shoot dry weight

sdw <- ggplot(data=ex39, aes(x=genotype, y=shoot_dw, colour=treatment))+geom_boxplot()+facet_wrap(~treatment)

sdw

sdw_final <- sdw+
  labs(title="Shoot dry weight", x= "Line name", y="shoot dry weight (mg)")+
  theme_bw()+
  theme (axis.text = element_text(size = 6, angle = 90),
         axis.title = element_text(size = 8),
         plot.title = element_text(size = 12),
         legend.title = element_text(size = 10),
         strip.text = element_text(size = 6))

sdw_final

# root dry weight

rdw <- ggplot(data=ex39, aes(x=genotype, y=root_dw, colour=treatment))+geom_boxplot()+facet_wrap(~treatment)

rdw
rdw_final <- rdw+
  labs(title="Root dry weight", x= "Line name", y="root dry weight (mg)")+
  theme_bw()+
  theme (axis.text = element_text(size = 6, angle = 90),
         axis.title = element_text(size = 8),
         plot.title = element_text(size = 12),
         legend.title = element_text(size = 10),
         strip.text = element_text(size = 6))

rdw_final

# total dry weight

tdw <- ggplot(data=ex39, aes(x=genotype, y=total_dw, colour=treatment))+geom_boxplot()+facet_wrap(~treatment)

tdw

tdw_final <- tdw+
  labs(title="Total dry weight", x= "Line name", y="total dry weight (mg)")+
  theme_bw()+
  theme (axis.text = element_text(size = 6, angle = 90),
         axis.title = element_text(size = 8),
         plot.title = element_text(size = 12),
         legend.title = element_text(size = 10),
         strip.text = element_text(size = 6))

tdw_final

# root length

rt_length <- ggplot(data=ex39, aes(x=genotype, y=root_length, colour=treatment))+geom_boxplot()+facet_wrap(~treatment)

rt_length
rt_length_final <- rt_length+
  labs(title="Total root length", x= "Line name", y="root length (cm)")+
  theme_bw()+
  theme (axis.text = element_text(size = 6, angle = 90),
         axis.title = element_text(size = 8),
         plot.title = element_text(size = 12),
         legend.title = element_text(size = 10),
         strip.text = element_text(size = 6))

rt_length_final

# final boxplot for dry weights 

final_plot_dw <- plot_grid(sdw_final+theme(legend.position ="none"), rdw_final+theme(legend.position ="none"), tdw_final+theme(legend.position ="none"))
final_plot_dw

# final boxplot for root lengths 

final_plot_rl <- plot_grid(long_root_final+theme(legend.position ="none"),rt_length_final+theme(legend.position ="none"))
final_plot_rl

# calculation of means, standard deviation and standard error for each variable

ex39_stats <- ex39 %>%
  group_by(genotype, treatment) %>% 
  summarise (num_rows = n(),
             Avg_long_root = mean(longest_root),
             med_long_root= median(longest_root),
             sd_long_root=sd(longest_root),
             se_long_root=(sd_long_root)/sqrt(num_rows),
             Avg_shoot_dw = mean(shoot_dw),
             sd_shoot_dw=sd(shoot_dw),
             se_shoot_dw=(sd_shoot_dw)/sqrt(num_rows),
             Avg_root_dw = mean(root_dw),
             sd_root_dw=sd(root_dw),
             se_root_dw =(sd_root_dw)/sqrt(num_rows),
             Avg_root_length = mean(root_length),
             sd_root_length=sd(root_length), 
             se_root_length= (sd_root_length)/sqrt(num_rows),
             Avg_total_dw = mean(total_dw),
             sd_total_dw=sd(total_dw),
             se_total_dw= (sd_total_dw)/sqrt(num_rows))
ex39_stats

