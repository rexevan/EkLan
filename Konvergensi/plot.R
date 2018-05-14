library(tidyverse)
library(ggthemes)
library(wbstats)
theme_set(theme_fivethirtyeight(base_family = "Roboto", base_size = 12))

# Input Data
GDPGRW_wb <- wb(country = "countries_only", indicator = "NY.GDP.MKTP.KD.ZG")
countries_lookup <- as_tibble(wbcountries()) %>% select(country, income, region)

GDPGRW <- as_tibble(GDPGRW_wb) %>% select(date, country, value)  %>% 
  filter(date >= 2000) %>% left_join(countries_lookup) %>% 
  mutate(date = parse_integer(date))

write_csv(GDPGRW, "data/GDPGRW.csv")

# jumlah negara
n_distinct(GDPGRW$country)

# Gambar ---------------

footer_538 <- function(gg) {
  library(grid)
  grid.newpage()
  text.Name <- textGrob('  14.8429', x=unit(0, 'npc'), gp=gpar(col='white', family='sans', fontsize=8), hjust=0)

  text.Source <- textGrob('Politeknik Statistika STIS ', x=unit(1, 'npc'), gp=gpar(col='white', family='', fontsize=10), hjust=1)
  
  footer = grobTree(rectGrob(gp=gpar(fill='#5B5E5F', lwd=0)), text.Name, text.Source)
  
  plt.final <- grid.arrange(gg, footer, heights=unit(c(0.94, 0.06), c('npc', 'npc')))
}
  

axis_label_x <- paste0("'", c("00", "04", "08", "12", "16"))

axis_label_y <- paste0(c("- 40", "- 20", "0%", "+ 20", "+ 40"))

gambar1 <-  ggplot(GDPGRW, aes(x = date, y = value)) + 
  geom_line(aes(group = country), alpha = 0.2) + 
  scale_x_continuous(labels = axis_label_x, breaks = seq(2000, 2016, 4)) + 
  scale_y_continuous(labels = axis_label_y, breaks = c(-40, -20, 0, 20, 40)) + 
  labs(title = "Pertumbuhan GDP 207 Negara \nPeriode 2010 - 2016")

footer_538(gambar1)

library(viridis)
gambar4 <- GDPGRW %>% 
  group_by(date, income) %>% 
  summarise(mean_value = mean(value, na.rm = T)) %>% 
  arrange(date, income) %>% 
  ggplot(aes(x = date, y = mean_value)) + 
  geom_line(aes(group = income, color = income), size = 1) + 
  scale_y_continuous(limits = c(-10, 10)) + 
  scale_x_continuous(labels = axis_label_x, breaks = seq(2000, 2016, 4)) + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.3), 
        legend.direction = "vertical", 
        legend.box.background = element_rect(size = 0.8)) + 
  labs(title = "Rata-rata Pertumbuhan PDB 207 Negara \nMenurut Tingkat Penghasilan")

footer_538(gambar4) 

ggsave(filename = "Gambar4.png", plot = footer_538(gambar4), 
       device = "png", dpi = 200, units = "cm", width = 15, height = 15)
  
ggsave(filename = "Gambar1.png", plot = footer_538(gambar1), 
       device = "png", dpi = 200, units = "cm", width = 15, height = 15)
