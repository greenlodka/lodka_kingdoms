library(ggplot2)
library(dplyr)
library(maps)

kingdoms <- read.csv('E:/HSE_m/kursach/Kingdoms_variable.csv')


kingdom_name <- kingdoms$Name
kingdom_lat <- kingdoms$lat
kingdom_lng <- kingdoms$lng

kingdom_coord <- as.data.frame(cbind(kingdom_lat, kingdom_lng))
kingdom_coord


# Cluster Dendrogram ------------------------------------------------------

d <- dist(kingdom_coord)
d

hc <- hclust(d, method = "ward.D2")
hc


plot(hc,  kingdom_name, cex = 0.7,
     sub = "Western Han Kingdoms Location", 
     xlab = NA, ylab = NA)
rect.hclust(hc, k = 5, border = 'dark red')



# Scatter plots -----------------------------------------------------------

groups5 <- cutree(hc, k = 5)
groups5

kingdoms <- kingdoms %>%
  mutate(groups5 = factor(groups5))


# total plots ----------------------------------------------------------

gg <- ggplot(data = kingdom_coord,
             aes(x = kingdom_lng,
                 y = kingdom_lat,
                 color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = kingdom_name,
                size = 0.2,
                vjust = 1,
                hjust = 0)) 
gg

# Try to add map background -----------------------------------------------

dat = map_data('world')

gg +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat, map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()


# plots4years -------------------------------------------------------------
## Emperor1 Gaozu---------------------------------------------------------------

kingdoms %>%
  filter(Gaozu != 'FALSE') %>% 
  select(Name,lat,lng,groups5)-> emp1

emperor1 <- 
  ggplot(data = emp1,
             aes(x = lng,
                 y = lat,
                 color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Gao-zu, 202-195 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor1

## Emperor2 Hui-di-------------------------------------------------------------

kingdoms %>%
  filter(Hui != 'FALSE') %>% 
  select(Name,lat,lng,groups5) -> emp2

emperor2 <- ggplot(data = emp2,
              aes(x = lng,
                  y = lat,
                  color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Hui-di, 195-188 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor2

## Emperor3 Lu hou--------------------------------------------------------------

kingdoms %>%
  filter(Lv != 'FALSE') %>%
  select(Name,lat,lng,groups5)-> emp3

emperor3 <- 
  ggplot(data = emp3,
              aes(x = lng,
                  y = lat,
                  color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Empress Lu hou, 188-180 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed() 

emperor3



## Emperor4 Wen-di--------------------------------------------------------------

kingdoms %>%
  filter(Wen != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp4

emperor4 <- 
  ggplot(data = emp4,
              aes(x = lng,
                  y = lat,
                  color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0.5))+
  labs(x = '', y = '',
       title = 'Emperor Wen-di, 180-157 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor4

## Emperor5 Jing-di-------------------------------------------------------------

kingdoms %>%
  filter(Jing != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp5

emperor5 <- 
  ggplot(data = emp5,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0.5))+
  labs(x = '', y = '',
       title = 'Emperor Jing-di, 156-141 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor5

## Emperor6 Wu-di--------------------------------------------------------------

kingdoms %>%
  filter(Wu != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp6

emperor6 <- 
  ggplot(data = emp6,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 0,
                hjust = 1))+
  labs(x = '', y = '',
       title = 'Emperor Wu-di, 141-87 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor6


## Emperor7 Zhao-di------------------------------------------------------------

kingdoms %>%
  filter(Zhao != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp7

emperor7 <- 
  ggplot(data = emp7,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Zhao-di, 87-74 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor7

## Emperor8 Liu He--------------------------------------------------------------

kingdoms %>%
  filter(Liu_He != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp8

emperor8 <- 
  ggplot(data = emp8,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Marquis Liu He, 74 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor8


## Emperor9 Xuan-di-------------------------------------------------------------

kingdoms %>%
  filter(Xuan != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp9

emperor9 <- 
  ggplot(data = emp9,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Xuan-di, 74-49 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor9


## Emperor10 Yuan-di------------------------------------------------------------

kingdoms %>%
  filter(Yuan != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp10

emperor10 <- 
  ggplot(data = emp10,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Yuan-di, 48-33 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor10


## Emperor11 Cheng-di-----------------------------------------------------------

kingdoms %>%
  filter(Cheng != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp11

emperor11 <- 
  ggplot(data = emp11,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Cheng-di, 33-7 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor11


## Emperor12 Ai-di--------------------------------------------------------------

kingdoms %>%
  filter(Ai != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp12

emperor12 <- 
  ggplot(data = emp12,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 1,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Ai-di, 7-1 BC') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor12


## Emperor13 Ping-di------------------------------------------------------------

kingdoms %>%
  filter(Ping != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp13

emperor13 <- 
  ggplot(data = emp13,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 0.8,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Emperor Ping-di, 1 BC - 6 AD') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor13


## Emperor14 Ruzi Ying----------------------------------------------------------

kingdoms %>%
  filter(Ruzi_Ying != 'FALSE') %>%
  select(Name,lat,lng,groups5) -> emp14

emperor14 <- 
  ggplot(data = emp14,
         aes(x = lng,
             y = lat,
             color = groups5)) +
  geom_point(size = 3)+
  geom_text(aes(label = Name,
                size = 0.2,
                vjust = 0.8,
                hjust = 0))+
  labs(x = '', y = '',
       title = 'Infant Ruzi Ying, 6-8 AD') +
  geom_map(data=dat[dat$region=='China',],
           map = dat[dat$region == 'China',],
           aes(x=long,y=lat,map_id = region),
           color='white', fill = "#7f7f7f",
           size = 0.05, alpha = 1/4) +
  ylim(27, 42)+
  xlim(110, 124)+
  coord_fixed()

emperor14






