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

library(dplyr)

kingdom_coord <- kingdom_coord %>%
        mutate(groups5 = factor(groups5),
               name = kingdom_name)

View(kingdom_coord)

library(ggplot2)

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

library(maps)

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




        
        