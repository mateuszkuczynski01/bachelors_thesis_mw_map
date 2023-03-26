#A map displaying the change of minimum wage in Europe over the years
#Based on Milos Popovic blog: https://milospopovic.net/blog
#Main goal: add insight to my bachelor's thesis with a map

setwd("D:\\minimum_wage_project")

library(ggplot2, quietly=T)
library(rgdal, quietly=T)
library(rgeos, quietly=T)
library(dplyr, quietly=T)
library(tidyverse, quietly=T)
library(classInt, quietly=T)
library(eurostat, quietly=T)
library(showtext, quietly = T)

url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip"
download.file(url, basename(url), mode="wb") 
unzip("ref-countries-2013-01m.shp.zip") 
unzip("CNTR_RG_01M_2013_4326.shp.zip")

map_contour <- readOGR(getwd(), "CNTR_RG_01M_2013_4326", verbose = TRUE, stringsAsFactors = FALSE)
plot(map)


out <- c("MA", "TN", "DZ", "EG", "LY","JO", "IL", "PS", "SY", "SA",
         "LB", "IQ", "IR", "GL")



map_contour_2 <- subset(map_contour, !FID%in%out)
map_contour_3 <- fortify(map_contour_2)

map_data <- readOGR(getwd(), "CNTR_RG_01M_2013_4326", verbose = TRUE, stringsAsFactors = FALSE)

minwage <- get_eurostat("earn_mw_cur")
minwage <- as.data.frame(na.omit(minwage))

unique(minwage$geo)

minwage <- minwage %>% arrange(time) %>% filter(time>"2003-07-01"&time<"2021-01-01") %>% 
  filter(!geo %in% c("ME", "MK", "HR", "DE", "US") & currency=="PPS") %>%
  select(-currency) %>% pivot_wider(names_from="time", values_from="values") %>%
  as.data.frame()


mw <- (select(minwage,(seq(2,34,2))) + select(minwage,seq(3,35,2)))/2
mw <- mw %>% mutate(geo=minwage$geo)


final_data <- mw %>% mutate(
  real_percent_growth=(((`2020-01-01`-`2004-01-01`)*100))/`2004-01-01`) %>% 
  mutate(CNTR_ID=geo) %>% select(CNTR_ID, real_percent_growth)

final_data

f1 <- merge(final_data, map_data, by="CNTR_ID")
e <- fortify(map_data, region = "CNTR_ID") %>%
  mutate(CNTR_ID = as.character(id))
d <- e %>% left_join(f1, by = "CNTR_ID")

ni <- classIntervals(d$real_percent_growth, n=5, style = "quantile")$brks

labels <- c()
for(i in 1:length(ni)){
  labels <- c(labels, paste0(round(ni[i], 0),
                             "–",
                             round(ni[i + 1], 0)))
}
labels <- labels[1:length(labels)-1]

d$cat <- cut(d$real_percent_growth,
             breaks = ni,
             labels = labels,
             include.lowest = T)
levels(d$cat)

lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No data"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No data"
levels(d$cat)


font_add_google("Roboto slab", family = "roboto_slab")
font_add_google("Montserrat", family = "montserrat")
showtext_auto()


plot <- ggplot() + geom_polygon(data = map_contour_3, aes(x=long, 
                                                          y=lat,
                                                          group=group),
                                fill="grey80") +
  geom_polygon(data=subset(d, !is.na(real_percent_growth)), aes(x=long, 
                                                                y=lat,
                                                                group=group,
                                                                fill=cat)) +
  geom_path(data=subset(d, !is.na(real_percent_growth)), aes(x=long, 
                                                             y=lat,
                                                             group=group),
            color=NA, size=0) +
  geom_path(data=map_contour_3, aes(x=long, y=lat, group=group),
            color="white", size=0.2)

plot2 <- plot + coord_map(xlim=c(-10.6600,44.07), ylim=c(32.5000,71.0500), projection="lambert", parameters=c(10.44,52.775)) +
  expand_limits(x=map_contour_3$long, y=map_contour_3$lat)

plot3 <- plot2 + labs(x = "", title="Real minimum wage % change, 2004-2020",
                      subtitle = "Country level, PPP adjusted",
                      caption="AddSource")

plot4 <- plot3 + scale_fill_manual(name= "% increase",
                                   values=c('#f4a77a', '#e18079', '#c35e7d', '#9b4283', '#6c2d83', "grey80"),
                                   labels=c("20–22", "22–55","55–64","64–102","102–410" ,"No data"),
                                   drop=F) +
  guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )
  )

plot5 <- plot4 + theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.45, .1),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=20, color="#6c2d83", hjust=0.5, vjust=-10, family = "roboto_slab"),
        plot.subtitle = element_text(size=14, color="#bd5288", hjust=0.5, vjust=-15, face="bold", family = "montserrat"),
        plot.caption = element_text(size=9, color="grey60", hjust=0.5, vjust=9, family = "montserrat"),
        axis.title.x = element_text(size=7, color="grey60", hjust=0.5, vjust=5, family = "montserrat"),
        legend.text = element_text(size=10, color="grey20", family = "montserrat"),
        legend.title = element_text(size=11, color="grey20", family = "montserrat"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

plot5



