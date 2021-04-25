#required libraries
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)
library(plotly)
windowsFonts("Arial" = windowsFont("Arial"))

#Get map data
in_sf <- ne_states(geounit = "india", returnclass = "sf")
#plot(in_sf)

# get donation data
donations <- tibble::tribble( 
  ~Name,           ~lat,     ~lon, ~Amount,
  "Ambaji Temple",24.3357282,72.8497416,100,
  "Swaminarayan Temples",23.02995854,72.59157904,188,
  "Mata Mansi Devi temple",30.72389394,76.86065773,1000,
  "Mahamaya Devi Temple",22.28867417,82.16001741,5,
  "Mahalaxmi Temple",17.96101819,74.07581057,200,
  "Somnath Temple",20.88827743,70.40111398,100,
  "Mahavir Mandir",25.60486636,85.13650351,100,
  "Kanchi Kamakoti Peetham",12.8436296,79.70088893,20,
  "Shri Saibaba Sansthan Trust",19.76683011,74.47728603,5100,
  "Baba Ramdev",29.93121456,78.10301204,2500,
  "Kangra Temple",31.87618763,76.3255675,900,
  "Salasar Balaji Temple",27.72628349,74.72426787,11,
  "Khatu Shyam Temple",27.3694031,75.41042833,11,
  "Brahma Temple (Rajpurohit)",26.4874359,74.5488297,11,
  "Vaishno Devi Temple",33.0312611,74.94901013,26,
  "Sanwaliya ji Temple",24.71863313,74.38029756,50,
  "Shree Jeen Mata Ji Mandir",27.44570649,75.19533879,5,
  "Gayatri Pariwar" ,29.99255853,78.19180088,100,
  "Mehandipur Balaji Mandir",26.94704802,76.7951216,11,
  "Gujarat Jalaram Bapa Mandir",22.15330721,70.66507298,510,
  "Ramdev Samadhi Samiti",27.00841787,71.92244756,11,
  
  )

#map theme
theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}


#map donation data on map data
plott <- ggplot() +
  geom_sf(data = in_sf, colour = "black", fill = "white")+
  theme_map(base_size = 5, base_family = "Times")+
  geom_text(data = in_sf, aes(x = longitude,y = latitude,label = postal), size = 3,fontface="bold", hjust = 1)+
  geom_point(data = donations, mapping = aes(x = lon, y = lat, text=Name, size = Amount ), colour = "darkorange1", alpha = .7) + 
  coord_sf()+
  ggtitle("Total amount = 109.59 Crores ")  +
  labs(size="Amount (in lakh)")+
  theme(
  plot.title = element_text(color="black", size=12, face="bold",hjust = 0.5,margin=margin(b = 5, unit = "pt")))


# save result as html

l <- plotly::ggplotly(plott,tooltip = c("Amount", "Name"))
htmlwidgets::saveWidget(l, "donation.html")

#save result as image
png("donation.png", units="in", family="Times New Roman",  width=5, height=5, res=300, pointsize = 2) #pointsize is font size| increase image size to see the key
plott
dev.off()





