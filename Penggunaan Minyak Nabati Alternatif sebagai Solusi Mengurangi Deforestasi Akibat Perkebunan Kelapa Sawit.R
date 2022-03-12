# library
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(rgdal)
library(leaflet)
library(RColorBrewer)

# datasets
oil_palm_prod <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/Crops_Processed_-_Production_Quantity_-_Oil_Palm_FAOSTAT_data_7-6-2021.csv")

oil_palm_import <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/New_Food_Balances_-_Import%20Quantity_-_Palm%20Oil_-_FAOSTAT_data_7-6-2021.csv")
area_harvest <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/Crops_-_Area_Harvested_-_Oil_Palm_Fruit_FAOSTAT_data_7-10-2021.csv")
deforest_indo <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/deforestation-drivers-indonesia.csv")
vegoil_prod13below <-  read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/FoodBalances_-_Production-OilOnly_2013nBelow.csv")
vegoil_prod14up <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/FoodBalances_-_Production-OilOnly_2014nUp.csv")
vegoil_area_harvest <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/Crops_Area_Harvested-OilOnly.csv")

vegoil_prod <- rbind(vegoil_prod13below, vegoil_prod14up)

# world map
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
              destfile="world_shape_file.zip")
# unzip
unzip(paste0(getwd(),"/world_shape_file.zip"),
      exdir=paste0(getwd(),"/world_shape_file"))

# Read this shape file with the rgdal library.
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/world_shape_file"), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_spdf@data <- cbind(index = as.numeric(rownames(world_spdf@data)),
                         world_spdf@data)

unique(oil_palm_prod$Area[!oil_palm_prod$Area %in% world_spdf@data$NAME])

# Cote d'Ivoire
# Korea, Democratic People's Republic of
# Venezuela
# China, mainland == China (nilainya sama aja) jadi diabaikan saja

world_spdf@data$NAME[grepl("Kor",world_spdf@data$NAME)]
world_spdf@data$NAME[grepl("Ivo",world_spdf@data$NAME)]
world_spdf@data$NAME[grepl("Vene",world_spdf@data$NAME)]

# menyamakan nama dengan data source
world_spdf@data$NAME[world_spdf@data$NAME == "Democratic People's Republic of Korea"] <- "Korea, Democratic People's Republic of"
world_spdf@data$NAME[world_spdf@data$NAME == "Côte d'Ivoire"] <- "Cote d'Ivoire"
world_spdf@data$NAME[world_spdf@data$NAME == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

oil_palm_prod18 <- oil_palm_prod[oil_palm_prod$Value > 0 &
                                   oil_palm_prod$Year == 2018,
                                 c("Area", "Value")]

world_spdf@data <- merge(x=world_spdf@data,
                         y=oil_palm_prod18,
                         by.x=c("NAME"),
                         by.y=c("Area"),
                         all.x=TRUE)

world_spdf@data <- world_spdf@data[order(world_spdf@data$index),]

summary(world_spdf@data)

# Create a color palette with handmade bins.
mybins <- c(0,10000,50000,250000,1000000,5000000,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$Value, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  world_spdf@data$NAME,"<br/>", 
  "Produksi: ", format(world_spdf@data$Value, big.mark = ".")," ton", 
  sep="") %>%
  lapply(htmltools::HTML)

#equator
eq <- data.frame (lng = c(-180, 180),
                  lat = c(0, 0))

library(htmltools)
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: normal;
    font-size: 20px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Produksi Kelapa Sawit Dunia tahun 2018")
)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(Value), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addPolygons(
    data = eq,
    lng = ~lng,
    lat = ~lat,
    weight = 3,
    opacity = 0.5
  ) %>%
  addControl(title, position = "topleft", className="map-title") %>%
  addLegend(pal=mypalette, values=~Value, opacity=0.9, title = "Production (Tonnes)", position = "bottomleft" )

# remove missing values and sum values
world_prod <- oil_palm_prod %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value, na.rm=TRUE))

world_imp <- oil_palm_import %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)*1000) # convert from 1000 tonnes to tonnes

# latest year
latest_year <- world_prod[which.max(world_prod$Year), ]$Year

# in latest year world production
world_prod_ly <- format(world_prod[world_prod$Year==latest_year,]$Value / 10**6, decimal.mark=",", digits=3)

# in latest year, indonesia production
indo_prod_ly <- oil_palm_prod[
  (oil_palm_prod$Year==latest_year) &
    (oil_palm_prod$Area=='Indonesia'),
]$Value

# percentage indonesia prod
ind_percent_ly <- format(indo_prod_ly / world_prod[world_prod$Year==latest_year,]$Value * 100, decimal.mark=",", digits=2)

# in latest year, malaysia production
malay_prod_ly <- oil_palm_prod[
  (oil_palm_prod$Year==latest_year) &
    (oil_palm_prod$Area=='Malaysia'),
]$Value

# percentage malaysia prod
malay_percent_ly <- format(malay_prod_ly / world_prod[world_prod$Year==latest_year,]$Value * 100, decimal.mark=",", digits=2)

# join production and import data using melt
world_prod_imp <- melt(list(Produksi=world_prod[world_prod$Year>2013,],
                            Impor=world_imp),
                       id.vars = "Year")

# rename columns
names(world_prod_imp) <- c("Year", "Variable", "Value", "Indicator")

# chart using ggplot and plotly
pi <- ggplot(world_prod_imp,
             aes(x=Year, y=Value, colour=Indicator, text=Indicator)) + 
  geom_line() +
  theme(legend.title=element_text(size = 1, colour = "white"),
        legend.position="top right") +
  labs(title = "Perbandingan Produksi dan Impor Minyak Sawit Dunia",
       x = "Tahun", y = "Jumlah (Ton)",
       caption = "Source:: FAOSTAT") +
  scale_y_continuous(labels=scales::unit_format(unit = "juta", scale = 1e-6))

pi <- ggplotly(pi)
pi

# get top 5 country in 2019
top_country <- top_n(area_harvest[area_harvest$Year=="2019",], 5, Value)$Area
# filter the data based the top 10 country
top_area_harvest <- filter(area_harvest, Area %in% top_country) %>%
  select(Area, Year, Value)

# other than the top 5 changed to 'other countries'
other_area_harvest <- filter(area_harvest, !Area %in% top_country) %>%
  group_by(Year) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)) %>%
  mutate(Area = "Other Countries") %>%
  select(Area, Year, Value)

world_area_harvest <- area_harvest[!is.na(area_harvest$Value),] %>% 
  group_by(Year) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)) %>%
  mutate(Area = "World") %>%
  select(Area, Year, Value)

# combine all the data
df_area_harvest <- rbind(top_area_harvest,
                         other_area_harvest,
                         world_area_harvest)

ah <- ggplot(df_area_harvest,
             aes(x=Year, y=Value, colour=Area)) + 
  geom_line() +
  theme(legend.position="right") +
  labs(title = "Penggunaan Lahan untuk Produksi Minyak Sawit Dunia",
       x = "Tahun", y = "Luas lahan (ha)",
       caption = "Source: FAOSTAT") +
  scale_x_continuous(breaks=seq(0, 2021, 5)) +
  scale_y_continuous(breaks=seq(0, 30*10**6, 5*10**6),
                     labels=scales::unit_format(unit = "juta", scale = 1e-6))
ah <- ggplotly(ah)
ah

# in 1961, world harvested area
world1961_area <- format(world_area_harvest[world_area_harvest$Year==1961,]$Value  / 10**6, decimal.mark=",", digits=2)

# in 2019, world harvested area
world2019_area <- format(world_area_harvest[world_area_harvest$Year==2019,]$Value / 10**6, decimal.mark=",", digits=2)

# percentage of Indonesia and Malaysia in 2019
IM_percent <- format(sum(
  df_area_harvest[
    (df_area_harvest$Area %in% c("Indonesia", "Malaysia")) &
      (df_area_harvest$Year == 2019),
  ]$Value) /
    df_area_harvest[
      (df_area_harvest$Area == "World") &
        (df_area_harvest$Year == 2019),
    ]$Value * 100,
  digits=2)

q <- ggplot(deforest_indo,
            aes(color=Entity, fill=Entity, x=Year, y=deforestation_hectares)) +
  theme(legend.title=element_text(size = 1, colour = "white")) +
  geom_bar(size=0.25, position="fill", stat="identity") +
  labs(title = "Pendorong deforestasi di Indonesia",
       x = "Tahun", y = "Persentase Luas Area",) +
  scale_x_continuous(breaks=seq(0, 2020, 2)) +
  scale_y_continuous(labels=scales::unit_format(unit = "%", scale = 1e+2)) +
  scale_color_manual(values=rep("white", 7)) +
  scale_fill_manual(values="green", limits="Oil palm plantations")
q <- ggplotly(q) %>%
  layout(hovermode = "closest")
  style(hovertext=iris[1:10,"Species"])
q

total_def_10 <- sum(deforest_indo$deforestation_hectares[
  deforest_indo$Year==2015])
opp_def_10 <- sum(deforest_indo[
  (deforest_indo$Entity=="Oil palm plantations") &
    (deforest_indo$Year==2015),]$deforestation_hectares)
persen_opp_def_10 <- format(opp_def_10/total_def_10 * 100, decimal.mark=",", digits=2)


add_newItemCode <- function(dataset) {
  mutate(dataset, Comodity = case_when(
    grepl("Coconut", dataset$Item, ignore.case = TRUE) ~ "Coconut",
    grepl("Cotton", dataset$Item, ignore.case = TRUE) ~ "Cottonseed",
    grepl("Groundnut", dataset$Item, ignore.case = TRUE) ~ "Groundnut",
    grepl("Maize", dataset$Item, ignore.case = TRUE) ~ "Maize",
    grepl("Olive", dataset$Item, ignore.case = TRUE) ~ "Olive",
    grepl("Palm", dataset$Item, ignore.case = TRUE) ~ "Palm",
    grepl("Palmkernel", dataset$Item, ignore.case = TRUE) ~ "Palm",
    grepl("Rape", dataset$Item, ignore.case = TRUE) ~ "Rape and Mustard",
    grepl("Mustard", dataset$Item, ignore.case = TRUE) ~ "Rape and Mustard",
    grepl("Rice", dataset$Item, ignore.case = TRUE) ~ "Ricebran",
    grepl("Sesame", dataset$Item, ignore.case = TRUE) ~ "Sesameseed",
    grepl("Soy", dataset$Item, ignore.case = TRUE) ~ "Soybean",
    grepl("Sunflower", dataset$Item, ignore.case = TRUE) ~ "Sunflower",
    TRUE ~ NA_character_
  ))
}

world_vegoil_prod <- add_newItemCode(vegoil_prod) %>% 
  group_by(Year, Comodity) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)*1000) %>%
  rename(Production = Value)

# world_vegoil_prod <- group_by(world_vegoil_prod, Year) %>%
  # mutate(Share_Percent = Value/sum(Value))

# world_vegoil_prod$Indicator <- "Production"


world_vegoil_areaharv <- add_newItemCode(vegoil_area_harvest) %>% 
  group_by(Year, Comodity) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)) %>%
  rename(Area_Harvested = Value)

# world_vegoil_areaharv <- group_by(world_vegoil_areaharv, Year) %>%
  # mutate(Share_Percent = Value/sum(Value))

# world_vegoil_areaharv$Indicator <- "Area Harvested"

wvo_prodvsareaharv <-
  merge(world_vegoil_prod, world_vegoil_areaharv, by=c("Year", "Comodity"))

wvo_prodvsareaharv$Prod_per_ha <-
  with(wvo_prodvsareaharv, Production/Area_Harvested)

# wvo_prodvsareaharv <- rbind(world_vegoil_prod, world_vegoil_areaharv)

wvo_prodvsareaharv$Share_Percent[wvo_prodvsareaharv$Indicator=="Production"] <-
  -wvo_prodvsareaharv$Share_Percent[wvo_prodvsareaharv$Indicator=="Production"]

df <- wvo_prodvsareaharv[wvo_prodvsareaharv$Year==2004,]

p <- ggplot(wvo_prodvsareaharv,
            aes(x=reorder(Comodity, -Share_Percent),
                y=Share_Percent,
                fill=Indicator)) + 
  geom_bar(stat="identity", position="identity", aes(frame=Year)) + 
  scale_y_continuous(breaks=seq(-1,1,by=0.2),labels=abs(seq(-1,1,by=0.2))) +
  coord_flip()
fig <- ggplotly(p)
fig

wvo_prodvsareaharv$Comodity = with(wvo_prodvsareaharv,
                                   reorder(Comodity, Prod_per_ha))

p <- ggplot(wvo_prodvsareaharv,
            aes(x=reorder(Comodity, +Prod_per_ha),
                y=Prod_per_ha,
                frame=Year)) + 
  geom_bar(stat="identity", position="identity") + 
  coord_flip()
p
fig <- ggplotly(p)
fig

deforest_my <- read.csv("https://raw.githubusercontent.com/risqi-r/data-visualization/main/datasets/treecover_loss__ha.csv")

colnames(deforest_my) <- c("Drivers_Type", "Year", "Area_Ha", "Gross_Emission_CO2")

fg <- ggplot(deforest_my, aes(fill=Drivers_Type, y=Area_Ha, x=Year)) +
  geom_bar(position="stack", stat="identity")
fg <- ggplotly(fg)
fg


library(broom)
spdf_fortified <- tidy(worldcountries, region = "AREA")

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes(x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)

mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Area: ", world_spdf@data$AREA, "<br/>", 
  "Population: ", round(world_spdf@data$POP2005, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(POP2005), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )