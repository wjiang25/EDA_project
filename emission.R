library(dplyr)
library(ggplot2)
library(stringr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Merge NEI and SCC data
merged_df <- merge(NEI,SCC,on="SCC") %>% select("SCC","fips","Pollutant","Emissions","type", "year","Short.Name")

## 1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.

total_PM2.5 <- merged_df %>% group_by(year) %>% summarize(pm2.5=sum(Emissions))
barplot(total_PM2.5$pm2.5~total_PM2.5$year,col="gray",ylim=(c(0,8000000)),xlab="Year",ylab="Emission(Ton)", main="Total Emissions of PM2.5 in US (1999-2008)")
text(x = barplot(total_PM2.5$pm2.5 ~ total_PM2.5$year, plot = FALSE),
    y = total_PM2.5$pm2.5 + 0.1,   # Adjust the 0.1 to position the text
    label = round(total_PM2.5$pm2.5,digit=0),
    pos = 3,    # Position the text above the bars
    col = "black")  # Text color

## 2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
## from 1999 to 2008? Use the base plotting system to make a plot answering this question.

PM2.5_Baltimore <- merged_df %>% group_by(fips,year) %>% filter(fips=="24510") %>% summarize(pm2.5=sum(Emissions))
PM2.5_Baltimore <- PM2.5_Baltimore %>% select(year, pm2.5) %>% ungroup()
barplot(PM2.5_Baltimore$pm2.5~PM2.5_Baltimore$year,col="gray",ylim=c(0,4000),xlab="Year",ylab="Emission(Ton)", 
        main="Total emissions in Baltimore City (1999-2008)")
text(x = barplot(PM2.5_Baltimore$pm2.5 ~ PM2.5_Baltimore$year, plot = FALSE),
     y = PM2.5_Baltimore$pm2.5 + 0.1,   # Adjust the 0.1 to position the text
     label = round(PM2.5_Baltimore$pm2.5,digit=0),
     pos = 3,    # Position the text above the bars
     col = "black")  # Text color

## 3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
## variable, which of these four sources have seen decreases in emissions from 1999–2008 
## for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 
## plotting system to make a plot answer this question. ANSWER="POINT"

emission_Balti <- merged_df %>% filter(fips=="24510") 

type_emis_Balti <- emission_Balti %>% group_by(year,type) %>% summarise(emissions=sum(Emissions))
ggplot(type_emis_Balti, aes(x=year,y=emissions)) + 
    geom_line() + 
    facet_grid(. ~ type) +
    scale_x_continuous(breaks = unique(type_emis_Balti$year)) +
    theme(axis.text.x = element_text(angle = 45)) 

## 4.Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

df <- merged_df[(str_detect(merged_df$Short.Name, "Coal") & str_detect(merged_df$Short.Name, "Combustion")),]
summed_data <- df %>% group_by(year) %>% summarize(emission=sum(Emissions)) 
ggplot(summed_data,aes(x=as.factor(year),y=emission)) + 
    geom_col() + 
    geom_text(aes(label = round(emission,digits=0), vjust = -0.5), size=3) +
    labs(x="Year", y="Emission (Ton)", title="Emissions from coal combustion_related sources\n in US (1999-2008)") +
    theme(plot.margin = margin(2, 2, 0, 2, "cm"), plot.title = element_text(face="bold",hjust=0.5))

## 5.How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
df_veh_balti <- emission_Balti[str_detect(emission_Balti$Short.Name, "Veh"),]
summed_veh_balti <- df_veh_balti %>% group_by(year) %>% summarise(emission=sum(Emissions))
ggplot(summed_veh_balti,aes(x=as.factor(year),y=emission)) +
    geom_col() +
    geom_text(aes(label = round(emission,digits=0), vjust = -0.5), size=3) +
    labs(x="Year", y="Emission (Ton)", title="Emissions from motor vehicle in Baltimore City\n (1999-2008)") +
    theme(plot.margin = margin(1, 2, 0.5, 2, "cm"), plot.title = element_text(face="bold",hjust=0.5)) +
    coord_cartesian(ylim = c(0, 400))
    
   

## 6.Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037")
## Which city has seen greater changes over time in motor vehicle emissions?

# Baltimore has greater changes over time in motor vehicle emissions
two_city <- merged_df %>% 
    filter((fips=="24510" | fips=="06037") & grepl("Veh", Short.Name)) %>% 
    group_by(fips, year) %>% 
    summarise(emission = sum(Emissions))
two_city <- two_city %>% mutate(city = ifelse(fips=="24510", "Baltimore", "Los Angeles"))
ggplot(two_city,aes(x=as.factor(year),y =emission, fill=city)) +
    geom_col() +
    facet_wrap( .~ city, scales = "free") +
    labs(x="Year", y="Emission (Ton)", title="Emissions of motor vehicle sources\n in Los Angeles and Baltimore City(1999-2008)") +
    theme(plot.margin = margin(1, 2, 0.5, 2, "cm"), plot.title = element_text(face="bold",hjust=0.5))
    
    