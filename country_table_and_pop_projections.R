library(readxl)
library(tidyverse)
library(plyr)
base<-read_xlsx("wb_input.xlsx", "base")


memberships<-read_xlsx("wb_input.xlsx", "memberships")

countries<- merge(base, memberships)

lac <- list(countries$iso3)



wb_population<-read_xlsx("wb_input.xlsx", "WB population")
pop_longer <- pivot_longer(wb_population, cols= "2015 [YR2015]":"2030 [YR2030]" , names_to="year", values_to = "population" )
pop_longer<- select(pop_longer, `Country Name` ,`Country Code`, `Series Name`,  year, population)
pop_wider <- pivot_wider(pop_longer, names_from = "Series Name", values_from = population)

lac <- list(countries$iso3)


pop_lac <- filter(pop_wider, pop_wider$`Country Code` %in% c("ABW", "AIA", "ARG" ,"ATG" ,"BES" ,"BHS", "BLM", "BLZ", "BOL", "BRA" ,"BRB" ,"CHL" ,"COL" ,"CRI",
                                                             "CUB" ,"CUW", "CYM" ,"DMA" ,"DOM", "ECU" ,"FLK" ,"GLP" ,"GRD" ,"GRL" ,"GTM" ,"GUF" ,"GUY" ,"HND",
                                                             "HTI", "JAM" ,"KNA", "LCA", "MAF", "MEX", "MSR" ,"MTQ", "NIC", "PAN", "PER" ,"PRI" ,"PRY" ,"SLV",
                                                             "SUR", "SXM", "TCA" ,"TTO" ,"URY", "VCT", "VEN" ,"VGB", "VIR"))

wb_gdp<-read_xlsx("wb_input.xlsx", "WB gdp area")
gdp_longer <- pivot_longer(wb_gdp, cols="2015 [YR2015]":"2021 [YR2021]" , names_to="year", values_to = "gdp" )
gdp_longer<- select(gdp_longer ,`Country Code`, `Series Name`,  year,gdp)

gdp_wider <- pivot_wider(gdp_longer, names_from = "Series Name", values_from = gdp)

population_data<-left_join(pop_lac, gdp_wider,  by = c("Country Code" = "Country Code", "year" = "year"))

la<- unique(select(population_data,`Country Code` ,`Land area (sq. km)`))

la<-filter(la, la$`Land area (sq. km)` >0)

demographic_data<-left_join(population_data, la,  by = c("Country Code" = "Country Code"))

demographic_data$`Land area (sq. km).x` <- NULL


final_table<- dplyr::rename(demographic_data,
         iso3 = `Country Code` ,
         country =`Country Name`,
         pop_total = `Population, total`,
         pop_rural = `Rural population`,
         pop_urban = `Urban population`,
         pop_female = `Population, female`,
         pop_male =`Population, male`,
         gdp_usd = `GDP (current US$)`,
         area_km2 = `Land area (sq. km).y`)

final_table$year <- as.numeric(substr(final_table$year, 1, 4))

basic_demo_table<-filter(final_table, year == 2020)

country_stats_table<-unique(merge(basic_demo_table, countries))


population_proj<-select(final_table, country, iso3, year, ,pop_total,  pop_rural, pop_urban )


write.csv(population_proj, "population_projections.csv")
write.csv(country_stats_table, "country_stats_table.csv")

