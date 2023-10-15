library(tidyverse)
library(readr)
library(cshapes)
library(shiny)
library(ggplot2)
library(sf)
library(countrycode)

setwd("/Users/ayman/OneDrive/Desktop/DataAcrossDisciplines")

conflict <- read_csv('fl_conflict_data.csv')
demographics <- read_csv('fl_demographic_data.csv')
economic <- read_csv('fl_economic_data.csv')
regime <- read_csv('fl_regime_data.csv')
polity5 <- read_csv('polity5v2018.csv')

# Import Country Shape data(need the geometry for sf maps)
countries1991 <- cshp(date = as.Date("1991-07-01"), useGW = FALSE, dependencies = TRUE) %>%
  rename(ccode = cowcode)

# make full conflict data set
fulldata <- conflict %>%
  left_join(regime,
            by = c('ccode', 'year')) %>%
  left_join(demographics,
            by = c('year', 'ccode')) %>%
  left_join(economic,
            by = c('year', 'ccode')) %>%
  select(-country.y.y,-cmark.y.y,-country.x.x,-cmark.x.x,-country.y,-cmark.y) %>%
  # above gets rid of extra country names and extra cmark
  rename(country = country.x, # rename last ones back to country and cmark
         cmark = cmark.x)

# add maps to countries data
fullcountries1991 <- countries1991 %>%
  left_join(fulldata,
            by = c('ccode'))

# clean polity5 data
polity5_clean2 <- polity5 %>%
  filter(is.finite(exrec)) %>%
  filter(parcomp > 0 & exrec > 0)

# make factionalization data frame
polity_faction <- polity5_clean2 %>%
  mutate(factionalization = ifelse(
    exrec < 6 & parcomp < 2, 'Full Autocracy', ifelse(
      exrec > 5 & parcomp < 2, 'Partial Autocracy', ifelse(
        exrec < 6 & parcomp > 1, 'Partial Autocracy', ifelse(
          exrec > 5 & (parcomp == 4 | parcomp == 2), 'Partial Democracy', ifelse(
            exrec > 5 & parcomp == 3, 'Partial Democracy w/ factionalism', ifelse(
              exrec > 5 & exrec < 8 & parcomp == 5, 'Partial Democracy', ifelse(
                exrec == 8 & parcomp == 5, 'Full Democracy', 'NA')))))))) %>%
  # select down to only relevant variables
  select(ccode, year, parcomp, exrec, factionalization)

# merge in factionalization data
full_faction <- fulldata %>%
  left_join(polity_faction,
            by = c('year','ccode'))

full_faction_laged <- full_faction %>%
  mutate(lag1pop = ifelse(ccode == lag(ccode), lag(pop), NA), # get previous year's pop
         pop_diff1 = ifelse(ccode == lag(ccode), pop-lag1pop, NA), # find difference from the previous year
         percent_diff1 = ifelse(ccode == lag(ccode), (pop_diff1/lag1pop)*100, NA), # make it percent difference from previous year
         # get 3 year lags
         lag3pop = ifelse(ccode == lag(ccode, n = 3), lag(pop, n = 3), NA),
         pop_diff3 = ifelse(ccode == lag(ccode, n = 3), pop-lag3pop, NA),
         percent_diff3 = ifelse(ccode == lag(ccode, n = 3), (pop_diff3/lag3pop)*100, NA),
         # get 5 year lags
         lag5pop = ifelse(ccode == lag(ccode, n = 5), lag(pop, n = 5), NA),
         pop_diff5 = ifelse(ccode == lag(ccode, n = 5), pop-lag5pop, NA),
         percent_diff5 = ifelse(ccode == lag(ccode, n = 5), (pop_diff5/lag5pop)*100, NA),
         # get averages per year
         avgpercdiff_3years = ifelse(is.finite(percent_diff3), percent_diff3/3, NA),
         avgpercdiff_5years = ifelse(is.finite(percent_diff5), percent_diff5/5, NA),
         # determine whether a war starts in the year after CY
         onset_lead = ifelse(ccode == lead(ccode), lead(onset),NA),
         eth_diff = 100*(plural - second),
         rel_diff = plurrel - minrelpc,
         At_war = ifelse(war == 1, 'Yes', 'No'))


# add in countries
full_faction_countries1991 <- countries1991 %>%
  left_join(full_faction_laged,
            by = 'ccode')

# create choices variables for regions and factionalization
regionChoices <- full_faction_laged$region %>% unique()
factionChoices <- full_faction_laged$factionalization %>% unique()

# do it for variable choices as well
variableChoices <- c('Population Change',
                     'Ethnic Fractionalization',
                     'Ethnic Difference',
                     'Religious Difference',
                     'Religious Fractionalization')

# Aggregated ethnic frac for map

codesfull <- codelist_panel %>% 
  select(year, cown, iso3c)

full_faction_laged_2 <- full_faction_laged %>% 
  left_join(codesfull, by = c("year", "ccode" = "cown"))

avg_ethnic <- full_faction_laged_2 %>% 
  group_by(iso3c) %>% 
  summarize(avgethnic = mean(ef)) %>% 
  filter(iso3c != 'NA')

countries <- st_read('/Users/ayman/OneDrive/Desktop/DataAcrossDisciplines/wb_boundaries_geojson_lowres/WB_Boundaries_GeoJSON_lowres/WB_countries_Admin0_lowres.geojson')

full_faction_laged_map <- full_faction_laged_2 %>%
  select(region, factionalization, iso3c)

avg_ef_map <- countries %>%
  left_join(avg_ethnic, by = c("ISO_A3_EH" = "iso3c")) %>%
  left_join(full_faction_laged_map, by = c("ISO_A3_EH" = "iso3c")) #%>%
  #select(NAME_EN, region, factionalization, avgethnic, geometry)

# Aggregated religious frac for map

avg_religious <- full_faction_laged_2 %>% 
  group_by(iso3c) %>% 
  summarize(avgrelig = mean(relfrac)) %>% 
  filter(iso3c != 'NA')

avg_rel_map <- countries %>%
  left_join(avg_religious, by = c("ISO_A3_EH" = "iso3c")) %>%
  left_join(full_faction_laged_map, by = c("ISO_A3_EH" = "iso3c")) #%>%
  #select(NAME_EN, region, factionalization, avgrelig, geometry)

# Aggregated eth diff for map

avg_eth_diff <- full_faction_laged_2 %>% 
  group_by(iso3c) %>% 
  summarize(avgethdiff = mean(eth_diff)) %>% 
  filter(iso3c != 'NA')

avg_ethdiff_map <- countries %>%
  left_join(avg_eth_diff, by = c("ISO_A3_EH" = "iso3c")) %>%
  left_join(full_faction_laged_map, by = c("ISO_A3_EH" = "iso3c")) #%>%
  #select(NAME_EN, region, factionalization, avgethdiff, geometry)

# Aggregated rel diff for map

avg_rel_diff <- full_faction_laged_2 %>% 
  group_by(iso3c) %>% 
  summarize(avgreldiff = mean(rel_diff)) %>% 
  filter(iso3c != 'NA')

avg_reldiff_map <- countries %>% 
  left_join(avg_rel_diff, by = c("ISO_A3_EH" = "iso3c")) %>%
  left_join(full_faction_laged_map, by = c("ISO_A3_EH" = "iso3c")) #%>%
  #select(NAME_EN, region, factionalization, avgreldiff, geometry)

# Aggregated pop change

avg_pop_diff <- full_faction_laged_2 %>% 
  filter(avgpercdiff_5years != 'NA') %>% 
  group_by(iso3c) %>% 
  summarize(avgpopdiff = mean(avgpercdiff_5years)) %>% 
  filter(iso3c != 'NA')

avg_popdiff_map <- countries %>% 
  left_join(avg_pop_diff, by = c("ISO_A3_EH" = "iso3c")) %>%
  left_join(full_faction_laged_map, by = c("ISO_A3_EH" = "iso3c")) #%>%
  #select(NAME_EN, region, factionalization, avgpopdiff, geometry)

# create the ui for the app
ui <- fluidPage(
  
  # Add Title Panel
  titlePanel('Demographic Indicators of Intra-State Conflicts'),
  
  sidebarLayout(
    sidebarPanel(
      # add a way for Users to choose which Variable they are viewing
      selectInput(
        inputId = 'selected_variable',
        label = 'Select Which Variable to Display',
        choices = variableChoices),
      checkboxGroupInput(
        inputId = 'selected_region',
        label = 'Select Which Regions to Display',
        choices = regionChoices,
        selected = regionChoices
      ),
      checkboxGroupInput(
        inputId = 'selected_faction',
        label = 'Select Which Regime types to Display',
        choices = factionChoices,
        selected = factionChoices
      )
    ),
    mainPanel(tabsetPanel(
      tabPanel('Graphs',plotOutput('figurePlot')),
      tabPanel('Maps',plotOutput('mapPlot')),
      tabPanel("Citations", textOutput('citationText'), textOutput('citationText2'))
    ))
  ))

server <- function(input, output) {
  
  # make the figure renders
  output$figurePlot <- renderPlot({
    if (input$selected_variable == 'Population Change') {
      full_faction_laged %>%
        filter(is.finite(percent_diff1) & is.finite(percent_diff3) & is.finite(percent_diff5)) %>%
        filter(is.finite(onset_lead) & onset_lead != 4) %>%
        mutate(Onset_Next_Year = ifelse(onset_lead == 1, "War", "No war")) %>%
        # filter for the deisred regime types and regions
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        mutate(faction_war = paste(factionalization, 'and:', Onset_Next_Year)) %>%
        ggplot() +
        geom_boxplot(aes(y = avgpercdiff_3years, fill = as.factor(faction_war))) +
        ylim(c(-5,10)) +
        labs(x = 'Regime Type and Future Conflict', y = 'Average Pop. Diff. over 5 years (%)',
             title = 'Population Change by Regime Type and Future Conflict Status',
             fill = 'Regime Type and Future Conflict') +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else if (input$selected_variable == 'Ethnic Difference'){
      full_faction_laged %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>% 
        ggplot()+
        geom_boxplot(aes(x = At_war, y = eth_diff))+
        xlab("State Experienced Intrastate Conflict?")+
        ylab('Ethnic Difference (Percentage Points)')+ 
        ggtitle('Relationship Between Magnitude of Ethnic Difference and Civil War')
    } else if (input$selected_variable == 'Ethnic Fractionalization'){
      full_faction_laged %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>% 
        ggplot(aes(x= At_war,y=ef)) +
        geom_boxplot()+
        xlab("State Experienced Intrastate Conflict?")+
        ylab("Ethnic Fractionalization")+
        ggtitle("Relationship Between Ethnic Fractionalization and Civil War")
    } else if (input$selected_variable == 'Religious Fractionalization'){
      full_faction_laged %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot(aes(x=At_war,y=relfrac)) +
        geom_boxplot()+
        xlab("State Experienced Intrastate Conflict?")+
        ylab("Religious Fractionalization")+
        ggtitle("Relationship Between Religious Fractionalization and Civil War")
    } else if (input$selected_variable == 'Religious Difference'){
      full_faction_laged %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_boxplot(aes(x = At_war, y = rel_diff))+
        xlab("State Experienced Intrastate Conflict?")+
        ylab('Religious Difference (Percentage Points)')+ 
        ggtitle('Relationship Between Magnitude of Religious Difference and Civil War')
    }
  })
  
  # make the maps
  output$mapPlot <- renderPlot({
    if (input$selected_variable == 'Population Change') {
      avg_popdiff_map %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_sf(aes(fill = avgpopdiff))+
        scale_fill_continuous(type = 'viridis', name = '% Population Change per Year')+
        ggtitle("Average Population Change After 5 Years, 1945-1999")
    } else if (input$selected_variable == 'Religious Fractionalization') {
      avg_rel_map %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_sf(aes(fill = avgrelig))+
        scale_fill_continuous(type = 'viridis', name = 'Religious Fractionalization')+
        ggtitle("Average Religious Fractionalization, 1945-1999")
    } else if (input$selected_variable == 'Ethnic Fractionalization') {
      avg_ef_map %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_sf(aes(fill = avgethnic))+
        scale_fill_continuous(type = 'viridis', name = 'Ethnic Fractionalization')+
        ggtitle("Average Ethnic Fractionalization, 1945-1999")
    } else if (input$selected_variable == "Ethnic Difference"){
      avg_ethdiff_map %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_sf(aes(fill = avgethdiff))+
        scale_fill_continuous(type = 'viridis', name = 'Ethnic Difference')+
        ggtitle("Average Ethnic Difference, 1945-1999")
    } else if (input$selected_variable == 'Religious Difference'){
      avg_reldiff_map %>% 
        filter(region %in% input$selected_region) %>%
        filter(factionalization %in% input$selected_faction) %>%
        ggplot()+
        geom_sf(aes(fill = avgreldiff))+
        scale_fill_continuous(type = 'viridis', name = 'Religious Difference')+
        ggtitle("Average Religious Difference, 1945-1999")
    }
  })
  output$citationText <- renderText({
    '-Fearon, James D., and David D.Laitin. "Ethnicity, Insurgency, and Civil War." American Political Science Review. Vol. 97, No. 1. February 2003.'
  })
  
  output$citationText2 <- renderText({
    '-Marshall, Monty G., and Ted Robert Gur. "Political Regime Characteristics and Transitions, 1800-2018." Polity5, Polity Project. Center for Systemic Peace. April 23, 2020. www.systemicpeace.org'
  })
  
}

# run the app  
shinyApp(ui = ui, server = server)
