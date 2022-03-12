library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv'
stressor <- read_csv(url)

stressor['start_month'] <- str_split_fixed(stressor$months, '-', 2)[,1]
stressor['year'] <- as.character(stressor$year)
stressor['start_month'] <- as.integer(factor(stressor$start_month, levels = month.name))
stressor["time"] <- paste0(stressor$year,"-",stressor$start_month,"-","01")
stressor <- stressor %>% select(-c(year, months, start_month))
stressor["period"] = paste0(year(stressor$time), "-", quarter(stressor$time)) 


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    dbcRow(
      list(dccGraph(id='plot-area'),
        dbcCol(
          list(
            htmlLabel('State'),
            dccDropdown(
              id='select_state',
              value='Alabama',
              options = unique(stressor$state %>%purrr::map(function(con) list(label = con, value = con))
              )
            )
          
  )
)
)
)))

app$callback(
  output('plot-area', 'figure'),
  list(input('select_state', 'value')),
  function(state) {
    p <- stressor %>%  filter(state == {{state}}) %>%
      ggplot(aes(x = period,
                 y = stress_pct,
                 fill = stressor,
                 text = stress_pct)) +
      geom_bar(position="stack", stat="identity") + 
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
  ggplotly(p)
  }
)
# app$run_server(debug = T)
app$run_server(host = '0.0.0.0')