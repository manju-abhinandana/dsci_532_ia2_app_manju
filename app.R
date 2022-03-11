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


start_date = unique(stressor[['period']])
end_date = unique(stressor[['period']])
state = unique(stressor[['state']])

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    dbcRow(
      list(dccGraph(id='plot-area'),
        dbcCol(
          list(
            htmlLabel('State'),
            dccDropdown(
              id='state',
              options = state,
              value='Alabama')
            )
          ),
        dbcCol( 
          list(
            htmlLabel('Start time'),
            dccDropdown(
              id='start_time',
              options = start_date,
              value='2015-1')
            )
          ),
        dbcCol(
          list(
            htmlLabel('End time'),
            dccDropdown(
              id='end_time',
              options = end_date,
              value='2015-4')
            )
          )
  )
)
)
)


app$callback(
  output('plot-area', 'figure'),
  list(input('state', 'value'),
       input('start_time', 'value'),
       input('end_time', 'value')),
  function(state, start_time, end_time) {
    p <- stressor %>%  filter(state == {{state}} & (period >= {{start_time}} & period <= {{end_time}})) %>%
      ggplot(aes(x = period,
                 y = stress_pct,
                 fill = stressor,
                 text = stress_pct)) +
      geom_bar(position="stack", stat="identity") 
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
  ggplotly(p)
  }
)
# app$run_server(debug = T)
app$run_server(host = '0.0.0.0')