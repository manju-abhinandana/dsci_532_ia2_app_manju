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


start_date_values = (unique(stressor[['period']]))
end_date_values = (unique(stressor[['period']]))
state_values = (unique(stressor[['state']]))

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
              options = state_values,
              className = 'text-dark'
              )
            )
          ),
        dbcCol( 
          list(
            htmlLabel('Start time'),
            dccDropdown(
              id='select_start_date',
              value='2015-1',
              options = start_date_values,
              className = 'text-dark'
              )
            )
          ),
        dbcCol(
          list(
            htmlLabel('End time'),
            dccDropdown(
              id='select_end_date',
              value='2015-4',
              options = end_date_values,
              className = 'text-dark'
              )
            )
          )
  )
)
)
)

app$callback(
  output('plot-area', 'figure'),
  list(input('select_state', 'value'),
       input('select_start_date', 'value'),
       input('select_end_date', 'value')),
  function(state, start_date, end_date) {
    p <- stressor %>%  filter(state == {{state}} & (period >= {{start_date}} & period <= {{end_date}})) %>%
      ggplot(aes(x = period,
                 y = stress_pct,
                 fill = stressor,
                 text = stress_pct)) +
      geom_bar(position="stack", stat="identity") + 
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
  ggplotly(p)
  }
)
# app$run_server(debug = T)
app$run_server(host = '0.0.0.0')