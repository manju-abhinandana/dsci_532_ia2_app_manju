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
# Read in global data

stressor <-  readr::read_csv("data/stressor.csv")
stressor


# Wrangle data
stressor <- stressor %>%
  tidyr::separate(months, into = c("start_month","start_month2"), sep = "-") %>%
  dplyr::mutate(time = lubridate::ym(paste(year, start_month)),
                period = lubridate::quarter(time, type = "year.quarter")) %>% 
  dplyr::select(state, stressor,stress_pct, time, period)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id = "stressor_chart"),
      dccDropdown(
        id = "state-widget",
        options = unique(stressor$state),
        value = "Alabama"
      ),
      dccDropdown(
        id = "start-date-widget",
        options = stressor$period %>%
          unique() %>%
          purrr::map(function(p)
            list(
              label = stringr::str_replace(as.character(p), stringr::fixed("."), "Q"),
              value = p
            )),
        value = 2015.1
      ),
      dccDropdown(
        id = "end-date-widget",
        options = stressor$period %>%
          unique() %>%
          purrr::map(function(p)
            list(
              label = stringr::str_replace(as.character(p), stringr::fixed("."), "Q"),
              value = p
            )),
        value = 2015.4
      )
    )
  )
)

# Plot stressor chart

app$callback(
  output('stressor_chart', 'figure'),
  list(
    input('state-widget', 'value'),
    input('start-date-widget', 'value'),
    input('end-date-widget', 'value') 
  ),
  plot_stressor_chart <- function(state_arg, start_date, end_date) {
    start_date <- lubridate::ym(start_date)
    end_date <- lubridate::ym(end_date)
    
    data <- stressor %>%  filter(state == state_arg, 
                                 lubridate::ym(period) %within% lubridate::interval(start = start_date,
                                                                                    end = end_date))
    
    plot_stressor <- data %>% ggplot(aes(x = stringr::str_replace(as.character(period), stringr::fixed("."), "Q"),
                                         y = stress_pct,
                                         fill = stressor)) +
      geom_bar(position="stack", stat="identity") + 
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
    
    
    ggplotly(plot_stressor, tooltip = c("y", "fill"))
  }
)

app$run_server(host = '0.0.0.0')