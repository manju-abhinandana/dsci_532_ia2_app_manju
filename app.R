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
library(ggthemes)
# Read in global data

stressor <-  readr::read_csv("data/stressor.csv")
stressor

# url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv'
# stressor <- read_csv(url)

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
        # options = unique(stressor$state),
        options = stressor$state %>%
          unique() %>%
          purrr::map(function(p)
            list(label = p, value = p)),
        value = "Alabama"
      )
    )
  )
)

# Plot stressor chart

app$callback(
  output('stressor_chart', 'figure'),
  list(
    input('state-widget', 'value')
  ),
  plot_stressor_chart <- function(state_arg) {

    
    data <- stressor %>%  filter(state == state_arg)
    
    plot_stressor <- data %>% ggplot(aes(x = period),
                                         y = stress_pct,
                                         fill = stressor)) +
      geom_bar(position="stack", stat="identity") + 
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title = 'Bee colony stressors', x = 'Time period', y = 'Impacted colonies(%)')
    
    
    ggplotly(plot_stressor, tooltip = c("y", "fill"))
  }
)

app$run_server(host = '0.0.0.0')