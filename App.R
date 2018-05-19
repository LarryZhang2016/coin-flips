library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)


# key function ------------------------------------------------------------
create_plot_4_coin_flips <- function(p = 0.5, epsilon = 0.03, N = 3000)
{# p is in [0.05, 0.95]
  the_df <-
    data.frame(flip_number = 1:N,
             flips = rbinom(n = N, size = 1, prob = p)) 
  
  the_df <-
    the_df %>% 
    mutate(p_hat = round(cumsum(flips) / flip_number, 4)) %>% 
    filter(flip_number > 100)
  
  the_plot <- the_df %>% 
    ggplot(aes(x = flip_number, y = p_hat)) +
    geom_point(size = 0.25) +
    geom_line(size = 0.25) +
    geom_hline(yintercept = p, color = 'coral', size = 0.25) +
    geom_hline(yintercept = p - epsilon, color = 'red', size = 0.25) +
    geom_hline(yintercept = p + epsilon, color = 'red', size = 0.25) +
    labs(x = "Number of flips", y = "p-hat")
  
  the_re <- ggplotly(the_plot) %>% config(displayModeBar = FALSE)
  
  the_re
  
}


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(title = "Flip a coin many times"),
  sidebarPanel(width = 4,
               sliderInput("HeadsPro", label = "choose p (probability of heads)", 
                           value = 0.5, min = 0.05, max = 0.95)
  ),
  mainPanel(wideth = 8, plotlyOutput("flip_plot"),
            br(),
            p(strong("Conclusion:"),  "When the true p is in [0.05, 0.95], we are almost 
              sure that 3000 flips will result in the p-hat falling in the range (p-0.03, p+0.03).")
))


# server ------------------------------------------------------------------
server <- function(input, output)
{output$flip_plot <- renderPlotly({
  create_plot_4_coin_flips(p = input$HeadsPro)
 })

}

shinyApp(ui = ui, server = server)