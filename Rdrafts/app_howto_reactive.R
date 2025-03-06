# One approach would be to create a separate reactive to filter your data, and use the filtered data in your plot. First, I would consider converting your data to long format (e.g., using tidyverse pivot_longer). For example:

global_indices_2y2 <- data.frame(
  Date = as.POSIXct(c("2018-01-29", "2018-01-30", "2018-01-31", "2018-02-01")),
  Australia = c(107, 106, 106, 107),
  US = c(113,112,112,112),
  UK = c(104,103,102,102)
) %>%
  pivot_longer(cols = -Date, names_to = "country", values_to = "index")

# Then add reactive to filter based on multiple selections in your server:

mydata <- reactive({
  global_indices_2y2 %>%
    filter(country %in% input$global_indices_input)
})

# Then plot filtered data:

output$plot_3 <- renderPlotly({
  plot_3 <- plot_ly(
    mydata(), 
    x = ~Date, 
    y = ~index, 
    name = ~country,
    type="scatter", 
    mode="lines"
  )
})

