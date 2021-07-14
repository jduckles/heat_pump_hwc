
hwcCalculatorUI <- function(id, panel_name="Hot Water Cylinder Payoff") {
  
  ns <- NS(id)
  
  tabPanel(panel_name, { 
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("elec_rate"), label = "Electricity rate $", 
                  value = 0.31),
        numericInput(ns("inflation"), label = "Inflation rate 0-1",
                  value = 0.015),
        numericInput(ns("hours"), label = "# Hours HWC runs per day <24",
                  val = 3),
        numericInput(ns("element"), label = "# Element size in kW",
                  val = 4),
        numericInput(ns("nyears"), label = "Number of years to model",
                  val = 20), 
        numericInput(ns("perf_coeff"), label = "Performance coefficient of Heat Pump", 
                  val = 3.9),
        numericInput(ns("hwc_cost"), label = "Cost of conventional cylinder",
                  val = 2000),
        numericInput(ns("hp_cost"), label = "Cost of Heat Pump cylinder",
                  val = 4000)
      ), 
      mainPanel(
        plotOutput(ns("payoff")) 
        )
    )
  })
  
}

hwcCalculator <- function(id) { 
  
  moduleServer(id, function(input, output, session) {
   output$payoff <- renderPlot({
 
     plot_hwc_payoff(inflation = input$inflation,
                     elec_rate = input$elec_rate, 
                     hours = input$hours,
                     element = input$element,
                     nyears = input$nyears,
                     perf_coeff = input$perf_coeff,
                     hwc_cost = input$hwc_cost,
                     hp_cost = input$hp_cost) %>% 
     ggplot(aes(x=year, y=value, color=name)) + geom_line() + 
       xlab("Year") + ylab("Cummulative Cost $")
      })
   
   
  })
}