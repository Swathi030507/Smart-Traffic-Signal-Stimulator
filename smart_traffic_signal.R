library(shiny)

ui <- fluidPage(
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      
      body {
        background: linear-gradient(135deg, #0f2027, #203a43, #2c5364);
        font-family: Arial, sans-serif;
        color: white;
      }

      .container-fluid {
        padding: 25px;
      }

      .title-panel {
        background: linear-gradient(90deg, #00c6ff, #0072ff);
        padding: 25px;
        border-radius: 18px;
        text-align: center;
        margin-bottom: 25px;
        box-shadow: 0px 8px 20px rgba(0,0,0,0.3);
      }

      .title-panel h2 {
        font-weight: bold;
        margin: 0;
      }

      .card {
        background: rgba(255,255,255,0.12);
        backdrop-filter: blur(10px);
        border-radius: 18px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0px 8px 20px rgba(0,0,0,0.25);
      }

      .section-title {
        color: #00e5ff;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .form-control {
        background: #ffffff;
        color: #222222;
        border-radius: 10px;
        border: none;
        height: 42px;
      }

      label {
        color: white;
        font-weight: bold;
      }

      .btn-primary {
        background: linear-gradient(90deg, #00c6ff, #0072ff);
        border: none;
        border-radius: 12px;
        font-weight: bold;
        font-size: 16px;
        padding: 12px 25px;
        width: 100%;
        box-shadow: 0px 5px 15px rgba(0,0,0,0.3);
      }

      .btn-primary:hover {
        background: linear-gradient(90deg, #0072ff, #00c6ff);
      }

      .result-box {
        background: rgba(0,0,0,0.35);
        border-radius: 15px;
        padding: 25px;
        min-height: 250px;
        white-space: pre-line;
        font-size: 17px;
        line-height: 1.8;
        color: white;
      }

      .traffic-card {
        background: rgba(255,255,255,0.10);
        border-radius: 15px;
        padding: 18px;
        text-align: center;
        margin-bottom: 15px;
      }

      .road-name {
        font-size: 20px;
        font-weight: bold;
        color: #00e5ff;
      }

      .vehicle-count {
        font-size: 30px;
        font-weight: bold;
        color: white;
      }

      .footer {
        text-align: center;
        margin-top: 25px;
        color: #b8eaff;
        font-size: 14px;
      }

    "))
  ),
  
  # Header
  div(
    class = "title-panel",
    h2("🚦 SMART ADAPTIVE TRAFFIC SIGNAL"),
    p("Intelligent Traffic Management & Emergency Priority System")
  ),
  
  fluidRow(
    
    # LEFT SIDE
    column(
      5,
      
      div(
        class = "card",
        
        h3("🚗 Traffic Input", class = "section-title"),
        
        numericInput(
          "north",
          "North Road - Vehicles",
          50,
          min = 0
        ),
        
        numericInput(
          "south",
          "South Road - Vehicles",
          30,
          min = 0
        ),
        
        numericInput(
          "east",
          "East Road - Vehicles",
          15,
          min = 0
        ),
        
        numericInput(
          "west",
          "West Road - Vehicles",
          40,
          min = 0
        )
      ),
      
      div(
        class = "card",
        
        h3("🚨 Emergency Priority",
           class = "section-title"),
        
        selectInput(
          "emergency",
          "Emergency Vehicle",
          choices = c(
            "None",
            "🚑 Ambulance",
            "🚒 Fire Engine",
            "🚓 Police"
          )
        ),
        
        selectInput(
          "emergency_road",
          "Emergency Vehicle Road",
          choices = c(
            "North",
            "South",
            "East",
            "West"
          )
        ),
        
        br(),
        
        actionButton(
          "simulate",
          "🚦  RUN TRAFFIC SIMULATION",
          class = "btn-primary"
        )
      )
    ),
    
    # RIGHT SIDE
    column(
      7,
      
      div(
        class = "card",
        
        h3("📊 Traffic Overview",
           class = "section-title"),
        
        fluidRow(
          
          column(
            3,
            div(
              class = "traffic-card",
              div("NORTH", class = "road-name"),
              div(textOutput("north_display"),
                  class = "vehicle-count")
            )
          ),
          
          column(
            3,
            div(
              class = "traffic-card",
              div("SOUTH", class = "road-name"),
              div(textOutput("south_display"),
                  class = "vehicle-count")
            )
          ),
          
          column(
            3,
            div(
              class = "traffic-card",
              div("EAST", class = "road-name"),
              div(textOutput("east_display"),
                  class = "vehicle-count")
            )
          ),
          
          column(
            3,
            div(
              class = "traffic-card",
              div("WEST", class = "road-name"),
              div(textOutput("west_display"),
                  class = "vehicle-count")
            )
          )
        )
      ),
      
      div(
        class = "card",
        
        h3("🚦 Signal Status",
           class = "section-title"),
        
        div(
          class = "result-box",
          textOutput("result")
        )
      )
    )
  ),
  
  div(
    class = "footer",
    "Smart Traffic Signal Simulator | R Programming Project"
  )
)


# ==========================================
# SERVER
# ==========================================

server <- function(input, output) {
  
  # Display vehicle counts
  
  output$north_display <- renderText({
    input$north
  })
  
  output$south_display <- renderText({
    input$south
  })
  
  output$east_display <- renderText({
    input$east
  })
  
  output$west_display <- renderText({
    input$west
  })
  
  
  # Green time function
  
  calculate_green_time <- function(x) {
    
    if (x <= 10) {
      
      return(20)
      
    } else if (x <= 20) {
      
      return(30)
      
    } else if (x <= 30) {
      
      return(40)
      
    } else if (x <= 40) {
      
      return(50)
      
    } else {
      
      return(60)
    }
  }
  
  
  # Simulation
  
  output$result <- renderText({
    
    input$simulate
    
    roads <- c(
      "North",
      "South",
      "East",
      "West"
    )
    
    vehicles <- c(
      input$north,
      input$south,
      input$east,
      input$west
    )
    
    times <- sapply(
      vehicles,
      calculate_green_time
    )
    
    
    # Emergency vehicle
    
    if (input$emergency != "None") {
      
      emergency_name <- gsub(
        "^[^ ]+ ",
        "",
        input$emergency
      )
      
      road <- input$emergency_road
      
      result <- paste0(
        
        "🚨 EMERGENCY VEHICLE DETECTED!\n\n",
        
        emergency_name,
        " detected on ",
        road,
        " Road.\n\n",
        
        "🟢 ",
        road,
        " Road → GREEN (PRIORITY)\n\n"
        
      )
      
      
      for (i in 1:4) {
        
        if (roads[i] != road) {
          
          result <- paste0(
            result,
            "🔴 ",
            roads[i],
            " Road → RED\n"
          )
        }
      }
      
      
      result <- paste0(
        result,
        "\n🚑 Emergency vehicle has priority.\n",
        "Normal scheduling will resume after clearance."
      )
      
      result
      
      
    } else {
      
      # Normal traffic
      
      highest <- which.max(vehicles)
      
      result <- paste0(
        
        "✅ NORMAL TRAFFIC MODE\n\n",
        
        "Highest Traffic Road: ",
        roads[highest],
        "\n",
        
        "Vehicle Count: ",
        vehicles[highest],
        "\n",
        
        "Green Time: ",
        times[highest],
        " seconds\n\n"
      )
      
      
      for (i in 1:4) {
        
        if (i == highest) {
          
          result <- paste0(
            
            result,
            
            "🟢 ",
            roads[i],
            " → GREEN (",
            times[i],
            " seconds)\n"
          )
          
        } else {
          
          result <- paste0(
            
            result,
            
            "🔴 ",
            roads[i],
            " → RED\n"
          )
        }
      }
      
      result
    }
  })
}


# Run App

shinyApp(
  ui = ui,
  server = server
)