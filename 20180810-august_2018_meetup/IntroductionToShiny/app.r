library(shiny)
library(shinydashboard)

ui <- dashboardPage( 
  dashboardHeader(
    
    #1 ADD A TITLE
    title = "Chronic Disease Dashboard"
    # ,titleWidth = 450
    ),
  dashboardSidebar(
    width = 450
    
    #ADD SOME INPUT FUNCTIONS
    
    # ,fluidRow(
    #   column(
    #     12,
    #     h2("RISK FACTORS", align = "center"),
    # 
    #     radioButtons("GENDER",
    #                  label = h3("Gender"),
    #                  choiceNames = c("Female", "Male"),
    #                  choiceValues = c("F", "M"),
    #                  selected = "M",
    #                  inline = T),
    # 
    #     sliderInput("AGE",
    #                 label = h3("Age"),
    #                 min = 18,
    #                 max = 80,
    #                 value = 55),
    # 
    #     sliderInput("Height",
    #                 label = h3("Height (in cm)"),
    #                 min = 70,
    #                 max = 220,
    #                 value = 180)
    #   )
    # )

    # ADD SOME MORE INPUTS
    
    # ,fluidRow(
    #   column(
    #     12,
    #     h2("Comorbidities", align = "center"),
    # 
    #     radioButtons("PREEXISTING_HYPERLIPIDAEMIA",
    #                  label = h3("Hyperlipidaemia present?"), choiceNames = c("No", "Yes"),
    #                  choiceValues = c("No", "Yes"), selected = "No", inline = T
    #     ),
    # 
    #     h2("Family history", align = "center"),
    #     radioButtons("FamilyHypertension",
    #                  label = h3("Family history of hypertension?"), choiceNames = c("No", "Yes"),
    #                  choiceValues = c("No", "Yes"), selected = "No", inline = T
    #     )
    #   )
    # )
    
    
    ),
  dashboardBody(
    
    #ADD A PLACEHOLDER FOR OUTPUT DEFINED LATER 
    
    # fluidRow(
    #     column(4,
    #            box( h3("Explaination of results goes here"),
    #                    align = "center",
    #                 flexdashboard::gaugeOutput("hypertensionPlot", height = "120px"),
    #                 h4("1 year development risk", align = "center"),
    #                 width = 12,
    #                 height = 400, title = h1("Hypertension", align = "center"), background = "navy")
    #            ))
    
    #ADD MORE INPUT SLIDERS IN THE DASHBOARD BODY
    
    # ,fluidRow(
    #   
    #   column(
    #     4,
    #     h2("MANAGEABLE RISK FACTORS", align = "center"),
    #     h3("Weight (in kg)"),
    #     sliderInput("Weight", label = NULL, min = 50, max = 170, value = 100),
    # 
    #     h3(textOutput("BMITextOutput"))
    #   ),
    # 
    # 
    #   column(
    #     4,
    #     h2("Lifestyle", align = "center"),
    # 
    #     h3("How many days do you work out per month?"),
    #     sliderInput("AVERAGE_PHYSICAL_ACTIVITY_EVENT_DAYS", label = NULL, min = 0, max = 31, value = 4, step = 1),
    # 
    # 
    #     h3("What proportion of your food basket contains:"),
    # 
    #     h4("Unhealthy starchy foods?"),
    #     sliderInput("BFC_UNHEALTHY_STARCHY_FOODS_PER_MEMBER_CT",
    #                 label = NULL,
    #                 min = 0, max = 0.5, value = 0.15, step = 0.05
    #     ),
    # 
    #     h4("Unhealthy convenience meals?"),
    #     sliderInput("BFC_UNHEALTHY_CONVENIENCE_MEALS_PER_MEMBER_CT",
    #                 label = NULL,
    #                 min = 0, max = 0.5, value = 0.10, step = 0.05
    #     ),
    # 
    #     h4("Unhealthy beverages?"),
    #     sliderInput("BFC_UNHEALTHY_BEVERAGES_PER_MEMBER_CT",
    #                 label = NULL,
    #                 min = 0, max = 0.5, value = 0.05, step = 0.05
    #     )
    #     )
    # )
)
)

server <- function(input, output) { 
  library(h2o)
  h2o.init()
  working = "C:/Users/brett27/Documents/Projects/Shiny presentation"
  glmlHypertension <- h2o.loadModel(paste0(working,"/hypertensionGLM"))

  vals <- reactiveValues()
  
  case_input <- reactive({
    vals$BMI <- round((input$Weight / (input$Height / 100)**2), 1)
    
    df <- data.frame(
      AGE = cut(as.numeric(input$AGE),
                breaks = c(17.99, 25.99, 35.99, 45.99, 55.99, Inf),
                labels = c("18-25", "26-35", "36-45", "46-55", "56+"), right = TRUE
      ),
      
      GENDER = input$GENDER,
      
      BMI_VALUE = as.numeric(vals$BMI),
      
      FAMILY_HYPERTENSION = input$FamilyHypertension,
      
      PREEXISTING_HYPERLIPIDAEMIA = input$PREEXISTING_HYPERLIPIDAEMIA,
      
      AVERAGE_PHYSICAL_ACTIVITY_EVENT_DAYS = input$AVERAGE_PHYSICAL_ACTIVITY_EVENT_DAYS,
   
      UNHEALTHY = input$BFC_UNHEALTHY_CONVENIENCE_MEALS_PER_MEMBER_CT +
        input$BFC_UNHEALTHY_STARCHY_FOODS_PER_MEMBER_CT +
        input$BFC_UNHEALTHY_BEVERAGES_PER_MEMBER_CT
    )
  })
  
  
  # HYPERTENSION
  
  case_pred_ci_hypertension <- reactive({
    case_input.hex <- as.h2o(req(case_input()))
    case_pred_hypertension <- h2o.predict(glmlHypertension, newdata = case_input.hex, type = "prob")
    case_pred_ci_hypertension <- as.data.frame(as.numeric(case_pred_hypertension["p1"]))
    case_pred_ci_hypertension <- as.numeric((case_pred_ci_hypertension[1, 1]))
    return(case_pred_ci_hypertension)
  })
  
  
  output$hypertensionPlot <- flexdashboard::renderGauge({
    val <- req(case_pred_ci_hypertension())
    
    flexdashboard::gauge(round(val,2),
                         min = 0, max = 1,
                         label = "Risk category",
                         flexdashboard::gaugeSectors(
                           success = c(0, 0.35), warning = c(0.351, 0.75), danger = c(0.751, 1)
                         )
    )
  })
  

  }

shinyApp(ui, server)

