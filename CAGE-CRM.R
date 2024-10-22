#
# Install required packages if not installed
required_packages <- c("shiny", "shinythemes", "bslib")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load required packages
library(shiny)
library(shinythemes)
library(bslib)

# Define average predicted max lifespan values (based on global statistics)
average_max_lifespan_male <- 76
average_max_lifespan_female <- 81

# Define the UI (User Interface) with a polished theme and enhanced styling
ui <- fluidPage(
  
  # Apply a custom theme and additional styling
  theme = shinytheme("flatly"),
  
  # Custom CSS for larger text and better aesthetics
  tags$style(HTML("
    h2 {
      font-size: 32px;
      color: #2C3E50;
      text-align: center;
      margin-bottom: 30px;
    }
    h3 {
      font-size: 24px;
      color: #18BC9C;
      text-align: left;
      margin-top: 20px;
    }
    p, label, .text-output {
      font-size: 18px;
      color: #34495E;
    }
    .risk-output {
      font-weight: bold;
      font-size: 22px;
      color: #E74C3C;
      margin: 10px 0;
    }
    .lifespan-output {
      font-weight: bold;
      font-size: 24px;
      color: #3498DB;
      margin: 20px 0;
    }
    .panel {
      background-color: #ECF0F1;
      padding: 15px;
      border-radius: 10px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      margin-bottom: 20px;
    }
    .result-panel {
      background-color: #FFFFFF;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 6px 8px rgba(0, 0, 0, 0.1);
      margin-bottom: 30px;
    }
    footer {
      text-align: center;
      font-size: 12px;
      color: #7F8C8D;
    }
  ")),
  
  titlePanel(tags$h2("Comprehensive AGE estimator based on Cardiovascular Risk Models")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "panel",
          numericInput("age", tags$h4("Enter your current age:"), value = 30, min = 18, max = 120),
          numericInput("cholesterol", tags$h4("Total cholesterol level (mg/dL):"), value = 200, min = 100, max = 400),
          numericInput("hdl", tags$h4("HDL cholesterol level (mg/dL):"), value = 50, min = 20, max = 100),
          numericInput("sysBP", tags$h4("Systolic blood pressure (mmHg):"), value = 120, min = 90, max = 200),
          selectInput("smoker", tags$h4("Smoking status:"), choices = c("No", "Yes"), selected = "No"),
          selectInput("diabetes", tags$h4("Diabetes status:"), choices = c("No", "Yes"), selected = "No"),
          selectInput("gender", tags$h4("Select your gender:"), choices = c("Female", "Male"), selected = "Female"),
          numericInput("bmi", tags$h4("Body Mass Index (BMI):"), value = 25, min = 10, max = 50),
          numericInput("hscrp", tags$h4("hsCRP level (mg/L):"), value = 1, min = 0, max = 10),
          selectInput("familyCVD", tags$h4("Family history of premature CVD:"), choices = c("No", "Yes"), selected = "No"),
          numericInput("cac", tags$h4("Coronary Artery Calcium (CAC) Score (MESA only):"), value = 0, min = 0, max = 400),
          selectInput("ethnicity", tags$h4("Select Ethnicity (for QRISK/PCE):"), choices = c("White", "Black", "South Asian", "Other"), selected = "White"),
          selectInput("deprivation", tags$h4("Social Deprivation Status (for QRISK and ASSIGN):"), choices = c("No", "Yes"), selected = "No"),
          selectInput("atrialFib", tags$h4("Atrial Fibrillation (for QRISK and CHA2DS2-VASc):"), choices = c("No", "Yes"), selected = "No")
      )
    ),
    
    mainPanel(
      # Output the results with improved spacing, alignment, and aesthetic text
      div(class = "result-panel",
          tags$h3("Risk Scores from Cardiovascular Risk Models"),
          tags$hr(),
          div(class = "risk-output", uiOutput("frsScore")),
          div(class = "risk-output", uiOutput("qriskScore")),
          div(class = "risk-output", uiOutput("reynoldsScore")),
          div(class = "risk-output", uiOutput("scoreChart")),
          div(class = "risk-output", uiOutput("pceScore")),
          div(class = "risk-output", uiOutput("mesaScore")),
          div(class = "risk-output", uiOutput("whoScore")),
          div(class = "risk-output", uiOutput("chadsScore")),
          div(class = "risk-output", uiOutput("globoriskScore")),
          div(class = "risk-output", uiOutput("assignScore"))
      ),
      
      div(class = "result-panel",
          tags$h3("Composite Risk and AGE estimation"),
          tags$hr(),
          div(class = "risk-output", textOutput("compositeRisk")),
          div(class = "lifespan-output", textOutput("lifespanPrediction")),
          textOutput("averageMaxLifespan")
      ),
      
      tags$footer(
        tags$hr(),
        tags$p(
          "Medical Disclaimer: This tool is for informational purposes only and does not substitute for professional medical advice.",
          style = "font-size: 11px; color: #7F8C8D; text-align: left;"
        ),
        tags$p(
          paste("© Sayan Mitra", format(Sys.Date(), "%Y")), 
          style = "font-size: 9px; color: #DFE5E6; text-align: center; position: fixed; bottom: 0; right: 0; width: 100%; padding-right: 20px;"
        )
      )
    )
  )
)

# Define server logic for predictions
server <- function(input, output) {
  
  # Framingham Risk Score (FRS)
  calculate_frs <- function(age, gender, cholesterol, hdl, sysBP, smoker, diabetes) {
    if (gender == "Male") {
      beta_age <- 3.06117
      beta_chol <- 1.12370
      beta_hdl <- -0.93263
      beta_sysBP <- 1.93303
      beta_smoke <- 0.65451
      beta_diabetes <- 0.57367
    } else {
      beta_age <- 2.32888
      beta_chol <- 1.20904
      beta_hdl <- -0.70833
      beta_sysBP <- 2.76157
      beta_smoke <- 0.52873
      beta_diabetes <- 0.69154
    }
    
    score <- (beta_age * log(age)) +
      (beta_chol * log(cholesterol)) +
      (beta_hdl * log(hdl)) +
      (beta_sysBP * log(sysBP)) +
      (beta_smoke * ifelse(smoker == "Yes", 1, 0)) +
      (beta_diabetes * ifelse(diabetes == "Yes", 1, 0))
    
    risk <- 1 - 0.88936^exp(score - 26.1931)
    return(risk)
  }
  
  # QRISK Score
  calculate_qrisk <- function(age, gender, cholesterol, sysBP, smoker, bmi, familyCVD, diabetes, ethnicity, deprivation, atrialFib) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (sysBP - 120) * 0.03 + (bmi - 25) * 0.05
    if (smoker == "Yes") score <- score + 2
    if (familyCVD == "Yes") score <- score + 1
    if (diabetes == "Yes") score <- score + 2
    if (deprivation == "Yes") score <- score + 1
    if (atrialFib == "Yes") score <- score + 1
    return(score)
  }
  
  # Reynolds Risk Score
  calculate_reynolds <- function(age, sysBP, cholesterol, hdl, hscrp, smoker, familyCVD) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (hdl - 50) * -0.02 + (sysBP - 120) * 0.03 + hscrp * 0.05
    if (smoker == "Yes") score <- score + 2
    if (familyCVD == "Yes") score <- score + 1
    return(score)
  }
  
  # SCORE (Systematic Coronary Risk Evaluation)
  calculate_score <- function(age, gender, cholesterol, sysBP, smoker) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (sysBP - 120) * 0.03
    if (smoker == "Yes") score <- score + 2
    return(score)
  }
  
  # Pooled Cohort Equations (PCE)
  calculate_pce <- function(age, gender, cholesterol, hdl, sysBP, smoker, diabetes, ethnicity) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (hdl - 50) * -0.02 + (sysBP - 120) * 0.03
    if (smoker == "Yes") score <- score + 2
    if (diabetes == "Yes") score <- score + 2
    return(score)
  }
  
  # WHO CVD Risk Score
  calculate_who <- function(age, gender, sysBP, smoker, diabetes) {
    score <- (age - 40) * 0.1 + (sysBP - 120) * 0.03
    if (smoker == "Yes") score <- score + 2
    if (diabetes == "Yes") score <- score + 2
    return(score)
  }
  
  # MESA Risk Score
  calculate_mesa <- function(age, cholesterol, sysBP, smoker, familyCVD, diabetes, cac) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (sysBP - 120) * 0.03 + cac * 0.05
    if (smoker == "Yes") score <- score + 2
    if (familyCVD == "Yes") score <- score + 1
    if (diabetes == "Yes") score <- score + 2
    return(score)
  }
  
  # CHADS2 / CHA2DS2-VASc
  calculate_chads <- function(age, sysBP, diabetes, smoker, atrialFib) {
    score <- ifelse(age >= 75, 2, ifelse(age >= 65, 1, 0)) +
      ifelse(diabetes == "Yes", 1, 0) +
      ifelse(atrialFib == "Yes", 1, 0) +
      ifelse(smoker == "Yes", 1, 0)
    return(score)
  }
  
  # Globorisk
  calculate_globorisk <- function(age, gender, cholesterol, sysBP, smoker, diabetes) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (sysBP - 120) * 0.03
    if (smoker == "Yes") score <- score + 2
    if (diabetes == "Yes") score <- score + 2
    return(score)
  }
  
  # ASSIGN Score
  calculate_assign <- function(age, gender, cholesterol, sysBP, smoker, deprivation, familyCVD) {
    score <- (age - 40) * 0.1 + (cholesterol - 200) * 0.02 + (sysBP - 120) * 0.03
    if (smoker == "Yes") score <- score + 2
    if (familyCVD == "Yes") score <- score + 1
    if (deprivation == "Yes") score <- score + 1
    return(score)
  }
  
  # Render results for each model with hyperlinks
  output$frsScore <- renderUI({
    frs <- calculate_frs(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes)
    HTML(paste("Framingham Risk Score:", round(frs, 2), '<a href="https://en.wikipedia.org/wiki/Framingham_Risk_Score" target="_blank">[Info]</a>'))
  })
  
  output$qriskScore <- renderUI({
    qrisk <- calculate_qrisk(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker, input$bmi, input$familyCVD, input$diabetes, input$ethnicity, input$deprivation, input$atrialFib)
    HTML(paste("QRISK Score:", round(qrisk, 2), '<a href="https://en.wikipedia.org/wiki/QRISK" target="_blank">[Info]</a>'))
  })
  
  output$reynoldsScore <- renderUI({
    reynolds <- calculate_reynolds(input$age, input$sysBP, input$cholesterol, input$hdl, input$hscrp, input$smoker, input$familyCVD)
    HTML(paste("Reynolds Risk Score:", round(reynolds, 2), '<a href="https://www.sciencedirect.com/topics/medicine-and-dentistry/reynolds-risk-score" target="_blank">[Info]</a>'))
  })
  
  output$scoreChart <- renderUI({
    score <- calculate_score(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker)
    HTML(paste("SCORE Chart:", round(score, 2), '<a href="https://www.jacc.org/doi/10.1016/j.jacc.2021.04.052" target="_blank">[Info]</a>'))
  })
  
  output$pceScore <- renderUI({
    pce <- calculate_pce(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes, input$ethnicity)
    HTML(paste("Pooled Cohort Equations Score:", round(pce, 2), '<a href="https://clincalc.com/cardiology/ascvd/pooledcohort.aspx#:~:text=The%20purpose%20of%20the%20Pooled,these%20events%20in%20the%20past." target="_blank">[Info]</a>'))
  })
  
  output$mesaScore <- renderUI({
    mesa <- calculate_mesa(input$age, input$cholesterol, input$sysBP, input$smoker, input$familyCVD, input$diabetes, input$cac)
    HTML(paste("MESA Risk Score:", round(mesa, 2), '<a href="https://doi.org/10.1161/JAHA.120.019351" target="_blank">[Info]</a>'))
  })
  
  output$whoScore <- renderUI({
    whoRisk <- calculate_who(input$age, input$gender, input$sysBP, input$smoker, input$diabetes)
    HTML(paste("WHO CVD Risk Score:", round(whoRisk, 2), '<a href="https://www.who.int/news/item/02-09-2019-who-updates-cardiovascular-risk-charts" target="_blank">[Info]</a>'))
  })
  
  output$chadsScore <- renderUI({
    chads <- calculate_chads(input$age, input$sysBP, input$diabetes, input$smoker, input$atrialFib)
    HTML(paste("CHADS2 / CHA2DS2-VASc Score:", round(chads, 2), '<a href="https://en.wikipedia.org/wiki/CHA2DS2%E2%80%93VASc_score" target="_blank">[Info]</a>'))
  })
  
  output$globoriskScore <- renderUI({
    globorisk <- calculate_globorisk(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker, input$diabetes)
    HTML(paste("Globorisk Score:", round(globorisk, 2), '<a href="https://www.globorisk.org/" target="_blank">[Info]</a>'))
  })
  
  output$assignScore <- renderUI({
    assign <- calculate_assign(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker, input$deprivation, input$familyCVD)
    HTML(paste("ASSIGN Score:", round(assign, 2), '<a href="https://www.assign-score.com/" target="_blank">[Info]</a>'))
  })
  
  # Composite Risk Score
  output$compositeRisk <- renderText({
    frs <- calculate_frs(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes)
    qrisk <- calculate_qrisk(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker, input$bmi, input$familyCVD, input$diabetes, input$ethnicity, input$deprivation, input$atrialFib)
    reynolds <- calculate_reynolds(input$age, input$sysBP, input$cholesterol, input$hdl, input$hscrp, input$smoker, input$familyCVD)
    score <- calculate_score(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker)
    pce <- calculate_pce(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes, input$ethnicity)
    mesa <- calculate_mesa(input$age, input$cholesterol, input$sysBP, input$smoker, input$familyCVD, input$diabetes, input$cac)
    
    composite <- (frs + qrisk + reynolds + score + pce + mesa) / 6
    paste("Composite Risk Score:", round(composite, 2))
  })
  
  # Lifespan Prediction
  output$lifespanPrediction <- renderText({
    composite <- (calculate_frs(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes) +
                    calculate_qrisk(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker, input$bmi, input$familyCVD, input$diabetes, input$ethnicity, input$deprivation, input$atrialFib) +
                    calculate_reynolds(input$age, input$sysBP, input$cholesterol, input$hdl, input$hscrp, input$smoker, input$familyCVD) +
                    calculate_score(input$age, input$gender, input$cholesterol, input$sysBP, input$smoker) +
                    calculate_pce(input$age, input$gender, input$cholesterol, input$hdl, input$sysBP, input$smoker, input$diabetes, input$ethnicity) +
                    calculate_mesa(input$age, input$cholesterol, input$sysBP, input$smoker, input$familyCVD, input$diabetes, input$cac)) / 6
    
    if (input$gender == "Male") {
      average_lifespan <- average_max_lifespan_male
    } else {
      average_lifespan <- average_max_lifespan_female
    }
    
    predicted_lifespan <- average_lifespan - composite
    if (input$age > predicted_lifespan) {
      "You have exceeded the average predicted lifespan!"
    } else {
      remaining_years <- predicted_lifespan - input$age
      paste("Estimated remaining years of life:", round(remaining_years, 2))
    }
  })
  
  # Display the average maximum lifespan based on gender
  output$averageMaxLifespan <- renderText({
    if (input$gender == "Male") {
      paste("Average maximum lifespan for males: ", average_max_lifespan_male, " years.")
    } else {
      paste("Average maximum lifespan for females: ", average_max_lifespan_female, " years.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
