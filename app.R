library(shiny)
library(ggplot2)
library(shinythemes)
library(performance)
library(marginaleffects)
library(survival)
library(ggfortify)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Stat App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var1Select"),
      uiOutput("var2Select"),
      uiOutput("var3Select"),
      selectInput("testSelect", "Select Test to Run", choices = c("T-Test", "Chi-Square Test", "Fisher's Exact Test", "One-Way ANOVA", "Kruskal-Wallis Test", "Linear Regression", "Log-Rank Test")),
      actionButton("run", "Run Test", class = "btn btn-primary"),
      uiOutput("varClassSelect"),
      actionButton("convertToNumeric", "Convert to Numeric", class = "btn btn-secondary"),
      actionButton("convertToFactor", "Convert to Factor", class = "btn btn-secondary")
    ),
    
    mainPanel(
      verbatimTextOutput("testResult"),
      verbatimTextOutput("testExplanation"),
      plotOutput("testPlot"),
      verbatimTextOutput("normality"),
      verbatimTextOutput("normalityExplanation"),
      verbatimTextOutput("homoscedasticity"),
      verbatimTextOutput("homoscedasticityExplanation"),
      verbatimTextOutput("expectedCounts"),
      tabsetPanel(
        tabPanel("Data Glimpse", tableOutput("dataGlimpse"), tableOutput("dataClasses"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactiveVal(NULL)
  
  observe({
    req(input$file)
    dataset(read.csv(input$file$datapath))
  })
  
  output$var1Select <- renderUI({
    req(dataset())
    label <- if (input$testSelect == "T-Test" | input$testSelect == "Linear Regression") "Select Dependent Variable" else if (input$testSelect == "Log-Rank Test") "Select Time Variable" else "Select Variable 1"
    selectInput("var1", label, choices = names(dataset()))
  })
  
  output$var2Select <- renderUI({
    req(dataset())
    label <- if (input$testSelect == "T-Test" | input$testSelect == "Linear Regression") "Select Independent Variable" else if (input$testSelect == "Log-Rank Test") "Select Status Variable" else "Select Variable 2"
    selectInput("var2", label, choices = names(dataset()))
  })
  
  output$var3Select <- renderUI({
    req(dataset())
    if (input$testSelect == "Log-Rank Test") {
      selectInput("var3", "Select Group Variable", choices = names(dataset()))
    }
  })
  
  output$testResult <- renderPrint({
    req(input$run)
    isolate({
      
      if (input$testSelect == "T-Test") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        t.test(var1 ~ var2)
      } else if (input$testSelect == "Chi-Square Test") {
        chisq.test(table(var1, var2))
      } else if (input$testSelect == "Fisher's Exact Test") {
        fisher.test(table(var1, var2))
      } else if (input$testSelect == "One-Way ANOVA") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        summary(aov(var1 ~ var2))
      } else if (input$testSelect == "Linear Regression") {
        avg_slopes(lm( as.formula(paste(input$var1, "~", input$var2)),data = dataset()))
      } else if (input$testSelect == "Log-Rank Test") {
        survdiff(as.formula(paste0("Surv(", input$var1, ", ", input$var2, ") ~ ", input$var3)),data = dataset())
      } else {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        kruskal.test(var1 ~ var2)
      }
    })
  })
  
  output$testExplanation <- renderPrint({
    req(input$run)
    isolate({
      if (input$testSelect == "T-Test") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        ttest_result <- t.test(var1 ~ var2)
        if (ttest_result$p.value < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant difference between the means of the dependent variable for different levels of the independent variable."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant difference between the means of the dependent variable for different levels of the independent variable."
        }
      } else if (input$testSelect == "Chi-Square Test") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        chisq_result <- chisq.test(table(var1, var2))
        if (chisq_result$p.value < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant association between the variables."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant association between the variables."
        }
      } else if (input$testSelect == "Fisher's Exact Test") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        fisher_result <- fisher.test(table(var1, var2))
        if (fisher_result$p.value < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant association between the variables."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant association between the variables."
        }
      } else if (input$testSelect == "One-Way ANOVA") {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        anova_result <- aov(var1 ~ var2)
        if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant difference between the means of the dependent variable for different levels of the independent variable."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant difference between the means of the dependent variable for different levels of the independent variable."
        }
      } else if (input$testSelect == "Linear Regression") {
        lm_result <- lm(as.formula(paste(input$var1, "~", input$var2)),data = dataset())
        #summary(lm_result)
        if (summary(lm_result)$coefficients[2, 4] < 0.05) {
          "The p-value for the slope is less than 0.05, indicating that there is a statistically significant relationship between the dependent and independent variables."
        } else {
          "The p-value for the slope is greater than 0.05, indicating that there is no statistically significant relationship between the dependent and independent variables."
        }
      } else if (input$testSelect == "Log-Rank Test") {
        logrank_result <- survdiff(as.formula(paste0("Surv(", input$var1, ", ", input$var2, ") ~ ", input$var3)),data = dataset())
        if (logrank_result$pvalue < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant difference in survival between the groups."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant difference in survival between the groups."
        }
      } else {
        var1 <- dataset()[[input$var1]]
        var2 <- as.factor(dataset()[[input$var2]])
        kruskal_result <- kruskal.test(var1 ~ var2)
        if (kruskal_result$p.value < 0.05) {
          "The p-value is less than 0.05, indicating that there is a statistically significant difference between the distributions of the dependent variable for different levels of the independent variable."
        } else {
          "The p-value is greater than 0.05, indicating that there is no statistically significant difference between the distributions of the dependent variable for different levels of the independent variable."
        }
      }
    })
  })
  
  output$testPlot <- renderPlot({
    req(input$run)
    isolate({
      if (input$testSelect == "T-Test" || input$testSelect == "One-Way ANOVA" || input$testSelect == "Kruskal-Wallis Test") {
        ggplot(dataset(), aes(x = factor(.data[[input$var2]]), y = .data[[input$var1]])) +
          geom_boxplot() +
          labs(title = "Boxplot of Dependent Variable by Independent Variable",
               x = "Independent Variable",
               y = "Dependent Variable")
      } else if (input$testSelect == "Linear Regression") {
        lm_result <- lm( as.formula(paste(input$var1, "~", input$var2)),data = dataset())
        check_model(lm_result)
      } else if (input$testSelect == "Log-Rank Test") {
        fit <- survfit(as.formula(paste0("Surv(", input$var1, ", ", input$var2, ") ~ ", input$var3)), data = dataset())
        ggsurvfit::ggsurvfit(fit)
      } else {
        ggplot(dataset(), aes(x = factor(.data[[input$var2]]), fill = factor(.data[[input$var1]]))) +
          geom_bar(position = "stack") +
          labs(title = "Bar Plot of Variable 1 by Variable 2",
               x = "Variable 2",
               fill = "Variable 1")
      }
    })
  })
  
  output$normality <- renderPrint({
    req(input$run, input$testSelect %in% c("T-Test", "One-Way ANOVA"))
    isolate({
      var1 <- dataset()[[input$var1]]
      if (input$testSelect == "One-Way ANOVA") {
        var2 <- as.factor(dataset()[[input$var2]])
        lapply(split(var1, var2), shapiro.test)
      } else {
        shapiro.test(var1)
      }
    })
  })
  
  output$normalityExplanation <- renderPrint({
    req(input$run, input$testSelect %in% c("T-Test", "One-Way ANOVA"))
    isolate({
      var1 <- dataset()[[input$var1]]
      if (input$testSelect == "One-Way ANOVA") {
        var2 <- as.factor(dataset()[[input$var2]])
        normality_results <- lapply(split(var1, var2), shapiro.test)
        sapply(normality_results, function(result) {
          if (result$p.value < 0.05) {
            "The p-value is less than 0.05, indicating that the dependent variable does not follow a normal distribution for this level of the independent variable."
          } else {
            "The p-value is greater than 0.05, indicating that the dependent variable follows a normal distribution for this level of the independent variable."
          }
        })
      } else {
        normality_result <- shapiro.test(var1)
        if (normality_result$p.value < 0.05) {
          "The p-value is less than 0.05, indicating that the dependent variable does not follow a normal distribution."
        } else {
          "The p-value is greater than 0.05, indicating that the dependent variable follows a normal distribution."
        }
      }
    })
  })
  
  output$homoscedasticity <- renderPrint({
    req(input$run, input$testSelect %in% c("T-Test", "One-Way ANOVA"))
    isolate({
      var1 <- dataset()[[input$var1]]
      var2 <- as.factor(dataset()[[input$var2]])
      bartlett.test(var1 ~ var2)
    })
  })
  
  output$homoscedasticityExplanation <- renderPrint({
    req(input$run, input$testSelect %in% c("T-Test", "One-Way ANOVA"))
    isolate({
      var1 <- dataset()[[input$var1]]
      var2 <- as.factor(dataset()[[input$var2]])
      homoscedasticity_result <- bartlett.test(var1 ~ var2)
      if (homoscedasticity_result$p.value < 0.05) {
        "The p-value is less than 0.05, indicating that the variances of the dependent variable are not equal across the levels of the independent variable (heteroscedasticity)."
      } else {
        "The p-value is greater than 0.05, indicating that the variances of the dependent variable are equal across the levels of the independent variable (homoscedasticity)."
      }
    })
  })
  
  output$expectedCounts <- renderPrint({
    req(input$run, input$testSelect == "Chi-Square Test")
    isolate({
      var1 <- dataset()[[input$var1]]
      var2 <- as.factor(dataset()[[input$var2]])
      chisq_result <- chisq.test(table(var1, var2))
      expected_counts <- chisq_result$expected
      if (any(expected_counts < 5)) {
        cat("Note: Some expected cell counts are less than 5. Consider running Fisher's Exact Test.\n")
      }
      expected_counts
    })
  })
  
  output$varClassSelect <- renderUI({
    req(dataset())
    selectInput("varClass", "Select Variable to Change Class", choices = names(dataset()))
  })
  
  observeEvent(input$convertToNumeric, {
    req(input$varClass)
    data <- dataset()
    data[[input$varClass]] <- as.numeric(data[[input$varClass]])
    dataset(data)
  })
  
  observeEvent(input$convertToFactor, {
    req(input$varClass)
    data <- dataset()
    data[[input$varClass]] <- as.factor(data[[input$varClass]])
    dataset(data)
  })
  
  output$dataClasses <- renderTable({
    req(dataset())
    sapply(dataset(), class)
  }, rownames = TRUE)
  
  output$dataGlimpse <- renderTable({
    req(dataset())
    head(dataset())
  })
}

shinyApp(ui = ui, server = server)