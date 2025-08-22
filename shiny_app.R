# ============================================================
# Shiny app with Variable Importance (RF merged in metrics)
# Displays + saves ROC, PR, Gains, Lift, Calibration, Scores, Dept, VarImp
# ============================================================

rm(list = ls())   # Clear environment

# ==== PACKAGES ============================================================
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("ranger", quietly = TRUE)) install.packages("ranger")
library(shiny); library(tidyverse); library(pROC); library(scales); library(ranger)

# ==== 1) PATHS & HELPERS =====================================================
# 🔧 Direct path to your IBM CSV
CSV_PATH <- "/Users/caliboi/Desktop/Resumes/Github/Project 3/ibm_hr_attrition.csv"

find_ibm <- function() {
  if (file.exists(CSV_PATH)) return(CSV_PATH)
  return(NA)
}

# Ensure outputs are created NEXT TO the CSV (not relative to working dir)
BASE_DIR <- dirname(CSV_PATH)
OUT <- file.path(BASE_DIR, "shiny_outputs")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ==== 2) UI ==================================================================
ui <- fluidPage(
  titlePanel("Attrition Modeling — Split Workflow"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataset", "Dataset",
                   c("IBM HR (ibm_hr_attrition.csv)" = "ibm")),
      radioButtons("modeltype", "Model",
                   c("Logistic (GLM)" = "glm", "Random Forest (ranger)" = "rf"), inline = TRUE),
      sliderInput("test_prop", "Test proportion", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
      numericInput("seed", "Random seed", value = 42, min = 1, step = 1),
      actionButton("train", "Train / Refresh", class = "btn-primary")
    ),
    mainPanel(
      fluidRow(
        column(3, h4("Test AUC"), textOutput("auc_txt")),
        column(3, h4("Average Precision"), textOutput("ap_txt")),
        column(3, h4("Brier Score"), textOutput("brier_txt")),
        column(3, h4("Baseline Rate"), textOutput("baseline_txt"))
      ),
      hr(),
      tabsetPanel(id = "tabs",
                  tabPanel("ROC", plotOutput("roc_plot", height = "420px")),
                  tabPanel("PR", plotOutput("pr_plot", height = "420px")),
                  tabPanel("Gains", plotOutput("gains_plot", height = "420px")),
                  tabPanel("Lift", plotOutput("lift_plot", height = "420px")),
                  tabPanel("Calibration", plotOutput("cal_plot", height = "420px")),
                  tabPanel("Scores", plotOutput("hist_plot", height = "420px")),
                  tabPanel("Department", plotOutput("dept_plot", height = "420px")),
                  tabPanel("Variable Importance", plotOutput("varimp_plot", height = "420px"))
      )
    )
  )
)

# ==== 3) SERVER ==============================================================
server <- function(input, output, session){
  
  # ---- 3a) Load IBM HR data
  load_data <- reactive({
    path <- find_ibm()
    validate(need(!is.na(path), "IBM HR CSV not found. Please check path."))
    df <- readr::read_csv(path, show_col_types = FALSE)
    
    # Drop non-informative columns
    drop_cols <- intersect(c("EmployeeNumber","EmployeeCount","Over18","StandardHours"), names(df))
    df <- df |> dplyr::select(-dplyr::all_of(drop_cols))
    
    df <- df |>
      mutate(
        Attrition01 = if_else(Attrition == "Yes", 1, 0),
        Department = factor(Department),
        JobRole = factor(JobRole),
        MaritalStatus = factor(MaritalStatus),
        OverTime = if_else(OverTime == "Yes", 1, 0)
      )
    list(df = df, name = "ibm")
  })
  
  # ---- 3b) Train model + compute artifacts
  trained <- eventReactive(input$train, {
    set.seed(input$seed)
    d <- load_data()$df
    n <- nrow(d)
    idx <- sample.int(n, size = floor(input$test_prop * n))
    test <- d[idx, ]; train <- d[-idx, ]
    
    for (c in c("Department","JobRole","MaritalStatus")) {
      lv <- levels(train[[c]])
      test[[c]] <- factor(test[[c]], levels = lv)
    }
    
    # 🔧 Simplified formula (only columns available in IBM dataset)
    form <- Attrition01 ~ Age + MonthlyIncome + OverTime + Department + JobRole + MaritalStatus
    
    varimp_df <- NULL
    mdl_name <- NULL
    if (input$modeltype == "glm") {
      mdl <- glm(form, data = train, family = binomial())
      test$prob <- predict(mdl, newdata = test, type = "response")
      mdl_name <- "Logistic Regression (GLM)"
    } else {
      train_rf <- train |> mutate(AttritionClass = factor(Attrition01, levels = c(0,1)))
      rf <- ranger::ranger(
        update(form, AttritionClass ~ .),
        data = train_rf,
        probability = TRUE, num.trees = 500, mtry = 3, seed = input$seed,
        importance = "impurity"
      )
      test$prob <- predict(rf, data = test)$predictions[, "1"]
      mdl <- rf; mdl_name <- "Random Forest (ranger)"
      
      # ---- Variable Importance (white background)
      varimp_df <- tibble(
        Feature = names(rf$variable.importance),
        Importance = rf$variable.importance
      ) |> arrange(desc(Importance))
      
      p_varimp <- ggplot(varimp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Random Forest Variable Importance", x = "Feature", y = "Importance") +
        theme_minimal(12) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA)
        )
      
      ggsave(file.path(OUT, "variable_importance.png"), p_varimp,
             width = 8, height = 6, dpi = 150, bg = "white")
    }
    
    # ---- 3c) Metrics
    roc_obj <- pROC::roc(response = test$Attrition01, predictor = test$prob)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    brier <- mean((test$Attrition01 - test$prob)^2)
    
    pr_pts <- test |> arrange(desc(prob)) |>
      mutate(tp_cum=cumsum(Attrition01), fp_cum=cumsum(1-Attrition01)) |>
      mutate(precision = tp_cum / pmax(tp_cum + fp_cum, 1e-9),
             recall = tp_cum / pmax(sum(Attrition01), 1e-9)) |>
      dplyr::select(recall, precision) |> dplyr::distinct(recall, .keep_all = TRUE) |> arrange(recall)
    ap <- sum(diff(c(0, pr_pts$recall)) * pr_pts$precision)
    baseline <- mean(test$Attrition01)
    
    # ---- 3d) Build metrics dataframe (merged with varimp if RF)
    if (!is.null(varimp_df)) {
      metrics_df <- varimp_df |>
        mutate(Model = mdl_name,
               TestAUC = auc_val,
               Brier = brier,
               AP = ap,
               Baseline = baseline) |>
        select(Model, TestAUC, Brier, AP, Baseline, Feature, Importance)
    } else {
      metrics_df <- tibble(Model = mdl_name, TestAUC = auc_val, Brier = brier, AP = ap, Baseline = baseline,
                           Feature = NA, Importance = NA)
    }
    
    readr::write_csv(metrics_df, file.path(OUT, "metrics.csv"))
    
    # ---- 3e) Plots (white backgrounds + white exports)
    roc_df <- tibble(tpr = rev(roc_obj$sensitivities), fpr = rev(1 - roc_obj$specificities))
    p_roc <- ggplot(roc_df, aes(fpr, tpr)) + geom_line(linewidth = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
      coord_equal(xlim = c(0,1), ylim = c(0,1)) +
      labs(title = paste0("ROC — ", mdl_name), subtitle = paste("AUC:", round(auc_val, 4)),
           x = "FPR", y = "TPR") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    p_pr <- ggplot(pr_pts, aes(recall, precision)) + geom_line(linewidth = 1) +
      geom_hline(yintercept = baseline, linetype = "dashed", color = "grey40") +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
      labs(title = "Precision–Recall — Test", subtitle = paste("AP:", round(ap,4), "| Baseline:", round(baseline,4)),
           x = "Recall", y = "Precision") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    lift_tbl <- test |> arrange(desc(prob)) |> mutate(decile = ntile(-prob, 10)) |>
      group_by(decile) |> summarise(n=n(), events=sum(Attrition01), avg_prob=mean(prob), .groups="drop") |>
      arrange(decile) |> mutate(cum_n=cumsum(n), cum_events=cumsum(events),
                                population_pct=cum_n/sum(n), capture_rate=cum_events/sum(events),
                                event_rate=events/n, baseline_rate=sum(events)/sum(n), lift=event_rate/baseline_rate)
    
    p_lift <- ggplot(lift_tbl, aes(factor(decile), lift)) + geom_col() + geom_hline(yintercept = 1, linetype = "dashed") +
      labs(title = "Lift by Decile — Test", x = "Decile", y = "Lift vs Overall") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    calib_tbl <- test |> mutate(bin = ntile(prob, 10)) |>
      group_by(bin) |> summarise(n=n(), mean_pred=mean(prob), obs_rate=mean(Attrition01), .groups="drop") |>
      arrange(mean_pred)
    
    p_cal <- ggplot(calib_tbl, aes(mean_pred, obs_rate)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color="grey40") +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      labs(title = "Calibration — Test", subtitle = paste("Brier:", round(brier,4)),
           x = "Mean Pred", y = "Observed") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    p_hist <- ggplot(test, aes(prob, fill = factor(Attrition01, labels = c("No","Yes")))) +
      geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.45, boundary = 0, closed = "left") +
      labs(title = "Score Distribution — Test", x = "Predicted Probability", y = "Count", fill = "Attrition") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    dept_sum <- d |>
      group_by(Department) |>
      summarise(headcount = n(), attrition_rate = mean(Attrition01), .groups = "drop")
    
    p_dept <- ggplot(dept_sum, aes(headcount, attrition_rate, size = headcount, label = Department)) +
      geom_point(alpha = 0.7) + geom_text(vjust = -1, size = 3.2) +
      labs(title = "Department View", x = "Headcount", y = "Attrition Rate", size = "Headcount") +
      theme_minimal(12) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    # ---- Save plots (export with bg="white")
    ggsave(file.path(OUT, "roc_curve.png"),         p_roc,  width = 8, height = 6, dpi = 150, bg = "white")
    ggsave(file.path(OUT, "pr_curve.png"),          p_pr,   width = 8, height = 6, dpi = 150, bg = "white")
    ggsave(file.path(OUT, "lift_chart.png"),        p_lift, width = 8, height = 6, dpi = 150, bg = "white")
    ggsave(file.path(OUT, "calibration_plot.png"),  p_cal,  width = 8, height = 6, dpi = 150, bg = "white")
    ggsave(file.path(OUT, "score_histogram.png"),   p_hist, width = 8, height = 6, dpi = 150, bg = "white")
    
    list(model = mdl, name = mdl_name, auc = auc_val, ap = ap, brier = brier, baseline = baseline,
         roc = p_roc, pr = p_pr, lift = p_lift, cal = p_cal, hist = p_hist, dept = p_dept,
         varimp = if (!is.null(varimp_df)) p_varimp else NULL)
  }, ignoreInit = TRUE)
  
  # ---- 3f) Outputs
  output$auc_txt <- renderText({ req(trained()); sprintf("%.4f", trained()$auc) })
  output$ap_txt  <- renderText({ req(trained()); sprintf("%.4f", trained()$ap) })
  output$brier_txt <- renderText({ req(trained()); sprintf("%.4f", trained()$brier) })
  output$baseline_txt <- renderText({ req(trained()); sprintf("%.4f", trained()$baseline) })
  
  output$roc_plot    <- renderPlot({ req(trained()); trained()$roc })
  output$pr_plot     <- renderPlot({ req(trained()); trained()$pr })
  output$gains_plot  <- renderPlot({ req(trained()); trained()$lift })
  output$lift_plot   <- renderPlot({ req(trained()); trained()$lift })
  output$cal_plot    <- renderPlot({ req(trained()); trained()$cal })
  output$hist_plot   <- renderPlot({ req(trained()); trained()$hist })
  output$dept_plot   <- renderPlot({ req(trained()); trained()$dept })
  output$varimp_plot <- renderPlot({ req(trained()); trained()$varimp })
}

# ==== 4) APP LAUNCH ==========================================================
shinyApp(ui, server)
