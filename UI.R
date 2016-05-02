require("dplyr")
require("tidyr")
require("lubridate")
#require("openxlsx")

require("ggplot2")
require("scales")
#require("VGAM")
require("mgcv")
require("Matrix")
require("xgboost")


require("shiny")
require("shinydashboard")
require("DT")

#require("plotly")
require("RColorBrewer")
require("highcharter")

header <- dashboardHeader(title = "Modeling Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput('file_data', 'Import Rds loss data',accept=c("rds")),
    menuItem(text = "Home", icon=icon("home"), newtab = F, href = "http://actuarial-analytics.ca/"),
    menuItem("GAM modeling", tabName = "gam", icon = icon("line-chart")),
    menuItem("GBM modeling", tabName = "gbm", icon = icon("line-chart")),
    numericInput("train_size","Training size", value = 0.8, min = 0.05, max=1, step = 0.05)
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "gam",
            fluidRow(
              box(width = 12, 
                  fluidRow(
                    column(width = 3,
                           selectInput("target_var",label = "Target", choices = c(""), multiple = F),
                           selectInput("offset_var",label = "Offset", choices = c(""), multiple = F),
                           selectInput("weight_var",label = "Weight", choices = c(""), multiple = F)
                    ),
                    column(width = 3,
                           selectInput("family","Distribution", choices=c("gaussian","Gamma","poisson","quasipoisson","binomial","Tweedie","inverse.gaussian","tw","negbin","nb","betar","multinom"), selected="gaussian", multiple = F),
                           selectInput("link","Link function", choices=c("identity","log","logit","probit"), selected="identity", multiple = F),
                           selectInput("method","Method", choices=c("GCV.Cp","GACV.Cp","REML","P-REML","ML","P-ML"), selected="GCV.Cp", multiple = F)
                    ),
                    column(width = 3,
                           selectInput("var_numeric","Numeric predictors", choices=c(""), selected="", multiple = T),
                           selectInput("var_factor","Factor/character predictors", choices=c(""), selected="", multiple = T),
                           actionButton("gam_formula_launch", "Generate model", icon=icon("gears"))
                    )
                  ),
                  fluidRow(
                    column(width = 6,
                           textInput("gam_formula", "GAM Formula", value="1"),
                           actionButton("fit_gam", "Fit", icon=icon("gears"))
                    )
                  ),
                  collapsible = T, solidHeader = T, title = "Model design")
            ),
            fluidRow(
              box(width=12,
                  column(width = 4, selectInput("group_gam_var",label = "Grouping variable", choices = c(""), multiple = F)
                  ),
                  column(width = 4, sliderInput("gam_bins", "Number of bins:", min = 5, max = 200, value = 30)
                  ),
                  column(width = 4, selectInput("gam_effect_var", "Single effect", choices = c(""), multiple = F)
                  ),
                  collapsible = T, solidHeader = T, title = "Exploration plot"
              )
            ),
            fluidRow(
              tabBox(tabPanel("Train", highchartOutput("plot_exploration_gam_train")), 
                     tabPanel("Test", highchartOutput("plot_exploration_gam_test")), 
                     width = 12, title = "Exploration plot")
            ),
            fluidRow(
              box(dataTableOutput("table_exploration_gam"), width=12,
                  collapsible = T, solidHeader = T, title = "Exploration table")
            ),
            fluidRow(
              box(width=12,
                  column(width = 6, highchartOutput("gam_auc_plot_train")),
                  column(width = 6, highchartOutput("gam_auc_plot_test")),
                  collapsible = T, solidHeader = T, title = "Model diagnostic")
            ),
            fluidRow(
              box(width=12,
                  column(width = 6, tableOutput("gam_model_output")),
                  collapsible = T, solidHeader = T, title = "Model parameters")
            )
    ),
    tabItem(
      tabName = "gbm"
    )
  )
)


dashboardPage(skin = "black", header, sidebar, body)