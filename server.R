require("data.table")
require("dplyr")
require("tidyr")
require("lubridate")

require("ggplot2")
require("scales")
#require("VGAM")
require("mgcv")
require("Matrix")
require("xgboost")

#lala<- readRDS("D:/Kaggle/BNP/data/train_full_for_GAM.rds")
#table(sapply(lala, class))

require("shiny")
require("shinydashboard")
require("DT")
#require("plotly")
require("RColorBrewer")
require("readr")
require("highcharter")

source("www/functions.R")

options(shiny.maxRequestSize=200*1024^2)



server <- function(input, output, session) {
  
  
  
  data_in <- reactive({
    
    inFile <- input$file_data
    
    if (is.null(inFile)) {
      
    } else {
  
      ext<- sub(pattern = "(.+\\.)(.+)",replacement = "\\2", x = tolower(inFile$name))

      if (ext=="rds") {
        data<- readRDS(inFile$datapath)
      } else if (ext=="csv") {
        data<- read_csv(inFile$datapath)
      }
      
      
      ### Define the list and characteristics of variables
      names(data)<- make.names(names(data), unique = T)
      var<- names(data)
      type<- sapply(data, class)
      var_numeric<- var[which(type %in% c("numeric","integer"))]
      var_logical<- var[which(type %in% c("logical"))]
      var_factor<- var[which(type %in% c("factor"))]
      var_character<- var[which(type %in% c("character"))]
      var_date<- var[which(type %in% c("Date"))]
      
      ### Convert character to factor
      if (length(var_character)>0) {
        data<- data %>% mutate_each_(funs(factor(.)), vars=var_character)
      }
      
      var<- names(data)
      type<- sapply(data, class)
      var_factor<- var[which(type %in% c("factor"))]
      var_character<- var[which(type %in% c("character"))]

    }
    
    list(data=data, var_numeric=var_numeric, var_logical=var_logical, var_factor=var_factor, var_character=var_character, var_date=var_date)
  })
  

  
  observeEvent(input$file_data, {
    
    var_numeric<- data_in()$var_numeric
    var_factor<- data_in()$var_factor
    var_character<- data_in()$var_character
    
    var_select<- match(c("loss","incurred","incurred_tot","loss_tot"), table = tolower(var_numeric))
    var_select<- var_select[!is.na(var_select)][1]
    if (is.na(var_select)) var_select<- 1
    
    vars_group<- c(var_numeric, var_factor, var_character)
    
    updateSelectInput(session = session, inputId = "target_var", choices = var_numeric)
    updateSelectInput(session = session, inputId = "offset_var", choices = c("",var_numeric))
    updateSelectInput(session = session, inputId = "weight_var", choices = c("",var_numeric))
    updateSelectInput(session = session, inputId = "group_var", choices = c("",vars_group))

    updateSelectInput(session = session, inputId = "var_numeric", choices = c("", var_numeric))
    updateSelectInput(session = session, inputId = "var_factor", choices = c("", var_factor))
  })
  
  
  #### Build modeling formula call based onvariable selection and allow for user modification
  observeEvent(input$gam_formula_launch, {
    
    var_numeric<- input$var_numeric
    var_factor<- input$var_factor

    formula_vanilla<- paste(var_numeric, var_factor, sep="+", collapse="+")
    
    formula_def<- paste(formula_vanilla, sep="+")

    formula_def<- gsub(pattern = "\\++", replacement = "\\+", formula_def)
    formula_def<- gsub(pattern = "(\\+$)|(^\\+)", replacement = "", formula_def)
    
    if (is.null(formula_def) | formula_def=="") formula_def<- "1"
    
    updateTextInput(session = session, inputId = "gam_formula", value=formula_def)

  })

  

  #################################################################
  #### GAM fitting
  #################################################################
  
  gam_fitting<- eventReactive(input$fit_gam,{
    
    data<- as.data.frame(data_in()$data)
    
    data[["target"]]<- data[[input$target_var]]
    
    if (input$offset_var=="") {
      data[["offset"]]<- 0
    } else data[["offset"]]<- data[[input$offset_var]]
    
    if (input$weight_var=="") {
      data[["weight"]]<- 1
    } else data[["weight"]]<- data[[input$weight_var]]
    
    set.seed(123123)
    train_rows<- sample(1:nrow(data), size = round(input$train_size*nrow(data)), replace = F)
    data$TRAIN_TEST<-"test"
    data$TRAIN_TEST[train_rows]<-"train"
    data$TRAIN_TEST<- factor(data$TRAIN_TEST)
    
    train<- data %>% filter(TRAIN_TEST=="train")
    test<- data %>% filter(TRAIN_TEST=="test")
    
    formula_left<- input$gam_formula
    gam_formula<- as.formula(paste("target", formula_left,sep=" ~ "))
    
    model<- gam(gam_formula, data=train, family=paste(input$family, "(link=", input$link, ")", sep=""), optimizer = c("outer","newton"))
    
    data$model<- as.numeric(predict.gam(model, newdata=data, type="response", se.fit = F))
    
    effects<- as.data.frame(predict(model, newdata=data, type="terms"))
    
    if (ncol(effects)>0) {
      effects_names<- make.names(names(effects), unique = T)
      effects_names<- paste("effect_", effects_names, sep="")
      names(effects)<- effects_names
      data_effects<- bind_cols(data, effects)
    } else {
      effects_names<- NA
      data_effects<- bind_cols(data)
    }
    
    
    ### Recreate list of variables type for model exploration
    var<- names(data_effects)
    type<- sapply(data_effects, class)
    var_numeric<- var[which(type %in% c("numeric","integer"))]
    var_logical<- var[which(type %in% c("logical"))]
    var_factor<- var[which(type %in% c("factor"))]
    var_date<- var[which(type %in% c("Date"))]
    
    list(model=model, effects=effects_names, data_effects=data_effects,
         var_numeric=var_numeric, var_logical=var_logical, var_factor=var_factor, var_date=var_date)
    
  })
  
  output$gam_model_output<- renderTable({
    data.frame(gam_fitting()$model$coefficients)
  })
  
  
  observeEvent(input$fit_gam, {
    effects_var<- gam_fitting()$effects
    var_numeric<- gam_fitting()$var_numeric
    var_logical<- gam_fitting()$var_logical
    var_factor<- gam_fitting()$var_factor
    var_date<- gam_fitting()$var_date
    
    if (!is.null(effects_var)) {
      updateSelectInput(session = session, inputId = "gam_effect_var", choices = c(effects_var))
      updateSelectInput(session = session, inputId = "group_gam_var", choices = c(var_numeric, var_logical, var_factor, var_date))
    } else {
      updateSelectInput(session = session, inputId = "group_gam_var", choices = c(var_numeric, var_logical, var_factor, var_date))
    }
  })
  
  
  #################################
  ### diagnostic
  gam_rmspe<- reactive({
    train<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="train")
    test<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="test")
    RMSPE_train<- RMSPE(target = train$target, submission = train$model)
    RMSPE_test<- RMSPE(target = test$target, submission = test$model)
    list(RMSPE_train=RMSPE_train, RMSPE_test=RMSPE_test)
  })
  
  gam_logloss<- reactive({
    train<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="train")
    test<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="test")
    LogLoss_train<- LogLoss(target = train$target, submission = train$model)
    LogLoss_test<- LogLoss(target = test$target, submission = test$model)
    list(LogLoss_train=LogLoss_train, LogLoss_test=LogLoss_test)
  })
  
  

  #################################
  ### Plot AUC graphs
  output$gam_auc_plot_train<- renderHighchart({
    
    data<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="train")
    
    auc_model<- data %>% arrange(-model) %>% 
      mutate(model_cum=cumsum(model),
             model_prop=model_cum/sum(model),
             weight_cum_model=cumsum(weight),
             weight_prop_model=weight_cum_model/sum(weight))
    
    auc_target<- auc_model %>% arrange(-target) %>% 
      mutate(target_cum=cumsum(target),
             target_prop=target_cum/sum(target),
             weight_cum_target=cumsum(weight),
             weight_prop_target=weight_cum_target/sum(weight))
    
    AUC<- NormalizedGini(solution = data$target, submission = data$model)
    
    rows<- unique(round(quantile(1:nrow(auc_model), probs = 0:100/100),0))
    auc_model<- auc_model[rows, ]
    auc_target<- auc_target[rows, ]
    
    plot<- highchart() %>% 
      hc_chart(zoomType= 'xy', animation=F) %>% 
      hc_add_series(animation=F, data = list.parse2(data.frame(x=c(0,1), y=c(0,1))), name = "baseline", type="line") %>% 
      hc_add_series(animation=F, data = list.parse2(auc_target[c("weight_prop_target","target_prop")]), name = "target", type="line") %>% 
      hc_add_series(animation=F, data = list.parse2(auc_model[c("weight_prop_model","model_prop")]), name = "model", type="line") %>% 
      hc_title(text = paste("<b>AUC train</b><br>", signif(AUC,4),sep="")) %>% 
      hc_colors(c("black","gray","lightblue")) %>% 
      hc_exporting(enabled = TRUE)
    
    plot
    
  })
  
  
  output$gam_auc_plot_test<- renderHighchart({
    
    data<- gam_fitting()$data_effects %>% filter(TRAIN_TEST=="test")
    
    auc_model<- data %>% arrange(-model) %>% 
      mutate(model_cum=cumsum(model),
             model_prop=model_cum/sum(model),
             weight_cum_model=cumsum(weight),
             weight_prop_model=weight_cum_model/sum(weight))
    
    auc_target<- auc_model %>% arrange(-target) %>% 
      mutate(target_cum=cumsum(target),
             target_prop=target_cum/sum(target),
             weight_cum_target=cumsum(weight),
             weight_prop_target=weight_cum_target/sum(weight))
    
    AUC<- NormalizedGini(solution = data$target, submission = data$model)
    
    rows<- unique(round(quantile(1:nrow(auc_model), probs = 0:100/100),0))
    auc_model<- auc_model[rows, ]
    auc_target<- auc_target[rows, ]
    
    plot<- highchart() %>% 
      hc_chart(zoomType= 'xy', animation=F) %>% 
      hc_add_series(animation=F, data = list.parse2(data.frame(x=c(0,1), y=c(0,1))), name = "baseline", type="line") %>% 
      hc_add_series(animation=F, data = list.parse2(auc_target[c("weight_prop_target","target_prop")]), name = "target", type="line") %>% 
      hc_add_series(animation=F, data = list.parse2(auc_model[c("weight_prop_model","model_prop")]), name = "model", type="line") %>% 
      hc_title(text = paste("<b>AUC test</b><br>", signif(AUC,4),sep="")) %>% 
      hc_colors(c("black","gray","lightblue")) %>% 
      hc_exporting(enabled = TRUE)
    
    plot
    
  })
  
  
  #################################
  ### exploration
  gam_exploration<- reactive({
    
    data<- gam_fitting()$data_effects
    effects_var<- gam_fitting()$effects
    
    if (input$group_gam_var=="") {
      data[["grouping"]]<- "TOTAL"
    } else data[["grouping"]]<- data[[input$group_gam_var]]
    
    if (input$group_gam_var %in% gam_fitting()$var_numeric) {
      bins<- input$gam_bins
      if (bins<length(unique(data$grouping))) {
        data$grouping<- cut(data$grouping, breaks = unique(quantile(data$grouping, probs = (0:bins)/bins, na.rm = T)), include.lowest = T)
      }
    } 
    
    data_group<- data %>% 
      mutate_each_(funs(weight*.), vars=c("target","model","offset")) %>% 
      group_by_(.dots = c("TRAIN_TEST","grouping")) %>% 
      summarise_each_(funs(sum(.,na.rm=T)), vars=c("target","model","offset","weight")) %>% 
      mutate_each_(funs(./weight), vars=c("target","model","offset")) %>% 
      ungroup()
    
    data_group_train<- data_group %>% filter(TRAIN_TEST=="train")
    data_group_test<- data_group %>% filter(TRAIN_TEST=="test")
    
    list(data_group_train=data_group_train, data_group_test=data_group_test)
  })
  
  output$table_exploration_gam<-renderDataTable({
    
    data_table<- gam_exploration()$data_group_train
    
    datatable(data_table, rownames = F, filter = "none", selection = "none",
              class=c("compact hover row-border"), 
              options = list(
                columnDefs = list(list(className = 'dt-center', targets=1:ncol(data_table)-1)),
                ordering=F,
                searching=F,
                info=F,
                paging=F
              ))
    
  })
  
  
  output$plot_exploration_gam_train<-renderHighchart({
    
    title<- input$group_gam_var
    bins<- input$gam_bins
    data_plot<- gam_exploration()$data_group_train
    
    #data_plot_melt<- data_plot %>% gather_(key_col = "type", value_col="value", gather_cols=c("target", "model","weight"))
    
    if (title %in% gam_fitting()$var_numeric) {
      
      plot<- highchart() %>% 
        hc_chart(zoomType= 'xy', animation=F) %>% 
        hc_yAxis(
          list(title = list(text = "Target"), align = "left"),
          list(title = list(text = "Weight"), align = "right", opposite=TRUE)
        ) %>% 
        hc_xAxis(categories = data_plot$grouping) %>% 
        hc_add_series(animation=F, data = data_plot$weight, name = "weight", type="column", yAxis=1) %>% 
        hc_add_series(animation=F, data = data_plot$target, name = "target", type="line", yAxis=0) %>% 
        hc_add_series(animation=F, data = data_plot$model, name = "model", type="line", yAxis=0) %>% 
        hc_colors(c("lightgray","lightblue","navy")) %>% 
        hc_exporting(enabled = TRUE)
      
    } else {
      plot<- highchart() %>% 
        hc_chart(zoomType= 'xy', animation=list(duration=0)) %>% 
        hc_yAxis(
          list(title = list(text = "Target"), align = "left"),
          list(title = list(text = "Weight"), align = "right", opposite=TRUE)
        ) %>% 
        hc_xAxis(categories = data_plot$grouping) %>% 
        hc_add_series(animation=F, data = data_plot$weight, name = "weight", type="column", yAxis=1) %>% 
        hc_add_series(animation=F, data = data_plot$target, name = "target", type="scatter", yAxis=0) %>% 
        hc_add_series(animation=F, data = data_plot$model, name = "model", type="scatter", yAxis=0) %>% 
        hc_colors(c("lightgray","lightblue","navy")) %>% 
        hc_exporting(enabled = TRUE)
    }
    
    plot
    
  })
  
  output$plot_exploration_gam_test<-renderHighchart({
    
    title<- input$group_gam_var
    bins<- input$gam_bins
    data_plot<- gam_exploration()$data_group_test
    
    #data_plot_melt<- data_plot %>% gather_(key_col = "type", value_col="value", gather_cols=c("target", "model","weight"))
    
    if (title %in% gam_fitting()$var_numeric) {
      
      plot<- highchart() %>% 
        hc_chart(zoomType= 'xy', animation=F) %>% 
        hc_yAxis(
          list(title = list(text = "Target"), align = "left"),
          list(title = list(text = "Weight"), align = "right", opposite=TRUE)
        ) %>% 
        hc_xAxis(categories = data_plot$grouping) %>% 
        hc_add_series(animation=F, data = data_plot$weight, name = "weight", type="column", yAxis=1) %>% 
        hc_add_series(animation=F, data = data_plot$target, name = "target", type="line", yAxis=0) %>% 
        hc_add_series(animation=F, data = data_plot$model, name = "model", type="line", yAxis=0) %>% 
        hc_colors(c("lightgray","lightblue","navy")) %>% 
        hc_exporting(enabled = TRUE)
      
    } else {
      plot<- highchart() %>% 
        hc_chart(zoomType= 'xy', animation=F) %>% 
        hc_yAxis(
          list(title = list(text = "Target"), align = "left"),
          list(title = list(text = "Weight"), align = "right", opposite=TRUE)
          ) %>% 
        hc_xAxis(categories = data_plot$grouping) %>% 
        hc_add_series(animation=F, data = data_plot$weight, name = "weight", type="column", yAxis=1) %>% 
        hc_add_series(animation=F, data = data_plot$target, name = "target", type="scatter", yAxis=0) %>% 
        hc_add_series(animation=F, data = data_plot$model, name = "model", type="scatter", yAxis=0) %>% 
        hc_colors(c("lightgray","lightblue","navy")) %>% 
        hc_exporting(enabled = TRUE)
    }
    
    plot
    
  })

  
}

