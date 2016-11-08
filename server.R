library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyjs)
library(highcharter)

source("utils.R")

existing_date <- Sys.Date() - 1

function(input, output, session) {

  if (Sys.Date() != existing_date) {
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Downloading portal data", value = 0)
    read_data()
    progress$set(message = "Finished downloading datasets", value = 1)
    existing_date <<- Sys.Date()
    progress$close()
  }

  cntr_reactive <- reactiveValues(selected_metric_a = NULL, selected_metric_f = NULL, selected_metric_l = NULL, selected_metric_m = NULL)
  
  all_country_data_dt <- reactive({
    if (input$cntr_sort_a %in% c("us_a", "custom_a")){
      fnl_tbl <- all_country_data %>%
      rbind(us_data) %>%
  	  filter(Date >= input$date_all_country[1]
  	         & Date <= input$date_all_country[2]
  	         & Country %in% selected_country_a()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- all_country_data %>%
  	  filter(Date >= input$date_all_country[1]
  	         & Date <= input$date_all_country[2]
  	         & Country %in% selected_country_a())
    }    
    return (fnl_tbl)
  })
  all_country_data_prop_dt <- reactive({
    if (input$cntr_sort_a %in% c("us_a", "custom_a")){
      fnl_tbl <- all_country_data_prop %>%
      rbind(us_data_prop) %>%
  	  filter(Date >= input$date_all_country[1]
  	         & Date <= input$date_all_country[2]
  	         & Country %in% selected_country_a()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- all_country_data_prop %>%
  	  filter(Date >= input$date_all_country[1]
  	         & Date <= input$date_all_country[2]
  	         & Country %in% selected_country_a())
    }    
    return (fnl_tbl)
  })
  observeEvent(input$traffic_select, {
	cntr_reactive$selected_metric_a <- switch(input$traffic_select,
	                                          events = "No. Events",
	                                          visits = "No. Visit",
	                                          sessions = "No. Session", 
	                                          ctr_all = "Overall Clickthrough Rate", 
	                                          ctr_vst = "Clickthrough Rate Per Visit", 
	                                          ctr_ses = "Clickthrough Rate Per Session"
	                                         )
  })
  selected_country_a <- reactive({
    all_country_temp<- all_country_data %>%
      filter(all_country_data$Date >= input$date_all_country[1]
             & all_country_data$Date <= input$date_all_country[2]) %>%
      select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
      group_by(Country) %>%
      summarize_(.dots = 
        if(input$traffic_select %in% c('events','visits','sessions')) as.formula(paste0("~ sum(`",cntr_reactive$selected_metric_a,"`)")) else as.formula(paste0("~ median(`",cntr_reactive$selected_metric_a,"`)"))
      ) %>%
      ungroup() %>% 
      as.data.frame()      
    all_country_temp <- all_country_temp[order(all_country_temp[,2], all_country_temp[,1]),]
    result <- switch(input$cntr_sort_a,
                     top10_a = {tail(all_country_temp, 10)$Country},
                     bottom50_a = {head(all_country_temp, 50)$Country},
                     us_a = unique(us_data$Country),
                     all_a = unique(all_country_data$Country),
                     all_nus_a = unique(all_country_data$Country)[!grepl("United States", unique(all_country_data$Country))],
                     custom_a = input$cntr_a
                    )
    return(result)
  })
  observeEvent(input$cntr_sort_a, {
    toggleClass("cntr_a_legend", "small", length(selected_country_a())>7 & input$cntr_combine_a==F)
    toggleClass("cntr_a_legend", "large", length(selected_country_a())<=7 | input$cntr_combine_a)
    if (input$cntr_sort_a %in% c("bottom50_a","all_a","all_nus_a")) {
      updateCheckboxInput(session, "cntr_combine_a", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_a", value = FALSE)
    }
  })
  output$all_country_dygraph <- renderDygraph({
    if (input$cntr_combine_a == T){
	    if (input$traffic_select == 'ctr_all'){
			data4dygraph <- all_country_data_dt() %>%
			  mutate(clicks=`No. Events`*`Overall Clickthrough Rate`) %>%
			  group_by(Date) %>%
			  summarize("Overall Clickthrough Rate"=sum(clicks)/sum(`No. Events`)) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
	    } else if (input$traffic_select == 'ctr_vst'){
			data4dygraph <- all_country_data_dt() %>%
			  mutate(clicks=`No. Visit`*`Clickthrough Rate Per Visit`) %>%
			  group_by(Date) %>%
			  summarize("Clickthrough Rate Per Visit"=sum(clicks)/sum(`No. Visit`)) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
	    } else if(input$traffic_select == 'ctr_ses'){
			data4dygraph <- all_country_data_dt() %>%
			  mutate(clicks=`No. Session`*`Clickthrough Rate Per Session`) %>%
			  group_by(Date) %>%
			  summarize("Clickthrough Rate Per Session"=sum(clicks)/sum(`No. Session`)) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
	    } else{
			data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
	      select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
			  group_by(Date) %>%
			  summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_a,"`)")) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
			names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_a))
	    }
	} else{
		if (length(selected_country_a())>1){
			data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
			  select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
			  tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_a, fill=0) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
		} else{
			data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
			  select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
			names(data4dygraph)[2] <- paste0(isolate(selected_country_a()))
		}
	}
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_a)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date", 
                         ylab = ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'), 
                         paste0("Proportion of ", cntr_reactive$selected_metric_a, " (%)"), 
                         cntr_reactive$selected_metric_a),
                         title = paste0(ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'), 
                         paste0("Out of all countries, the proportion of ", cntr_reactive$selected_metric_a, " (%)"), 
                         cntr_reactive$selected_metric_a), 
                           " from ", ifelse(input$cntr_sort_a %in% c("all_a","all_nus_a"), ifelse(input$cntr_sort_a=="all_a","All Countries","All Countries but U.S."), paste0(selected_country_a(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_a) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_a_legend", show = "always", showZeroValues = FALSE) %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "black")
})
  output$traffic_pie_pl <- renderHighchart({      
    if(input$traffic_select %in% c('events','ctr_all')){
      this_metric <- "No. Events"
    } else if(input$traffic_select %in% c('visits','ctr_vst')){
      this_metric <- "No. Visit"
    } else{
      this_metric <- "No. Session"
    }
    data4pie <- all_country_data_dt() %>%
      group_by(Region) %>%
      mutate_(region_total=paste0("sum(`",this_metric,"`)")) %>%
      group_by(Region, Country) %>%
      mutate_(country_total=paste0("sum(`",this_metric,"`)")) %>%
      select(Region, Country, country_total, region_total) %>%
      unique() %>% ungroup() %>% arrange(Region, Country)
    data4pie_us <- us_data %>%
	  filter(us_data$Date >= input$date_all_country[1]
	         & us_data$Date <= input$date_all_country[2]) %>%
      group_by(Country) %>%
      summarise_(us_total=paste0("sum(`",this_metric,"`)")) %>% ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0(this_metric, " by Country")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
                   shadow = FALSE,
                   center = c("50%", "50%")
        )
      ) %>%
      hc_add_series(data = data4pie %>% select(name=Region, y=region_total) %>% 
                      mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60, 
                                      color = 'black',
                                      formatter = JS("function () {
                                      return this.percentage > 5 ? this.point.name : null;
                                      }"))
                   ) %>%
      hc_add_series(data = data4pie %>% select(name=Country, y=country_total) %>% 
                      mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% mutate(id=tolower(Region)) %>% 
          select(name=Country, y=country_total, id) %>% 
          unique() %>% 
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          )))
      )  
      hc
  })
  output$all_country_tbl <- DT::renderDataTable(
    {if(input$prop_a){
      fnl_dt <- all_country_data_prop_dt()
      colnames(fnl_dt) <- c("Date", "Country", "Region", "Events Proportion", 
     	  "Overall Clickthrough Rate", "Visits Proportion", "Clickthrough Rate Per Visit", 
     	  "Sessions Proportion", "Clickthrough Rate Per Session")
    } else{
      fnl_dt <- all_country_data_dt()
    }
     fnl_dt}, 
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)      
    )


  first_visits_country_dt <- reactive({
    if (input$cntr_sort_f %in% c("us_f", "custom_f")){
      fnl_tbl <- first_visits_country %>%
      rbind(first_visits_us) %>%
  	  filter(Date >= input$date_first_visit[1]
  	         & Date <= input$date_first_visit[2]
  	         & Country %in% selected_country_f()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- first_visits_country %>%
  	  filter(Date >= input$date_first_visit[1]
  	         & Date <= input$date_first_visit[2]
  	         & Country %in% selected_country_f())
    }    
    return (fnl_tbl)
  })
  first_visits_country_prop_dt <- reactive({
    if (input$cntr_sort_f %in% c("us_f", "custom_f")){
      fnl_tbl <- first_visits_country_prop %>%
      rbind(first_visits_us_prop) %>%
  	  filter(Date >= input$date_first_visit[1]
  	         & Date <= input$date_first_visit[2]
  	         & Country %in% selected_country_f()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- first_visits_country_prop %>%
  	  filter(Date >= input$date_first_visit[1]
  	         & Date <= input$date_first_visit[2]
  	         & Country %in% selected_country_f())
    }    
    return (fnl_tbl)
  })
  observeEvent(input$action_select_f, {
  cntr_reactive$selected_metric_f <- switch(input$action_select_f,
                                            nact_f = "No Action",
                                            olv_f = "Other Languages",
                                            oproj_f = "Other Projects", 
                                            prln_f = "Primary Links", 
                                            search_f = "Search", 
                                            secln_f = "Secondary Links"
                                           )
  })
  selected_country_f <- reactive({
    all_country_temp<- first_visits_country %>%
      filter(first_visits_country$Date >= input$date_first_visit[1]
             & first_visits_country$Date <= input$date_first_visit[2]) %>%
      select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
      group_by(Country) %>%
      summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      arrange(value, Country)	 
    result <- switch(input$cntr_sort_f,
                     top10_f = {tail(all_country_temp, 10)$Country},
                     bottom50_f = {head(all_country_temp, 50)$Country},
                     us_f = unique(first_visits_us$Country),
                     all_f = unique(first_visits_country$Country),
                     all_nus_f = unique(first_visits_country$Country)[!grepl("United States", unique(first_visits_country$Country))],
                     custom_f = input$cntr_f
                    )
    return(result)
  })
  observeEvent(input$cntr_sort_f, {
    toggleClass("cntr_f_legend", "small", length(selected_country_f())>7 & input$cntr_combine_f==F)
    toggleClass("cntr_f_legend", "large", length(selected_country_f())<=7 | input$cntr_combine_f)
    if (input$cntr_sort_f %in% c("bottom50_f","all_f","all_nus_f")) {
      updateCheckboxInput(session, "cntr_combine_f", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_f", value = FALSE)
    }
  })
  output$first_visit_dygraph <- renderDygraph({
    if (input$cntr_combine_f == T){
        data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
          select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
          group_by(Date) %>%
          summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
          rename(date=Date) %>%
          fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
        names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_f))
  } else{
  	if (length(selected_country_f())>1){
  		data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
  		  select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
  		  tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_f, fill=0) %>%
  		  rename(date=Date) %>%
  		  fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
  	} else{
  		data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
  		  select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
  		  rename(date=Date) %>%
  		  fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
  		names(data4dygraph)[2] <- paste0(isolate(selected_country_f()))
  	}
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_f)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date",     
                         ylab = ifelse(input$prop_f, paste0("Proportion of ", cntr_reactive$selected_metric_f, " (%)"), cntr_reactive$selected_metric_f),
                         title = paste0(ifelse(input$prop_f, "Out of all countries, the proportion of clicks to ", "The number of clicks to "), cntr_reactive$selected_metric_f, " from ", ifelse(input$cntr_sort_f %in% c("all_f","all_nus_f"), ifelse(input$cntr_sort_f=="all_f","All Countries","All Countries but U.S."), paste0(selected_country_f(), collapse=", ")), " at first visit")) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_f) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_f_legend", show = "always", showZeroValues = FALSE)  %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "black")
  })
  output$first_visit_pie_pl <- renderHighchart({      
    data4pie <- first_visits_country_dt() %>%
      group_by(Region) %>%
      mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      group_by(Region, Country) %>%
      mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      select(Region, Country, country_total, region_total) %>%
      unique() %>% ungroup() %>% arrange(Region, Country)
    data4pie_us <- first_visits_us %>%
    filter(first_visits_us$Date >= input$date_all_country[1]
           & first_visits_us$Date <= input$date_all_country[2]) %>%
      group_by(Country) %>%
      summarise_(us_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>% ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_f, " by Country")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
                   shadow = FALSE,
                   center = c("50%", "50%")
        )
      ) %>%
      hc_add_series(data = data4pie %>% select(name=Region, y=region_total) %>% 
                      mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60, 
                                      color = 'black',
                                      formatter = JS("function () {
                                      return this.percentage > 5 ? this.point.name : null;
                                      }"))
                   ) %>%
      hc_add_series(data = data4pie %>% select(name=Country, y=country_total) %>% 
                      mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% mutate(id=tolower(Region)) %>% 
          select(name=Country, y=country_total, id) %>% 
          unique() %>% 
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          )))
      )  
      hc
  })
  output$first_visits_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()}, 
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)      
    )


  last_action_country_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- last_action_country %>%
      rbind(last_action_us) %>%
  	  filter(Date >= input$date_last_action[1]
  	         & Date <= input$date_last_action[2]
  	         & Country %in% selected_country_l()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- last_action_country %>%
  	  filter(Date >= input$date_last_action[1]
  	         & Date <= input$date_last_action[2]
  	         & Country %in% selected_country_l())
    }    
    return (fnl_tbl)
  })
  last_action_country_prop_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- last_action_country_prop %>%
      rbind(last_action_us_prop) %>%
  	  filter(Date >= input$date_last_action[1]
  	         & Date <= input$date_last_action[2]
  	         & Country %in% selected_country_l()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- last_action_country_prop %>%
  	  filter(Date >= input$date_last_action[1]
  	         & Date <= input$date_last_action[2]
  	         & Country %in% selected_country_l())
    }    
    return (fnl_tbl)
  })
  observeEvent(input$action_select_l, {
  cntr_reactive$selected_metric_l <- switch(input$action_select_l,
                                            nact_l = "No Action",
                                            olv_l = "Other Languages",
                                            oproj_l = "Other Projects", 
                                            prln_l = "Primary Links", 
                                            search_l = "Search", 
                                            secln_l = "Secondary Links"
                                           )
  })
  selected_country_l <- reactive({
    all_country_temp<- last_action_country %>%
      filter(last_action_country$Date >= input$date_last_action[1]
             & last_action_country$Date <= input$date_last_action[2]) %>%
      select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
      group_by(Country) %>%
      summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      arrange(value, Country)	 
    result <- switch(input$cntr_sort_l,
                     top10_l = {tail(all_country_temp, 10)$Country},
                     bottom50_l = {head(all_country_temp, 50)$Country},
                     us_l = unique(last_action_us$Country),
                     all_l = unique(last_action_country$Country),
                     all_nus_l = unique(last_action_country$Country)[!grepl("United States", unique(last_action_country$Country))],
                     custom_l = input$cntr_l
                    )
    return(result)
  })
  observeEvent(input$cntr_sort_l, {
    toggleClass("cntr_l_legend", "small", length(selected_country_l())>7 & input$cntr_combine_l==F)
    toggleClass("cntr_l_legend", "large", length(selected_country_l())<=7 | input$cntr_combine_l)
    if (input$cntr_sort_l %in% c("bottom50_l","all_l","all_nus_l")) {
      updateCheckboxInput(session, "cntr_combine_l", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_l", value = FALSE)
    }
  })
  output$last_action_dygraph <- renderDygraph({
    if (input$cntr_combine_l == T){
        data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
          select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
          group_by(Date) %>%
          summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
          rename(date=Date) %>%
          fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
        names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_l))
  } else{
  	if (length(selected_country_l())>1){
  		data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
  		  select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
  		  tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_l, fill=0) %>%
  		  rename(date=Date) %>%
  		  fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
  	} else{
  		data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
  		  select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
  		  rename(date=Date) %>%
  		  fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
  		names(data4dygraph)[2] <- paste0(isolate(selected_country_l()))
  	}
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_l)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date", 
                         ylab = ifelse(input$prop_l, paste0("Proportion of ", cntr_reactive$selected_metric_l, " (%)"), cntr_reactive$selected_metric_l),                         
                         title = paste0(ifelse(input$prop_l, "Out of all countries, the proportion of sessions whose last action is ", "The number of sessions whose last action is "), cntr_reactive$selected_metric_l, " from ", ifelse(input$cntr_sort_l %in% c("all_l","all_nus_l"), ifelse(input$cntr_sort_l=="all_l","All Countries","All Countries but U.S."), paste0(selected_country_l(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_l) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_l_legend", show = "always", showZeroValues = FALSE)  %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "black")
  })
  output$last_action_pie_pl <- renderHighchart({      
    data4pie <- last_action_country_dt() %>%
      group_by(Region) %>%
      mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      group_by(Region, Country) %>%
      mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      select(Region, Country, country_total, region_total) %>%
      unique() %>% ungroup() %>% arrange(Region, Country)
    data4pie_us <- last_action_us %>%
    filter(last_action_us$Date >= input$date_all_country[1]
           & last_action_us$Date <= input$date_all_country[2]) %>%
      group_by(Country) %>%
      summarise_(us_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>% ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_l, " by Country")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
                   shadow = FALSE,
                   center = c("50%", "50%")
        )
      ) %>%
      hc_add_series(data = data4pie %>% select(name=Region, y=region_total) %>% 
                      mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60, 
                                      color = 'black',
                                      formatter = JS("function () {
                                      return this.percentage > 5 ? this.point.name : null;
                                      }"))
                   ) %>%
      hc_add_series(data = data4pie %>% select(name=Country, y=country_total) %>% 
                      mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% mutate(id=tolower(Region)) %>% 
          select(name=Country, y=country_total, id) %>% 
          unique() %>% 
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          )))
      )  
      hc
  })
  output$last_action_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()}, 
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)      
    )


  most_common_country_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- most_common_country %>%
      rbind(most_common_us) %>%
  	  filter(Date >= input$date_most_common[1]
  	         & Date <= input$date_most_common[2]
  	         & Country %in% selected_country_l()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- most_common_country %>%
  	  filter(Date >= input$date_most_common[1]
  	         & Date <= input$date_most_common[2]
  	         & Country %in% selected_country_l())
    }    
    return (fnl_tbl)
  })
  most_common_country_prop_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- most_common_country_prop %>%
      rbind(most_common_us_prop) %>%
  	  filter(Date >= input$date_most_common[1]
  	         & Date <= input$date_most_common[2]
  	         & Country %in% selected_country_l()) %>%
      arrange(Date, Country)
    } else{
      fnl_tbl <- most_common_country_prop %>%
  	  filter(Date >= input$date_most_common[1]
  	         & Date <= input$date_most_common[2]
  	         & Country %in% selected_country_l())
    }    
    return (fnl_tbl)
  })
  observeEvent(input$action_select_m, {
	cntr_reactive$selected_metric_m <- switch(input$action_select_m,
	                                          olv_m = "Other Languages",
	                                          oproj_m = "Other Projects", 
	                                          prln_m = "Primary Links", 
	                                          search_m = "Search", 
	                                          secln_m = "Secondary Links"
	                                         )
  })
  selected_country_m <- reactive({
    all_country_temp<- most_common_country %>%
      filter(most_common_country$Date >= input$date_most_common[1]
             & most_common_country$Date <= input$date_most_common[2]) %>%
      select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
      group_by(Country) %>%
      summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
      arrange(value, Country)	 
    result <- switch(input$cntr_sort_m,
                     top10_m = {tail(all_country_temp, 10)$Country},
                     bottom50_m = {head(all_country_temp, 50)$Country},
                     us_m = unique(most_common_us$Country),
                     all_m = unique(most_common_country$Country),
                     all_nus_m = unique(most_common_country$Country)[!grepl("United States", unique(most_common_country$Country))],
                     custom_m = input$cntr_m
                    )
    return(result)
  })
  observeEvent(input$cntr_sort_m, {
    toggleClass("cntr_m_legend", "small", length(selected_country_m())>7 & input$cntr_combine_m==F)
    toggleClass("cntr_m_legend", "large", length(selected_country_m())<=7 | input$cntr_combine_m)
    if (input$cntr_sort_m %in% c("bottom50_m","all_m","all_nus_m")) {
      updateCheckboxInput(session, "cntr_combine_m", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_m", value = FALSE)
    }
  })
  output$most_common_dygraph <- renderDygraph({
    if (input$cntr_combine_m == T){
        data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
          select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
          group_by(Date) %>%
          summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
          rename(date=Date) %>%
          fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
        names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_m))
	} else{
		if (length(selected_country_m())>1){
			data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
			  select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
			  tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_m, fill=0) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
		} else{
			data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
			  select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
			  rename(date=Date) %>%
			  fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
			names(data4dygraph)[2] <- paste0(isolate(selected_country_m()))
		}
	}
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_m)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date", 
                         ylab = ifelse(input$prop_m, paste0("Proportion of ", cntr_reactive$selected_metric_m, " (%)"), cntr_reactive$selected_metric_m),                         
                         title = paste0(ifelse(input$prop_m, "Out of all countries, the proportion of visits whose most common section clicked on is ", "The number of visits whose most common section clicked on is "), cntr_reactive$selected_metric_m, " from ", ifelse(input$cntr_sort_m %in% c("all_m","all_nus_m"), ifelse(input$cntr_sort_m=="all_m","All Countries","All Countries but U.S."), paste0(selected_country_m(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_m) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_m_legend", show = "always", showZeroValues = FALSE)  %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "black")
})
output$most_common_pie_pl <- renderHighchart({      
  data4pie <- most_common_country_dt() %>%
    group_by(Region) %>%
    mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
    group_by(Region, Country) %>%
    mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
    select(Region, Country, country_total, region_total) %>%
    unique() %>% ungroup() %>% arrange(Region, Country)
  data4pie_us <- most_common_us %>%
  filter(most_common_us$Date >= input$date_all_country[1]
         & most_common_us$Date <= input$date_all_country[2]) %>%
    group_by(Country) %>%
    summarise_(us_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>% ungroup()
  hc <- highchart() %>%
    hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
    hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_m, " by Country")) %>%
    hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
    hc_plotOptions(
      pie = list(
                 shadow = FALSE,
                 center = c("50%", "50%")
      )
    ) %>%
    hc_add_series(data = data4pie %>% select(name=Region, y=region_total) %>% 
                    mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                  size = '60%',
                  name = "All Continents",
                  dataLabels = list(distance = -60, 
                                    color = 'black',
                                    formatter = JS("function () {
                                    return this.percentage > 5 ? this.point.name : null;
                                    }"))
                 ) %>%
    hc_add_series(data = data4pie %>% select(name=Country, y=country_total) %>% 
                    mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                  name = "All Countries", size = '100%', innerSize = "60%") %>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = data4pie %>% mutate(id=tolower(Region)) %>% 
        select(name=Country, y=country_total, id) %>% 
        unique() %>% 
        split(.$id) %>%
        lapply(function(x){
          list(id= as.character(unique(x[,3])),
               data=list_parse(x[,-3]))
        }) %>%
        magrittr::set_names(NULL) %>%
        append(list(list(
          id="united states",
          data=list_parse2(data4pie_us)
        )))
    )  
    hc
})
  output$most_common_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()}, 
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)      
    )
}



# cdata <- session$clientData
#  # debug
#  output$ex_out <- renderText({
#      cnames <- names(cdata)
#
#          allvalues <- lapply(cnames, function(name) {
#            paste(name, cdata[[name]], sep=" = ")
#          })
#          paste(allvalues, collapse = "\n")
#    })

