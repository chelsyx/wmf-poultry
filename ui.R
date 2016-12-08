library(shiny)
library(shinydashboard)
library(dygraphs)
library(highcharter)

all_country_data <- polloi::read_dataset("portal/all_country_data.tsv", col_types = "Dcididid")

function(request) {
  dashboardPage(

    dashboardHeader(title = "Wikipedia.org Portal", disable = FALSE),

    dashboardSidebar(
	  shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
        tags$script(src = "custom.js")
      ),
      sidebarMenu(id = "tabs",
        menuItem(text = "Traffic and Clickthrough Rate", tabName = "all_country"),
  	    menuItem(text = "First Visit", tabName = "first_visits_by_country"),
  	    menuItem(text = "Last Action", tabName = "last_action_by_country"),
  	    menuItem(text = "Most Common Section", tabName = "most_common_by_country"),
        menuItem(text = "Global Settings",
                 selectInput(inputId = "smoothing_global", label = "Smoothing", selectize = TRUE, selected = "day",
                             choices = c("No Smoothing" = "day", "Weekly Median" = "week", "Monthly Median" = "month", "Splines" = "gam")),
                 br(style = "line-height:25%;"), icon = icon("cog", lib = "glyphicon"))
      ),
      div(icon("info-sign", lib = "glyphicon"), HTML("<strong>Tip</strong>: you can drag on the graphs with your mouse to zoom in on a particular date range."), style = "padding: 10px; color: white;"),
      div(bookmarkButton(), style = "text-align: center;")
    ),

    dashboardBody(
    tabItems(

      tabItem(tabName = "all_country",
              fluidRow(
                shiny::column(width=3,
                  dateRangeInput("date_all_country", "Date Range",
                                 start = min(all_country_data$date),
                                 end = max(all_country_data$date), 
                                 startview = "month",
                                 separator = " to "),
                  checkboxInput("prop_a", "Use Proportion in All Countries", FALSE)),
				shiny::column(width=2,
                  selectInput("traffic_select", label = "Metrics", 
                              choices = list("Number of Events" = 'events', 
                              "Number of Visits" = 'visits', "Number of Sessions" = 'sessions', 
                              "Overall Clickthrough Rate"='ctr_all', 
                              "Clickthrough Rate Per Visit"='ctr_vst', 
                              "Clickthrough Rate Per Session"='ctr_ses'),
                              selected = 'events')),
	            shiny::column(width = 2,
                  selectInput("cntr_sort_a", "Group of Countries",
                              list("Top 10" = "top10_a",
                                   "Bottom 50" = "bottom50_a",
                                   "United States" = "us_a",
                                   "All" = "all_a",
                                   "All but US" = "all_nus_a",
                                   "Custom" = "custom_a"),
	                               "all_a")),
               shiny::column(width = 3,
                 conditionalPanel("input.cntr_sort_a == 'custom_a'", 
                   selectizeInput("cntr_a", "Countries", choices=sort(c(unique(all_country_data$country), "United States")), selected=c("United Kingdom","Germany","India","Canada","U.S. (South)"), multiple = TRUE))),
	             shiny::column(width = 2,
	               conditionalPanel("input.cntr_a.length > 1", checkboxInput("cntr_combine_a", "Combine Countries", FALSE)),
	               conditionalPanel("(input.traffic_select=='events' || input.traffic_select=='visits' || input.traffic_select=='sessions') && !input.prop_a", checkboxInput("cntr_logscale_a", "Use Log scale", FALSE)),
				   polloi::smooth_select("smoothing_cntr_a"))				
				),				
              fluidRow(
                  highchartOutput("traffic_pie_pl",height = "500px"),
                  br(),
        				  dygraphOutput("all_country_dygraph"),
        				  div(id = "cntr_a_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),	 
        				  DT::dataTableOutput("all_country_tbl")
              ),				
              includeMarkdown("./tab_documentation/traffic_ctr.md")
      ),
			  
      tabItem(tabName = "first_visits_by_country",
              fluidRow(
                shiny::column(width=3,
                  dateRangeInput("date_first_visit", "Date Range",
                                 start = min(all_country_data$date),
                                 end = max(all_country_data$date),
                                 startview = "month",
                                 separator = " to "),
                  checkboxInput("prop_f", "Use Proportion in All Countries", FALSE)),
                shiny::column(width=2,
                  selectInput("action_select_f", label = "Actions",
                              choices = list("No Action" = 'nact_f',
                              "Other Languages" = 'olv_f', "Other Projects" = 'oproj_f',
                              "Primary Links"='prln_f',
                              "Search"='search_f',
                              "Secondary Links"='secln_f'),
                              selected = 'search_f')),
                shiny::column(width = 2,
                  selectInput("cntr_sort_f", "Group of Countries",
                              list("Top 10" = "top10_f",
                              "Bottom 50" = "bottom50_f",
                              "United States" = "us_f",
                              "All" = "all_f",
                              "All but US" = "all_nus_f",
                              "Custom" = "custom_f"),
                              "all_f")),
                shiny::column(width = 3,
                  conditionalPanel("input.cntr_sort_f == 'custom_f'",
                    selectizeInput("cntr_f", "Countries", choices=sort(c(unique(all_country_data$country),"United States")), selected=c("United Kingdom","Germany","India","Canada","U.S. (South)"), multiple = TRUE))),
                shiny::column(width = 2,
                  conditionalPanel("input.cntr_f.length > 1", checkboxInput("cntr_combine_f", "Combine Countries", FALSE)),
                  conditionalPanel("!input.prop_f", checkboxInput("cntr_logscale_f", "Use Log scale", FALSE)),
                  polloi::smooth_select("smoothing_cntr_f"))
),
                fluidRow(
                    highchartOutput("first_visit_pie_pl",height = "500px"),
                    br(),
                    dygraphOutput("first_visit_dygraph"),
                    div(id = "cntr_f_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
                    DT::dataTableOutput("first_visits_by_country_tbl")
                ),
                includeMarkdown("./tab_documentation/first_visit.md")
      ),

      tabItem(tabName = "last_action_by_country",
              fluidRow(
                shiny::column(width=3,
                  dateRangeInput("date_last_action", "Date Range",
                                 start = min(all_country_data$date),
                                 end = max(all_country_data$date),
                                 startview = "month",
                                 separator = " to "),
                  checkboxInput("prop_l", "Use Proportion in All Countries", FALSE)),
                shiny::column(width=2,
                  selectInput("action_select_l", label = "Actions",
                              choices = list("No Action" = 'nact_l',
                              "Other Languages" = 'olv_l', "Other Projects" = 'oproj_l',
                              "Primary Links"='prln_l',
                              "Search"='search_l',
                              "Secondary Links"='secln_l'),
                              selected = 'search_l')),
                shiny::column(width = 2,
                  selectInput("cntr_sort_l", "Group of Countries",
                              list("Top 10" = "top10_l",
                              "Bottom 50" = "bottom50_l",
                              "United States" = "us_l",
                              "All" = "all_l",
                              "All but US" = "all_nus_l",
                              "Custom" = "custom_l"),
                              "all_l")),
                shiny::column(width = 3,
                  conditionalPanel("input.cntr_sort_l == 'custom_l'",
                    selectizeInput("cntr_l", "Countries", choices=sort(c(unique(all_country_data$country), "United States")), selected=c("United Kingdom","Germany","India","Canada","U.S. (South)"), multiple = TRUE))),
                shiny::column(width = 2,
                  conditionalPanel("input.cntr_l.length > 1", checkboxInput("cntr_combine_l", "Combine Countries", FALSE)),
                  conditionalPanel("!input.prop_l", checkboxInput("cntr_logscale_l", "Use Log scale", FALSE)),
                  polloi::smooth_select("smoothing_cntr_l"))
            ),
                fluidRow(
                    highchartOutput("last_action_pie_pl",height = "500px"),
                    br(),
                    dygraphOutput("last_action_dygraph"),
                    div(id = "cntr_l_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
                    DT::dataTableOutput("last_action_by_country_tbl")
                ),
                includeMarkdown("./tab_documentation/last_action.md")
      ),

      tabItem(tabName = "most_common_by_country",
              fluidRow(
                shiny::column(width=3,
                  dateRangeInput("date_most_common", "Date Range",
                                 start = min(all_country_data$date),
                                 end = max(all_country_data$date),
                                 startview = "month",
                                 separator = " to "),
                  checkboxInput("prop_m", "Use Proportion in All Countries", FALSE)),
                shiny::column(width=2,
                  selectInput("action_select_m", label = "Actions",
                              choices = list("Other Languages" = 'olv_m',
                              "Other Projects" = 'oproj_m',
                              "Primary Links"='prln_m',
                              "Search"='search_m',
                              "Secondary Links"='secln_m'),
                              selected = 'search_m')),
                shiny::column(width = 2,
                  selectInput("cntr_sort_m", "Group of Countries",
                              list("Top 10" = "top10_m",
                              "Bottom 50" = "bottom50_m",
                              "United States" = "us_m",
                              "All" = "all_m",
                              "All but US" = "all_nus_m",
                              "Custom" = "custom_m"),
                              "all_m")),
                shiny::column(width = 3,
                  conditionalPanel("input.cntr_sort_m == 'custom_m'",
                    selectizeInput("cntr_m", "Countries", choices=sort(c(unique(all_country_data$country), "United States")), selected=c("United Kingdom","Germany","India","Canada","U.S. (South)"), multiple = TRUE))),
                shiny::column(width = 2,
                  conditionalPanel("input.cntr_m.length > 1", checkboxInput("cntr_combine_m", "Combine Countries", FALSE)),
                  conditionalPanel("!input.prop_m", checkboxInput("cntr_logscale_m", "Use Log scale", FALSE)),
                  polloi::smooth_select("smoothing_cntr_m"))
            ),
                fluidRow(
                    highchartOutput("most_common_pie_pl",height = "500px"),
                    br(),
                    dygraphOutput("most_common_dygraph"),
                    div(id = "cntr_m_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
                    DT::dataTableOutput("most_common_by_country_tbl")
                ),
                includeMarkdown("./tab_documentation/most_common.md")
      )
    )
    ),

    skin = "black", title = "Portal Dashboard | Discovery | Engineering | Wikimedia Foundation")
}



# debug
# verbatimTextOutput('ex_out'),

