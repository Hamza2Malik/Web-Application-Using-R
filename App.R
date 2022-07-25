library("shiny")
library(shinydashboard)
library(dplyr)
library(tidyr)
library(DT)
library("plotly")
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(maps)
library("shinycssloaders")


ui <- dashboardPage(
  dashboardHeader(title="Exploring US 1973",  
                  tags$li(class="dropdown", tags$a(href="https://www.youtube.com/", icon("youtube"),"YouTube", target="_blank"))
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem(text = "Dataset", tabName = "data", icon = icon("dashboard")),
      menuItem(text = "Visualization", tabName = "viz", icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'dist'",selectInput(inputId = "var1", label ="Slect input choices" , choices = c1 , selected ="Rape")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'",selectInput(inputId = "var2", label ="Slect input for X choices" , choices = c1 , selected ="Assault")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'",selectInput(inputId = "var3", label ="Slect input for Y choices" , choices = c1 , selected ="Assault")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var4" , label ="Select the Arrest type" , choices = c1)),
      menuItem(text = "Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", 
              tabBox(
                id = "t1",width = 12,
                tabPanel(title = "About", icon = icon("dashboard"),
                         fluidPage(
                           #column(width = 8, tags$img(src="qwe.jpg", width =600 , height = 300),
                            #      tags$br() , 
                             #     tags$a("Photo by Campbell Jensen on Unsplash"), align = "center"),
                           column(width = 4, tags$br() ,
                                  tags$p("This data set comes along with base R and contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also, given is the percent of the population living in urban areas.")
                           )
                         )),
                tabPanel(title = "Data",  dataTableOutput("dataT"),icon = icon("dashboard")),
                tabPanel(title = "Structure", icon = icon("dashboard"), verbatimTextOutput("structure")),
                tabPanel(title = "Summary Stats", icon = icon("dashboard"), verbatimTextOutput("summary"))
              )),
      tabItem(tabName = "viz", 
              tabBox(
                id = "t2",width = 12,
                tabPanel("Crime Trends by State", value="trends",
                         fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                  tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                  
                         ),
                         (plotlyOutput("bar"))
                ),
                tabPanel(title = "Distribution",icon = icon("dashboard"), plotlyOutput("histplot"), value = "dist"),
                tabPanel("Correlation Matrix", id="corr" , plotlyOutput("cor")),
                tabPanel(title = "Relation among", icon = icon("dashboard"),value = "relation", 
                         radioButtons(inputId = "fit",label = "select smothing method",choices = c("lm","loess"), selected = "lm", inline= TRUE),
                         plotlyOutput("scatter"), side="left")
              )),
      tabItem(
        tabName = "map",
        box(      selectInput("crimetype","Select Arrest Type", choices = c1, selected="Rape", width = 250),
                  withSpinner(plotOutput("map_plot")), width = 12))
        
    )
    
  )
)
  

server <- function(input, output){
  output$structure <- renderPrint(
    my_data %>% str()
  )
  
  output$summary <- renderPrint(
    my_data %>% summary()
  )
  
  output$dataT <- renderDataTable(
    my_data
  )
  
# 2nd tab item
  output$bar <- renderPlotly({
    my_data %>% 
      plot_ly() %>% 
      add_bars(x=~states, y=~get(input$var4)) %>% 
      layout(title = paste("Statewise Arrests for", input$var4),
             xaxis = list(title = "State"),
             yaxis = list(title = paste(input$var4, "Arrests per 100,000 residents") ))
  })
  
  
  
  
  
  output$histplot <- renderPlotly({
    p1 = my_data %>% 
      plot_ly() %>% 
      add_histogram(x=~get(input$var1)) %>% 
      layout(xaxis = list(title = paste(input$var1)))
    
    
    p2 = my_data %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
    # stacking the plots on top of each other
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
  })
  
  output$scatter <- renderPlotly({
    t1 <- my_data %>% ggplot(aes(x=get(input$var2), y = get(input$var3)))+ geom_point()+geom_smooth(method = get(input$fit))+
      labs(title = paste("Relation between ", input$var2, "and", input$var3 ), x = input$var2, y =  input$var3)+
      theme(plot.title = element_textbox_simple(size = 10,halign = 0.5))
    ggplotly(t1)
  })
  
  output$cor <- renderPlotly({
    my_df <- my_data %>% 
      select(-states)
    
    
    corr <- round(cor(my_df), 1)
    
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
  
  output$head1 <- renderText(
    paste("5 states with high rate of", input$var4, "Arrests")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 states with low rate of", input$var4, "Arrests")
  )
  
  
  # Rendering table with 5 states with high arrests for specific crime type
  output$top5 <- renderTable({
    
    my_data %>% 
      select(states, input$var4) %>% 
      arrange(desc(get(input$var4))) %>% 
      head(5)
    
  })
  output$low5 <- renderTable({
    
    my_data %>% 
      select(states, input$var4) %>% 
      arrange(get(input$var4)) %>% 
      head(5)
    
  })
  
  output$map_plot <- renderPlot({
    new_join %>% 
      ggplot(aes(x=long, y=lat,fill=get(input$crimetype) , group = group)) +
      geom_polygon(color="black", size=0.4) +
      scale_fill_gradient(low="#73A5C6", high="#001B3A", name = paste(input$crimetype, "Arrest rate")) +
      theme_void() +
      labs(title = paste("Choropleth map of", input$crimetype , " Arrests per 100,000 residents by state in 1973")) +
      theme(
        plot.title = element_textbox_simple(face="bold", 
                                            size=18,
                                            halign=0.5),
        
        legend.position = c(0.2, 0.1),
        legend.direction = "horizontal"
        
      ) +
      geom_text(aes(x=x, y=y, label=abb), size = 4, color="white")
    
    
    
  })
  
  
  
  
}

my_data <- USArrests

my_data %>% str()

my_data %>% summary()


states <- rownames(my_data)
states 

my_data <- my_data %>%mutate("states"= states)

my_data%>%summary()

my_data

p1 <- my_data %>% plot_ly() %>% add_histogram(~Rape) %>% layout(xaxis = list(title = "Ploty"))
p1

p2 <- my_data %>% plot_ly() %>% add_boxplot(~Rape) %>% layout(yaxis = list(showticklabels = F))
p2

subplot(p2,p1, nrows = 2) %>% hide_legend() %>% layout(title = "Graphic Chart Histogram, Boxplot",
                                                       yaxis = list(title = "Frequency"))


c1 <- my_data %>% select(-states)%>%names()
c1

my_data %>% ggplot(aes(x=Rape, y = Assault))+ geom_point()+geom_smooth(method = "lm")+
  labs(title = "Relation between Rape and assult", x = "Rape", y = "Assults")+
  theme(plot.title = element_textbox_simple(size = 10,halign = 0.5))



my_data %>% 
  plot_ly() %>% 
    add_bars(x=~states, y=~Rape) %>% 
    layout(title = paste("Statewise Arrests for"),
           xaxis = list(title = "State"),
           yaxis = list(title = paste("Arrests per 100,000 residents") ))


my_data %>% 
  select(states, Rape) %>% 
  arrange(desc(Rape)) %>% 
  head(5)

state_map <- map_data("state") 

my_data1 = my_data %>% 
  mutate(State = tolower(states))  

merged =right_join(my_data1, state_map,  by=c("State" = "region"))


st = data.frame(abb = state.abb, stname=tolower(state.name), x=state.center$x, y=state.center$y)


new_join = left_join(merged, st, by=c("State" = "stname"))





shinyApp(ui,server)























