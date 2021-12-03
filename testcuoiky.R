## app.R ##
library(shinydashboard)
library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(Hmisc)
library(psych)
datamovie=read.csv("Z:\\VKU-Study\\datasetforR\\Cuoiky\\movie_data.csv")
names(datamovie)[3:18]
names(datamovie)
datamovie_100 = head(datamovie, 100)
### dropdow function
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    style="background-color:#BCD3EC; border-radius: 10px",
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

## menu data


## menu bieu do menu1_left
{menuleft_1 <- tabItem(tabName = "dashboard",
   
  fluidRow(

    tabBox(
      width = 12,
      title = "Dataset",
      side = "right", height = "800px",
      selected = "Tables",
      tabPanel(
        title = "Tables",
        div(
          h4("Data tables")
        ),
        div(
          dropdownButton(
            label = "Hide field", status = "default", width = 650,
            tags$label("Choose :"),
            fluidRow(
              column(
                width = 4,
                checkboxGroupInput("checkInput1",label = NULL,choices = c(names(datamovie)[1:9]))
              ),
              column(
                width = 4,
                checkboxGroupInput("checkInput2",label = NULL,choices = c(names(datamovie)[10:18]))
              ),
              column(
                width = 4,
                checkboxGroupInput("checkInput3",label = NULL,choices = c(names(datamovie)[19:26]))
              )
            )
          ),
          fluidRow(
            column(4, selectInput(width = "200px","selectInput1", "Choose a Country:",
                                  c("All", sort(unique(datamovie$country))))),
            column(4, selectInput(width = "200px","selectInput2", "Choose a Language:",
                                  c("All", sort(unique(datamovie$language))))),
          ),
          
        ),
        div(style = 'overflow-y:scroll;height:500px;',
            tableOutput('table_dataset')
        )
        
      ),
    ),
    
    
  )
  
)}
## menu left chart
{menuleft_chart <- tabItem(tabName = "menuchart",
  fluidRow(
    div(
      style="padding:15px;",
      dropdownButton(
        label = "Hide field", status = "default",
        tags$label("Choose :"),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              inputId  = "inputOfChart",
              label = NULL,
              choices = c(names(datamovie)[1:9]),
              selected = names(datamovie)[1]
            )
          ),
          column(
            width = 4,
            radioButtons(
              inputId  = "inputOfChart",
              label = NULL,
              choices = c(names(datamovie)[10:18]),
              selected = "mpg"
            )
          ),
          column(
            width = 4,
            radioButtons(
              inputId  = "inputOfChart",
              label = NULL,
              choices = c(names(datamovie)[19:26]),
              selected = "mpg"
            )
          )
        )
      ),
      
      selectInput(width = "200px","selectInput_menuchart", "Choose a variable:",
                  c(sort(unique(names(datamovie)))))
    ),
    tabBox(
      width = 12,
      title = "Dataset",
      side = "right",
      selected = "Boxplot",
      tabsetPanel(
        tabPanel("Boxplot", plotOutput("plot_menu_chart")),
        tabPanel("Summary", verbatimTextOutput("summary_menu_chart")),
      )
    ),
   
  ),
)
}
##menuquestion
{menuquestion <- tabItem(tabName = "menuquestion",
  div(
    h5("1, Moi tuong quan giua nam san xuat va diem cua bo phim")
  ),
  tabsetPanel(
    tabPanel("Histogram plot", plotOutput("hisplot_q1_chart")),
    tabPanel("Histogram 2 plot", plotOutput("hisplot2_q1_chart")),
    tabPanel("Boxplot", plotOutput("boxplot_q1_chart")),
  ),
  div(
    h5("2, Tuong quan giua quoc gia san xuat va diem ibmd")
  ),
  tabsetPanel(
    tabPanel("Movie vs Ibmd", plotOutput("hisplot_q2_chart")),
    tabPanel("Movie vs Count", plotOutput("hisplot2_q2_chart")),
    tabPanel("Top Ibmd", plotOutput("hisplot3_q2_chart")),
  ),
  div(
    h5("3, Moi tuong quan giua ngan sach(xoa <1) va tong thu")
  ),
  tabsetPanel(
    tabPanel("Budget vs Gross", plotOutput("hisplot_q3_chart")),
    
    
  ),
  div(
    h5("4, Moi tuong quan giua ngan sach(xoa <1) va tong thu")
  ),
  tabsetPanel(
    tabPanel("Budget vs Gross", plotOutput("hisplot_q4_chart")),
  ),
  div(
    h5("5, Moi tuong quan giua the loai phim va IMDB")
  ),
  tabsetPanel(
    tabPanel("Genres vs Imdb", plotOutput("hisplot_q5_chart")),
    
  ),
  
  div(
    h5("6, Tac gia")
  ),
  tabsetPanel(
    tabPanel("tac gia vs Imdb", plotOutput("hisplot_q6_chart")),
    tabPanel("tac gia vs Imdb", plotOutput("hisplot2_q6_chart")),
    
  ),
)
}

## ui function
{ui <- dashboardPage(
  dashboardHeader(title = "Project for R"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bieu do", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Chart", tabName = "menuchart", icon = icon("th")),
      menuItem("Question", tabName = "menuquestion", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      menuleft_1,
      # Second tab content
      menuleft_chart,
      menuquestion
    )
  )
)}

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  output$table_dataset=renderTable(
    {
      x1<-input$checkInput1
      if(is.null(x1))x1<-character(0)
      x2<-input$checkInput2
      if(is.null(x2))x2<-character(0)
      x3<-input$checkInput3
      if(is.null(x3))x3<-character(0)
      
      data_sub_hide = select(datamovie,-x1,-x2,-x3)
      #data_sub_hide = subset(datamovie,select=-c(-x1,-x2,-x3))
      
      # has choose country
      if(input$selectInput1 == "All"){
        if(input$selectInput2 == "All"){
          head(data_sub_hide,100)
        }
        # has 2 choose
        else{
          data_choose=subset(data_sub_hide,language==input$selectInput2)
            head(data_choose,100)
        }
      }
      #has choose country
      else{
        data_choose=subset(data_sub_hide,country==input$selectInput1)
        update_select=c(unique(data_choose$language))
        updateSelectInput(
          session,"selectInput2",choices=c("All",update_select),selected = "All", "Choose a Language:")
        if(input$selectInput2 == "All"){
            head(data_choose,100)
        }
        # has 2 choose
        else{
          updateSelectInput(
            session,"selectInput2",choices=c("All",update_select),selected = input$selectInput2, "Choose a Language:")
          temp=subset(data_choose,language==input$selectInput2)
          head(temp,100)
        }
      }
      
    }
  ) 
  # controller menu chart
  output$summary_menu_chart = renderPrint({
    summary(datamovie[, input$inputOfChart])
  })
  # controller question
  #Q1 ##############
  output$hisplot_q1_chart  = renderPlot({
    datamovie %>%
      group_by(title_year) %>%
      summarise(mean_score = mean(imdb_score)) %>%
      ggplot(aes(x = title_year, y = mean_score, group = 1)) + 
      geom_point() +
      geom_line() +
      geom_smooth() + 
      labs(x="Movies Year", y="Mean IMDB score every year") + 
      ggtitle("Mean IMDB score for each year")
  })
  output$hisplot2_q1_chart= renderPlot({
    datamovie %>%
      group_by(title_year) %>%
      summarise(count_year = n()) %>%
      ggplot(aes(x = title_year, y = count_year, fill = count_year)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(2009, 280, label = "Max = 260" )) + 
      labs(x="Movies Year", y="Year Count") + 
      ggtitle("Distribution of year for the movies count") + 
      scale_fill_gradient(low = "purple", high = "red")
  })
  output$boxplot_q1_chart  = renderPlot({
    boxplot(imdb_score ~ title_year, data=datamovie, col='indianred')
    title("IMDB score vs Title year")
  })
  #Q2 ###########################################################
  country_group <- group_by(datamovie, country)
  movie_by_country <- summarise(country_group,
                                mean_score = mean(imdb_score),
                                n = n()) 
  output$hisplot_q2_chart  = renderPlot({
    ggplot(aes(x = country, y = mean_score, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=7)) +
      coord_flip() + ggtitle('Countries vs IMDB Scores')
  })
  output$hisplot2_q2_chart = renderPlot({
    ggplot(aes(x = country, y = n, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=6)) +
      coord_flip() + ggtitle('Countries vs Number of Movies')
  })
  output$hisplot3_q2_chart = renderPlot({
    newdata <- movie_by_country[order(-movie_by_country$mean_score),]
    temp = head(newdata, 10)
    ggplot(aes(x = country, y = mean_score, fill = country), data = temp) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=7)) +
      coord_flip() + ggtitle('Countries vs Number of Movies')
  })
  
  #Q3 ##########################
  q3_data = datamovie
  q3_cleaned_data = q3_data %>%
    subset(!is.na(gross)) %>%
    subset(!is.na(budget)) 
  function_convert_05 <- function(score) {
    x = score%/%0.5
    y = x*0.5
    if(y<score){
      y <- y  + 0.5
    }
    return (y)
  }
  q3_cleaned_data$imdb_score_level <- function_convert_05(q3_cleaned_data$imdb_score)
  
  output$hisplot_q3_chart  = renderPlot({
    q3_cleaned_data %>%
      group_by(imdb_score_level) %>%
      summarise(score_count = n(), 
                mean_budget = mean(budget, na.rm = TRUE)) %>%
      ggplot() +
      geom_point(aes(x = as.factor(imdb_score_level), y = mean_budget)) +
      geom_path(aes(x = as.factor(imdb_score_level), y = mean_budget, group = 1), size = 1, color = 4) +
      labs(x = "IMDB score level", y = "Each score level mean budget(US Dollar)", title = "Different IMDB score level's mean budget")
    
  })
  
  #Q4 #####################
  q4_cleaned_data <- q3_cleaned_data
  output$hisplot_q4_chart = renderPlot({
    q4_cleaned_data %>%
      group_by(imdb_score_level) %>%
      summarise(score_count = n(), 
                mean_gross = mean(gross, na.rm = TRUE)) %>%
      ggplot() +
      geom_point(aes(x = as.factor(imdb_score_level), y = mean_gross)) +
      geom_path(aes(x = as.factor(imdb_score_level), y = mean_gross, group = 1), size = 1, color = 6) +
      labs(x = "IMDB score level", y = "Each score level mean gross(US Dollar)", title = "Different IMDB score level's mean gross")
  })
  #Q5 ####################
  judgeAndSplit <- function(x) {
    sp = strsplit(x, '[|]')[[1]][1]
    return(sp)
  }
  genres_name = as.character(datamovie$genres) 
  newdatamovie = datamovie %>%
    mutate(genres1 = as.factor(sapply(genres_name, judgeAndSplit)))
  typeLabelCount = newdatamovie %>%
    group_by(genres1) %>%
    summarise(count = n()) %>%
    as.data.frame()
  output$hisplot_q5_chart = renderPlot({
    newdatamovie %>%
      ggplot(aes(reorder(genres1, imdb_score, median, order = TRUE), y = imdb_score, fill = genres1)) + 
      geom_boxplot() + 
      coord_flip() + 
      geom_label(data = typeLabelCount, aes(x = genres1, y = 10, label = count),  hjust = 0, size = 3) + 
      ggtitle("Ordered imdb scores distribution by popular movie genres") + 
      guides(fill=FALSE) + 
      ylim(0, 11) +
      labs(x = "Popular movie genre", y = "IMDB score")
  })
  # Q6 ###############
  q6_data <- datamovie
  
  output$hisplot_q6_chart = renderPlot({
    q6_data %>%
      subset(director_name != "") %>%
      group_by(director_name) %>%
      summarise(director_movies_count = n()) %>%
      filter(director_movies_count >= 10) %>%
      ggplot(aes(x = reorder(director_name, director_movies_count), y = director_movies_count)) + 
      geom_bar(stat = "identity", aes(fill = as.factor(director_movies_count))) +
      coord_flip() + 
      guides(fill=guide_legend(title="MoviesnQuantity")) +
      labs(x = "Director Name", y = "Movies number", title = "Director movies number ranking")
  })
  topDirectors = q6_data %>%
    subset(director_name != "") %>%
    group_by(director_name) %>%
    summarise(director_movies_count = n()) %>%
    filter(director_movies_count >= 10)
  directorList = lapply(topDirectors[1], as.factor) 
  output$hisplot2_q6_chart = renderPlot({
    q6_data[q6_data$director_name %in% directorList$director_name, ] %>%
      ggplot(aes(reorder(director_name, imdb_score, median, order = TRUE), y = imdb_score, fill = director_name)) + 
      geom_boxplot() + 
      guides(fill=FALSE) + 
      coord_flip() + 
      labs(x = "Director Name", y = "IMDB score", title = "Director IMDB score ranking")
  })
}

shinyApp(ui, server)