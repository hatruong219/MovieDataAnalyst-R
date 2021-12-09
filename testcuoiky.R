## app.R ##
rm(list = ls())
library(shinydashboard)
library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(factoextra)
library(psych)
datamovie=read.csv("Z:\\VKU-Study\\datasetforR\\Cuoiky\\movie_data.csv", stringsAsFactors = TRUE)
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
## menu dataset
{menu_dataset <- tabItem(tabName = "menu_dataset",
   fluidRow(
     infoBoxOutput(width = 3 ,"menu_dataset_inforbox_1"),
     infoBoxOutput(width = 3, "menu_dataset_inforbox_2"),
     infoBoxOutput(width = 3, "menu_dataset_inforbox_3"),
     infoBoxOutput(width = 3, "menu_dataset_inforbox_4")
   ),
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
        tabPanel(
          title = "Detail",
          div(
            strong(
              span("color : ", style = "color:blue"),
              " màu sắc của phim",
              br(),
              span("num_critic_for_reviews: ", style = "color:blue"),
              " số bài đánh giá của các nhà phê bình",
              br(),
              span("duration : ", style = "color:blue"),
              " thời lượng của phim",
              br(),
              span("genres : ", style = "color:blue"),
              " thể loại của phim",
              br(),
              span("aspect_ratio : ", style = "color:blue"),
              " tỷ lệ khung hình được sử dụng trong phim",
              br(),
              span("language : ", style = "color:blue"),
              " ngôn ngữ được sử dụng trong phim",
              br(),
              span("country : ", style = "color:blue"),
              " quốc gia sản xuất",
              br(),
              span("movie_title : ", style = "color:blue"),
              " tên phim",
              br(),
              span("content_rating : ", style = "color:blue"),
              " Hệ thống phân loại phim",
              br(),
              span("movie_imdb_link : ", style = "color:blue"),
              " link phim trên imdb",
              br(),
              span("title_year : ", style = "color:blue"),
              " năm",
              br(),
              span("plot_keywords : ", style = "color:blue"),
              " từ khóa được tìm kiếm",
              br(),
              span("num_user_for_reviews : ", style = "color:blue"),
              " số người dùng cho đánh giá",
              br(),
              span("num_voted_users : ", style = "color:blue"),
              " số người bình chọn",
              br(),
              span("facenumber_in_poster : ", style = "color:blue"),
              " số người xuất hiện trong poster",
              br(),
              span("budget : ", style = "color:blue"),
              " Ngân sách làm phim",
              br(),
              span("gross : ", style = "color:blue"),
              " Doanh thu",
              br(),
              
              
              span("{director_name, director_facebook_likes,</br>
              actor_1_name, actor_1_facebook_likes,
              actor_2_name, actor_2_facebook_likes,
                   actor_3_name, actor_3_facebook_likes }", style = "color:blue"),
              br(),
              "Tên và số like facebook của các đối tượng"
              
              
            ),
          )
        )
    
    ),
    
    
  )
  
)}
## menu left chart
{menuleft_chart <- tabItem(tabName = "menuchart",
   fluidRow(
     box(plotOutput("so_luong_phim_qua_tung_nam"), width = 12),
     box(plotOutput("so_luong_phim_theo_quoc_gia"), width = 12),
     box(plotOutput("so_luong_binh_chon_tung_phim"), width = 12),
     box(plotOutput("so_luong_danh_gia_tung_phim"), width = 12),
     box(plotOutput("danh_thu"), width = 12),
   )
)
}
##menuquestion
{menuquestion <- tabItem(tabName = "menuquestion",
  div(
    h5("Mối tương quan giữa năm sản xuất và điểm của phim")
  ),
  tabsetPanel(
    tabPanel("Plot 1", plotOutput("hisplot_q1_chart")),
    tabPanel("Plot 2", plotOutput("hisplot2_q1_chart")),
    tabPanel("Boxplot", plotOutput("boxplot_q1_chart")),
  ),
  div(
    h5("Điểm imdb đối với phim từ các quốc gia khác nhau")
  ),
  tabsetPanel(
    tabPanel("Plot 1", plotOutput("hisplot_q2_chart")),
    tabPanel("Plot 2", plotOutput("hisplot2_q2_chart")),
    tabPanel("Plot 3", plotOutput("hisplot3_q2_chart")),
  ),
  div(
    h5("Ngân sách ảnh hưởng như thế nào đến điểm IMDB")
  ),
  tabsetPanel(
    tabPanel("Plot", plotOutput("hisplot_q3_chart")),
    
    
  ),
  div(
    h5("Điểm Imdb tác động thế nào đến tổng thu từ bộ phim")
  ),
  tabsetPanel(
    tabPanel("Plot", plotOutput("hisplot_q4_chart")),
  ),
  div(
    h5("Thể loại phim và IMDB")
  ),
  tabsetPanel(
    tabPanel("Plot", plotOutput("hisplot_q5_chart")),
    
  ),
  
  div(
    h5("Tìm hiểu về các tác giả")
  ),
  tabsetPanel(
    tabPanel("Plot 1", plotOutput("hisplot_q6_chart")),
    tabPanel("Plot 2", plotOutput("hisplot2_q6_chart")),
    
  ),
)
}

##menu one var

{menu_model_1_predict <- tabItem(tabName = "menu_model_1_predict",
     fluidRow(
       box(plotOutput("predict_menu_1_plot"), width = 6),
       box(verbatimTextOutput("predict_menu_1_summary"), width = 6),
       
     )
)}

####menu predict
{menu_model_predict <- tabItem(tabName = "menu_model_predict",
       tabsetPanel(
         
         tabPanel("Summary", verbatimTextOutput("summary_predict")),
         tabPanel("Plot", 
                  plotOutput("plot_predict_1"),
         ),
         tabPanel("Phương trình", 
                  div(
                    pre(
                      print("
imdb_score = (4.928e+01) + (2.693e-03)*num_critic_for_reviews 
              + (1.134e-02)*duration 
              + (3.266e-06)*num_voted_users 
              + (-5.379e-04)*num_user_for_reviews 
              + (-2.224e-02)*title_year 
              + (-5.203e-09)*budget ")
                    )
                  )
                  ),
         
       ),
       tabsetPanel(
       tabPanel(
         title = "Predict",
         div(
           fluidRow(
             column(width = 4,numericInput('input_title_year', 'Enter title_year', 0),),
             column(width = 4,numericInput('input_num_critic_for_reviews', 'Enter num_critic_for_reviews', 0),),
             column(width = 4,numericInput('input_duration', 'Enter duration', 0),),
           ),
           fluidRow(
             column(width = 4,numericInput('input_num_voted_users', 'Enter num_voted_users', 0),),
             column(width = 4,numericInput('input_num_user_for_reviews', 'Enter num_user_for_reviews', 0),),
             column(width = 4,numericInput('input_budget', 'Enter butget', 0),)
           ),

           fluidRow(
             column(width = 4, actionButton("show_predict1", "Show modal dialog"))
           )
         )
       )
     )
)
}

##kmeans
{kmeans_tab <- tabItem(tabName = "kmeans_tab",
     tabsetPanel(
       
       tabPanel("ebow", plotOutput("ebow_plot")),
       tabPanel("kmeans", 
                plotOutput("kmeans_plot"),
       )
       
     ),
)}


## ui function
{ui <- dashboardPage(
  dashboardHeader(title = "Project for R"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "menu_dataset", icon = icon("dashboard")),
      menuItem("Chart", tabName = "menuchart", icon = icon("bar-chart-o")),
      menuItem("Question", tabName = "menuquestion", icon = icon("th")),
      
      menuItem("Model one var", tabName = "menu_model_1_predict", icon = icon("list-alt")),
      menuItem("Model Predict", tabName = "menu_model_predict", icon = icon("list-alt")),
      menuItem("Kmeans", tabName = "kmeans_tab", icon = icon("list-alt"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      menu_dataset,
      # Second tab content
      menuleft_chart,
      menuquestion,
      menu_model_1_predict,
      menu_model_predict,
      kmeans_tab
    )
  ),
  tags$head(tags$style(HTML('* {font-family: "Arial", charset="utf-8"};')))
)}

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  # menu 1
  output$menu_dataset_inforbox_1 <- renderInfoBox({
    infoBox(
      "Total Movie", dim(datamovie)[1],
      color = "purple"
    )
  })
  output$menu_dataset_inforbox_2 <- renderInfoBox({
    infoBox(
      "Mean Imbd", format(round(mean(datamovie$imdb_score), 2), nsmall = 2),
      color = "purple"
    )
  })
  output$menu_dataset_inforbox_3 <- renderInfoBox({
    infoBox(
      "Max Imbd", max(datamovie$imdb_score),
      color = "purple"
    )
  })
  output$menu_dataset_inforbox_4 <- renderInfoBox({
    infoBox(
      "Min Imbd", min(datamovie$imdb_score),
      color = "purple"
    )
  })
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
  
  # controller menu help #################
  data = datamovie
  output$so_luong_phim_qua_tung_nam <- renderPlot({
    
    qplot(data$imdb_score, geom = "histogram", xlab = "IMDB score", ylab = "Count", fill=I("blue"),
          col=I("red"),
          binwidth = 1,
          alpha=I(.5))+  scale_color_manual(name = "statistics", values = c(mean = "red"))
    
   
    
  })
  
  output$so_luong_phim_theo_quoc_gia <- renderPlot({
    
    qplot(data$duration, geom = "histogram", xlab = "Thời lượng phim", ylab = "Count", fill=I("black"),
          col=I("red"),
          binwidth = 1,
          alpha=I(.5))+
      scale_color_manual(name = "statistics", values = c(mean = "red"))
    
    
  })
  
  #Bieu do the hien so luong binh chon cua tung phim, lay 5 phi co so luong cao nhat
  
  output$so_luong_binh_chon_tung_phim <- renderPlot({
    qplot(data$num_user_for_reviews, geom = "histogram", xlab = "Reviews", ylab = "Count", fill=I("black"),
          col=I("green"),
          binwidth = 1,
          alpha=I(.5))+
      scale_color_manual(name = "statistics", values = c(mean = "grey"))
    
  })
  
  
  #Bieu do the hien so luong binh chon cua tung phim, lay 5 phi co so luong cao nhat
  
  output$so_luong_danh_gia_tung_phim <- renderPlot({
    
    data = data.frame(
      movie = data$movie_title,
      number = data$num_user_for_reviews
    )
    
    data = head(arrange(data,desc(data$number)), n = 7)
    
    vote <- ggplot(data, aes(x = movie, y = number, group=1)) +
      geom_line(color="green")+
      geom_point()
    
    # add title ...
    print(vote + labs(title= "Bieu do the hien so luong danh gia cua tung phim, lay 5 phi co so luong cao nhat",
                      y="So luong", x = "Ten phim"))
    
  })
  
  # bieu do the hien ti so cau thu voi tuoi
  output$danh_thu <- renderPlot({
    
    danh_thu = data.frame(
      x = c(data$movie_title),
      y = c(data$gross)
    )
    
    danh_thu = head(arrange(danh_thu,desc(danh_thu$y)), n = 10)
    
    ggplot(danh_thu, aes(x, y))+
      geom_bar(stat="identity", width=0.5, fill="red") +
      labs(title="Bieu do the hien so luong phim qua tung nam", subtitle="In May, 2021")+
      theme_minimal()
    
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
      labs(x="Năm", y="Điểm trung bình Imdb") + 
      ggtitle("Điểm Imdb qua mỗi năm")
  })
  output$hisplot2_q1_chart= renderPlot({
    datamovie %>%
      group_by(title_year) %>%
      summarise(count_year = n()) %>%
      ggplot(aes(x = title_year, y = count_year, fill = count_year)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(2009, 280, label = "Max = 260" )) + 
      labs(x="Năm sản xuất", y="Số lượng") + 
      ggtitle("Biểu đồ phân phối số lượng phim qua từng năm") + 
      scale_fill_gradient(low = "purple", high = "red")
  })
  output$boxplot_q1_chart  = renderPlot({
    boxplot(imdb_score ~ title_year, data=datamovie, col='indianred')
    title("Biểu đồ box tương quan giữa điểm imdb và năm sản xuất")
  })
  #Q2 ###########################################################
  country_group <- group_by(datamovie, country)
  movie_by_country <- summarise(country_group,
                                mean_score = mean(imdb_score),
                                n = n()) 
  output$hisplot_q2_chart  = renderPlot({
    ggplot(aes(x = country, y = mean_score, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=7)) +
      coord_flip() + ggtitle('Quốc gia và điểm IMDB')
  })
  output$hisplot2_q2_chart = renderPlot({
    ggplot(aes(x = country, y = n, fill = country), data = movie_by_country) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=6)) +
      coord_flip() + ggtitle('Quốc gia và số lượng phim')
  })
  output$hisplot3_q2_chart = renderPlot({
    newdata <- movie_by_country[order(-movie_by_country$mean_score),]
    temp = head(newdata, 10)
    ggplot(aes(x = country, y = mean_score, fill = country), data = temp) + geom_bar(stat = 'identity') + theme(legend.position = "none", axis.text=element_text(size=7)) +
      coord_flip() + ggtitle('Những quốc gia có điểm IMDB cao nhất')
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
      labs(x = "Mức điểm IMDB", y = "Ngân sách phim tại mỗi mức điểm", title = "Ngân sách trung bình tại các mức điểm IMDB")
    
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
      labs(x = "Mức điểm IMDB", y = "Tổng thu trung bình", title = "Tổng thu tại mỗi mức điểm IMDB")
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
      ggtitle("Biểu đồ phân phối điểm IMDB theo thể loại phim") + 
      guides(fill=FALSE) + 
      ylim(0, 11) +
      labs(x = "Thể loại phim", y = "Điểm IMDB")
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
      guides(fill=guide_legend(title="Số lượng phim")) +
      labs(x = "Tên đạo diễn", y = "Số lượng phim", title = "Bảng xếp hạng đạo diễn theo số lượng phim sản xuất")
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
      labs(x = "Tên đạo diễn", y = "Điểm IMDB", title = "Bảng xếp hạng đạo diễn theo điểm IMDB")
  })
  
  
  # menu don bien  #########################
  #bieu do
  movie_data_predict <- subset(datamovie, datamovie$budget > 0)
  movie_data_predict = movie_data_predict %>%
    subset(!is.na(gross)) %>%
    subset(!is.na(budget)) 
  movie_data_predict =movie_data_predict[ -c(2988), ]
  output$predict_menu_1_plot <- renderPlot({
    data = data.frame(
      num_critic_for_reviews = c(movie_data_predict$num_critic_for_reviews),
      imdb_score = c(movie_data_predict$imdb_score)
    )
    
    hoiquy <- lm(data = data, imdb_score ~ num_critic_for_reviews)
    
    plot(imdb_score~num_critic_for_reviews,data=data)
    abline(hoiquy,col="red")
    
  })
  
  output$predict_menu_1_summary <- renderPrint({
    data = data.frame(
      num_critic_for_reviews = c(movie_data_predict$num_critic_for_reviews),
      imdb_score = c(movie_data_predict$imdb_score)
    )
    
    hoiquy <- lm(data = data, imdb_score ~ num_critic_for_reviews)
    summary(hoiquy)
    
  })
  #menu model predict ##############
  newmovie <- na.omit(q3_cleaned_data) 
  lm_datamovieaa = subset(newmovie, newmovie$country == "USA")
  lm_datamovie <- select(lm_datamovieaa, c(
    num_critic_for_reviews,
    duration ,
    num_voted_users,    
    num_user_for_reviews,   
    title_year ,  
    budget,
    imdb_score
  ))
  lm_mov <- lm(imdb_score ~ ., data=lm_datamovie)
  lm_small <- step(lm_mov, direction = "backward", trace = FALSE)
  
  output$summary_predict <- renderPrint({
    summary(lm_mov)
  })
  output$plot_predict_1 <- renderPlot({
    pairs.panels(lm_datamovie, col='red')
  })

  
  
  observeEvent(input$show_predict1, {
    showModal(modalDialog(
      title = "Result Predict",
      tabsetPanel(
        tabPanel("Result", verbatimTextOutput("hisplot_predict_2")),
        
      ),
      
    ))
    
  })
  histdata <- rnorm(500)
  output$hisplot_predict_2 <- renderPrint({
    num_critic_for_reviews = as.numeric(input$input_num_critic_for_reviews)
    duration  = as.numeric(input$input_duration)
    num_voted_users = as.numeric(input$input_num_voted_users)  
    num_user_for_reviews = as.numeric(input$input_num_user_for_reviews) 
    title_year  = as.numeric(input$input_title_year)
    budget = as.numeric(input$input_budget)
    
    mov_fh <- data.frame(
      num_critic_for_reviews = num_critic_for_reviews,
      duration = duration,
      num_voted_users= num_voted_users,
      num_user_for_reviews= num_user_for_reviews, 
      title_year = title_year,
      budget = budget
    )
    
    prediction_TFH <- predict(lm_small, newdata=mov_fh, interval="confidence")
    print(prediction_TFH)
    
    
  })
  
  
  # kmeans ##################
  topDirectors = movie %>%
    subset(director_name != "") %>%
    group_by(director_name) %>%
    subset(mean_score = mean(imdb_score))%>%
    summarise(director_movies_count = n(), meanscore = mean(imdb_score))%>%
    filter(director_movies_count > 1)
  data_ss = select(topDirectors, c(2,3))
  data_scale = scale(data_ss)
  irisdata = dist(data_scale)
  output$ebow_plot = renderPlot({
    fviz_nbclust(data_scale, kmeans, method = "wss")+labs(subtitle = "Ebow")
  })
  output$kmeans_plot = renderPlot({
    km.out <- kmeans(data_scale, centers = 6, nstart = 100)
    fviz_cluster(list(data= data_scale, cluster= km.out$cluster))
  })
  
  
  
}

shinyApp(ui, server)