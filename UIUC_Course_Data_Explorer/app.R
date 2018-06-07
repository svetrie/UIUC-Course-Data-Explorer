library(shiny)

library(tidyr)
library(dplyr)
library(magrittr)
library(stringr)

library(ggplot2)
library(plotly)
library(scales)

course_info = read.csv("course_info.csv")
gen_eds_info = read.csv("gen_eds_info.csv")
professor_info = read.csv("professor_info.csv")

#---------------------------------------------UI------------------------------------------------------------------

ui <- fluidPage(theme = "app_style.css", 
  
  div(class = "title", titlePanel(h1("UIUC Course Data Explorer")),
       h2("Created by Vetrie Senthilkumar")),
   

  navbarPage(title = NULL,
              tabPanel(title = span("Search for Courses", class = "navtitle"),
                       sidebarLayout(
                         
                         sidebarPanel(
                           div(selectInput(
                            inputId = "Course.Number",
                            label = "Choose a Course", 
                            choices = unique(course_info$Course.Number)
                           ), class = "selectinput"),
                           
                           div(htmlOutput(outputId = "CourseLink"), class = "course_link")
                           #Can insert some graphs here
                                  # Student Enrollment vs GPA for professors 
                                  # Stacked bar chart for grade distributions for top ten professors with most students
                           #Maybe add link to course 
                         ),
                         
                         mainPanel(
                           div(tableOutput(outputId = "Overall.Info"), class = "overall-info"),
                           div(plotlyOutput(outputId = "CourseProfessors.Graph"), class = "courseprofs_graph"),
                           div(tableOutput(outputId = "CourseProfessors.Info"), class = "course_profs")
                         )
                         
                       )),
              tabPanel(title = span("Search for Professor", class = "navtitle"),
                       sidebarLayout(
                         
                         sidebarPanel(
                           div(selectInput(inputId = "Professor",
                                       label = "Select an Instructor",
                                       choices = sort(unique(professor_info$Instructor)),
                                       selected = "Abbamonte, Peter M")
                           # Number of students vs GPA
                         ), class = "selectinput"),
                         
                         mainPanel(
                           div(tableOutput(outputId = "Instructor.Info"), class = "prof_info"),
                           div(plotlyOutput(outputId = "InstructorInfo.Graph"), class = "classestaught_graph")
                         )
                       )),
              tabPanel(title = span("Search for Gen Eds", class = "navtitle"),
                       sidebarLayout(

                         sidebarPanel(
                           div(checkboxGroupInput(inputId = "GenEd.Categories",
                                              choices = c("Advanced Composition" = "ACP",
                                                          "Humanity and Arts" = "HUM",
                                                          "Social and Behavioral Sciences" = "SBS",
                                                          "Natural Sciences" = "NAT",
                                                          "Western Culture" = "WCC",
                                                          "Non-Western/U.S. Minority Culture" = "NW/US",
                                                          "Quantitative Reasoning 1" = "QR1",
                                                          "Quantitative Reasoning 2" = "QR2"),
                                              label = "Select Gen Ed Categories")

                         ), class = "checkboxes"),
                         
                         mainPanel(
                           div(plotlyOutput(outputId = "GenEd.Graph"), class = "gened_plot"),
                           div(tableOutput(outputId = "GenEd.Info"), class = "gen_ed_info")
                         )
                       )),
              tabPanel(title = span("Search by Subject", class = "navtitle"),
                       sidebarLayout(
                        
                         sidebarPanel(
                           div(selectInput(inputId = "Subject",
                                       label = "Select Subject",
                                       choices = unique(course_info$Subject) %>% sort()),
                               class = "selectinput"),
                           div(radioButtons(inputId = "Sort", 
                                        label = "Sort Courses by ",
                                        choices = c("Highest Average GPA to Lowest Average GPA" = "descending",
                                                    "Lowest Average GPA to Highest Average GPA" = "ascending"))
                           # Avg GPA vs Course Number, size of circle dependent on student enrollment  
                         ), class = "radiobuttons"),
                         
                         mainPanel(
                           plotlyOutput(outputId = "Subject.Graph"),
                           div(tableOutput(outputId = "Subject.Courses"), class = "subject_info")
                         )
                         
                       )),
              tabPanel(title = span("Website Info", class = "navtitle"),
                      
                      div(class = "appinfo",
                          
                         h3("About the Creator"),
                         
                         p("\tHey guys! My name is Vetrie Senthilkumar and I'm a sophmore majoring in Computer Science
                         and Statistics at the University of Illinois. After my freshman year of college, I was
                         intrigued by the endless possibilties of data science, a field that seemed to blend both
                         aspects of my major. UIUC Course Data Explorer is an effort to apply my passion for data science to an idea
                         that could be beneficial to my college community. I hope you guys find this as an useful tool,
                         especially when it comes to the difficult prospect of choosing what classes to take next 
                         semester. Since this website is meant to serve U of I students, feel free to hit me up with features
                         you would like to see added or improved at vetsenthilkumar@gmail.com"),
                         
                         h3("Designing the Website"),
                         
                         p("\tFirst, I obtained the publicly available data from Professor Wade Fagen-Ulmschneider's Github
                         repository. The datasets used were the GPA and Gen Eds datasets (link attached below). I used R
                         to preprocess the data, adding important information that wasn't initally provided, like 
                         Average GPA for example. During this step, I also did some further analysis that was not included
                         in this website. If you're curious about the findings, I've created an R Markdown report with a link attached below.
                         The next step was creating a user interface that would allow other
                         students to explore the data for themselves. To accomplish this, I used Shiny, a popular R library
                         that allows the integration of R code with HTML/Javascript features. Then, I used CSS to add my
                         customization and style to the website's design. Finally, UIUC Course Data Explorer was deployed using
                         Shinyapps.IO"),
                         
                         tags$br(),
                         #tags$br(),
                         
                         h4("Links"),
                         
                         a("Link to Datasets", href = "https://github.com/wadefagen/datasets") ,
                         
                         tags$br(),
                         
                         a("Link to Report")
                       )
                        
                 ))
  
)

#---------------------------------------------SERVER LOGIC-------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   overall_course_info = reactive({
     sub = subset(course_info, course_info$Course.Number == input$Course.Number)
     names(sub)[c(1:3, 10)] = c("Title", "Course", "Students", "GPA")
     return (sub)
   })
   
   course_professors = reactive({
     sub = subset(professor_info, professor_info$Course.Number == input$Course.Number) %>% 
       arrange(desc(Total.Students))
     names(sub)[c(2, 3, ncol(sub))] = c("Course", "Students", "GPA")
     return (sub)
   })
   
   instructor_info = reactive({
     sub = subset(professor_info, professor_info$Instructor == input$Professor) %>% 
       arrange(desc(Total.Students))
     names(sub)[c(2, 3, ncol(sub))] = c("Course", "Students", "GPA")
     return (sub)
   })
   
   gen_ed_subset = reactive({
     sub = gen_eds_info
     
     if("ACP" %in% input$GenEd.Categories) {
       sub = sub[sub$ACP != "", ]
       #sub = sub %>% subset(ACP != "")
     }
     if("HUM" %in% input$GenEd.Categories) {
       sub = sub[sub$HUM != "", ]
     }
     if("SBS" %in% input$GenEd.Categories) {
       sub = sub[sub$SBS != "", ]
     }
     if("NAT" %in% input$GenEd.Categories) {
       sub = sub[sub$NAT != "", ]
     }
     if("WCC" %in% input$GenEd.Categories) {
       sub = sub[sub$CS == "WCC", ]
     }
     if("NW/US" %in% input$GenEd.Categories) {
       sub = sub[sub$CS == "NW" | sub$CS == "US", ]
     }
     if("QR1" %in% input$GenEd.Categories) {
       sub = sub[sub$QR == "QR1", ]
     }
     if("QR2" %in% input$GenEd.Categories) {
       sub = sub[sub$QR == "QR2", ]
     }
     
     sub = sub[, -c(1:3, 6:11)]
     sub = arrange(sub, desc(Avg.GPA))
     names(sub)[c(2:4)] = c("Title", "GPA", "Students")
     
     return (sub)
   })
   
   subject_subset = reactive({
     sub = subset(course_info, course_info$Subject == input$Subject)[, -c(11, 12)]
     
     if(input$Sort == "ascending") {
       sub = arrange(sub, Avg.GPA)
     } else {
       sub = arrange(sub, desc(Avg.GPA))
     }
     
     names(sub)[c(1:3, ncol(sub))] = c("Title", "Course", "Students", "GPA")
     
     return (sub)
   })
  
   output$Overall.Info = renderTable(overall_course_info()[,-c(11,12)])
   output$CourseLink = renderUI({
     tags$a('Link to Course Description', href = overall_course_info()$Links)
   })
   output$CourseProfessors.Info = renderTable(course_professors())
   
   output$Instructor.Info = renderTable(instructor_info())
   
   output$GenEd.Info = renderTable(gen_ed_subset())
   
   output$Subject.Courses = renderTable(subject_subset())
   
   course_professors_graph = reactive({
     letter_grades = gather(course_professors(), key = "Grade", value = "Percent.Students", 4:9)
     letter_grades$Percent.Students = sub("%", "", letter_grades$Percent.Students) %>% as.numeric()
     
     g = ggplot(letter_grades, aes(x = Instructor, y = Percent.Students,
                                   fill = Grade,
                                   text = paste0("Instructor: ", Instructor, "\n",
                                                 "Average GPA: ", GPA, "\n",
                                                 "Total Students: ", Students, "\n",
                                                 "Letter Grade: ", Grade, "\n",
                                                "Percent of Students: ", round(Percent.Students), "%"))) +
       geom_bar(position = "fill", stat = "identity") +
       scale_y_continuous(labels = percent_format()) +
       labs(title = "Grade Distributions for Different Instructors", y = "Percent of Students", x = "Instructor") +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       theme(axis.title = element_text(size = 14, family = "Arial")) +
       theme(title = element_text(size = 12, face = "bold", family = "Arial")) +
       #Alternative Color Gradient scale_fill_manual(values = c("#66d9ff", "#66ff66", "#ffff66", "#ffb366", "#ff6666", "#ff66ff"))
       scale_fill_manual(values = c("#0066ff", "#34cb34", "#e6e600", "#ff9900", "#cc0000", "#ff00ff"))
       
     return (g)
   })
   
   classes_taught_graph = reactive({
     grade_distributions = gather(instructor_info(), key = "Grade", value = "Percent.Students", 4:9)
     grade_distributions$Percent.Students = sub("%", "", grade_distributions$Percent.Students) %>% as.numeric()
     
     g = ggplot(grade_distributions, aes(x = Course, y = Percent.Students,
                                         fill = Grade,
                                         text = paste0("Course: ", Course, "\n",
                                                       "Average GPA: ", GPA, "\n",
                                                       "Total Students: ", Students, "\n",
                                                       "Grade: ", Grade, "\n",
                                                       "Percent of Students: ", Percent.Students, "%"))) +
       geom_bar(position = "fill", stat = "identity") +
       scale_y_continuous(labels = percent_format()) +
       labs(title = "Grade Distributions for Different Sections Taught", y = "Percent of Students", x = "Course") +
       theme(axis.title = element_text(size = 14, family = "Arial")) +
       theme(title = element_text(size = 12, face = "bold", family = "Arial")) +
       scale_fill_manual(values = c("#3399ff", "#66ff66", "#ffff66", "#ffb366", "#ff6666", "#ff66ff"))
     
     return (g)
   })
   
   gen_ed_plot = reactive({
     g = NA
     
     if (nrow(gen_ed_subset()) != 0) {
       g = ggplot(gen_ed_subset(), aes(x = Students, y = GPA,
                                       text = paste0("Title: ", Title, "\n",
                                                     "Course: ", Course, "\n",
                                                     "Total Students: ", Students, "\n",
                                                     "Average GPA: ", GPA),
                                       color = GPA)) +
         geom_point(size = 3, position = "jitter") +
         theme(legend.position = "none") +
         scale_color_gradient(low = "#ff6666", high = "#6666ff") +
         labs(x = "Number of Students", y = "Average GPA", title = "Gen Eds") +
         theme(axis.title = element_text(size = 14, family = "Arial")) +
         theme(title = element_text(size = 12, family = "Arial", face = "bold")) +
         scale_x_continuous(breaks = seq(0, 27000, by = 3000)) 
       
       
     } else {
        g = ggplot(data = NULL) +
          geom_blank() +
          labs(title = "No such classes exist")
     }
     
     return (g)
   })
   
   subject_plot = reactive({
     subject_data = subject_subset()
     subject_data$Course.Level = str_extract(subject_data$Course, "[0-9]+") %>% as.numeric()
     
     g = ggplot(subject_data) +
       geom_point(aes(x = Course.Level, y = GPA,
                      size = Students,
                      color = GPA,
                      text = paste("Title:", Title, "\n",
                                   "Course:", Course, "\n",
                                   "Total Students:", Students, "\n",
                                   "Average GPA;", GPA))) +
       scale_color_gradient(low = "#ff6666", high = "#6666ff") +
       scale_x_continuous(breaks = seq(0, (max(subject_data$Course.Level) / 100 %>% ceiling()) * 100, by = 100)) +
       ylim(1.5, 4)+
       labs(title = paste(input$Subject, "Classes"), x = "Course Level", y = "Average GPA") +
       theme(axis.title = element_text(size = 14, family = "Arial")) +
       theme(title = element_text(size = 12, family = "Arial", face = "bold")) +
       theme(legend.position = "none")
       
     return (g)
   })
   
   output$CourseProfessors.Graph = renderPlotly({
     ggplotly(course_professors_graph(), tooltip = "text")
   })
   
   output$InstructorInfo.Graph = renderPlotly({
     ggplotly(classes_taught_graph(), tooltip = "text")
   })
   
   output$GenEd.Graph = renderPlotly({
     ggplotly(gen_ed_plot(), tooltip = "text")
   })
   
   output$Subject.Graph = renderPlotly({
     ggplotly(subject_plot(), tooltip = "text")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

