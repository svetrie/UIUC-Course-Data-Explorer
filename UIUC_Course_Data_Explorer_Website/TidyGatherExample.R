library(tidyr)
library(magrittr)
library(scales)
library(plotly)

Professor_Info = read.csv("professor_info.csv")
AAS_100_Professors = Professor_Info %>% subset(Course.Number == "AAS 100")

#----------------------------GGPLOT METHOD-----------------------------------------------------------

long_data = gather(AAS_100, key = "Letter_Grade", value = "Percent_Students", 4:9)
#Use gather to condense a group of related variables into one column 
#key = what category due all the variables being condensed fall under?
#value = what type of information do all of these variables store?

long_data$Percent_Students = sub("%", "", long_data$Percent_Students) %>% as.numeric() 

ggplot(long_data, aes(x = Instructor, y = Percent_Students, fill = Letter_Grade)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format()) + 
  coord_flip()

#-------------------------------------PLOTLY METHOD-----------------------------------------------------

for (i in 4:9) {
  AAS_100_Professors[, i] = sub("%", "", AAS_100_Professors[, i]) %>% as.numeric()
}

plot_ly(data = AAS_100_Professors, x = ~A, y = ~Instructor, type = "bar", orientation = "h", name = "% As",
        marker = list(color = 'rgba(246, 78, 139, 0.6)',
                      line = list(color = 'rgba(246, 78, 139, 1.0)',
                                  width = 3))) %>%
  add_trace(x = ~B, name = '% Bs',
            marker = list(color = 'rgba(58, 71, 80, 0.6)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         xaxis = list(title = "Percent of Students"),
         yaxis = list(title = "Instructor"))

#---------------------------------------WORKSPACE------------------------------------------------------
course_info = read.csv("course_info.csv")
gen_eds = read.csv("gen_eds_info.csv")

min(course_info$Avg.GPA)
gen_eds = na.omit(gen_eds)
max(gen_eds$Student.Enrollment)


