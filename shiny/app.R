library(shiny)

############################ 0. Cleaning the data ##############################
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
import::from(magrittr, `%>%`)

# reading cs enrollment in high schools in IN school corporations
inhs.wide <- read.csv("inhs.csv", header = TRUE)
inhs.long <- reshape2::melt(inhs.wide,
                            id.vars = c("year", "subject", "ethnicity", "gender"), 
                            measure.vars = 6:275, 
                            variable.name = "corpID", 
                            value.name = "N")
inhs.long[is.na(inhs.long)] <- 0
inhs.long$corpID <- gsub("X", "", inhs.long$corpID)

# merging the data with locale information of each school
corp_locale <- read.csv("corp_locale.csv")
corp_locale$corpID <- stringr::str_pad(corp_locale$corpID, 4, pad = "0")
enroll <- dplyr::left_join(inhs.long, corp_locale, by = "corpID")
enroll <- dplyr::select(enroll, corpID, corpName, locale, year, subject, ethnicity, gender, N)

# combining three types of races into one:
# "American Indian", "Asian or Pacific Islander", "Multiracial"
# into Multiracial and Others"
enroll_agg <- enroll %>% 
    dplyr::group_by(corpID, year, subject, gender) %>% 
    dplyr::filter(ethnicity %in% c("American Indian", 
                                   "Asian or Pacific Islander", 
                                   "Multiracial")) %>% 
    dplyr::mutate(n = sum(N))
enroll_agg <- enroll_agg %>% dplyr::select(-N, N = n)
enroll_agg$ethnicity <- "Multiracial and Others"
enroll_trim <- enroll_agg %>% dplyr::distinct()
enroll_trim <- as.data.frame(enroll_trim)
# taking fourth races from the previous data
enroll_4eth <- enroll %>% 
    dplyr::filter(ethnicity %in% c("White", "Asian", "Black", "Hispanic"))
# combining the ones with the four races and the newly created dataframe
enroll_5eth <- rbind(enroll_4eth, enroll_trim) %>% rename(course = subject)
enroll_5eth$corpName <- as.character(enroll_5eth$corpName)
enroll_5eth$course <- as.character(enroll_5eth$course)
enroll_5eth$ethnicity <- as.character(enroll_5eth$ethnicity)

enr <- enroll_5eth

# To arrange the appearnce of values
course.basic <- c("Introduction to CS", "CS I", "AP CS Principles")
course.advanced <- c("AP CS A", "CS II", "CS III: Databases", 
                     "CS III: Special Topics", "CS III: Informatics", 
                     "CS III: Cybersecurity", "CS III: Software Dev")
course.IB <- c("CS Higher Level, IB", "CS Standard Level, IB", 
               "Cambridge Int AS and A")
ethnicity.order <- c("White", "Asian", "Black", "Hispanic", "Multiracial and Others")
course.order <- c(course.basic, course.advanced, course.IB)
locale.order <- c("City", "Suburb", "Rural", "Town")

max.y = max(enr %>% group_by(year) %>% summarize(n = sum(N)))
################################################################################
############################### 1. Define UI ###################################


ui <- fluidPage(
    theme = shinythemes::shinytheme('united'),
    titlePanel("Indiana CS Enrollment in High Schools"),
    "by",
    strong("Minji Jeon"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "interest", 
                         label = "What would you like to investigate?", 
                         choices = c("Total" = "year",
                                     "Gender" = "gender",
                                     "Ethnicity" = "ethnicity", 
                                     "Locale" = "locale")
            ),
            conditionalPanel(
                condition = "input.interest != 'year'",
                selectInput("plot_type", "Choose the plot type",
                            choices = c("line", "bar"))
            ),
                
            # Adjusting the data
            radioButtons(inputId = "adjust", 
                        label = "Which part of the data would you include?", 
                        choices = c("Year" = "year",
                                    "Gender" = "gender",
                                    "Ethnicity" = "ethnicity", 
                                    "Locale" = "locale",
                                    "Course" = "course")
            ),
            conditionalPanel(
                condition = "input.adjust == 'year'",
                sliderInput("year_slider", "Choose years",
                            value = c(2014, 2020), 
                            min = 2014, max = 2020, sep = "")
            ),
            conditionalPanel(
                condition = "input.adjust == 'gender'",
                checkboxGroupInput("gender_check", "Choose genders to show",
                                   choices = unique(enr$gender),
                                   selected = unique(enr$gender))
            ),
            conditionalPanel(
                condition = "input.adjust == 'ethnicity'",
                checkboxGroupInput("ethnicity_check", "Choose races to show",
                                   choices = ethnicity.order,
                                   selected = ethnicity.order)
            ),
            conditionalPanel(
                condition = "input.adjust == 'locale'",
                checkboxGroupInput("locale_check", "Choose locales to show",
                                   choices = locale.order,
                                   selected = locale.order)
            ),
            conditionalPanel(
                condition = "input.adjust == 'course'",
                selectInput("course_check", 
                            "Choose courses to show. 
                            (Press backspace if you want to delete any selected course)",
                            choices = course.order,
                            multiple = TRUE,
                            selected = c(course.basic, course.advanced))
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotly::plotlyOutput("plot")),
                tabPanel("Table", DT::DTOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    enr_react <- reactive({
        enr %>% 
            subset(year >= input$year_slider[1] & year <= input$year_slider[2]) %>% 
            subset(gender %in% input$gender_check) %>% 
            subset(ethnicity %in% input$ethnicity_check) %>% 
            subset(locale %in% input$locale_check) %>% 
            subset(course %in% input$course_check)
    })

    output$plot <- plotly::renderPlotly({
        if (input$interest == "year") {
            enr_react() %>% group_by(year) %>% summarize(n = sum(N)) %>% 
                ggplot(aes(x = year, y = n)) + geom_line() + 
            ylab("Enrollees") + ylim(0, max.y) + 
            scale_x_continuous(breaks = pretty_breaks())
        } else if (input$interest == "gender") {
            df = enr_react() %>% group_by(year, gender) %>% summarize(n = sum(N))
            if (input$plot_type == "line") {
                df %>% ggplot(aes(x = year, y = n, color = gender)) + 
                geom_line() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            } else {
                df %>% ggplot(aes(x = year, y = n, fill = gender)) + 
                geom_col() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            }
        } else if (input$interest == "ethnicity") {
            df = enr_react() %>% group_by(year, ethnicity) %>% 
              summarize(n = sum(N)) %>% arrange(n) %>% 
              mutate(ethnicity = factor(ethnicity,
                                        levels = ethnicity.order))
            if (input$plot_type == "line") {
                df %>% ggplot(aes(x = year, y = n, color = ethnicity)) + 
                geom_line() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            } else {
                df %>% ggplot(aes(x = year, y = n, fill = ethnicity)) + 
                geom_col() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            }
        } else {
            df = enr_react() %>% group_by(year, locale) %>% 
              summarize(n = sum(N)) %>% arrange(n) %>% 
              mutate(locale = factor(locale, levels = locale.order))
            if (input$plot_type == "line") {
                df %>% ggplot(aes(x = year, y = n, color = locale)) + 
                geom_line() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            } else {
                df %>% ggplot(aes(x = year, y = n, fill = locale)) + 
                geom_col() + ylab("Enrollees") + ylim(0, max.y) + 
                scale_x_continuous(breaks = pretty_breaks())
            }
        }
        
    })
    
    output$table <- DT::renderDT({
        enr_react()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
