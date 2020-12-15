library(shiny)

############################ 1. Cleaning the data ##############################
############################ 1-a. Enrollment data ##############################
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(viridis)
library(wesanderson)
import::from(magrittr, `%>%`)

# reading cs enrollment in high schools of IN school corporations
inhs.wide <- read.csv("enroll.csv", header = TRUE)
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
# ylim(0, max.y)
################################################################################
#
#
#
######################## 1-b. Subgroup population data #########################

# reading subgroup population in high schools of IN school corporations
pop.wide <- read.csv("pop.csv", header = TRUE)
pop.long <- reshape2::melt(pop.wide,
                           id.vars = c("year", "ethnicity", "gender"), 
                           measure.vars = 5:274, 
                           variable.name = "corpID", 
                           value.name = "corp_n")
pop.long[is.na(pop.long)] <- 0
pop.long$corpID <- gsub("X", "", pop.long$corpID)
pop.long$corpID <- stringr::str_pad(pop.long$corpID, 4, pad = "0")
# merging the data with locale information of each school
corp_locale <- read.csv("corp_locale.csv")
corp_locale$corpID <- stringr::str_pad(corp_locale$corpID, 4, pad = "0")
pop <- dplyr::left_join(pop.long, corp_locale, by = "corpID")
pop <- dplyr::select(pop, corpID, corpName, locale, year, ethnicity, gender, corp_n)

# combining three types of races into one:
# "American Indian", "Asian or Pacific Islander", "Multiracial"
# into Multiracial and Others"
pop_agg <- pop %>% 
  dplyr::group_by(corpID, year, gender) %>% 
  dplyr::filter(ethnicity %in% c("American Indian", 
                                 "Native Hawaiian or Other Pacific Islander", 
                                 "Multiracial")) %>% 
  dplyr::mutate(n = sum(corp_n))
pop_agg <- pop_agg %>% dplyr::select(-corp_n, corp_n = n)
pop_agg$ethnicity <- "Multiracial and Others"
pop_trim <- pop_agg %>% dplyr::distinct()
pop_trim <- as.data.frame(pop_trim)
# taking fourth races from the previous data
pop_4eth <- pop %>% 
  dplyr::filter(ethnicity %in% c("White", "Asian", "Black", "Hispanic"))
# combining the ones with the four races and the newly created dataframe
pop_5eth <- rbind(pop_4eth, pop_trim)
pop_5eth$corpName <- as.character(pop_5eth$corpName)
pop_5eth$ethnicity <- as.character(pop_5eth$ethnicity)

pop <- pop_5eth

enr_pop <- merge(enr, pop, 
                 by = c("corpID", "corpName", "locale", 
                        "year", "ethnicity", "gender")) 
################################################################################
#
#
#
############################### 2. Define UI ###################################
ui <- fluidPage(
  theme = shinythemes::shinytheme('united'),
  
  tags$div(
    h2("Indiana CS Enrollment in High Schools"),
    p("by", strong("Minji Jeon"), strong(" | "), "Indiana University Bloomington", 
      strong(" | "), code("jeonmin@iu.edu"), strong(" | "),
      a(href = "https://sites.google.com/view/minji-jeon", 
        "link to Minji's personal website")
      )
  ),
  
  navbarPage("IN CS Enrollment",
######################### 2-a. First tab: Absolute #############################
    tabPanel(title = "Absolute",

    # 1st Sidebar
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "interest", 
                         label = "A-1. Compare:", 
                         choices = c("Total" = "year",
                                     "Gender" = "gender",
                                     "Ethnicity" = "ethnicity", 
                                     "Locale" = "locale")
            ),
            selectInput("plot_type", "A-2. Plot Type:",
                        choices = c("Line Plot" = "line",
                                    "Grouped Barplot" = "grouped bar",
                                    "Stacked Barplot" = "stacked bar")
            ),

            # Adjusting the data
            radioButtons(inputId = "adjust", 
                        label = "B-1. Which part of the data would you include/exclude?", 
                        choices = c("Year" = "year",
                                    "Gender" = "gender",
                                    "Ethnicity" = "ethnicity", 
                                    "Locale" = "locale",
                                    "Course" = "course")
            ),
            conditionalPanel(
                condition = "input.adjust == 'year'",
                sliderInput("year_slider", "B-2. Years Range:",
                            value = c(2014, 2020), 
                            min = 2014, max = 2020, sep = "")
            ),
            conditionalPanel(
                condition = "input.adjust == 'gender'",
                checkboxGroupInput("gender_check", "B-2. Genders to show:",
                                   choices = c("Female", "Male"),
                                   selected = c("Female", "Male"))
            ),
            conditionalPanel(
                condition = "input.adjust == 'ethnicity'",
                checkboxGroupInput("ethnicity_check", "B-2. Ethnicity to show:",
                                   choices = ethnicity.order,
                                   selected = ethnicity.order)
            ),
            conditionalPanel(
                condition = "input.adjust == 'locale'",
                checkboxGroupInput("locale_check", "B-2. Locales to show:",
                                   choices = locale.order,
                                   selected = locale.order)
            ),
            conditionalPanel(
                condition = "input.adjust == 'course'",
                selectInput("course_select", 
                            "B-2. Courses to show:
                            (Press backspace to delete any selected course.)",
                            choices = course.order,
                            multiple = TRUE,
                            selected = course.order)
            )
        ),

        # 1st Main panel for the plot & table
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotly::plotlyOutput("plot",
                                                      width = "95%", height = "500px")),
                tabPanel("Table", DT::DTOutput("table"))
            )
        )
    )),
######################### 2-b. Second tab: Proportion ##########################
    tabPanel(title = "Proportion",

           # 2nd Sidebar
           sidebarLayout(
               sidebarPanel(
                   radioButtons(inputId = "interest_2", 
                                label = "A-1. Compare:", 
                                choices = c("Year" = "year",
                                            "Gender" = "gender",
                                            "Ethnicity" = "ethnicity", 
                                            "Locale" = "locale")
                   ),
                   conditionalPanel(
                     condition = "input.interest_2 != 'year'",
                     sliderInput("year_slider_2", "A-2. Year:",
                                 min = 2014, max = 2020, sep = "",
                                 value = 2020)
                   ),
                   # Adjusting the data
                   radioButtons(inputId = "adjust_2", 
                                label = "B-1. Which part of the data would you include/exclude?", 
                                choices = c("Gender" = "gender",
                                            "Ethnicity" = "ethnicity", 
                                            "Locale" = "locale",
                                            "Course" = "course")
                   ),
    
                   conditionalPanel(
                     condition = "input.adjust_2 == 'gender'",
                     checkboxGroupInput("gender_check_2", "B-2. Genders to show:",
                                        choices = c("Female", "Male"),
                                        selected = c("Female", "Male"))
                   ),
                   conditionalPanel(
                     condition = "input.adjust_2 == 'ethnicity'",
                     checkboxGroupInput("ethnicity_check_2", "B-2. Ethnicity to show:",
                                        choices = ethnicity.order,
                                        selected = ethnicity.order)
                   ),
                   conditionalPanel(
                     condition = "input.adjust_2 == 'locale'",
                     checkboxGroupInput("locale_check_2", "B-2. Locales to show:",
                                        choices = locale.order,
                                        selected = locale.order)
                   ),
                   conditionalPanel(
                     condition = "input.adjust_2 == 'course'",
                     selectInput("course_select_2", 
                                 "B-2. Courses to show:
                                (Press backspace to delete any selected course.)",
                                 choices = course.order,
                                 multiple = TRUE,
                                 selected = course.order)
                   )
               ),
               
               # 2nd Main panel for the plot & table
               mainPanel(
                  tabsetPanel(
                      tabPanel("Plot", plotly::plotlyOutput("plot_prop",
                                                         width = "95%", height = "500px")),
                      tabPanel("Table", DT::DTOutput("table_prop"))
                  )
               )
           )
    ),

    tabPanel(title = "District",
             sidebarLayout(
               sidebarPanel(
                 p("The number of CS enrollees,"),
                 p("the number of total students in a corporation,"),
                 p("and its proportion in the year:"),
                 sliderInput("year_slider_3", "",
                             min = 2014, max = 2020, sep = "",
                             value = 2020)
               ),
               mainPanel(
                 DT::DTOutput("table_district")
               )
             )
    )

))
################################################################################
#
#
#
############################# 3. Define Server Logic ###########################

server <- function(input, output) {
  
############################ 3-a. First tab: Absolute ##########################
    enr_react <- reactive({
        enr %>% 
        subset(year >= input$year_slider[1] & year <= input$year_slider[2]) %>% 
        subset(gender %in% input$gender_check) %>% 
        subset(ethnicity %in% input$ethnicity_check) %>% 
        subset(locale %in% input$locale_check) %>% 
        subset(course %in% input$course_select)
    })
    
    gg1 <- list(expand_limits(y = 0),
                labs(x = "", y = "Number of Enrollees"),
                theme_bw(),
                theme(
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.border = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.ticks.y = element_blank()
               )
    )
    
    output$plot <- plotly::renderPlotly({
      # 3-a-1. Year Total
        if (input$interest == "year") {
          df = enr_react() %>% group_by(year) %>% summarize(n = sum(N))
          if (input$plot_type == "line") {
            df %>% ggplot(aes(x = year, y = n)) + 
              geom_line(color = "grey", size = 1.2) + 
              geom_point(shape = 21, color = "black", fill = "pink", size = 4) +
              scale_x_continuous(breaks = unique(enr$year)) + 
              gg1
          } else {
            df %>% 
              mutate(year = as.factor(year)) %>%
              ggplot(aes(x = year, y = n)) + 
              geom_col(fill = "pink") +
              gg1
          }
    # 3-a-2. Gender
        } else if (input$interest == "gender") {
            df = enr_react() %>% 
              group_by(year, gender) %>% summarize(n = sum(N))
            if (input$plot_type == "line") {
                df %>% ggplot(aes(x = year, y = n, color = gender)) + 
                geom_line(size = 1) + 
                geom_point(size = 3) + 
                scale_x_continuous(breaks = unique(enr$year)) +
                gg1
            } else if (input$plot_type == "stacked bar") {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = gender)) + 
                geom_col() + 
                gg1
            } else {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = gender)) +
                geom_bar(stat = "identity", position = "dodge") +
                gg1
            }
    # 3-a-3. Ethnicity
        } else if (input$interest == "ethnicity") {
            df = enr_react() %>% group_by(year, ethnicity) %>% 
              summarize(n = sum(N)) %>% arrange(n) %>% 
              mutate(ethnicity = factor(ethnicity,
                                        levels = ethnicity.order))
            if (input$plot_type == "line") {
                df %>% 
                ggplot(aes(x = year, y = n, color = ethnicity)) + 
                geom_line(size = 1) + 
                geom_point(size = 3) + 
                scale_x_continuous(breaks = unique(enr$year)) +
                gg1 +
                scale_color_manual(
                  values = wesanderson::wes_palette(
                    n = length(ethnicity.order), name = "Moonrise3"))
            } else if (input$plot_type == "stacked bar") {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = ethnicity)) + 
                geom_col() + 
                gg1 +
                scale_fill_manual(
                  values = wesanderson::wes_palette(
                    n = length(ethnicity.order), name = "Moonrise3"))
            } else {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = ethnicity)) +
                geom_bar(stat = "identity", position = "dodge") +
                gg1 +
                scale_fill_manual(
                  values = wesanderson::wes_palette(
                    n = length(ethnicity.order), name = "Moonrise3"))
            }
    # 3-a-4. Locales
        } else {
            df <- enr_react() %>% 
              group_by(year, locale) %>% 
              summarize(n = sum(N)) %>% arrange(n) %>% 
              mutate(locale = factor(locale, levels = locale.order))
            if (input$plot_type == "line") {
                df %>% ggplot(aes(x = year, y = n, color = locale)) + 
                geom_line(size = 1) + 
                geom_point(size = 3) + 
                scale_x_continuous(breaks = unique(enr$year)) +
                gg1 +
                scale_color_viridis(discrete = TRUE)
            } else if (input$plot_type == "stacked bar") {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = locale)) + 
                geom_col() + 
                gg1 +
                scale_fill_viridis(discrete = TRUE)
            } else {
                df %>% 
                mutate(year = as.factor(year)) %>%
                ggplot(aes(x = year, y = n, fill = locale)) +
                geom_bar(stat = "identity", position = "dodge") +
                gg1 +
                scale_fill_viridis(discrete = TRUE)
            }
        }
        
    })
    output$table <- DT::renderDT({
      df.table = enr_react()
      # 3-a-1. Year Total
      if (input$interest == "year") {
        df.table %>% 
          group_by(year) %>% 
          summarize(n = sum(N))
        # 3-a-2. Gender
      } else if (input$interest == "gender") {
        df.table %>% 
          group_by(year, gender) %>% 
          summarize(n = sum(N))
        # 3-a-3. Ethnicity
      } else if (input$interest == "ethnicity") {
        df.table %>% group_by(year, ethnicity) %>% 
          summarize(n = sum(N))
        # 3-a-4. Locales
      } else {
        df.table %>% 
          group_by(year, locale) %>% 
          summarize(n = sum(N))
      }
    })
    
    
############################ 3-b. Second tab: Proportion #######################
    
    prop_react <- reactive({
      enr_pop %>%
        subset(year == input$year_slider_2) %>%
        subset(gender %in% input$gender_check_2) %>%
        subset(ethnicity %in% input$ethnicity_check_2) %>%
        subset(locale %in% input$locale_check_2) %>%
        subset(course %in% input$course_select_2)
    })
    
    gg2 <- list(coord_flip(), 
                theme_bw(),
                theme(
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.border = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank()
                ),
                ylim(0, 10)
    )
    
    output$plot_prop <- plotly::renderPlotly({
      
      if (input$interest_2 == "year") {
        # 3-b-1. Year
        enr_pop %>%
          subset(gender %in% input$gender_check_2) %>%
          subset(ethnicity %in% input$ethnicity_check_2) %>%
          subset(locale %in% input$locale_check_2) %>%
          subset(course %in% input$course_select_2) %>%
          group_by(year) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100) %>%
          mutate(year = factor(year)) %>% 
          ggplot(aes(x = year, y = proportion)) +
          geom_segment(aes(xend = year, yend = 0), color = "mediumslateblue", 
                       size = 1.2, alpha = .4) +
          geom_point(size = 4, color = "midnightblue", fill = "mediumslateblue", 
                     alpha = .7, shape = 23, stroke = 1.2) +
          gg2 +
          labs(x = "Year", y = "Proportion(%)")
      } else if (input$interest_2 == "gender") {
        # 3-b-2. Gender
        prop_react() %>% 
          group_by(gender) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100) %>% 
          arrange(proportion) %>% # First sort by val. This sort the dataframe but NOT the factor levels
          mutate(gender = factor(gender, levels = gender)) %>% # This trick update the factor levels
          ggplot(aes(x = gender, y = proportion)) +
          geom_segment(aes(xend = gender, yend = 0), color = "indianred1", 
                       size = 1.2, alpha = .4) +
          geom_point(size = 4, color = "firebrick1", fill = "indianred1", 
                     alpha = .7, shape = 21, stroke = 1.2) +
          gg2 +
          labs(x = "Gender", y = "Proportion(%)")
      } else if (input$interest_2 == "ethnicity") {
        # 3-b-3. Ethnicity
        prop_react() %>% 
          group_by(ethnicity) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100) %>%
          arrange(proportion) %>% # First sort by val. This sort the dataframe but NOT the factor levels
          mutate(ethnicity = factor(ethnicity, levels = ethnicity)) %>% # This trick update the factor levels
          ggplot(aes(x = ethnicity, y = proportion)) +
          geom_segment(aes(xend = ethnicity, yend = 0), color = "darkslategray3", 
                       size = 1.2, alpha = .4) +
          geom_point(size = 4, color = "lightseagreen", fill = "darkslategray3", 
                     alpha = .7, shape = 22, stroke = 1.2) +
          gg2 +
          labs(x = "Ethnicity", y = "Proportion(%)")
      } else if (input$interest_2 == "locale") {
        # 3-b-4. Locales
        prop_react() %>%
          group_by(locale) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100) %>% 
          arrange(proportion) %>% # First sort by val. This sort the dataframe but NOT the factor levels
          mutate(locale = factor(locale, levels = locale)) %>% # This trick update the factor levels
          ggplot(aes(x = locale, y = proportion)) +
          geom_point(size = 4, color = "dodgerblue4", alpha = .7, shape = 8, stroke = 1) +
          geom_segment(aes(xend = locale, yend = 0), color = "dodgerblue2", 
                       size = 1.2, alpha = .4) +
          gg2 +
          labs(x = "Locale", y = "Proportion(%)")
      }
    })
    
    output$table_prop <- DT::renderDT({
      if (input$interest_2 == "year") {
        # 3-b-1. Year
        enr_pop %>%
          subset(gender %in% input$gender_check_2) %>%
          subset(ethnicity %in% input$ethnicity_check_2) %>%
          subset(locale %in% input$locale_check_2) %>%
          subset(course %in% input$course_select_2) %>%
          group_by(year) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100)
      } else if (input$interest_2 == "gender") {
        # 3-b-2. Gender
        prop_react() %>% 
          group_by(gender) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100)
      } else if (input$interest_2 == "ethnicity") {
        # 3-b-3. Ethnicity
        prop_react() %>% 
          group_by(ethnicity) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100)
      } else if (input$interest_2 == "locale") {
        # 3-b-4. Locales
        prop_react() %>%
          group_by(locale) %>% 
          summarize(proportion = sum(N) / sum(corp_n) * length(unique(course)) * 100)
      }
    })

############################## 3-c. Third tab: District ########################
    district_react <- reactive({
      dist_enr <- enr %>%
                  subset(year == input$year_slider_3) %>%
                  group_by(year, corpID, corpName) %>% 
                  summarize(enrollee = sum(N))
      
      dist_pop <- pop %>%
                  subset(year == input$year_slider_3) %>%
                  group_by(year, corpID, corpName) %>% 
                  summarize(total = sum(corp_n))
      
      dist_enr_pop <- merge(dist_enr, dist_pop, 
                            by = c("year", "corpID", "corpName")) %>%
        mutate(proportion = enrollee / total * 100) %>% 
        arrange(desc(proportion))
    })
    
    output$table_district <- DT::renderDT({
      district_react()
    })
      
    
}
################################################################################
#
#
#
############################## 4. Run the tapp #################################
shinyApp(ui = ui, server = server)
################################################################################


