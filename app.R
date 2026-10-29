# Dashboard uses SIMULATED / FICTIONAL student records only.
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
students <- read_csv("student_attendance.csv", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Student Attendance and Academic Performance"),
  p("DEMONSTRATION ONLY: all records are simulated, not real student data."),
  sidebarLayout(
    sidebarPanel(
      sliderInput("min_attendance", "Minimum attendance (%)", min=50, max=100, value=70, step=5),
      sliderInput("min_grade", "Minimum final grade", min=40, max=100, value=50, step=5),
      checkboxInput("show_trend", "Show trend line", TRUE),
      helpText("Change filters to update the table, summaries, and plots.")
    ),
    mainPanel(
      h3("Summary"),
      fluidRow(
        column(4, wellPanel(h4("Students shown"), textOutput("n_students"))),
        column(4, wellPanel(h4("Mean attendance"), textOutput("mean_attendance"))),
        column(4, wellPanel(h4("Mean final grade"), textOutput("mean_grade")))
      ),
      tabsetPanel(
        tabPanel("Filtered data", br(), tableOutput("student_table")),
        tabPanel("Attendance vs. grade", br(), plotOutput("scatter_plot", height="420px")),
        tabPanel("Attendance distribution", br(), plotOutput("attendance_plot", height="420px")),
        tabPanel("Group comparison", br(), plotOutput("group_plot", height="420px"))
      )
    )
  )
)
server <- function(input, output, session) {
  filtered_students <- reactive({
    students %>% filter(Attendance_Percentage >= input$min_attendance,
                        Final_Grade >= input$min_grade)
  })
  output$n_students <- renderText(nrow(filtered_students()))
  output$mean_attendance <- renderText({
    d <- filtered_students()
    if(nrow(d)==0) "N/A" else paste0(round(mean(d$Attendance_Percentage),1), "%")
  })
  output$mean_grade <- renderText({
    d <- filtered_students()
    if(nrow(d)==0) "N/A" else round(mean(d$Final_Grade),1)
  })
  output$student_table <- renderTable(filtered_students(), digits=1)
  output$scatter_plot <- renderPlot({
    d <- filtered_students()
    validate(need(nrow(d)>1, "Select filters that include at least two students."))
    p <- ggplot(d, aes(Attendance_Percentage, Final_Grade)) +
      geom_point(color="#FF6B6B", size=3) +
      labs(title="Attendance and Final Grade", x="Attendance (%)", y="Final grade") +
      theme_minimal(base_size=13)
    if(input$show_trend) p <- p + geom_smooth(method="lm", se=TRUE, color="#5B4BDB")
    p
  })
  output$attendance_plot <- renderPlot({
    ggplot(filtered_students(), aes(Attendance_Percentage)) +
      geom_histogram(binwidth=5, fill="#00A6A6", color="white") +
      labs(title="Attendance distribution", x="Attendance (%)", y="Students") +
      theme_minimal(base_size=13)
  })
  output$group_plot <- renderPlot({
    d <- filtered_students() %>% mutate(Attendance_Group=case_when(
      Attendance_Percentage < 75 ~ "Below 75%",
      Attendance_Percentage < 85 ~ "75% to <85%",
      TRUE ~ "85% and above"
    )) %>% group_by(Attendance_Group) %>%
      summarise(Average_Final_Grade=mean(Final_Grade), .groups="drop")
    validate(need(nrow(d)>0, "No records match the current filters."))
    ggplot(d, aes(Attendance_Group, Average_Final_Grade, fill=Attendance_Group)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=c("#FFB703","#00A6A6","#5B4BDB")) +
      labs(title="Average final grade by attendance group", x="Attendance group",
           y="Average final grade") + theme_minimal(base_size=13)
  })
}
shinyApp(ui=ui, server=server)
