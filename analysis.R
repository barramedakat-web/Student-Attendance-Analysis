# IMPORTANT: student_attendance.csv contains SIMULATED / FICTIONAL records.
# Install packages once if needed:
# install.packages(c("ggplot2", "dplyr", "readr"))
library(ggplot2)
library(dplyr)
library(readr)

students <- read_csv("student_attendance.csv", show_col_types = FALSE)
print(head(students))
print(str(students))
print(summary(students))
stopifnot(nrow(students) == 50)

descriptive_summary <- students %>% summarise(
  Number_of_Students = n(),
  Mean_Attendance = mean(Attendance_Percentage, na.rm = TRUE),
  Mean_Quiz_Average = mean(Quiz_Average, na.rm = TRUE),
  Mean_Assignment_Score = mean(Assignment_Score, na.rm = TRUE),
  Mean_Final_Grade = mean(Final_Grade, na.rm = TRUE),
  Median_Final_Grade = median(Final_Grade, na.rm = TRUE),
  SD_Final_Grade = sd(Final_Grade, na.rm = TRUE)
)
print(descriptive_summary)

correlation_results <- students %>% summarise(
  Attendance_vs_Final_Grade = cor(Attendance_Percentage, Final_Grade, use = "complete.obs"),
  Quiz_vs_Final_Grade = cor(Quiz_Average, Final_Grade, use = "complete.obs"),
  Assignment_vs_Final_Grade = cor(Assignment_Score, Final_Grade, use = "complete.obs")
)
print(correlation_results)

students <- students %>% mutate(Attendance_Group = case_when(
  Attendance_Percentage < 75 ~ "Below 75%",
  Attendance_Percentage < 85 ~ "75% to <85%",
  TRUE ~ "85% and above"
))
group_summary <- students %>% group_by(Attendance_Group) %>% summarise(
  Students = n(), Average_Final_Grade = mean(Final_Grade, na.rm = TRUE), .groups = "drop"
)
print(group_summary)

p1 <- ggplot(students, aes(Attendance_Percentage)) +
  geom_histogram(binwidth = 5, fill = "#00A6A6", color = "white") +
  labs(title = "Distribution of Student Attendance", x = "Attendance (%)", y = "Students") +
  theme_minimal(base_size = 13)
print(p1); ggsave("attendance_distribution.png", p1, width = 8, height = 5, dpi = 150)

p2 <- ggplot(students, aes(Attendance_Percentage, Final_Grade)) +
  geom_point(color = "#FF6B6B", size = 3, alpha = .85) +
  geom_smooth(method = "lm", se = TRUE, color = "#5B4BDB", linewidth = 1) +
  labs(title = "Attendance and Final Grade", subtitle = "Simulated data; association is not causation",
       x = "Attendance (%)", y = "Final grade") + theme_minimal(base_size = 13)
print(p2); ggsave("attendance_vs_final_grade.png", p2, width = 8, height = 5, dpi = 150)

p3 <- ggplot(group_summary, aes(Attendance_Group, Average_Final_Grade, fill = Attendance_Group)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#FFB703", "#00A6A6", "#5B4BDB")) +
  labs(title = "Average Final Grade by Attendance Group", x = "Attendance group",
       y = "Average final grade") + theme_minimal(base_size = 13)
print(p3); ggsave("grade_by_attendance_group.png", p3, width = 8, height = 5, dpi = 150)

write_csv(descriptive_summary, "descriptive_summary.csv")
write_csv(correlation_results, "correlation_results.csv")
write_csv(group_summary, "attendance_group_summary.csv")
cat("Analysis complete. All records are simulated.\\n")
