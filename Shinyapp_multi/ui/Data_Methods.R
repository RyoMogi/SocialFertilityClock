mainPanel(
  includeMarkdown("md/Data_Methods.Rmd"),
  tags$iframe(style = "height:400px; width:100%; scrolling = yes", 
              src = "../Graph/multistate_path.pdf"),
  includeMarkdown("md/title_S1.Rmd")
)