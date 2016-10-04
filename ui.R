library(shinydashboard)

####Modal
my_modal <- function(id, title, ...) {
  mo <- tags$div(class = "modal fade", id = id,
                 tags$div(class = "modal-dialog modal-md",
                          tags$div(class = "modal-content",
                                   tags$div(class = "modal-header",
                                            tags$button(type="button",class="close",
                                                        `data-dismiss`="modal",HTML("&times;")),
                                            tags$h4(title)),
                                   tags$div(class = "modal-body", ...),
                                   tags$div(class = "modal-footer",
                                            tags$button(type="button", class="btn btn-default",
                                                        `data-dismiss`="modal","Close")))))
}

#### Title ####
header <- dashboardHeader(
  titleWidth = 300, 
  title = tagList(shiny::icon("book", lib = "glyphicon"), 
                  "Book List Recommendation"))

#### Sidebar ####
sidebar <- dashboardSidebar(
  width = 300,
  
  # Logo
  sidebarUserPanel("Miaozhi Yu", image = "booklogo.png"),
  
  # Tabs
  sidebarMenu(
    menuItem("History", tabName = "tab_history",
             icon = icon("map")),
    menuItem("Recommendation", tabName = "tab_recommendation", 
             icon = icon("book")),
    menuItem("Data", tabName = "tab_data", 
             icon = icon("database")),
    # menuItem("Review", tabName = "tab_review", 
    #          icon = icon("star")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
   menuItem('About', tabName = 'tab_about',
             icon = icon('info-circle'))
   
  )
)

#### Tabs - About ####
tab_about <- tabItem(
  tabName = 'tab_about',
  fluidRow(
    tabBox(
      width = 12,
      title = tagList(shiny::icon("info-circle"), 'Info'),
      id = 'tabbox1',
      tabPanel(
        "About Goodreads",
        fluidRow(
          column(
            width = 5, align = 'center',
            tags$img(src = 'download.png', width = "356px", height = "300px")),
          column(
            width = 7,
            tags$h3('Goodreads is an Amazon company and "social cataloging" website founded in December 2006 and launched in 
                    January 2007 by Otis Chandler, II, a software engineer and entrepreneur, and Elizabeth Chandler'),
            tags$h3('The website allows individuals to freely search Goodreads extensive user-populated database of 
                    books, annotations, and reviews. Users can sign up and register books to generate library catalogs and reading lists.'),
            tags$h3('On July 23, 2013, it was announced on their website that the user base had grown to 20 million members, 
                    having doubled in close to 11 months.'),
            tags$a(href = 'https://www.goodreads.com/',
                   'Learn more about Good Reads',
                   class="btn btn-primary"))
            )), # End of Tab Panel 1
      tabPanel(
        "About Me",
        fluidRow(
          box(
            width = 12, title = ,
            column(
              width = 2,
              tags$img(
                src = 'Miaozhi_Yu.jpg',
                width = "100px", height = "100px")),
            column(
              width = 10,
              #tags$p(aboutme),
              br(),
              br(),
              tags$p(tags$a(icon('github-alt'), target = '_blank',
                            'My Github',
                            href = 'https://github.com/MiaozhiYu',
                            class="btn btn-primary")),
              tags$p(
                tags$a(icon('linkedin'), target = '_blank',
                       'My LinkedIn',
                       href = 'https://www.linkedin.com/in/miaozhi-yu-87429739',
                       class="btn btn-primary"))
            )))) # End of Tab Panel 2
  ))) # End of Tab Box

### Tabs - Map ####
tab_history <- tabItem(
  tabName = "tab_history",
  # Row 1, Region Selection
  fluidRow(
    column(6, align = 'justify',
           box(title = 'About This Tab', width = 15,
               status = 'info', solidHeader = F,
               'This dashboard shows the how literature hot spots change over time.'
           )
    ),
    column(6,
             sliderInput("Publish_Year", "Year:",
                         min = 1600, max = 2000, value =1600 ,step=100,animate = T)
    )

    ),

  # Row 2, Map motion
  fluidRow(
    box(
      title = 'World Map', status = 'primary', solidHeader = T,
      height = 800, width = 12,
      htmlOutput("map_motion")
    ) # End of Map Box
  )

) # End of history Tab

tab_recommendation <- tabItem(
  tabName = "tab_recommendation", 
  # Row 1, Region Selection
  fluidRow(
    
    column(4,
           selectInput(
             inputId = "type", 
             label = h3("Type of Literature"),
             choices = levels(data_rating$type),
             selected = levels(data_rating$type), multiple = T
           )
    ),
    
    column(4,
           selectInput(
             inputId = "period", 
             label = h3("Time Period"),
             choices = levels(data_rating$period),
             selected = levels(data_rating$period), multiple = T
           )
      
    ),
    
    column(4,
           selectInput(
             inputId = "country", 
             label = h3("Country"),
             choices = levels(as.factor(data_rating$country)),
             selected = levels(as.factor(data_rating$country)),multiple = T
           )
           
    )
    
    
  ), # end of selection bar
  
  #row2, recommendation start
  fluidRow(
    column(12,
           h3('Recommendations:')
    )
  ),
 
   #row3, recommendation
  
    fluidRow(
      column(3,offset = 1,
             #conditionalPanel("input.n >= 50",
                             htmlOutput("rec_pic1",style="border:1px solid #ccc;",
                                        `data-toggle`="modal",
                                        `data-target`="#mymodal1"),
             my_modal('mymodal1','Word Cloud',plotOutput("plot_1"))
      ),
      column(3,
             htmlOutput('rec_pic2',style="border:1px solid #ccc;",
                        `data-toggle`="modal",
                        `data-target`="#mymodal2"),
             my_modal('mymodal2','Word Cloud',plotOutput("plot_2"))
             
      ),
      column(3,
             htmlOutput('rec_pic3',style="border:1px solid #ccc;",
                        `data-toggle`="modal",
                        `data-target`="#mymodal3"),
             my_modal('mymodal3','Word Cloud',plotOutput("plot_3"))
      )
    )
  # fluidRow(
  #   column(3,
  #          bsModal(
  #            "modalWordCloud1",
  #            "Word Cloud",
  #            "tabWC1",
  #            size = "large",
  #            plotOutput("wordcloud1")
  #          )
  #     )
  # )
  
   
)# End of Recommendation Tab

#begin of review tab
# tab_review = tabItem(tabName = "tab_review",
#                    fluidRow(
#                      column(4,
#                             selectInput(
#                               inputId = "selection", 
#                               label = h3("Choose a book: "),
#                               choices = levels(data_rating$title))
#                      ),
#                      column(4,
#                             actionButton("update", "Get review")
#                        ),
#                      fluidRow(
#                        plotOutput("plot")
#                      )
#                      
#     )
# )#end of review tab

#begin of data tab
tab_data = tabItem(tabName = "tab_data",
        fluidRow(box(dataTableOutput('tbl'), width = 12))
)

body <- dashboardBody(
  ### Tabs
  tabItems(
    tab_history,  # history tab
    tab_recommendation,
    #tab_review,
    tab_data,
    tab_about  # About tab
  ) # End of Tabs
) # End of Body

#### Main Frame ####
dashboardPage(
  # Properties
  skin = "purple",
  
  # Main Frame
  header,   
  sidebar,
  body
)