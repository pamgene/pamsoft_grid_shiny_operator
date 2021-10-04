library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

shinyUI(
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    sidebarLayout(
      
      sidebarPanel(
                   fluidRow( column(12, uiOutput("imageusedpanel")),
                             column(12, uiOutput("imagepanel"))   ),
                   
                   fluidRow( column(12, DT::dataTableOutput("images"))  )
                   ),
      mainPanel(
        fluidRow( column(12,
            plotOutput("selectedImage", inline=FALSE, click = "plot_click", height = "600px")  %>% withSpinner(color="#0dc5c1", hide.ui=FALSE),
            tags$script(HTML("
                            $('#selectedImage').mouseup(function(e){
                            
                                bounds=this.getBoundingClientRect();
                                var left=bounds.left;
                                var top=bounds.top;
                                var x = e.pageX - left;
                                var y = e.pageY - top;

                                var relX=x;
                                var relY=y;
                            

                              Shiny.setInputValue('x', relX);
                              Shiny.setInputValue('y', relY);
                              Shiny.setInputValue('mouseup', 1);
                              Shiny.setInputValue('mousedown', 0);
                            }).mousedown(function(e){
                             bounds=this.getBoundingClientRect();
                                var left=bounds.left;
                                var top=bounds.top;
                                var x = e.pageX - left;
                                var y = e.pageY - top;

                                var relX=x;
                                var relY=y;
                              Shiny.setInputValue('xDwn', relX);
                              Shiny.setInputValue('yDwn', relY);
                              Shiny.setInputValue('mousedown', 1);
                              Shiny.setInputValue('mouseup', 0);
                            });
                                         ")) )),
        fluidRow( column(3,
           sliderInput("brightness", "Brightness",
                        min = -0.5, max = 0.5, ticks=FALSE, 
                        value = 0, step = 0.05) ),
            
            
            column(3, sliderInput("contrast", "Contrast",
                        min = 0.2, max = 4,ticks=FALSE, 
                        value = 1, step = 0.05))),
            
          fluidRow(  column(2, 
                            tags$div(title="Apply grid position changes to other images which used the same grid", 
                                     disabled(actionButton("applyBtn", label = "Apply Grid"  )) )
                            ),
                     column(2, 
                            tags$div(title="Save all grid position changes", 
                                     actionButton("saveBtn", label = "Save Changes"  ))
                            ),  
            textOutput("mouseup"))
      )          
      
            
  
)

)
 
)