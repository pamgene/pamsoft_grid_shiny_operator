library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders)

library(shinybusy)

js <- "
$(document).on('shiny:sessioninitialized', function(event) {
  Shiny.setInputValue('pageLoaded', Math.random());
});

$(document).on('shiny:value', function(evt) {

  if(evt.name === 'selectedImage') {
    setTimeout(function(){
      grid = new Array();
      
      for (let i = 0; i < globalThis.x.length; i++) {
        var spot = new Object();
        spot.x = globalThis.x[i]
        spot.y = globalThis.y[i]
        spot.radius = globalThis.radius[i]
        spot.type = globalThis.type[i]
        grid.push(spot)
      }
      
      var startX = 0;
      var startY = 0;
      
      
      customMessageHandler(grid)
    }, 0);
  }
});
"




shinyUI(
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    tags$script(HTML(js)),
    tags$head(HTML("<script type='text/javascript'>
                  
                  function componentToHex(c) {
                      var hex = c.toString(16);
                      return hex.length == 1 ? '0' + hex : hex;
                    }
                    
                    function rgbToHex(r, g, b) {
                      return '#' + componentToHex(r) + componentToHex(g) + componentToHex(b);
                    }
                  
                  var customMessageHandler = function(list){
                  var Arc = function(x, y, radius, radians, type) {
                      this.x = x;
                      this.y = y;
                      this.radius = radius;
                      this.radians = radians;
                      this.isDragging = false;
                      this.type = type;
              
              
                      this.render = function(ctx) {
                          ctx.save();
              
                          ctx.beginPath();
                          ctx.arc(this.x, this.y, this.radius, 0, this.radians, false);
                          ctx.linewidth = 2;
                          typeBin = (type >>> 0).toString(2)
                          var red = 0;
                          var blue = 0;
                          var green = 0;
                          
                          if (type == 0){
                            green = 255;
                            ctx.linewidth = 2;
                          }

                          if(typeBin.charAt(0) == 1){ //BAD
                            red = 255;
                            green = 50;
                            blue = 50;
                            
                          }
                          if(typeBin.charAt(1) == 1){ //EMPTY
                            ctx.setLineDash([5,5])
                          }
                          if(typeBin.charAt(2) == 1){ //OUTLIER
                            blue = 255;
                          }
                          if(typeBin.charAt(3) == 1){ //REPLACED
                            ctx.setLineDash([5,2])
                            
                          }
                          ctx.strokeStyle = rgbToHex(red, green, blue);
                          

                          ctx.stroke();
              
                          ctx.restore();
                      }
                  }
              
                  var MouseTouchTracker = function(canvas, callback){
                      this.isDragging = false;
                      this.isRotating = false;

                      this.list = new Array();
              
                      function processEvent(evt) {
                      
                          var rect = canvas.getBoundingClientRect();
                          var offsetTop = rect.top;
                          var offsetLeft = rect.left;
              
                          if (evt.touches) {
                              return {
                                  x: evt.touches[0].clientX - offsetLeft,
                                  y: evt.touches[0].clientY - offsetTop
                              }
                          } else {
                              return {
                                  x: evt.clientX - offsetLeft,
                                  y: evt.clientY - offsetTop
                              }
                          }
                      }
              
                      function onDown(evt) {
                          evt.preventDefault();
                          var coords = processEvent(evt);
                          
                          callback('down', coords.x, coords.y, evt.shiftKey);
                      }
              
                      function onUp(evt) {
                          evt.preventDefault();
                          callback('up');
                      }
              
                      function onMove(evt) {
                          evt.preventDefault();
                          var coords = processEvent(evt);
                          callback('move', coords.x, coords.y);
                      }
              
                      canvas.ontouchmove = onMove;
                      canvas.onmousemove = onMove;
              
                      canvas.ontouchstart = onDown;
                      canvas.onmousedown = onDown;
                      canvas.ontouchend = onUp;
                      canvas.onmouseup = onUp;
                  }
              
                  function isHit(shape, x, y) {
                      if (shape.constructor.name === 'Arc') {
                          var dx = shape.x - x;
                          var dy = shape.y - y;
                          if (dx * dx + dy * dy < shape.radius * shape.radius) {
                              return true
                          }
                      } else {
                          if (x > shape.x - shape.width * 0.5 && y > shape.y - shape.height * 0.5 && x < shape.x + shape.width - shape.width * 0.5 && y < shape.y + shape.height - shape.height * 0.5) {
                              return true;
                          }
                      }
              
                      return false;
                  }
              
                  var gridImage = document.getElementById('gridImage');
                  var canvas = document.getElementById('gridCanvas');
              
                  canvas.width = 697
                  canvas.height = 520
              
                  var ctx = canvas.getContext('2d');
                  

          
              
                  globalThis.grid = list.map(spot => new Arc(spot.x, spot.y, spot.radius, Math.PI * 2, spot.type));
                  globalThis.mtt = new MouseTouchTracker(canvas,
                      function(evtType, x, y, isRotate) {
                          ctx.clearRect(0, 0, canvas.width, canvas.height);
                          

                          switch(evtType) {
              
                              case 'down':
                                  startX = x;
                                  startY = y;
              
                                  found = globalThis.grid.find(element => isHit(element, x, y));
              
                                  if (found){
                                      found.isDragging = true;
                                  } else {
                                      this.isDragging = true;
                                  }
                                  
                                  if(isRotate){
                                    this.isRotating = true;
                                  }else{
                                    this.isRotating = false;
                                  }
                                  
                                  break;
              
                              case 'up':

                                  globalThis.grid.forEach(el => el.isDragging = false);
                                  this.isDragging = false;
                                  this.isRotating = false;

                                  Shiny.setInputValue('gridOverlay', globalThis.grid);
                                  break;
              
                              case 'move':
                                  var dx = x - startX;
                                  var dy = y - startY;
                                  startX = x;
                                  startY = y;
              

                                  found = globalThis.grid.find(element =>  element.isDragging);
              
                                  if (found){
              
                                      found.x += dx;
                                      found.y += dy;
                                  } else {
                                  
                                      if (this.isDragging){
                                          if(this.isRotating){
                                            var N = globalThis.grid.length;
                                            var cx = 0;
                                            var cy = 0;
                                            
                                            globalThis.grid.forEach(el => {
                                                cx += el.x;
                                                cy += el.y;
                                            })
                                            cx /= N;
                                            cy /= N;
                                            
                                            var radians = (Math.PI / 180) * 0.2;
                                            
                                            if(dy > 0 && startX > cx){
                                              radians *= -1;
                                            }
                                            
                                            if(dy < 0 && startX < cx){
                                              radians *= -1;
                                            }
                   
                                            var cos = Math.cos(radians);
                                            var sin = Math.sin(radians);
                                            
                                            
                                            
                                            globalThis.grid.forEach(el => {
                                                el.x = (cos * (el.x - cx)) + (sin * (el.y - cy)) + cx;
                                                el.y = (cos * (el.y - cy)) - (sin * (el.x - cx)) + cy;
                                            })
                                            

                                            
                                          }else{
                                            globalThis.grid.forEach(el => {
                                                el.x += dx;
                                                el.y += dy;
                                            })
                                          }
                                          
                                      }
                                  }
                                  break;
                          }
              
                          ctx.drawImage(gridImage,0,0)
                          globalThis.grid.forEach(spot => spot.render(ctx));
                          
ctx.fillStyle = '#00AA00';
                  
                  if( globalThis.manual == 1){
                  ctx.fillStyle = '#FFFF00';
                  }
                  ctx.fillRect(0,0, 20,20);
              
                      }
                  );
              
                  
                  ctx.drawImage(gridImage,0,0)
                  globalThis.grid.forEach(spot => spot.render(ctx));
                  ctx.fillStyle = '#007700';
                  
                  if( globalThis.manual == 1){
                  ctx.fillStyle = '#FFFF33';
                  }
                  ctx.fillRect(0,0, 20,20);
              }
              
              
              
              
              var startX = 0;
              var startY = 0;
              
              Shiny.addCustomMessageHandler('imageDisplay', function(gridList){
                //Set this before triggering the image loaded event
                globalThis.x      = gridList.x;
                globalThis.y      = gridList.y;
                globalThis.radius = gridList.radius;
                globalThis.type   = gridList.type;

                globalThis.manual = gridList.manual[0];
              });
              

                </script>")),
    tags$script(HTML(
      "var currentRow = 0;
       var isButton = 0;
      
      Shiny.addCustomMessageHandler('init_grid_images',function(unused) {
        Shiny.setInputValue('selectedImageRow', 1);
      });
      

      Shiny.addCustomMessageHandler('select_row',function(rows) {
      var table = $('#images').find('table').DataTable();
      
      table.rows().deselect();
      table.row(rows-1).select();
      
      currentRow = rows-1;
      });
      
      Shiny.addCustomMessageHandler('button_evt',function(isBtn) {
        isButton = isBtn;
      })"
      )),
    
    sidebarLayout(
      
      sidebarPanel(
                   fluidRow( column(12, uiOutput("imageusedpanel")),
                             column(12, uiOutput("imagepanel"))   ),
                   fluidRow(column(2, disabled(actionButton("prevGridBtn", label = "<< Grid"  )) ),
                            column(2, disabled(actionButton("prevImgBtn", label = "< Image"  )) ) ,
                            column(2, actionButton("nextImgBtn", label = "Image >"  ) )  ,
                            column(2, actionButton("nextGridBtn", label = "Grid >>"  ) )
                            ),
                   br(),
                   fluidRow( column(12, DT::dataTableOutput("images"))  ) 
                   ),
      mainPanel(
        
        # 
        fluidRow( column(8, tags$canvas(id="gridCanvas", style="background-color: white") ),
                        style = "height:620px; "),
        
        
        fluidRow( column(4, sliderInput("brightness", "Brightness",
                                        min = -0.5, max = 0.5, ticks=FALSE, 
                                        value = 0, step = 0.01) ),
                  
                  column(4, sliderInput("contrast", "Contrast",
                                        min = 0.2, max = 4,ticks=FALSE, 
                                        value = 1, step = 0.05))
                  ),
        fluidRow(  
                  column(2, 
                         tags$div(title="Save all grid position changes", 
                                  disabled(actionButton("runBtn", label = "Run", width="120px"  )))),
                  column(2, 
                         tags$div(title="Create a new grid for the current image", 
                                  disabled(actionButton("gridBtn", label = "New Grid", width="120px"  ))))
                  ),
        fluidRow( column(8, imageOutput(outputId = "selectedImage")   ,  
                         style = "height:5px; visibility:hidden") ),
        ) # END mainPanel

    )  # END sidebarLayout          
  ) # END fluidPage
) # END shinyUI
 
