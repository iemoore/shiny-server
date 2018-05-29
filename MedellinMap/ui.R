


# Simple header -----------------------------------------------------------

dm <- dropdownMenu(type="messages")
mm <- dropdownMenu(type="notifications")
tm <- dropdownMenu(type="tasks")

# header <- dashboardHeader(title="Medellin", dm, mm, tm)
header <- dashboardHeader(title="Medellin")

# No sidebar --------------------------------------------------------------

sm <- sidebarMenu(id = "tabs",
  
  searchInput(
    inputId = "id", 
    label = "Enter your search :", 
    placeholder = "Place, Food, ect.", 
    btnSearch = icon("search"), 
    btnReset = icon("remove"),
    width = "100%"
  ),
  
  menuItem(
    text="Map",
    tabName="main",
    icon=icon("database")),
  
  menuItem(
    text="Settings",
    tabName="tab1",
    icon=icon("forward"))
  
)

sidebar <- dashboardSidebar(sm)

# Compose dashboard body --------------------------------------------------


body <- dashboardBody(
  
  useShinyjs(),
  includeScript("solid/js/key.js"),
  includeScript("solid/js/keyMessage.js"),
  includeScript("solid/js/oldGPS.js"),
  
  HTML(paste0("<meta content='width=device-width, initial-scale=1.0,",
              " maximum-scale=1.0, user-scalable=0' name='viewport' />")),
  
  # includeScript("solid/js/geoLoc.js"),
  # extendShinyjs(text = jsCode),
  # includeCSS('solid/appStyles.css'),
  # includeScript("solid/js/watchpos.js"),
  
  
  useShinyalert(),
  
  tabItems(
    tabItem(
      tabName="main",
      
      bootstrapPage(
        
        # wellPanel(
          # leafletOutput("map", width = "100%")
          # )
        # leafletOutput("map")
        
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
        leafletOutput("map", width = "100%")#,
        # absolutePanel(top=20, left=70, textInput("target_zone", "" , "Ex: Bamako"))
        # actionButton("geoloc", "Localize me", class="btn btn-primary",
        #              onClick="shinyjs.geoloc()")
        
      )
    ),
    
    tabItem(
      tabName="tab1",
      
      fluidPage(
        
            tags$script('
$(document).ready(function () {

function getLocation(callback){
var options = {
enableHighAccuracy: true,
timeout: 5000,
maximumAge: 0
};

navigator.geolocation.getCurrentPosition(onSuccess, onError);

function onError (err) {
Shiny.onInputChange("geolocation", false);
}

function onSuccess (position) {
setTimeout(function () {
var coords = position.coords;
var timestamp = new Date();

console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
Shiny.onInputChange("geolocation", true);
Shiny.onInputChange("lat", coords.latitude);
Shiny.onInputChange("long", coords.longitude);
Shiny.onInputChange("accuracy", coords.accuracy);
Shiny.onInputChange("time", timestamp)

console.log(timestamp);

if (callback) {
callback();
}
}, 1100)
}
}

var TIMEOUT = 10000; //SPECIFY
var started = false;
function getLocationRepeat(){
//first time only - no delay needed
if (!started) {
started = true;
getLocation(getLocationRepeat);
return;
}

setTimeout(function () {
getLocation(getLocationRepeat);
}, TIMEOUT);

};

getLocationRepeat();

});
'),

        # Show a plot of the generated distribution
        fluidRow(column(width = 5,
                        verbatimTextOutput("lat"),
                        verbatimTextOutput("long"),
                        verbatimTextOutput("geolocation"),
                        verbatimTextOutput("accuracy"),
                        verbatimTextOutput("time"))
      
        )
      )
    )  
    
  )
)


shinyUI( 

  dashboardPage(

      header = header, 
      sidebar = sidebar,
      body = body,
      skin="black"
      
  )
)  












