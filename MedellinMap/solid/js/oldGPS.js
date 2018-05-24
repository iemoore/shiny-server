

var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});





$( document ).ready(function() {
  $(".leaflet-control-gps .gps-button").on("click", function (event) {
    
    
    console.log("Location clicked"); 
    
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    
    function onError (err) {
      Shiny.onInputChange("geolocation", false);
    }
    
    function onSuccess (position) {
      setTimeout(function () {
        var coords = position.coords;
        console.log(coords.latitude + ", " + coords.longitude);
        Shiny.onInputChange("geolocation", true);
        Shiny.onInputChange("lat1", [coords.latitude,Math.random()]);
        Shiny.onInputChange("long1", [coords.longitude,Math.random()]);
      }, 1100);
    }    
  });
});



