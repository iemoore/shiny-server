

$( document ).ready(function() {
  $("a#ID1").on("click", function (event) {
    
    Shiny.onInputChange("id1_click",Math.random());
    
  });
});


$(document).on("click", ".ID1", function(e) {
  //e.preventDefault();
  Shiny.onInputChange("id1_click2",Math.random());
});


$(document).ready(function() {
 
  document.getElementById("mydiva1").onclick = function() {
    var number = Math.random();
    Shiny.onInputChange("id1_click2", number);
  };
 
});