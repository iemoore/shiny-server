 
 
 


/*########################################################################*/


$(document).keypress(function(e) {
      if(e.which == 13) {
        if(ctrlPressed) {
          console.log( "Ctr+Enter pressed, #ctrEnter_mes sent" );
          Shiny.onInputChange("ctrEnter_mes", Math.random());
          } else {
          console.log( "Enter pressed, #enter_mes sent" );
          Shiny.onInputChange("enter_mes", Math.random());
          }
    }
});

$.key('ctrl+enter', function() {
    console.log( "Ctr+Enter pressed, #applyFilters_mes sent" );
    Shiny.onInputChange("ctrEnter_mes", Math.random());
});



/*########################################################################*/



$(function() {
  $(document).keypress(function(e) {
    if (e.key == "1" || e.key == "2" || e.key == "3" || e.key == "4" ||
        e.key == "5" || e.key == "6" || e.key == "7" || e.key == "8" ||
        e.key == "9" || e.key == "0") {
      Shiny.onInputChange("numKeys", [e.key, Math.random()]);
    }
  });
});


$(function() {
  $(document).keyup(function(e) {
    if (e.which == 37 || e.which == 38 || e.which == 39 || e.which == 40 ) {
      Shiny.onInputChange("dirKeys", [e.which, Math.random()]);
    }
  });
});


$(function() {
  $(document).keyup(function(e) {
    if (e.which == 32 ) {
      Shiny.onInputChange("shift_key", [e.which, Math.random()]);
    }
  });
});













