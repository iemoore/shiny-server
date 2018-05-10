


$(document).on("click", ".go-map-save-ve", function(e) {
  e.preventDefault();
  $el = $(this);
  var row = $el.data("row");
  Shiny.onInputChange("verbSave", {
    row: row,
    nonce: Math.random()
  });
});


$(document).on("click", ".go-map-rem-ver", function(e) {
  e.preventDefault();
  $el = $(this);
  var row = $el.data("row");
  Shiny.onInputChange("verbSaveRemoved", {
    row: row,
    nonce: Math.random()
  });
});



$(document).on("click", ".go-map-rem-ve", function(e) {
  e.preventDefault();
  $el = $(this);
  var row = $el.data("row");
  Shiny.onInputChange("verbRem", {
    row: row,
    nonce: Math.random()
  });
});

$(document).on("click", ".go-map-rem-ves", function(e) {
  e.preventDefault();
  $el = $(this);
  var row = $el.data("row");
  Shiny.onInputChange("verbRemSaved", {
    row: row,
    nonce: Math.random()
  });
});



/*########################################################################*/

var ctrlPressed = false;
$(window).keydown(function(evt) {
  if (evt.which == 17) { // ctrl
    ctrlPressed = true;
  }
}).keyup(function(evt) {
  if (evt.which == 17) { // ctrl
    ctrlPressed = false;
  }
});


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


/*########################################################################*/


$(document).keyup(function(e) {
  if(e.which == 16) {
    /*alert('You pressed enter!');*/
      /*$( "#toggleAdvanced" ).click();*/
      /*$( "#tabBut" ).click();*/
    Shiny.onInputChange("moTrigKey", Math.random());
    console.log( "'Shift' pressed, #moTrog called." );
  }
});




$( document ).ready(function() {
  $("#tabBut").on("click", function (event) {
    
    Shiny.onInputChange("tab_click",Math.random());
    
  });
});



$(document).on("click", ".go-map-like", function(e) {
  //e.preventDefault();
  Shiny.onInputChange("icon_click_like",Math.random());
});

$(document).on("click", ".go-map-dislike", function(e) {
  //e.preventDefault();
  Shiny.onInputChange("icon_click_dislike",Math.random());
});

$(document).on("click", ".go-map-save", function(e) {
  //e.preventDefault();
  Shiny.onInputChange("icon_click_save",Math.random());
});




















