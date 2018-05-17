


$(document).on("click", ".go-flash-web", function(e) {
  e.preventDefault();
  $el = $(this);
  var row = $el.data("row");
  Shiny.onInputChange("flashClick_web", {
    row: row,
    nonce: Math.random()
  });
});