$(function() {
  $("#var_list").on("click", "button", function() {
    $("#var_list button").removeClass("btn-primary");
    $(this).addClass("btn-primary");
    // set to null first to register same item click
    Shiny.onInputChange("var_selected", null);
    Shiny.onInputChange("var_selected", $(this).text());
  });

  $("#author_reorder").click(function() {
    ord = $("select.author_order").map(function(){
      return this.value;
    }).get().join(",");
    Shiny.onInputChange("author_order", null);
    Shiny.onInputChange("author_order", ord);
  });

  $(".section_list").on("click", "a.section_edit", function() {
    v = $(this).attr("section") + "_edit";
    Shiny.onInputChange(v, null);
    Shiny.onInputChange(v, $(this).attr("section_idx"));
  }).on("click", "a.section_delete", function() {
    v = $(this).attr("section") + "_delete";
    Shiny.onInputChange(v, null);
    Shiny.onInputChange(v, $(this).attr("section_idx"));
  });


  $("#author_list").on("click", "a.author_edit", function() {
    Shiny.onInputChange("author_edit", null);
    Shiny.onInputChange("author_edit", $(this).attr("data"));
  });
  $("#author_list").on("click", "a.author_delete", function() {
    Shiny.onInputChange("author_delete", null);
    Shiny.onInputChange("author_delete", $(this).attr("data"));
  });


});
