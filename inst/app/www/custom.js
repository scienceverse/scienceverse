$(function() {
  $("#author_reorder").click(function() {
    ord = $("select.author_order").map(function(){
      return this.value;
    }).get().join(",");
    Shiny.onInputChange("author_order", ord);
  });
  $("#author_list").on("click", "a.author_edit", function() {
    Shiny.onInputChange("author_edit", $(this).attr("data"));
  });
  $("#author_list").on("click", "a.author_delete", function() {
    Shiny.onInputChange("author_delete", $(this).attr("data"));
  });
});
