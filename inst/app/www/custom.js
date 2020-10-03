$(function() {
  $("#var_list").on("click", "button", function() {
    $("#var_list button").removeClass("btn-primary");
    $(this).addClass("btn-primary");
    Shiny.onInputChange("var_selected", $(this).text());
  });

  $("#author_reorder").click(function() {
    ord = $("select.author_order").map(function(){
      return this.value;
    }).get().join(",");
    Shiny.onInputChange("author_order", ord);
  });

  $(".section_list").on("click", "a.section_edit", function() {
    Shiny.onInputChange($(this).attr("section") + "_edit",
                        $(this).attr("section_idx"));
  }).on("click", "a.section_delete", function() {
    Shiny.onInputChange($(this).attr("section") + "_delete",
                        $(this).attr("section_idx"));
  });


  $("#author_list").on("click", "a.author_edit", function() {
    Shiny.onInputChange("author_edit", $(this).attr("data"));
  });
  $("#author_list").on("click", "a.author_delete", function() {
    Shiny.onInputChange("author_delete", $(this).attr("data"));
  });


});
