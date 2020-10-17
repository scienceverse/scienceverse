$(function() {
  $("#var_list").on("click", "button", function() {
    $("#var_list button").removeClass("btn-primary");
    $(this).addClass("btn-primary");
    // set to null first to register same item click
    Shiny.setInputValue("var_selected", null);
    Shiny.setInputValue("var_selected", $(this).text());
  });

  $("#author_reorder").click(function() {
    ord = $("select.author_order").map(function(){
      return this.value;
    }).get().join(",");
    Shiny.setInputValue("author_order", null);
    Shiny.setInputValue("author_order", ord);
  });

  $('#level_list_display').on("change", 'input',
    function() {
      Shiny.setInputValue("level_list_update", null);
      Shiny.setInputValue("level_list_update", 1);
  });

  $(".section_list").on("click", "a.section_edit", function() {
    v = $(this).attr("section") + "_edit";
    Shiny.setInputValue(v, null);
    Shiny.setInputValue(v, $(this).attr("section_idx"));
  }).on("click", "a.section_delete", function() {
    v = $(this).attr("section") + "_delete";
    Shiny.setInputValue(v, null);
    Shiny.setInputValue(v, $(this).attr("section_idx"));
  });


  $("#author_list").on("click", "a.author_edit", function() {
    Shiny.setInputValue("author_edit", null);
    Shiny.setInputValue("author_edit", $(this).attr("data"));
  });
  $("#author_list").on("click", "a.author_delete", function() {
    Shiny.setInputValue("author_delete", null);
    Shiny.setInputValue("author_delete", $(this).attr("data"));
  });

  $("#crit_table").on("click", "button.delete", function() {
    idx = $(this).attr("idx");
    Shiny.setInputValue("crit_delete", null);
    Shiny.setInputValue("crit_delete", idx);
  })

});
