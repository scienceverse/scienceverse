$(function() {
  $("#var_list").on("click", "button", function() {
    $("#var_list button").removeClass("btn-primary");
    $(this).addClass("btn-primary");
    // set to null first to register same item click
    Shiny.setInputValue("var_selected", null);
    Shiny.setInputValue("var_selected", $(this).text());
  });

  $("#aut_reorder").click(function() {
    ord = $("select.aut_order").map(function(){
      return this.value;
    }).get().join(",");
    Shiny.setInputValue("aut_order", null);
    Shiny.setInputValue("aut_order", ord);
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

  $("#crit_table").on("click", "button.delete", function() {
    idx = $(this).attr("idx");
    Shiny.setInputValue("crit_delete", null);
    Shiny.setInputValue("crit_delete", idx);
  });

  closeBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (!box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

  openBox = function(boxid) {
    var box = $('#' + boxid).closest('.box');
    if (box.hasClass('collapsed-box')) {
      box.find('[data-widget=collapse]').click();
    }
  };

});
