options("scipen" = 10,
        "digits" = 4,
        "DT.autoHideNavigation" = TRUE)

# datatable constants ----
dt_options <- list(
  info = FALSE,
  lengthChange = FALSE,
  paging = FALSE,
  ordering = FALSE,
  searching = FALSE,
  pageLength = 500,
  keys = TRUE
)

# javascript for datatables ----
table_tab_js <- c(
  "table.on('key', function(e, datatable, key, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  if(key == 13 && targetName == 'body'){",
  "    $(cell.node()).trigger('dblclick.dt').find('input').select();",
  "  }",
  "});",
  "table.on('keydown', function(e){",
  "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
  "    $(e.target).trigger('blur');",
  "  }",
  "});",
  "table.on('key-focus', function(e, datatable, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  var type = originalEvent.type;",
  "  if(type == 'keydown' && targetName == 'input'){",
  "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
  "      $(cell.node()).trigger('dblclick.dt').find('input').select();",
  "    }",
  "  }",
  "});"
)
