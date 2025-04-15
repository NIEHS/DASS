// GENERAL 
//// hide child elements of jq_selector with class hiddenBlock 
function resetHidden(jq_selector) {
  $(`${jq_selector}`).find(".hiddenBlock").hide()
}

//// hide child elements of tabName with class hiddenBlock */
function resetHiddenTab(tabName) {
  $(".tab-pane[data-value='"+tabName+"']").find(".hiddenBlock").hide();
}

function tabFocus(tabName) {
  $(`a.nav-link[data-value='${tabName}']`).focus();
}

function resetFileInputText(input_id) {
  Shiny.setInputValue(`${input_id}`, null);
  $(`#${input_id}-fname`).val("");
  $(`#${input_id}-fname`).attr("placeholder", "No file selected.");
}

// TABLES
//// DT callback, makes table body tabbable
function tabBody(tbl) {
  let dt=tbl.table().body();
  $(dt).attr('tabindex', '0');
}

//// Style flagged rows in review table
function styleWarnRow(row, data) {
  if(data[data.length-1]===true) {
    row.setAttribute("class", "warningText");
  }
}

//// initComplete callback for std results table
function updateDT(table_name, colvis=false) {
  // Add aria label to column filters
  let head_names = $(`#${table_name} .table thead tr th`);
  $(`#${table_name} table thead tr td`). each(function(idx, tde) {
    $(tde).find("input[type = 'search']").attr("aria-label", `Filter for column ${head_names[idx].innerHTML}`);
  });
  
  // Put table in scrollbox
  $(`#${table_name} table`).wrap("<div style = 'overflow:auto; width: 100%;'></div>");
  /* in place of scrollX b/c scrollX causes unexpected scrolling behavior */
  
  if(colvis) {
    // Add title to each column name
    $(`#${table_name} .buttons-collection`).on("click", function(e1) {
      let $cvis_btn=$(e1.currentTarget);
      $cvis_btn
        .siblings(".dropdown-menu")
        .find("a.buttons-collection")
        .on("click", function(e2) {
          $cvis_btn
            .siblings(".dropdown-menu")
            .find("a.buttons-columnVisibility")
            .each(function(idx, btn) {
              $(btn).attr("title", btn.innerText);
            });
        })
    })
  }
}

// ## STARTUP
$(document).on('shiny:sessioninitialized', function() {
  $(".hiddenBlock").hide();
  
  /* Updates placeholder text when file input changes. */
  $("input[type='file'").each(function(i, obj) {
    let input_id = $(obj).attr("id");
    $(obj).change(function(e) {
      fname = $(obj).val().split('\\').pop();
      $(`#${input_id}-fname`).val(fname);
      $(`#${input_id}-fname`).attr("placeholder", fname)
      $(`#${input_id}-fname`).focus();
    })
  })

  $(".blr_caro_nav label").addClass("sr-only");

  /* Enable keyboard nav for file input */
  $("label[for='fpath'] span")
    .on("keyup", function(e) {
      if (e.key === "Enter") {
        $("#fpath").click();
      }
    })

  $(".modal").each(function(i, obj) {
    $(obj)
      .on("shown.bs.modal", function(e) {$(e.target).attr("aria-hidden", false)})
      .on("hidden.bs.modal", function(e) {$(e.target).attr("aria-hidden", true)});
  })
});