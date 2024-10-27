/*
File Name: jd.js
Original Creator: ktto
Contact Information: ICE-support@niehs.nih.gov
Date Created: 2023-08-05
License: MIT
Description: js for DASS App
*/

// ## GENERAL
function resetHiddenTab(tabName) {
  $(".tab-pane[data-value='"+tabName+"']").find(".hiddenBlock").hide();
}

// ## DATA TABLE
// Makes tables show NA instead of blank
function showNA() {
  for (let i = 0; i < data.length; i++) {
    if (data[i]===null) {
      $(this.api().cell(row, i).node()).css({"color": "red"})
    }
  }
}

function addFilterLabel(table_name) {
  let head_names = $(`#${table_name} .dataTables_scrollHead thead tr th`);
  $(`#${table_name} .dataTables_scrollHead thead tr td`). each(function(idx, tde) {
    $(tde).find("input[type = 'search']").attr("aria-label", `Filter for column ${head_names[idx].innerHTML}`);
  })
}

function styleWarnRow(row, data) {
  if(data[data.length-1]===true) {
    row.setAttribute("class", "warningText");
  }
}

// ## SELECT UI
function updateSelectUI(ke31=true) {
  if(ke31) {
    $('#ke1_select_ui summary').html('Key Event 1 - DPRA');
    $("#ke1_call_select h2 span").html("DPRA Call");
    $("#ke1_dep_select h2 span").html("DPRA Mean Depletion Value");

    $('#ke3_select_ui summary').html('Key Event 3 - h-CLAT');
    $("#ke3_quant_ui h2 span").html("h-CLAT MIT");
  } else {
    $('#ke1_select_ui summary').html('Key Event 1');
    $("#ke1_call_select h2 span").html("KE1 Call");
    $("#ke1_dep_select h2 span").html("KE1 Mean Depletion Value");

    $('#ke3_select_ui summary').html('Key Event 3');
    $("#ke3_quant_ui h2 span").html("KE3 Quantitative Endpoint");
  }
}

function addLabel(mutationList, observer) {
  console.log("oy");
    $("#ref_col_block .selectize-control").prepend("<label class='control-label' for = 'perf_ref_col-selectized'>Select Reference Columns</label>")
}

// ## STARTUP
$(document).on('shiny:connected', function() {
  $(".hiddenBlock").hide();
  $(".tabbable .tab-content").attr("aria-live", "assertive");
  
  $("input[name='selected_da'][value='da_2o3']").attr("aria-labelledby", "2o3_radio_label");
  $("input[name='selected_da'][value='da_its']").attr("aria-labelledby", "its_radio_label");
  $("input[name='selected_da'][value='da_ke31']").attr("aria-labelledby", "ke31_radio_label");
  $("#do_da_2o3_bl").attr("aria-labelledby", "2o3_bl_cb_label");
  $(".blr_caro_nav label").addClass("sr-only");
  $("#perf_ref_col-label").attr("aria-hidden", "true");
  $("#perf_ref_col-label").attr("for", "");
  
  /*
    Want to use selectize for multiselect input, but need to add label. Add when user navigates to compare tab. 
    Can't add to input element at shiny:connected b/c label gets removed when values are updated after result generation
  */
  $("#goToCompare").on("click", () => {
    $("#perf_ref_col-selectized").attr("aria-label", "Select one or more reference columns");
  });
  $("#step_set li a[data-value='Compare']").on("focus", () => {$("#perf_ref_col-selectized").attr("aria-label", "Select one or more reference columns");})
});