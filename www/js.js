/*
File Name: jd.js
Original Creator: ktto
Contact Information: ICE-support@niehs.nih.gov
Date Created: 2023-08-05
License: MIT
Description: js for DASS App
*/

let data0;
let row0;
let thisapi0;

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

// ## STARTUP
$(document).on('shiny:connected', function() {
  $(".hiddenBlock").hide();
  $(".tabbable .tab-content").attr("aria-live", "assertive");
  
  $("input[name='selected_da'][value='da_2o3']").attr("aria-labelledby", "2o3_radio_label");
  $("input[name='selected_da'][value='da_its']").attr("aria-labelledby", "its_radio_label");
  $("input[name='selected_da'][value='da_ke31']").attr("aria-labelledby", "ke31_radio_label");
  $("#do_da_2o3_bl").attr("aria-labelledby", "2o3_bl_cb_label");
  
});