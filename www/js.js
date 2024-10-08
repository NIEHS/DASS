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
/*
function showNA(row, data) {
  for (var i=0; i<data.length; i++) {
    if(data[i]===null){
      $('td:eq('+i+')', row).html('NA')
        .css({'color': 'red'});
    }
  }
}
*/

function styleWarnRow(row, data) {
  if(data[data.length-1]===true) {
    row.setAttribute("class", "warningText");
  }
}

// ## SELECT UI
function updateSelectUI(ke31=true) {
  if(ke31) {
    $('#ke1_select_ui summary').html('Key Event 1 - DPRA');
    $("#ke1_call_select h2").html("DPRA Call");
    $("#ke1_dep_select h2").html("DPRA Mean Depletion Value");

    $('#ke3_select_ui summary').html('Key Event 3 - h-CLAT');
    $("#ke3_quant_ui h2").html("h-CLAT MIT");
    $("label[for='ke3_val_col']").html("Minimum Induction Threshold Column")
  } else {
    $('#ke1_select_ui summary').html('Key Event 1');
    $("#ke1_call_select h2").html("KE1 Call");
    $("#ke1_dep_select h2").html("KE1 Mean Depletion Value");

    $('#ke3_select_ui summary').html('Key Event 3');
    $("#ke3_quant_ui h2").html("KE3 Quantitative Endpoint");
    $("label[for='ke3_val_col']").html("Quantitative Endpoint Column")
  }
}

// ## STARTUP
$(document).on('shiny:connected', function() {
  $(".hiddenBlock").hide();
  
  $("input[name='selected_da'][value='da_2o3']").attr("aria-labelledby", "2o3_radio_label");
  $("input[name='selected_da'][value='da_its']").attr("aria-labelledby", "its_radio_label");
  $("input[name='selected_da'][value='da_ke31']").attr("aria-labelledby", "ke31_radio_label");
  $("#do_da_2o3_bl").attr("aria-labelledby", "2o3_bl_cb_label");
  
});

/*

function toggleAssaySelect(input_name, assay_value, disable = true) {
  const assay_input = $(`input[name = '${input_name}'][value='${assay_value}']`);
  console.log(assay_input);
  if (disable) {
    assay_input.addClass("disabled");
    assay_input.attr("disabled", true);
    assay_input.attr("aria-disabled", true);
    assay_input.parent().addClass("disabled");
    assay_input.attr("aria-disabled", true);
  } else {
    assay_input.removeClass("disabled");
    assay_input.attr("disabled", false);
    assay_input.attr("aria-disabled", false);
    assay_input.parent().removeClass("disabled");
    assay_input.attr("aria-disabled", false);
  }
}





/*
$(document).on('shiny:connected', function() {
  document.querySelector
  let predCol = document.querySelector("#perfPredCol");
  let refCol = document.querySelector("#perfRefRes");
  predCol.setAttribute("title", "Select prediction columns");
  refCol.setAttribute("title", "Select reference columns");
  
});
*/