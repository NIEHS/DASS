/*
File Name: jd.js
Original Creator: ktto
Contact Information: ICE-support@niehs.nih.gov
Date Created: 2023-08-05
License: MIT
Description: js for DASS App
*/

function showNA(row, data) {
  for (var i=0; i<data.length; i++) {
    if(data[i]===null){
      $('td:eq('+i+')', row).html('NA')
        .css({'color': 'rgb(0,0,0)'});
    }
  }
}

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

function resetHidden(hideAll=true) {
  if (hideAll) {
    $(".hiddenBlock").hide();
  } else {
    $(".hiddenBlock:not(#data_block)").hide();
  }
}

$(document).on('shiny:connected', function() {
  resetHidden();
  
  $("input[name='selected_da'][value='da_2o3']").attr("aria-labelledby", "2o3_radio_label");
  $("input[name='selected_da'][value='da_its']").attr("aria-labelledby", "its_radio_label");
  $("input[name='selected_da'][value='da_ke31']").attr("aria-labelledby", "ke31_radio_label");
    $("#do_da_2o3_bl").attr("aria-labelledby", "2o3_bl_cb_label");
  
});

/*
$(document).on('shiny:connected', function() {
  document.querySelector
  let predCol = document.querySelector("#perfPredCol");
  let refCol = document.querySelector("#perfRefRes");
  predCol.setAttribute("title", "Select prediction columns");
  refCol.setAttribute("title", "Select reference columns");
  
});
*/