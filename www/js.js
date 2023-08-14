/*
File Name: jd.js
Original Creator: ktto
Contact Information: ICE-support@niehs.nih.gov
Date Created: 2023-08-05
License: MIT
Description: js for DASS App
*/

function showScroll(showID, htmlTag, attrName, attrValue) {
  $('#' + showID).show(() => {
    $($.fn.dataTable.tables(true)).DataTable()
      .columns.adjust();
    document.querySelector(`${htmlTag}[${attrName}='${attrValue}']`)
      .scrollIntoView({behavior: 'smooth'}, true);
  });
}

function showNA(row, data) {
  for (var i=0; i<data.length; i++) {
    if(data[i]===null){
      $('td:eq('+i+')', row).html('NA')
        .css({'color': 'rgb(89,89,89)'});
    }
  }
}

function togglePdepl() {
  const isChecked = $("input[value='pdepl']").is(":checked");
  if (isChecked) {
    dpraDep.show();
  } else {
    dpraDep.hide();
  }
}

const dpraDep = $("#dpraDepSelect");
function addDPRAListener() {
  $("#dpra_call_choice").on("change", () => togglePdepl());
}

function rmDPRAListener() {
  $("#dpra_call_choice").off("change", () => togglePdepl());
}

function resetHidden(hideAll=true) {
  if (hideAll) {
    $(".hiddenBlock").hide();
  } else {
    $(".hiddenBlock:not(#user_data_block_confirm)").hide();
  }
}

$(document).on('shiny:connected', resetHidden);