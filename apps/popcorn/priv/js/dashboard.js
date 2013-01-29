function maybe(v, d) { if(v == 0 || v) return v; if(d == 0 || d) return d; return "" };

function percent(value) { return (Math.round(value * 100 * 100.0) / 100.0) + "%"; };

function setNodePercents() {
  var total = 0;
  var nodes = {};
  var rows = $('.node-row');
  for(var i = 0; i < rows.length; i++) {
    tr = rows[i];
    var value = parseInt($('#' + tr.id + ' .node-total').text(), 10);
    total += value;
    nodes[tr.id] = value
  }
  for(var i = 0; i < rows.length; i++) {
    tr = rows[i];
    $('#' + tr.id + ' .node-percent .progress .bar').width(percent(nodes[tr.id] / total))
  }
};

function updateAlertRow(table, counter) {
  var rowName = counter.location;
  if($('#' + rowName).length > 0) {
    $('#' + rowName + ' .seen').text(counter.count);
    $('#' + rowName + ' .recent').text(counter.recent);
    $('#' + rowName + ' span.message').text(counter.message);
  } else {
    var newRow =
    "<tr id='" + rowName +"'>" +
    "<td><a id='" + rowName + "' class='btn btn-mini btn-alert-options'>...</a></td>"+
    "<td>[" + maybe(counter.severity, "alert") + "] " + maybe(counter.name) + " line " + maybe(counter.line) + "<br/><span class='message'>" + maybe(counter.message) + "</span></td>" +
    "<td><span class='recent'>" + maybe(counter.recent, 0) + "</span> recent / <span class='seen'>" + maybe(counter.count, 1) + "</span> seen</td>" +
    "<td align='right'>" + maybe(counter.product) + " " + maybe(counter.version) + "</td></tr>";
    $(table + ' tbody').prepend(newRow);
    register_to_context_click($('a.btn-alert-options'));
  };
  $('#' + rowName + ' td').addClass('highlight');
  setTimeout("$('#" + rowName + " td').removeClass('highlight')", 1000);
};
