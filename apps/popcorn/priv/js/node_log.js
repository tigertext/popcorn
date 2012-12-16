var isVisible = false;
var clickedAway = false;

$(document).ready(function() {
  updateHistoryState = function() {
    var cleanUrl = History.getState().cleanUrl;
    var params = [];
    if (appliedFilters['node_names'].length > 0) {
      params.push('nodes=' + encodeURIComponent(appliedFilters['node_names']));
    }
    if (appliedFilters['severities'].length > 0) {
      params.push('severities=' + appliedFilters['severities']);
    }

    History.pushState({}, '', '?' + params.join('&'));
  };

  $('.filter-severity').click(function(e) {
    var severitiesOn = [];
    $.each($('.filter-severity'), function(k, v) {
      if ($(v).prop('checked')) {
        severitiesOn.push(parseInt($(this).attr('data-val')));
      }
    });

    appliedFilters['severities'] = severitiesOn;
    updateHistoryState();

    $.ajax({type:'PUT',url:'/log/stream/' + streamId,
            data:'severities=' + severitiesOn,
            success:function() { },
            error:function() {
              alert('Unable to update filter');
            }});
  });

  $('.icon-pause').click(function(e) {
    var data = 'stream_id=' + encodeURIComponent(streamId);
    $.ajax({type:'POST',url:'/log/stream/pause',data:data,
            success:function(data,textStatus,xhr) {
              if (data['is_paused']) {
                $('#log-pause').removeClass('icon-pause').addClass('icon-play');
                $('#log-messages tbody').prepend($('<tr />')
                                                 .append($('<td />').attr('colspan', '4').css('text-align', 'center')
                                                         .html('Paused...')));
              } else {
                $('#log-pause').removeClass('icon-play').addClass('icon-pause');
                $('#log-messages tbody').prepend($('<tr />')
                                                 .append($('<td />').attr('colspan', '4').css('text-align', 'center')
                                                         .html('Resumed')));
              }
            },
            error:function(xhr,textStatus) {
              alert('Unable to toggle pause state');
            }});
    e.preventDefault();
  });
  $('.close-popover').live('click', function() {
    $('.show-more').popover('hide');
  });

  $('.show-more').popover({html: true, trigger: 'manual'})
                 .click(function(e) {
                     $('.show-more').popover('hide');
                     $(this).popover('show');
                     e.preventDefault();
                 });

  var timestampPopoverContent = '<label class="radio timestamp-radio-label">' +
                                  '<input type="radio" name="timestamp-radio" value="relative" checked></input>' +
                                  'Relative' +
                                  '<div id="timespan-relative">' +
                                    '<span class="timestamp-description">Show the past<br /></span>' +
                                    '<input type="text" id="relative-val"></input>' +
                                    '<select id="relative-unit">' +
                                      '<option val="minutes">minutes</option>' +
                                      '<option val="hours">hours</option>' +
                                      '<option val="days">days</option>' +
                                      '<option val="weeks">weeks</option>' +
                                    '</select>' +
                                    '<div class="timestamp-change"><a href="#" class="btn btn-mini">apply</a></div>' +
                                  '</div>' +
                                '</label>' +
                                '<label class="radio timestamp-radio-label">' +
                                  '<input type="radio" name="timestamp-radio" value="absolute"></input>' +
                                  'Absolute' +
                                  '<div id="timespan-absolute" style="display:none;">' +
                                    '<span class="timestamp-description">From</span>' +
                                    '<div class="input-append date" id="absolute-date-start" data-date="12-02-2012" data-date-format="dd-mm-yyyy">' +
                                      '<span class="add-on"><i class="icon-th"></i></span>' +
                                      '<input type="text" value="12-02-2012" readonly style="width:145px;">' +
                                    '</div>' +
                                    '<span class="timestamp-description">To</span>' +
                                    '<div class="input-append date" id="absolute-date-end" data-date="12-02-2012" data-date-format="dd-mm-yyyy">' +
                                      '<span class="add-on"><i class="icon-th"></i></span>' +
                                      '<input type="text" value="12-02-2012" readonly style="width:145px;">' +
                                    '</div>' +
                                    '<div class="timestamp-change"><a href="#" class="btn btn-mini">apply</a></div>' +
                                  '</div>' +
                                '</label>';

  $('#log-timestamp').popover({html: true,
                               trigger: 'click',
                               title: 'Message Timestamp',
                               placement: 'bottom',
                               content: timestampPopoverContent});

  $('#log-timestamp').click(function() {
    $('#absolute-date-start').datepicker();
    $('#absolute-date-end').datepicker();
  });

  $('input[name=timestamp-radio]').live('change', function() {
    if ($(this).val() == 'absolute') {
      $('#timespan-absolute').show();
      $('#timespan-relative').hide();
    } else if($(this).val() == 'relative') {
      $('#timespan-relative').show();
      $('#timespan-absolute').hide();
    }
  });

  // select the default filters
  for (appliedFilter in appliedFilters) {
    var values = appliedFilters[appliedFilter];

    if (appliedFilter === 'node_names') {
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        $('.filter-node[data-val=\''+value+'\']').prop('checked', true);
      }
    } else if (appliedFilter === 'severities') {
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        $('.filter-severity[data-val=\''+value+'\']').prop('checked', true);
      }
    }
  }

  showNewLogMessage = function(log_message) {
    var row = $('<tr />');
    var cell = $('<td />').css('padding-right', '12px');
    var more = $('<a />').attr('href', '#')
                          .attr('rel', 'popover')
                          .attr('data-placement', 'bottom')
                          .attr('data-html', true)
                          .attr('data-content', log_message.find_more_html)
                          .attr('data-original-title', 'Find Similar<div style="float:right;"><button class="close close-popover">&times;</button></div>')
                          .addClass('btn').addClass('btn-mini').addClass('show-more')
                          .html('...');
    more.popover({html: true, trigger: 'manual'})
                .click(function(e) {
                  $('.show-more').popover('hide');
                  $(this).popover('show');
                  e.preventDefault();
                });
    cell.append(more);
    row.append(cell);
    row.append($('<td />').html(log_message.time));
    row.append($('<td />').html(log_message.message_severity));
    row.append($('<td />').html(log_message.message));

    $('#log-messages tbody').prepend(row);

    // truncate the table to 500 rows // TODO make this less static
    while ($('#log-messages tr').length > 500) {
      $('#log-messages tr:last').remove();
    }
  }
});


