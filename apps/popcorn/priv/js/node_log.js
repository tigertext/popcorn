var isVisible = false;
var clickedAway = false;

$(document).ready(function() {
  updateHistoryState = function() {
    var cleanUrl = History.getState().cleanUrl;
    var params = [];
    if (appliedFilters['roles'].length > 0) {
      params.push('nodes=' + encodeURIComponent(appliedFilters['node_names']));
    }
    if (appliedFilters['node_names'].length > 0) {
      params.push('nodes=' + encodeURIComponent(appliedFilters['node_names']));
    }
    if (appliedFilters['severities'].length > 0) {
      params.push('severities=' + appliedFilters['severities']);
    }

    History.pushState({}, '', '?' + params.join('&'));
  };

  $('.filter-role').click(function(e) {
    var rolesOn = [];
    $.each($('.filter-role'), function(k, v) {
      if ($(v).prop('checked')) {
        rolesOn.push($(this).attr('data-val'));
      }
    });

    appliedFilters['roles'] = rolesOn;
    updateHistoryState();

    $.ajax({type:'POST',
            url:'/log/stream/' + streamId,
            data:'roles=' + rolesOn.join("%2C"),
            success:function() { },
            error:function(request, textstatus, error) {
              alert('Unable to update role filter response='+request.responseText+" status="+textstatus+" error="+error+' roles='+rolesOn);
            }});
  });

  $('.filter-node').click(function(e) {
    var nodesOn = [];
    $.each($('.filter-node'), function(k, v) {
      if ($(v).prop('checked')) {
        nodesOn.push($(this).attr('data-val'));
      }
    });

    appliedFilters['nodes'] = nodesOn;
    updateHistoryState();

    $.ajax({type:'POST',
            url:'/log/stream/' + streamId,
            data:'nodes=' + nodesOn.join("%2C"),
            success:function() { },
            error:function(request, textstatus, error) {
              alert('Unable to update node filter'+request.responseText+" "+textstatus+" "+error);
            }});
  });

  $('.filter-severity').click(function(e) {
    var severitiesOn = [];
    $.each($('.filter-severity'), function(k, v) {
      if ($(v).prop('checked')) {
        severitiesOn.push(parseInt($(this).attr('data-val'), 10));
      }
    });

    appliedFilters['severities'] = severitiesOn;
    updateHistoryState();

    $.ajax({type:'POST',
            url:'/log/stream/' + streamId,
            data:'severities=' + severitiesOn.join("%2C"),
            success:function() { },
            error:function(request, textstatus, error) {
              alert('Unable to update severity filter'+request.responseText+" "+textstatus+" "+error);
            }});
  });

  $('.icon-remove').click(function(e) {
    e.preventDefault();
    $('#log-messages tr:gt(0)').remove();
  });

  $('.icon-pause').click(function(e) {
    e.preventDefault();
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

  var today = new Date();
  var defaultDate = today.getMonth() + 1 + '-' + today.getDate() + '-' + today.getFullYear();
  var defaultTime = '';
  if (today.getHours() < 13) {
    if (today.getHours() < 10) {
      defaultTime += '0';
    }
    defaultTime += today.getHours() + ':00 AM';
  } else {
    var h = today.getHours() - 12;
    if (h < 10) {
      defaultTime += '0';
    }
    defaultTime += h + ':00 PM'
  }
  // todo: consider making this cleaner and more maintainable... jquery's dom builder style maybe?
  var timestampPopoverContent = '<label class="radio timestamp-radio-label">' +
                                  '<input type="radio" name="timestamp-radio" value="current" checked></input>' +
                                  'Current Stream' +
                                '</label>' +
                                '<label class="radio timestamp-radio-label">' +
                                  '<input type="radio" name="timestamp-radio" value="previous"></input>' +
                                  'Earlier (UTC)' +
                                  '<div id="timespan-absolute" style="display:none;">' +
                                    '<span class="timestamp-description">Limit</span>' +
                                    '<div class="input-append date" id="absolute-date-start" data-date="' + defaultDate + '" data-date-format="mm-dd-yyyy">' +
                                      '<span class="add-on"><i class="icon-th"></i></span>' +
                                      '<input type="text" id="selected-date-value" value="' + defaultDate + '" readonly style="width:145px;"></input>' +
                                    '</div>' +
                                    '<div class="input-append bootstrap-timepicker-component">' +
                                      '<span class="add-on"><i class="icon-time"></i></span>' +
                                      '<input type="text" value="' + defaultTime + '" class="timepicker-default input-small" id="absolute-time-start" readonly></input>' +
                                    '</div>' +
                                  '</div>' +
                                  '<div class="timestamp-change"><a href="#" class="btn btn-mini" id="apply-time">apply</a></div>' +
                                '</label>';

  $('#log-timestamp').popover({html: true,
                               trigger: 'click',
                               title: 'Message Timestamp<div style="float:right;"><button class="close" id="close-timestamp">&times;</button></div>',
                               placement: 'bottom',
                               content: timestampPopoverContent});

  $('#log-timestamp').click(function(e) {
    $('#absolute-date-start').datepicker();
    $('#absolute-date-end').datepicker();
    $('#absolute-time-start').timepicker({'defaultTime': 'current'});
    e.preventDefault();
  });

  $('#close-timestamp').live('click', function(e) {
    $('#log-timestamp').popover('hide');
    e.preventDefault();
  });

  $('#apply-time').live('click', function(e) {
    e.preventDefault();
    if ($('input[name=timestamp-radio]:checked').val() == 'current') {
      $.ajax({type:'POST',url:'/log/stream/' + streamId,
              data:'time_filter_type=stream',
              success:function() { },
              error:function() {
                alert('Unable to update time filter');
              }});
    } else if ($('input[name=timestamp-radio]:checked').val() == 'previous') {
      $.ajax({type:'POST',url:'/log/stream/' + streamId,
              data:'time_filter_type=previous' +
                   '&max_date=' + encodeURIComponent($('#selected-date-value').val()) +
                   '&max_time=' + encodeURIComponent($('#absolute-time-start').val()),
              success:function() { },
              error:function() {
                alert('Unable to update time filter');
              }});
    }

  });

  $('input[name=timestamp-radio]').live('change', function() {
    if ($(this).val() == 'previous') {
      $('#timespan-absolute').slideDown();
      $('#timespan-relative').slideUp();
    } else if($(this).val() == 'current') {
      $('#timespan-relative').slideDown();
      $('#timespan-absolute').slideUp();
    }
  });

  // select the default filters
  for (appliedFilter in appliedFilters) {
    var values = appliedFilters[appliedFilter];

    if (appliedFilter === 'roles') {
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        $('.filter-role[data-val=\''+value+'\']').prop('checked', true);
      }
    } else if (appliedFilter === 'node_names') {
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

  executeCommand = function(command_payload) {
    if (command_payload['name'] == 'clear') {
      $('#log-messages tr:gt(0)').remove();
    } else {
      console.log('Unrecognized command: ' + command);
    }
  };

  showNewLogMessage = function(log_message) {
    showLogMessage('top', log_message);
  };

  showOldLogMessage = function(log_message) {
    showLogMessage('bottom', log_message);
  };

  showLogMessage = function(location, log_message) {
    var row = $('<tr />').attr('data-timestamp', log_message['timestamp']);
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
    row.append($('<td />').addClass('log-message').html(log_message.message));

    if (location == 'top') {
      $('#log-messages tbody').prepend(row);
    } else if (location == 'bottom') {
      $('#log-messages tbody').append(row);
    }

    // truncate the table to 100 rows // TODO make this less static
    while ($('#log-messages tr').length > 100) {
      $('#log-messages tr:last').remove();
    }
  }
});


