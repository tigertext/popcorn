var isVisible = false,
    clickedAway = false,
    foundSeverities = [],
    foundNodes = [],
    foundRoles = [],
    foundTopics = [],
    foundIdentities = [];

var MAX_IDENTITIES = 15;

$(document).ready(function() {
  initPopovers = function() {
    $('#filter-properties').popover({
      html: true,
      trigger: 'click',
      title: 'Filter Properties<div style="float:right;"><button class="close" id="close-filter">&times;</button></div>',
      placement: 'bottom',
      template: '<div class="popover filter-popover-outer"><div class="arrow"></div><div class="popover-inner filter-popover-inner"><h3 class="popover-title"></h3><div class="popover-content"><p></p></div></div></div>',
      content: $('#filter-popover').html()});
  };

  initPopovers();

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

  $('.topic').live('click', function(e) {
    e.preventDefault();

    $.ajax({type:'POST',
            url:'/log/stream/' + streamId,
            data:'topics_add=' + $(this).attr('data'),
            success:function() { },
            error:function(request, textstatus, error) {
              alert('Unable to add topic to filter, response='+request.responseText+' status='+textstatus+' error='+error+' topics_add='+(this).attr('data'));
            }});
  });

  $('.identity').live('click', function(e) {
    e.preventDefault();
  });

  $('.filter-role').live('click', function(e) {
    e.preventDefault();
    if ($(this).attr('filter-selected') == '1')  {
      $(this).attr('filter-selected', '0');
      $(this).find('i').removeClass('icon-ok').addClass('icon-remove');
    } else {
      $(this).attr('filter-selected', '1');
      $(this).find('i').removeClass('icon-remove').addClass('icon-ok');
    }
    appliedFilters['roles'] = rolesOn;
    updateHistoryState();

    $.ajax({type:'POST',
            url:'/log/stream/' + streamId,
            data:'roles=' + rolesOn.join("%2C"),
            success:function() { },
            error:function(request, textstatus, error) {
              alert('Unable to update role filter response='+request.responseText+' status='+textstatus+' error='+error+' roles='+rolesOn);
            }});
  });

  $('.filter-node').live('click', function(e) {
    e.preventDefault();
    if ($(this).attr('filter-selected') == '1')  {
      $(this).attr('filter-selected', '0');
      $(this).find('i').removeClass('icon-ok').addClass('icon-remove');
    } else {
      $(this).attr('filter-selected', '1');
      $(this).find('i').removeClass('icon-remove').addClass('icon-ok');
    }

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

  $('.filter-severity').live('click', function(e) {
    e.preventDefault();
    if ($(this).attr('filter-selected') == '1')  {
      $(this).attr('filter-selected', '0');
      $(this).find('i').removeClass('icon-ok').addClass('icon-remove');
    } else {
      $(this).attr('filter-selected', '1');
      $(this).find('i').removeClass('icon-remove').addClass('icon-ok');
    }
    var severitiesOn = [];
    var selectedSeverities = $('.filter-popover-inner').find('.filter-severity[filter-selected=1]');
    $.each(selectedSeverities, function(k, v) {
      if ($(this).attr('filter-selected') == '1') {
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

  $('#filter-properties').click(function(e) {
    e.preventDefault();

    $('.filter-popover-inner input#identity-search').typeahead({
      items: 10,
      source: function(query, process) {
        var d = ['california', 'arkansas', 'soemthing'];
        console.log(d);
        return process(d);
      }});
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

  $('#close-filter').live('click', function() {
    $('#filter-properties').popover('hide');
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
    if (foundSeverities.indexOf(log_message.message_severity) == -1) {
      foundSeverities.push(log_message.message_severity);
      var severityVal = -1;
      for (var s in knownSeverities) {
        if (knownSeverities[s]['label'] == log_message.message_severity) {
          severityVal = knownSeverities[s]['val'];
        }
      }
      var severity = '<span class="label filter-item filter-severity" data-val="' + severityVal + '" filter-selected="1"><i class="icon-ok"></i>' + log_message.message_severity + '</span>&nbsp;';
      $('#severities-list').append(severity);
      $('a#filter-properties').data('popover').options.content = $('#filter-popover').html();
    }

    if (foundNodes.indexOf(log_message.node) == -1) {
      foundNodes.push(log_message.node);
      var node = '<span class="label filter-item filter-node" data-val="' + log_message.node + '" filter-selected="1"><i class="icon-ok"></i>' + log_message.node + '</span>&nbsp;';
      $('#nodes-list').append(node);
      $('a#filter-properties').data('popover').options.content = $('#filter-popover').html();
    }

    if (foundRoles.indexOf(log_message.role) == -1) {
      foundRoles.push(log_message.role);
      var role = '<span class="label filter-item filter-role" data-val="' + log_message.role + '" filter-selected="1"><i class="icon-ok"></i>' + log_message.role + '</span>&nbsp;';
      $('#roles-list').append(role);
      $('a#filter-properties').data('popover').options.content = $('#filter-popover').html();
    }

    for (var idx in log_message.topics) {
      if (foundTopics.indexOf(log_message.topics[idx]) == -1) {
        foundTopics.push(log_message.topics[idx]);
        var topic = '<span class="label filter-item filter-topic" data-val="' + log_message.topics[idx] + '" filter-selected="1"><i class="icon-ok"></i>' + log_message.topics[idx] + '</span>&nbsp;';
        $('#topics-list').append(topic);
        $('a#filter-properties').data('popover').options.content = $('#filter-popover').html();
      }
    }

    for (var idx in log_message.identities) {
      if (foundIdentities.indexOf(log_message.identities[idx]) == -1) {
        foundIdentities.unshift(log_message.identities[idx]);
        if (foundIdentities.length > MAX_IDENTITIES) {
          var removedIdentities = foundIdentities.splice(MAX_IDENTITIES, foundIdentities.length);
          for (var removedIdx in removedIdentities) {
            $('.filter-identity[data-val='+removedIdentities[removedIdx]+']').remove();
          };
        }
        var topic = '<span class="label filter-item filter-identity" data-val="' + log_message.identities[idx] + '" filter-selected="1"><i class="icon-ok"></i>' + log_message.identities[idx] + '</span>&nbsp;';
        $('#identities-list').prepend(topic);
        $('a#filter-properties').data('popover').options.content = $('#filter-popover').html();
      }
    }

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
