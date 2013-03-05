var isVisible = false,
    clickedAway = false,
    foundSeverities = [],
    foundNodes = [],
    foundRoles = [],
    foundTopics = [],
    foundIdentities = [],
    messages = [],
    messageFilter = crossfilter(messages),
    timeFilterDimension = messageFilter.dimension(function(log_message) {
      return Math.floor(log_message['timestamp'] / 1000);
    }),
    timeFilterGroupByMinute = timeFilterDimension.group(function(second) {
      return Math.floor(second / 60);
    }),
    messagesTable, tbody,
    timeChart,
    isLogMessagesDirty = false,
    LOG_REFRESH_INTERVAL = 250,
    CHART_REFRESH_INTERVAL = 2000,
    MAX_MESSAGES = 500;

var MAX_IDENTITIES = 15;

$(document).ready(function() {
  var timeChartColumnWidth = 15,
      timeChartColumnHeight = 80;
  var timeChartWidthFunction = d3.scale.linear()
      .domain([0, 1])
      .range([0, timeChartColumnWidth]);
  var timeChartHeightFunction = d3.scale.linear()
      .domain([0, 100])
     .rangeRound([0, timeChartColumnHeight]);

  timeChart = d3.select("#visualization-container").append("svg")
            .attr('class', 'chart time-chart')
            .attr('width', timeChartColumnWidth * 100)
            .attr('height', timeChartColumnHeight);

  // an interval for updating the chart
  setInterval(function() {
    appendColumn = function(d) {
      this.attr('x', function(d, i) { return timeChartWidthFunction(i); })
          .attr('y', function(d) { return timeChartColumnHeight - timeChartHeightFunction(d.value); })
          .attr('width', timeChartColumnWidth)
          .attr('height', function(d) { return timeChartHeightFunction(d.value); });
    };

    if (isChartDataDirty) {
      var chartData = timeChart.selectAll('rect').data(timeFilterGroupByMinute.top(75));

      chartData.call(appendColumn);

      chartData.enter()
               .append('rect')
               .call(appendColumn);

      chartData.exit().remove();
    }
    isChartDataDirty = false;
  }, CHART_REFRESH_INTERVAL);

  // an interval for updating the log entries
  setInterval(function() {
    appendMessageCell = function(d) {
      this.html(function(d) { 
         if (d.column === 'message') {
           $(this).addClass('log-message');
         }

         if (d.column === 'find_more_html') {
           var more = $('<a />').attr('href', '#')
                                .attr('rel', 'popover')
                                .attr('data-placement', 'bottom')
                                .attr('data-html', true)
                                .attr('data-content', d.value)
                                .attr('data-template', '<div class="popover message-more-popover-outer"><div class="arrow"></div><div class="popover-inner message-more-popover-inner"><h3 class="popover-title"></h3><div class="popover-content"><p></p></div></div></div>')
                                .attr('data-original-title', 'More Info<div style="float:right;"><button class="close close-popover">&times;</button></div>')
                                .addClass('btn').addClass('btn-mini').addClass('show-more')
                                .html('<i class="icon-info-sign"></i>');
           return $('<div />').append(more).html();
         } else {
           return d.value;
         }
       });
    };

    if (isLogMessagesDirty) {
      var minTimestamp = 0;
      if (messages.length > MAX_MESSAGES) {
        minTimestamp = messages.sort(sortTimeDescending)[MAX_MESSAGES - 1]['timestamp'];
      }

      var columns = ['find_more_html', 'time', 'message_severity', 'message'];
      var rows = tbody.selectAll('tr')
                      .data(messages.filter(function(log_message) { return log_message.timestamp > minTimestamp; }));

      rows.enter().append('tr');
      rows.exit().remove();

      var cells = rows.selectAll('td')
                      .data(function(log_message) {
                        return columns.map(function(column) {
                          return {column: column, value: log_message[column]};
                        });
                      });

      cells.call(appendMessageCell);
      cells.exit().remove();
      cells.enter().append('td').call(appendMessageCell);

      tbody.selectAll('tr').sort(sortTimeDescending);

      isLogMessagesDirty = false;
    }
  }, LOG_REFRESH_INTERVAL);

  sortTimeDescending = function(a, b) {
    return d3.descending(a['timestamp'], b['timestamp']);
  };

  $('.show-more').live('click', function(e) {
    e.preventDefault();
    $('.show-more').popover('hide');
    $(this).popover('show');
  });

  messagesTable = d3.select("#logs").append("table").attr('id', 'log-messages').attr('class', 'table table-striped full-section table-hover');
  var thead = messagesTable.append("thead");
  theadrow = thead.append('tr');
  theadrow.append('th');
  theadrow.append('th').html('Time');
  theadrow.append('th').html('Severity');
  theadrow.append('th').html('Message');
  tbody = messagesTable.append("tbody").attr('id', 'log-messages-body');

  initPopovers = function() {
    $('#stream-properties').popover({
      html: true,
      trigger: 'click',
      title: 'Stream Properties<div style="float:right;"><button class="close" id="close-stream">&times;</button></div>',
      placement: 'bottom',
      template: '<div class="popover stream-popover-outer"><div class="arrow"></div><div class="popover-inner stream-popover-inner"><h3 class="popover-title"></h3><div class="popover-content"><p></p></div></div></div>',
      content: $('#stream-popover').html()});

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

  $('.stream-role').live('click', function(e) {
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
              alert('Unable to update role stream response='+request.responseText+' status='+textstatus+' error='+error+' roles='+rolesOn);
            }});
  });

  $('.stream-node').live('click', function(e) {
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
              alert('Unable to update node stream'+request.responseText+" "+textstatus+" "+error);
            }});
  });

  $('.stream-severity').live('click', function(e) {
    e.preventDefault();
    if ($(this).attr('filter-selected') == '1')  {
      $(this).attr('filter-selected', '0');
      $(this).find('i').removeClass('icon-ok').addClass('icon-remove');
    } else {
      $(this).attr('filter-selected', '1');
      $(this).find('i').removeClass('icon-remove').addClass('icon-ok');
    }
    var severitiesOn = [];
    var selectedSeverities = $('.stream-popover-inner').find('.filter-severity[filter-selected=1]');
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

  $('#stream-properties').click(function(e) {
    e.preventDefault();
  });

  $('#filter-properties').click(function(e) {
    e.preventDefault();
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

  $('#close-stream').live('click', function() {
    $('#stream-properties').popover('hide');
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
    messages.push(log_message);
    messageFilter.add([log_message]);
    isLogMessagesDirty = true;
    isChartDataDirty = true;
  };
});


