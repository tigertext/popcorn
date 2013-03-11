var pie = d3.layout.pie(),
    color = d3.scale.category20(),
    incomingSummaryMessages = [],
    countsByPeriod = [],
    severitiesByPeriod = [],
    nodesByPeriod = [],
    rolesByPeriod = [],
    fullMessages = [],
    timeChart, severityChart, roleChart, nodeChart,
    severityChartPath,
    timestampOffset = Math.floor(new Date().getTime() / 1000);
    MAX_MESSAGES_TO_SHOW = 500,
    FLUSH_PERIOD = 5,  // when grouping severity, node, role, what period to group by
    TIME_CHART_PERIOD = 5,   // the time period for the time d3 graph
    FLUSH_DATA_INTERVAL = 100,  // how often to flush data from the incoming array and into the data that powers the charts
    CHART_REFRESH_INTERVAL = 150, // how often to refresh the charts
    EMPTY_SEVERITY_HASH = {1:0, 2:0, 4:0, 8:0, 16:0, 32:0, 64:0, 128:0},
    SEVERITY_LOOKUP_HASH = {1:0, 2:1, 4:2, 8:3, 16:4, 32:5, 64:6, 128:7};

var arc = d3.svg.arc().innerRadius(110 * .6).outerRadius(110);

while (timestampOffset % 5) { timestampOffset--; }

$(document).ready(function() {
  var timeChartColumnWidth = 15, timeChartColumnHeight = 80;
  timeChart = d3.select('#visualization-container').append('svg').attr('class', 'chart time-chart').attr('height', timeChartColumnHeight);
  severityChart = d3.select('#visualization-container').append('svg').attr('class', 'chart severity-chart').append('g').attr('transform', 'translate(110, 110)');
  roleChart = d3.select('#visualization-container').append('svg').attr('class', 'chart role-chart');
  nodeChart = d3.select('#visualization-container').append('svg').attr('class', 'chart node-chart');

  messagesTable = d3.select("#logs").append("table").attr('id', 'log-messages').attr('class', 'table table-striped full-section table-hover');
  var thead = messagesTable.append("thead");
  theadrow = thead.append('tr');
  theadrow.append('th');
  theadrow.append('th').html('Time');
  theadrow.append('th').html('Severity');
  theadrow.append('th').html('Message');
  tbody = messagesTable.append("tbody").attr('id', 'log-messages-body');

  updateTimeChart = function() {
    var maxValue = 0;  // TODO get this value instead of iterating
    for (var i = 0; i < countsByPeriod.length; i++) {
      if (countsByPeriod[i] > maxValue) {
        maxValue = countsByPeriod[i];
      }
    }
    var timeChartColumnLocX = d3.scale.linear().domain([0, 60]).rangeRound([1100, 0]);
    var timeChartHeightFunction = d3.scale.linear().domain([0, maxValue]).rangeRound([0, timeChartColumnHeight]);

    appendColumn = function() {
      this.attr('x', function(d, i) { return timeChartColumnLocX(i); })
          .attr('y', function(d) { return timeChartColumnHeight - timeChartHeightFunction(d) + 1; })
          .attr('width', timeChartColumnWidth)
          .attr('height', function(d) { return timeChartHeightFunction(d); });
    };

    var chartData = timeChart.selectAll('rect').data(countsByPeriod.slice(-60).reverse());
    chartData.call(appendColumn);
    chartData.enter().append('rect').call(appendColumn);
    chartData.exit().remove();
  };
  updateSeverityChart = function() {
    var severityCounts = [0, 0, 0, 0, 0, 0, 0, 0];
    for (var i in severitiesByPeriod) {
      for (var j in severitiesByPeriod[i]) {
        severityCounts[SEVERITY_LOOKUP_HASH[j]] += severitiesByPeriod[i][j];
      }
    }

    var totalSeverityCount = 0;
    for (var i = 0; i < severityCounts.length; i++) {
      totalSeverityCount += severityCounts[i];
    }

    if (totalSeverityCount) {
      if (!severityChartPath) {
        severityChartPath = severityChart.selectAll('path').data(pie(severityCounts))
                                         .enter().append('path')
                                         .attr('fill', function(d, i) { return color(i); })
                                         .attr('d', arc);
      } else {
        severityChartPath = severityChartPath.data(pie(severityCounts));
        severityChartPath.attr('d', arc);
      }
    }
  };
  updateRoleChart = function() {

  };
  updateNodeChart = function() {

  };
  updateLogMessages = function() {
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

    var minTimestamp = 0;
    var columns = ['find_more_html', 'time', 'message_severity', 'message'];
    var rows = tbody.selectAll('tr').data(fullMessages);

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
  };

  sortTimeDescending = function(a, b) {
    return d3.descending(a['timestamp'], b['timestamp']);
  };

  setInterval(function() {
    updateTimeChart();
    updateSeverityChart();
    updateRoleChart();
    updateSeverityChart();
  }, CHART_REFRESH_INTERVAL);

  $('.close-popover').live('click', function() {
    $('.show-more').popover('hide');
  });

  $('.show-more').popover({html: true, trigger: 'manual'})
                 .click(function(e) {
                     $('.show-more').popover('hide');
                     $(this).popover('show');
                     e.preventDefault();
                 });

  $('.show-more').live('click', function(e) {
    e.preventDefault();
    $('.show-more').popover('hide');
    $(this).popover('show');
  });

  flushData = function() {
    var messagesCopy = incomingSummaryMessages.slice(0);
    incomingSummaryMessages = [];
    var messageFilter = crossfilter(messagesCopy);

    var severityFilterDimension = messageFilter.dimension(function(log_message) {
      return log_message['severity'];
    });
    var severityFilterGroup = severityFilterDimension.group(function(severity) {
      return severity;
    });
    var roleFilterDimension = messageFilter.dimension(function(log_message) {
      return log_message['role'];
    });
    var roleFilterGroup = roleFilterDimension.group(function(role) {
      return role;
    });
    var nodeFilterDimension = messageFilter.dimension(function(log_message) {
      return log_message['node'];
    });
    var nodeFilterGroup = nodeFilterDimension.group(function(node) {
      return node;
    });
    var timeSecondFilterDimension = messageFilter.dimension(function(log_message) {
      return Math.floor(log_message['timestamp'] / 1000 / 1000);
    });
    var timeFilterGroupByTimeInterval = timeSecondFilterDimension.group(function(second) {
      return Math.floor(second / TIME_CHART_PERIOD);
    });

    var key = Math.floor(new Date().getTime() / 1000) - timestampOffset;
    while (key % 5) { key--; };
    key = key / 5;

    // pull the counts
    var lastCount = countsByPeriod[key] || 0;
    for (var count in timeFilterGroupByTimeInterval.all()) {
      lastCount += timeFilterGroupByTimeInterval.all()[count]['value'];
    };
    countsByPeriod[key] = lastCount;

    // pull the severities
    var lastSeverity = severitiesByPeriod[key] || $.extend({}, EMPTY_SEVERITY_HASH);
    for (var severity in severityFilterGroup.all()) {
      var k = severityFilterGroup.all()[severity]['key'];
      var v = severityFilterGroup.all()[severity]['value'];
      if (lastSeverity[k]) { lastSeverity[k] += v; } else { lastSeverity[k] = v; }
    };
    severitiesByPeriod[key] = lastSeverity;

  };

  setInterval(function() {
    flushData();
  }, FLUSH_DATA_INTERVAL);

  showSummaryMessage = function(summary_message) {
    incomingSummaryMessages.push(summary_message);
  };

  showLogMessage = function(log_message) {
    if (log_message['name'] && log_message['name'] == 'clear') {
      fullMessages = [];
      updateLogMessages(); // TODO put this somewhere else
    } else {
      fullMessages.push(log_message);
      if (fullMessages.length > MAX_MESSAGES_TO_SHOW) {
        fullMessages.shift();
      }

      updateLogMessages();  // TODO this cannot be here, it needs to be on an interval
    }
  }
});


