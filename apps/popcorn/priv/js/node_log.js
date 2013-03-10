var arc = d3.svg.arc().innerRadius(110 * .6).outerRadius(110),
    donut = d3.layout.pie(),
    color = d3.scale.category20(),
    incomingMessages = [],
    countsByPeriod = [],
    severitiesByPeriod = [],
    nodesByPeriod = [],
    rolesByPeriod = [],
    timeChart, severityChart, roleChart, nodeChart,
    maxTimestamp = 0,
    timestampOffset = Math.floor(new Date().getTime() / 1000);
    FLUSH_PERIOD = 5,  // when grouping severity, node, role, what period to group by
    TIME_CHART_PERIOD = 5,   // the time period for the time d3 graph
    FLUSH_DATA_INTERVAL = 100,  // how often to flush data from the incoming array and into the data that powers the charts
    CHART_REFRESH_INTERVAL = 100, // how often to refresh the charts
    EMPTY_SEVERITY_HASH = {1:0, 2:0, 4:0, 8:0, 16:0, 32:0, 64:0, 128:0},
    SEVERITY_LOOKUP_HASH = {1:0, 2:1, 4:2, 8:3, 16:4, 32:5, 64:6, 128:7};


while (timestampOffset % 5) { timestampOffset--; }

$(document).ready(function() {
  var timeChartColumnWidth = 15, timeChartColumnHeight = 80;
  timeChart = d3.select('#visualization-container').append('svg').attr('class', 'chart time-chart').attr('height', timeChartColumnHeight);
  severityChart = d3.select('#visualization-container').append('svg').attr('class', 'chart severity-chart');
  roleChart = d3.select('#visualization-container').append('svg').attr('class', 'chart role-chart');
  nodeChart = d3.select('#visualization-container').append('svg').attr('class', 'chart node-chart');

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
    var severityCounts = [];
    for (var i in severitiesByPeriod) {
      for (var j in severitiesByPeriod[i]) {
        if (severityCounts[SEVERITY_LOOKUP_HASH[j]]) {
          severityCounts[SEVERITY_LOOKUP_HASH[j]].value += severitiesByPeriod[i][j];
        } else {
          severityCounts[SEVERITY_LOOKUP_HASH[j]] = {'key': j, 'value': severitiesByPeriod[i][j]};
        }
      }
    }

    var totalSeverityCounts = 0;
    for (var i = 0; i < severityCounts.length; i++) {
      totalSeverityCounts += severityCounts[i].value;
    }
    if (totalSeverityCounts === 0) {
      return;
    }

    appendSegment = function() {
      this.selectAll('path').attr('d', arc);
    };

    severityChart.data([severityCounts]);
    var arcs = severityChart.selectAll("g.arc")
      .data(donut.value(function(d) { return d.value; }));

    arcs.call(appendSegment);
    arcs.enter().append('svg:g').attr('class', 'arc').attr('transform', 'translate(110, 110)')
                .append('svg:path').attr("fill", function(d, i) { return color(i); });// attr("d", arc);

  };
  updateRoleChart = function() {

  };
  updateNodeChart = function() {

  };

  setInterval(function() {
    updateTimeChart();
    updateSeverityChart();
    updateRoleChart();
    updateSeverityChart();
  }, CHART_REFRESH_INTERVAL);

  flushData = function() {
    var messagesCopy = incomingMessages.slice(0);
    incomingMessages = [];
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

  showLogMessage = function(log_message) {
    if (maxTimestamp < log_message.timestamp) {
      maxTimestamp = log_message.timestamp;
    }

    incomingMessages.push(log_message);
  };
});


