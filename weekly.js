/// <reference path="d3.v3.js" />
// TODO share with calendar.js
require.config({
  paths: {
    d3: "http://d3js.org/d3.v3.min",
    queue: "http://d3js.org/queue.v1.min"
  },
  shim: {
    'jsonp': {
      deps: ['d3']
    }
  }
});

require(["weeklychart"], function (weeklychart) {
//  chartWeek("2014-09-14", function() {});
});
