define(["d3", "dataloader", "runningahead", "strava"], function (d3, dataloader, ra, strava) {
  function createWeekChart(data, date) {
    date = new Date(date);

    if (d3.time.sunday.utc(date) - date !== 0) {
      throw "Not sunday!";
    }

    var monday = d3.time.monday.utc;
    var dates = d3.time.day.utc.range(monday(date), monday.ceil(date))
      .map(function (d) { return d3.time.format("%Y-%m-%d")(d); });

    var weekValues = dates.map(function (d) { return data.get(d) || 0; });

    var svg = d3.select("div.chart svg");
    svg.attr("width", 70)
        .attr("height", 30);

    var max = d3.max(weekValues);
    var y = d3.scale.linear()
      .domain([0, max])
      .range([0, 30]);

    var kmfmt = function (x) { return d3.format(".1f")(x) + "km"; };

    svg.selectAll("rect")
        .data(weekValues)
      .enter().append("rect")
        .attr("width", 8)
        .attr("height", function (a) { return y(a); })
        .attr("x", function (x, i) { return i * 10; })
        .attr("y", function (a, i) { return 30 - y(a); })
        .style("fill", "#E7E7E7")
        .style("stroke", "none")
      .append("title")
        .text(kmfmt);
    d3.select(".total").text(kmfmt(d3.sum(weekValues)));
  }

  var dataSources = [
    ra.fromFile("log.txt"),
    strava.fromFile("activities.json")
  ];

  function chartWeek(date, callback) {
    dataloader.loadData(dataSources, (function (data) {
      try {
        createWeekChart(data, date);
        callback(null, null); // data, error
      } catch (e) {
        callback(null, e);
      }
    }));
  }

  return { chartWeek: chartWeek };
});
