define(["d3"], function(d3) {
  var margin = { top: 10, bottom: 10, midBreak: 10, left: 40, right: 18 },
      cellSize = 17,
      barHeight = cellSize * 3,
      width = 960,
      monthTotalHeight = 7,
      height = cellSize * 7 + 1 + monthTotalHeight,
      totalHeight = height + barHeight + margin.midBreak;

  function day(d) {
      return (d.getDay() + 6) % 7; // sunday-start => monday-start
  }

  function distance(d) {
      return d3.format(".1f")(d) + " km";
  }

  var week = d3.time.format("%W"),
      format = d3.time.format("%Y-%m-%d");

  function monthPath(t0) {
      var t1 = new Date(t0.getFullYear(), t0.getMonth() + 1, 0),
          d0 = +day(t0), w0 = +week(t0),
          d1 = +day(t1), w1 = +week(t1);
      return "M" + (w0 + 1) * cellSize + "," + d0 * cellSize
          + "H" + w0 * cellSize + "V" + 7 * cellSize
          + "H" + w1 * cellSize + "V" + (d1 + 1) * cellSize
          + "H" + (w1 + 1) * cellSize + "V" + 0
          + "H" + (w0 + 1) * cellSize + "Z";
  }

  var colorClasses = d3.range(10).map(function (d) { return "q" + d; });
  colorClasses.reverse();
  var fixedRange = d3.scale.threshold()
      .domain([5, 10, 15, 20, 25, 30, 40, 50, 70]);
  var color = fixedRange.range(colorClasses);

  function createChart(data, start, end, showWeeks) {
      var max = d3.max(data.values());
      var scaledRange = d3.scale.quantize().domain([0, max]);

      // Create the rows
      var yeardata = d3.range(start, end);
      var svg = d3.select("body").selectAll("svg.year")
          .data(yeardata, function (y) { return y; });

      var svgenter = svg
        .enter().append("svg")
          .attr("width", width + margin.left + margin.right)
          .attr("height", totalHeight + margin.top + margin.bottom)
          .attr("class", "year hcl2")
        .append("g")
          .attr("class", "outer")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

      svg.order();

      svg.exit().remove();

      // var svg = d3.selectAll("svg.year").select("g").data(yeardata);

      // var yeartitle = d3.selectAll("svg.year").select("text.yearTitle").data(yeardata);
      svgenter.append("text")
          .attr("class", "yearTitle")
          .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
          .style("text-anchor", "middle");
      svg.select("text.yearTitle").text(function (d) { return d; });

      // Create the cells
      var rect = svgenter
          .append("g")
          .attr("transform", "translate(0," + monthTotalHeight + ")")
        .selectAll(".day")
          .data(function (d) { return d3.time.days(new Date(d, 0, 1), new Date(d + 1, 0, 1)); });
      rect
        .enter().append("rect")
          .attr("class", "day")
          .attr("width", cellSize)
          .attr("height", cellSize)
          .attr("x", function (d) { return week(d) * cellSize; })
          .attr("y", function (d) { return day(d) * cellSize; })
          .datum(format);
      rect.exit().remove();


      // Tooltip for all (shows date every time)
      rect.append("title")
          .text(function (d) { return d; });

      // Monthly data
      var monthData = d3.nest()
          .key(function (x) { return new Date(x.key).getFullYear(); })
          .key(function (x) { return (new Date(x.key)).getMonth(); })
          .rollup(function (xs) { return d3.sum(xs, function (x) { return x.value; }); })
          .map(data.entries().filter(function (pair) { var y = new Date(pair.key).getFullYear(); return y >= start && y < end; }));

      function getMonthData(d) {
          var months = monthData[d.getFullYear()] || {};
          return months[d.getMonth()] || 0;
      }

      var monthTotal = svgenter.selectAll(".monthTotal")
          .data(function (d) { return d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1)); });
      monthTotal.enter().append("text")
          .attr("class", "monthTotal")
          .attr("x", function (d) { return week(d3.time.monday.ceil(d)) * cellSize; });
      monthTotal
          .text(function (d) {
              var dist = getMonthData(d);
              return dist > 0 ? d3.time.format("%b")(d) + ": " + d3.format(".0f")(dist) : "";
          });

          // Draw the month path
          svgenter.append("g")
              .attr("transform", "translate(0," + monthTotalHeight + ")")
            .selectAll(".month")
              .data(function (d) { return d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
            .enter().append("path")
              .attr("class", "month")
              .attr("d", monthPath);

          // Tooltip for non-empty. e.g. "2014-03-23: 50.5"
          rect.filter(function (d) { return data.has(d); })
              .attr("class", function (d) { return "day " + color(data.get(d)); })
              .select("title")
              .text(function (d) { return d + " (" + d3.time.format("%a")(new Date(d)) + "): " + distance(data.get(d)); });


      // Weekly data
      var weekData = d3.nest()
          .key(function (x) { return new Date(x.key).getFullYear(); })
          .key(function (x) { return d3.time.mondayOfYear(new Date(x.key)); })
          .rollup(function (xs) { return d3.sum(xs, function (x) { return x.value; }); })
          .map(data.entries().filter(function (pair) { var y = new Date(pair.key).getFullYear(); return y >= start && y < end; }));
      var weekMax = d3.max( d3.values(weekData).map(d3.values).map(function (x) { return d3.max(x); }));
      var weekScale = d3.scale.linear().domain([0, weekMax]).range([barHeight, 0]).nice(50);

      function getWeekData(d){
          var weeks = weekData[d.getFullYear()] || {};
          return weeks[d3.time.mondayOfYear(d)] || 0;
      }
      function weekBarHeight(d) {
          return weekScale(getWeekData(d));
      };

      var yAxis = d3.svg.axis()
       .scale(weekScale)
       .ticks(5)
       .orient("left");

  d3.selectAll("g.weekAxis").remove();
  if (showWeeks){

      svg.select("g").append("g")
          .attr("class", "weekAxis")
          .attr("transform", "translate(0," + (height + margin.midBreak) + ")")
          .call(yAxis);
  }

      // Draw weekly bars
      d3.selectAll(".week").remove();
      var weeks = svg.select("g").selectAll(".week")
          .data(function (d) { return d3.time.mondays(new Date(d, 0, 1), new Date(d + 1, 0, 1)); });
    if (showWeeks){
      weeks.enter().append("rect")
          .attr("class", "week")
          .attr("width", cellSize)
          .attr("height", function (x) { return  barHeight - weekBarHeight(x); })
          .attr("x", function (d) { return week(d) * cellSize; })
          .attr("y", function (d) { return height + margin.midBreak + weekBarHeight(d); })
          .append("title")
          .text(function (d) {
            return d.getFullYear() + ", week " + d3.time.mondayOfYear(d) + ": " + distance(getWeekData(d))
          });
      weeks.exit().remove();

    }

  }
  // Key
  function text(d) {
      var cls = colorClasses[d];
      var extent = color.invertExtent(cls);
      var fmt = d3.format(".0f");
    return fmt(extent[0]||0) + "-" + fmt(extent[1]||100 /* || d3.round(max+5, -1)*/) + " km";
  }

  function makeKey() {
    d3.select("svg.key").remove();
    var key = d3.select("body").append("svg")
        .attr("height", "20px")
        .attr("width", "1000px")
        .attr("class", "hcl2 key");

    var weekMargin = 110;
    key.append("text")
        .text("Key (total/day):")
        .attr("x", 30)
        .attr("y", 15);


    key.selectAll("rect")
        .data(d3.range(colorClasses.length))
      .enter()
        .append("rect")
        .attr("width", 17)
        .attr("height", 17)
        .attr("class", function (d) { return colorClasses[d]; })
        .attr("x", function (d) { return weekMargin + d * 80; })
        .attr("y", function (d) { return 5; })
        .append("title").text(text);

    key.selectAll(".keyText")
        .data(d3.range(colorClasses.length))
        .enter()
        .append("text")
        .attr("class", "keyText")
        .attr("x", function (d) { return weekMargin + d * 80 + 22; })
        .attr("y", function (d) { return 15; })
        .text(text);
  }

  return {
    createChart: createChart,
    makeKey: makeKey
  };
});
