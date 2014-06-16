/// <reference path="d3.v3.js" />

var margin = { top: 10, bottom: 10, midBreak: 10, left: 40, right: 18 },
    cellSize = 17,
    barHeight = cellSize * 3,
    width = 960,
    height = cellSize * 7 + 1,
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

function sumDailyRuns(d) {
    var distance = 0;
    for (var i = 0; i < d.length; i++) {
        if (d[i].Type !== "Run") continue;
        var multiplier = 1.0;
        if (d[i].DistanceUnit === "Mile") {
            multiplier = 1.609;
        }
        else if (d[i].DistanceUnit === "Meter") {
            multiplier = 1 / 1000;
        }
        distance += d[i].Distance * multiplier;
    }
    return distance;
}

function createChart(data, start, end) {
    var colorClasses = d3.range(10).map(function (d) { return "q" + d; });
    colorClasses.reverse()
    var max = d3.max(d3.values(data));

    var scaledRange = d3.scale.quantize().domain([0, max]);
    var fixedRange = d3.scale.threshold()
        .domain([5, 10, 15, 20, 25, 30, 40, 50, 70]);

    var color = fixedRange.range(colorClasses);

    // Create the rows
    var svg = d3.select("body").selectAll("svg")
        .data(d3.range(start, end))
        .enter().append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", totalHeight + margin.top + margin.bottom)
        .attr("class", "hcl2")
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // Year text on left side (vertical)
    svg.append("text")
        .attr("transform", "translate(-6," + cellSize * 3.5 + ")rotate(-90)")
        .style("text-anchor", "middle")
        .text(function (d) { return d; });

    // Create the cells
    var rect = svg.selectAll(".day")
        .data(function (d) { return d3.time.days(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
        .enter().append("rect")
        .attr("class", "day")
        .attr("width", cellSize)
        .attr("height", cellSize)
        .attr("x", function (d) { return week(d) * cellSize; })
        .attr("y", function (d) { return day(d) * cellSize; })
        .datum(format);


    // Tooltip for all (shows date every time)
    rect.append("title")
        .text(function (d) { return d; });

    
    // Weekly data
    var weekData = d3.nest()
        .key(function (x) { return new Date(x.key).getFullYear(); })
        .key(function (x) { return d3.time.mondayOfYear(new Date(x.key)); })
        .rollup(function (xs) { return d3.sum(xs, function (x) { return x.value; }); })
        .map(d3.entries(data).filter(function (pair) { var y = new Date(pair.key).getFullYear(); return y >= start && y < end; }));
    var weekMax = d3.max( d3.values(weekData).map(d3.values).map(function (x) { return d3.max(x); }));
    var weekScale = d3.scale.linear().domain([0, weekMax]).range([barHeight, 0]).nice(20);

    function getWeekData(d){
        return weekData[d.getFullYear()][d3.time.mondayOfYear(d)] || 0;
    }
    function weekBarHeight(d) {
        return weekScale(getWeekData(d));
    };
    
    var yAxis = d3.svg.axis()
     .scale(weekScale)
     .orient("left");

    svg.append("g")
        .attr("class", "weekAxis")
        .attr("transform", "translate(0," + (height + margin.midBreak) + ")")
        .call(yAxis);

    // Draw weekly bars
    var weekRect = svg.selectAll(".week")
        .data(function (d) { return d3.time.mondays(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
        .enter().append("rect")
        .attr("class", "week")
        .attr("width", cellSize)
        .attr("height", function (x) { return  barHeight - weekBarHeight(x); })
        .attr("x", function (d) { return week(d) * cellSize; })
        .attr("y", function (d) { return height + margin.midBreak + weekBarHeight(d); })
        .append("title")
        .text(function (d) { return d.getFullYear() + ", week " + d3.time.mondayOfYear(d) + ": " + distance(weekData[d.getFullYear()][d3.time.mondayOfYear(d)]) });

    // Draw the month path
    svg.selectAll(".month")
        .data(function (d) { return d3.time.months(new Date(d, 0, 1), new Date(d + 1, 0, 1)); })
        .enter().append("path")
        .attr("class", "month")
        .attr("d", monthPath);

    // Tooltip for non-empty. e.g. "2014-03-23: 50.5"
    rect.filter(function (d) { return d in data; })
        .attr("class", function (d) { return "day " + color(data[d]); })
        .select("title")
        .text(function (d) { return d + " (" + d3.time.format("%a")(new Date(d)) + "): " + distance(data[d]); });

    // Key
    function text(d) {
        var cls = colorClasses[d];
        var extent = color.invertExtent(cls);
        var fmt = d3.format(".0f");
        return fmt(extent[0]||0) + "-" + fmt(extent[1] || d3.round(max+5, -1)) + " km";
    }

    var key = d3.select("body")
        .append("svg")
        .attr("height", "20px")
        .attr("width", "1000px")
        .attr("class", "hcl2 key");
    key.selectAll("rect")
        .data(d3.range(colorClasses.length))
        .enter()
        .append("rect")
        .attr("width", 17)
        .attr("height", 17)
        .attr("class", function (d) { return colorClasses[d]; })
        .attr("x", function (d) { return 30 + d * 80; })
        .attr("y", function (d) { return 5; })
        .append("title").text(text);
    
    key.selectAll("text")
        .data(d3.range(colorClasses.length))
        .enter()
        .append("text")
        .attr("x", function (d) { return 30 + d * 80 + 22; })
        .attr("y", function (d) { return 15; })
        .text(text);
}

d3.tsv("log.txt", function (error, csv) {
    var data = d3.nest()
      .key(function (d) { return d.Date; })
      .rollup(sumDailyRuns)
      .map(csv.filter(function (x) { return x.Type === "Run"; }));

    var start = new Date(csv[0].Date).getFullYear();
    var end = new Date(csv[csv.length - 1].Date).getFullYear() + 1;

    // limit
    start = Math.max(end - 3, start);

    createChart(data, start, end);
});

d3.select(self.frameElement).style("height", "2910px");
