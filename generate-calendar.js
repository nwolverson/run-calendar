var page = require("webpage").create();
page.viewportSize = { width: 1018 , height: 140 };

var system = require("system");

page.open("calendar.html", function(status) {
    page.evaluate(function() {
      require(["d3", "dataloader", "calendarchart", "strava", "runningahead"],
        function (d3, dataloader, calendarchart, strava, ra) {
          dataloader.loadData([
              ra.fromFile("log.txt"),
              strava.fromFile("activities.json")
            ], function (data) {
              calendarchart.createChart(data, 2014, 2015, true);
              window.callPhantom();
            });
      });
    });
});

page.onCallback = function(data) {

  var output = page.evaluate(function() {
    d3.select("body").style("margin", 0);
    d3.select("div").remove();
    return d3.select("body").html();
  });

  var outFile = "output/calendar";

  page.render(outFile + ".png");

  var fs = require("fs");
  fs.write(outFile + ".html", output, "w");

  phantom.exit();

};
