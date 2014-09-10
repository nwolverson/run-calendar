var page = require("webpage").create();
page.viewportSize = { width: 70 , height: 30};

var system = require("system");
var args = system.args;

if (args.length < 2)
{
  console.log("Usage: phantomjs generate.js 2014-09-14");
  phantom.exit();
}

var date = args[1];
console.log("Generating for week: "+ date);

page.open("weekly.html", function(status) {
    page.evaluate(function(date) {
      require(["weeklychart"], function (weekly) {
        weekly.chartWeek(date, function() { window.callPhantom(); });
      })
    }, date);
});

page.onCallback = function(data) {

  var output = page.evaluate(function() {
    d3.select("body").style("margin", 0);
    return d3.select("div.chart").html();
  });

  var outFile = "output/week-" + date;

  page.render(outFile + ".png");

  var fs = require("fs");
  fs.write(outFile + ".html", output, "w");

  phantom.exit();

};
