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

page.open("http://localhost:8000", function(status) {
    page.evaluate(function(date) {
      PS.CalendarChart_Main.mainWeek(new Date(date))();
    }, date);
});

page.onCallback = function(isError) {
  if (isError) {
    console.log("Error, not generating weekly graph.")
    phantom.exit();
  }

  var output = page.evaluate(function() {
    d3.select("body").style("margin", 0);
    return d3.select("div.weekchart").html();
  });

  var outFile = "output/week-" + date;

  page.render(outFile + ".png");

  var fs = require("fs");
  fs.write(outFile + ".html", output, "w");

  phantom.exit();

};
