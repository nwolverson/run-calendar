var page = require("webpage").create();
page.viewportSize = { width: 70 , height: 30};

var system = require("system");
var args = system.args;

if (args.length < 2)
{
  console.log("Usage: phantomjs generate.js 2014-09-14");
  phantom.exit();
}

page.onResourceRequested = function (request) {
    console.log('Request ' + JSON.stringify(request, undefined, 4));
};
page.onError = function (msg, trace) {
    console.log(msg);
    trace.forEach(function(item) {
        console.log('  ', item.file, ':', item.line);
    });
};

var date = args[1];
console.log("Generating for week: "+ date);

page.open("http://localhost:8000/index.html", function(status) {
    page.evaluate(function(date) {
      ChartWeek(new Date(date));
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
