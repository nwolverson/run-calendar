var page = require("webpage").create();
page.viewportSize = { width: 70 , height: 30};
page.onResourceRequested = function(request) {
  console.log('Request: ' + request.url);
};
page.onError = function (msg, trace) {
    console.log(msg);
    trace.forEach(function(item) {
        console.log('  ', item.file, ':', item.line);
    })
}

page.onCallback = function(data) {
  var output = JSON.stringify(data);
  console.log("Downloaded Strava: " + (data.length) + " entries");

  var fs = require("fs");
  fs.write("strava-output.json", output, "w");

  phantom.exit();
};

page.open("stravadownload.html");
