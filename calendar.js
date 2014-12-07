/// <reference path="d3.v3.js" />

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

require(["d3", "jsonp", "strava", "runningahead", "calendarchart", "dataloader"],
  function (d3, _jsonp, strava, ra, chart, dataloader) {

var allData = [];
var data = allData;

function getValue(d) { return d.type + "_" + d.file; }
function getText(d) { return d.type + ": " + d.file; }

function dataLoaded(data) {
  var dates = data.keys().sort();

  var start = new Date(dates[0]).getFullYear();
  var end = new Date(dates[dates.length - 1]).getFullYear() + 1;

  // Number of Years shown dropdown
  var years = d3.select("#years").selectAll("option")
      .data(d3.range(start,end).reverse());
  years.enter().append("option")
      .attr("value", function (x) { return x;})
      .html(function (x) { return end-x;});
  years.exit().remove();

  // Update on user input change
  var update = function() {
    var showStart = d3.select("#years").property("value");
    var showWeeks = d3.select("#showweeks").property("checked");

    chart.createChart(data, showStart, end, showWeeks);
  };
  d3.select("#years").on("change", update);
  d3.select("#showweeks").on("change", update)
  update();

  chart.makeKey();
};

var ds = d3.select("#data_sources");
function reloadData() {
  data = allData.filter(function (x) {
    var o = ds.selectAll("option")
      .filter(function (_) { return this.value == getValue(x); });
    return o.node().selected;
  });

  // TODO fix update of non-weekly data
  d3.selectAll("svg.year").remove();

  dataloader.loadData(data, dataLoaded);
}

ds.on("change", reloadData);

function setupDataList() {
  ds.selectAll("option").remove();
  allData.forEach(function (d) {
    ds.append("option").attr("value", getValue(d)).text(getText(d));
  });
}
setupDataList();
reloadData();

var filesel = d3.select("#upload_ra");
filesel.on("change", function() {
  var upload = filesel.node(), file = upload.files[0];
  allData.push(ra.fromFileUpload(file));
  setupDataList();
});


var fileselstr = d3.select("#upload_strava");
fileselstr.on("change", function() {
  var upload = fileselstr.node(), file = upload.files[0];
  allData.push(strava.fromFile(file.name)); // TODO weird only works from same dir
  setupDataList();
});

d3.select("#connect_strava").on("click", function () {
  var stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force";
  var popup = window.open(stravaUrl, 'login', 'height=500,width=800');
  if (window.focus) { popup.focus(); }
});

window.loadedStrava = function(token) {
  allData.push(strava.fromLatest(token), strava.fromYTD(token));
  d3.select("#connect_strava").remove();
  setupDataList();
};

// d3.select(self.frameElement).style("height", "2910px");
});
