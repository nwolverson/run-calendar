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

var token = "90c08839b0254904d57d81d6ff3768f54a1a8767";

require(["strava", "d3", "jsonp"], function (strava, d3, jsonp) {

  var item = strava.fromLatest(token);
  // TODO copied
  var providers = {};
  strava.providers.forEach(function (p) {
    providers[p.key] = p.value;
  });

  providers[item.type].loadFile(item, 1, function (err, data) {
    window.callPhantom(data);
  });

});
