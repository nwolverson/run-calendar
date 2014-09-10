define(["queue", "strava", "runningahead"], function (queue, strava, ra) {
  var providers = {};
  [].concat(strava.providers, ra.providers).forEach(function (p) {
    providers[p.key] = p.value;
  });

  // TODO think about local storage.
  function loader(provider) {
    return function(datum, callback) {
      if (datum.cache) {
        callback(null, datum.cache);
      } else {
        providers[provider].loadFile(datum, function (error, data) {
          var result = providers[provider].parse(data);
          datum.cache = result; // TODO parsed or unparsed? may matter with local storage
          callback(error, result);
        })
      }
    };
  }

  function combineMaps(a, b) {
    var map = d3.map(a);
    d3.entries(b).forEach(function (e) {
      map.set(e.key, e.value);
    });
    return map;
  }

  function loadData(data, callback) {
    if (data.length > 0) {
      var q = queue();
      data.forEach(function (d) { q.defer(loader(d.type), d); });
      q.awaitAll(function (error, results) {
        callback(results.reduce(combineMaps, {}));
      });
    }
  }

  return {
    providers: providers,
    loadData: loadData
  }
});
