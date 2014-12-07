define(function() {
  function parseStravaData(json) {
    var data = d3.nest()
      .key(function (a) {
        var d = new Date(a["start_date"]);
        return d3.time.format("%Y-%m-%d")(d);
      })
      .rollup(function (runs) {
        var total = 0;
        runs.forEach(function (r) { total += r.distance; });
        return total / 1000;
      })
      .map(json);
    return data;
  }

  function fetchStravaLatest(datum, page, callback, acc) {
    acc = acc || [];
    var url = "https://www.strava.com/api/v3/athlete/activities?per_page=200" +
      "&page=" + page +
      "&access_token=" + datum.token + "&callback={callback}";

    d3.jsonp(url, function(data) {
      //var start = data[data.length-1].start_date;
      //var end = data[0].start_date;
      acc = acc.concat(data);
      if (data.length < 200) {
        callback(null, acc);
      } else {
        fetchStravaLatest(datum, page+1, callback, acc);
      }
    });
  }

  function fetchStravaYTD(datum, callback) {
    var url = "https://www.strava.com/api/v3/athlete/activities?after=" +
      (new Date(new Date().getFullYear(), 0, 0).getTime() / 1000) +
      "&access_token=" + datum.token  + "&callback={callback}";

    d3.jsonp(url, function(data) {
      callback(null, data);
    });
  }
  /*
  "Strava Latest": ,

  */
  function makeItem(key, token) {
    return { type: key, file: (new Date()).toDateString(), token: token }
  }

  return {
    providers: [
      { key: "Strava", value: { loadFile: function (d, cb) { d3.json(d.file, cb); }, parse: parseStravaData } },
      { key: "Strava Latest", value: { loadFile: fetchStravaLatest, parse: parseStravaData } },
      { key: "Strava YTD", value: { loadFile: fetchStravaYTD, parse: parseStravaData } }
    ],

    fromLatest: function (token) {
      return makeItem("Strava Latest", token);
    },
    fromYTD: function (token) {
      return makeItem("Strava YTD", token);
    },
    fromFile: function (fileName) {
      return { type: "Strava", file: fileName }
    }
  };
});
