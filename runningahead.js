define(function() {
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

  function parseRAData(tsv) {
    return d3.nest()
      .key(function (d) { return d.Date; })
      .rollup(sumDailyRuns)
      .map(tsv.filter(function (x) { return x.Type === "Run"; }));
  }

  function loadRaFileReader(datum, callback) {
    reader = new FileReader();
    reader.onload = function (event) {
      callback(null, d3.tsv.parse(reader.result));
    };
    reader.readAsText(datum.fileObj);
  }

  return {
    providers: [
        { key: "RA Upload", value: { loadFile: loadRaFileReader, parse: parseRAData } },
        { key: "RA", value: { loadFile: function (d, cb) { d3.tsv(d.file, cb); }, parse: parseRAData } },
    ],
    fromFile: function (file) { return { file: file, type: "RA" } },
    fromFileUpload: function (file) { return { file: file.name, type: "RA Upload", fileObj: file }; }
  };
});
