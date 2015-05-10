var secret = "8c25ab98336640a94f332b31877a1d5f8ffb7544"; // TODO move to config!!!

var finalhandler = require('finalhandler')
var serveStatic = require('serve-static')
var http = require('http');

var serve = serveStatic('/Users/nicholaw/git/run-calendar/dist/', {'index': ['interactive.html']});

http.createServer(function (req, res) {
  var url = require('url').parse(req.url, true);

  if (url.pathname === "/token_exchange")
{
  var q = url.query;
  var request = require('request');

  request.post("https://www.strava.com/oauth/token", {
    form: {
      client_id: 2746,
      client_secret: secret,
      code: q.code
    }
  }, function (error, response, body) {
      res.writeHead(200, {'Content-Type': 'text/html'});

      var data = JSON.parse(body);

      var response = "<html><body><script>window.opener.PS.CalendarChart_Main.downloadedStrava('" + data.access_token + "')(); window.close()</script></body></html>"

      res.end(response);

    });
  }
  else {
    var done = finalhandler(req, res);
    serve(req, res, done);
  }
}).listen(8123, '127.0.0.1');
