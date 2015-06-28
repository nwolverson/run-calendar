var page = require("webpage").create();
page.viewportSize = { width: 70 , height: 30};


page.onConsoleMessage = function(msg, lineNum, sourceId) {
  console.log(msg);
};
page.onCallback = function (code) {
  phantom.exit(code);
};

page.open("http://localhost:8123/test.html", function(status) {
    page.evaluate(function() {
       window.phantom = window.phantom || {};
       window.phantom.exit = function (code) {
         callPhantom(code);
       }
      testMain();
    });
});
