// Generated by CoffeeScript 1.10.0
(function() {
  var action, app, express, expressWs, feeds, fs, mainWs, minify, server;

  express = require('express');

  app = express();

  server = require('http').Server(app);

  expressWs = require('express-ws')(app, server);

  minify = require('express-minify');

  fs = require('fs');

  server.listen(8000);

  app.use(minify());

  app.use(express["static"]('public'));

  app.get('/', function(req, res) {
    res.sendfile(__dirname + '/index.html');
  });

  app.ws('/', function(ws, req) {});

  mainWs = expressWs.getWss('/');

  feeds = [];

  fs.readFile('update.log', 'utf-8', function(err, data) {
    return feeds = data.split("\n");
  });

  action = function() {
    return mainWs.clients.forEach(function(client) {
      return client.send(feeds[Math.floor(Math.random() * feeds.length)]);
    });
  };

  setInterval(action, 5000);

}).call(this);
