express = require('express')
app = express()
server = require('http').Server(app)
expressWs = require('express-ws')(app, server)
minify = require('express-minify')
fs = require('fs')

server.listen 8000

app.use minify()
app.use express.static('public')

app.get '/', (req, res) ->
    res.sendfile __dirname + '/index.html'
    return

app.ws '/', (ws, req) ->

mainWs = expressWs.getWss('/')

feeds = []
fs.readFile 'update.log', 'utf-8', (err, data) ->
    feeds = data.split("\n")

action = () ->
    mainWs.clients.forEach (client) ->
        # random feeds
        client.send(feeds[Math.floor(Math.random() * feeds.length)])

setInterval action, 5000

