express = require('express')
app = express()
server = require('http').Server(app)
io = require('socket.io')(server)
nsq = require('nsqjs')
r = require('rethinkdb')
minify = require('express-minify')


server.listen 8000

app.use minify()
app.use express.static('public')

app.get '/', (req, res) ->
    res.sendfile __dirname + '/index.html'
    return

sendData = (socket) ->
    r.connect({db: "dadog", host: "rethinkdb-driver"})
        .then (conn) ->
            r.db('dadog')
                .table('bets')
                .filter(r.row('time').gt(r.now()))
                .filter(r.row('league').eq('NBA'))
                .group('competition_token')
                .orderBy(r.asc(r.row('meta')('created_at'))).run(conn)
        .then (groups) ->
            groups.each (err, group) ->
                throw err if err
                bets = group['reduction']
                bets.forEach (bet) ->
                    socket.emit 'update', bet
        .catch (err) ->
            console.log err
        .done()

io.on 'connection', (socket) ->

    reader = new (nsq.Reader)('update', 'site',
        # lookupdHTTPAddresses: 'nsqlookupd:4161'
        nsqdTCPAddresses: 'nsqd:4150')

    sendData(socket)

    reader.connect()
    reader.on 'message', (msg) ->
        j = msg.json()
        if j.league != 'nba'
            msg.finish()
            return

        io.emit 'update', j
        msg.finish()
        return
    return

# ---
# generated by js2coffee 2.1.0
