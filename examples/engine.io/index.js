/*
 * To run the test server:
 * npm install
 * DEBUG=engine* node index.js
*/

var express = require('express');
var app = express();
var server = require('http').createServer(app);
// Un-comment to test opening with the websocket transport.
// var io = require('engine.io').attach(server, {transports: ['websocket']});
var io = require('engine.io').attach(server);

app.get('/', function(req, res){
    res.sendFile(__dirname + '/index.html');
});
app.get('/engine.io.js', function(req, res){
    res.sendFile(__dirname + '/node_modules/engine.io-client/engine.io.js');
});

io.on('connection', function(socket){
    console.log('new connection', socket.id);
    socket.on('message', function(data) {
        console.log(socket.id, 'message', arguments);
        socket.send('thank you for your message, you sent: ' + data);
    });
});

server.listen(3001, function() {
    console.log('listening on *:3001');
});
