var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http, {allowUpgrades: true});

app.get('/', function(req, res){
    res.sendFile(__dirname + '/index.html');
});

io.on('connection', function(socket){
    console.log('a user connected');

    // Test client sending ACKs
    setTimeout(function() {
        socket.emit('pls respond', 'hello', function(answer) {
            console.log('got an answer', arguments);
        });
    }, 1000);

    // Test client requesting ACKs
    socket.on('pls respond too', function(msg, fn) {
        fn('server is responding to your neediness: ' + msg);
    });

    socket.on('chat message', function(msg){
        console.log('message: ' + msg);
        io.emit('chat message', msg);
    });
    socket.on('disconnect', function(){
        console.log('user disconnected');
    });
});

http.listen(3000, function(){
    console.log('listening on *:3000');
});
