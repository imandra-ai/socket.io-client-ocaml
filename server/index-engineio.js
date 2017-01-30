var engine = require('engine.io');
console.log('serving on 3001');
var server = engine.listen(3001);

server.on('connection', function(socket){
    // socket.send('utf 8 string');
    // socket.send(new Buffer([0, 1, 2, 3, 4, 5])); // binary data
    console.log('connection', arguments);
    socket.on('packet', function() {
        console.log('packet', arguments);
    });
    socket.on('packetCreate', function() {
        console.log('packetCreate', arguments);
    });
    socket.on('error', function() {
        console.log('error', arguments);
    });
    socket.on('message', function(data) {
        console.log('message', arguments);
        socket.send('thank you for your message, you sent: ' + data);
    });
    socket.on('close', function() {
        console.log('close', arguments);
    });
    socket.on('flush', function() {
        console.log('flush', arguments);
    });
    socket.on('drain', function() {
        console.log('drain', arguments);
    });
});
