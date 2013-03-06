// Holds a list of connections. {name: "actual name"}
var connections = [];

var WebSocketServer = require('ws').Server;
var posix = require('posix');

var wss = new WebSocketServer({port: 10100});

console.log("started websocket server, listening on port 10100");

wss.on('connection', function(ws) {

    ws.on('message', function(message) {

    	var json = JSON.parse(message);

    	var eventName = json.event;

    	var data = json.data;

    	if (eventName === "users") {
    		ws.send('{"event": "users", "data": [{"name": "etz"}]}');
    	}

    	else if (eventName === "new_message") {
    		connections.forEach(function(connection) {
    			connection.send(message);
    		})
    	}

    	else if (eventName === "me") {
            
    	}

    	else {
    		console.log("received unknown event");
    	}

    });

	connections.push(ws);

    ws.send('{"event": "welcome", "data": {"text": "Welcome"}}');
});