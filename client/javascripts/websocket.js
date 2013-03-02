'use strict';

angular.module('chat.websocket', []).
  factory('websocket', function ($rootScope) {

    if (!("WebSocket" in window)) {  
      alert("This browser does not support WebSockets");  
      return;  
    }

    var websocket = new WebSocket("ws://localhost:10100");  

    websocket.onclose = function() {  
      console.log("Connection closed");  
    };  

    return websocket;
  });
