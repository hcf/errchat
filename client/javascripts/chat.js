angular.module('chat', ['chat.websocket']);

function ChatController($scope, websocket) {


	$scope.users = [
	];

	$scope.me = {
		name: "etz"
	};

	$scope.messages = [
	];

	$scope.addMessage = function() {

		var message = {
			by: $scope.me,
			text: $scope.newMessageText
		};

		//$scope.messages.push({by: $scope.me, text: $scope.newMessageText});
    	$scope.newMessageText = "";

    	sendMessage("new_message", message);
	};

	// Websocket callbacks

	addUser = function(user) {
		$scope.$apply(function() {
			$scope.users.push(user);
		});
	}

	addMessage = function(message) {
		$scope.$apply(function() {
			$scope.messages.push(message);
		});
	}


	// Websocket communication

	sendMessage = function(eventName, data) {
		console.log("Sending message: " +eventName + " data");
        websocket.send(JSON.stringify({
          "event": eventName,
          "data":  data
        }));
	};

	websocket.onopen = function() {
      console.log("Connected via websocket");

      sendMessage("me", $scope.me);
    }

	websocket.onmessage = function (event) {  

        var received_data = event.data;  

        console.log("Received: " + received_data);  

       	var json = JSON.parse(event.data);


       	if (json.event === "new_user") {
       		addUser(json.data);
       	}

       	if (json.event === "new_message") {
       		addMessage(json.data);
       	}

       	if (json.event === "welcome") {
       		addMessage({
       			by: {name: "_server_"}, 
       			text: json.data.text
       		});
       	}



    };  

}
