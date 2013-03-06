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

	addUsers = function(users) {
		$scope.$apply(function() {
			$scope.users.push.apply($scope.users, users);
		});
	}

	addMessage = function(message) {
		$scope.$apply(function() {
			if ($scope.messages.length > 100) {
				$scope.messages.splice(0, 1);
			}
			$scope.messages.push(message);
		});
	}


	// Websocket communication

	sendMessage = function(eventName, data) {
		console.log("Sending message: " +eventName + " data" + data);

		var msg = {
			"event": eventName
		}

		if (data) {
			msg.data = data;
		}

        websocket.send(JSON.stringify(msg));
	};

	websocket.onopen = function() {
      console.log("Connected via websocket");

      sendMessage("me", $scope.me);

      sendMessage("users", {"dummy": "value"});
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

       	if (json.event === "users") {
       		addUsers(json.data);
       	}



    };  

}
