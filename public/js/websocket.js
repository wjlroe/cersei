var BuildGroup = function(name, projects) {
  this.name = name;
  this.projects = ko.observableArray(projects);
};

var ws = new Object;

function send()
{
  ws.send("hello world!");
  console.log('Message sent');
}

function open()
{
  if (!("WebSocket" in window)) {
    alert("This browser does not support WebSockets");
    return;
  }
  /* @todo: Change to your own server IP address */
  ws = new WebSocket("ws://localhost:10100/websocket");
  ws.onopen = function() {
    console.log('Connected');
  };
  ws.onmessage = function (evt)
  {
    var received_msg = evt.data;
    console.log("Received: " + received_msg);
    var txt = document.createTextNode("Simon says: " + received_msg);
    document.getElementById('msgs').appendChild(txt);
  };
  ws.onclose = function()
  {
    console.log('Connection closed');
  };
}

window.onload = function() {
  open();
};
