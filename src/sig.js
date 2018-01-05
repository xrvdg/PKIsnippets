s = new sigma(document.getElementById('sg'));

var config = {
    nodeMargin: 3.0,
    scaleNodes: 1.3
};

// Configure the algorithm
var listener = s.configNoverlap(config);

// Bind all events:
listener.bind('start stop interpolate', function(event) {
    console.log(event.type);
});
