// Code goes here
var getData = function(data) {
  var demand = []
  for (var i = data.length - 1; i >= 0; i--) {
    demand.push(
      data[i].MW
    )
  }
  return demand
};

var margin = {
    top: 30,
    right: 20,
    bottom: 70,
    left: 50
  },
    width = 600 - margin.left - margin.right,
    height = 300 - margin.top - margin.bottom;
    

d3.csv("Delhi_India_Halfhourly_Demand.csv", function(data) {
  var demand = getData(data); // 365 days/year x 24 hours/day = 8760 observations
  demand = demand.slice(0, 7 * 24) // 7 days x 24 hours/day = 168 observations
  console.log(data);
  console.log(demand);

// defin global variables
//  var w = 700;
//  var h = w / 2;
  var barPadding = 0;
  var padding = 0

  var yScale = d3.scale.linear()
    .domain([0, d3.max(demand, function(d) {
      return d;
    })])
    .range([height, 0]);

  //var yAxis = d3.svg.axis()
    //.scale(yScale)
    //.orient("left")
    //.ticks(5);
    
    var yAxis = d3.svg.axis()
    .scale(yScale)
    .orient("left");
    
  //var svg = d3.select("body")
    //.append("svg")
    //.attr({
    //  width: w,
    //  height: h,
    //  fill: "rgb(255,100,0)"
    // });
    
    var svg = d3.select("body")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform",
      "translate(" + margin.left + "," + margin.top + ")");
    
  svg.selectAll("rect")
    .data(demand)
    .enter()
    .append("rect")
    .attr({
      x: function(d, i) {
        return i * (width / demand.length);
      },
      y: function(d) {
        return yScale(d);
      }, //height of svg minus data value
      width: width / demand.length - barPadding,
      height: function(d) {
        return height - yScale(d);
      }, //data value
      // fill: function(d) {return "rgb(" + (d * 1/30) + " 0, 0)"; },
    });

 svg.append("g")
  .attr("class", "axis")
  .attr("transform",  "translate(" + padding + " ,0)" )
  .call(yAxis);


});

// alternatively, use this:
//      d3.select("body").selectAll("div")
//        .data(demand)
//        .enter()
//        .append("div")
//        .attr("class", "bar")
//        .style("height", function(d) {
//          var barHeight = d * (1 / 10);
//          return barHeight + "px"
//        });barHeight + "px"
//        });