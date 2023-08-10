var data=[["cat","coyote (PEE)",2],
["deer","coyote (PEE)",7],
["oppossum","coyote (PEE)",2],
["rabbit","coyote (PEE)",1],
["Rabbit-like","coyote (PEE)",0],
["raccoon","coyote (PEE)",21],
["Rodent-like","coyote (PEE)",0],
["skunk","coyote (PEE)",5],
["squirrel","coyote (PEE)",8],
["cat","coyotes (Obs)",1],
["deer","coyotes (Obs)",0],
["oppossum","coyotes (Obs)",0],
["rabbit","coyotes (Obs)",2],
["Rabbit-like","coyotes (Obs)",3],
["raccoon","coyotes (Obs)",1],
["Rodent-like","coyotes (Obs)",13],
["skunk","coyotes (Obs)",0],
["squirrel","coyotes (Obs)",1]]



 function sort(sortOrder){
                    return function(a,b){ return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b)) }
                  }
var color = {'Unlinked':'#3366CC','coyote (PEE)':'rgb(56,43,61)','coyotes (Obs)':'rgb(56,43,61)'};




var g1 = svg.append("g").attr("transform","translate(224,50)");
                         var bp1=viz.bP()
                         .data(data)
                         .value(d=>d[2])
                         .min(10)
                         .pad(1)
                         .height(400)
                         .width(200)
                         .barSize(35)
                         .fill(d=>color[d.secondary])
.orient("vertical");

g1.call(bp1);g1.append("text")
                        .attr("x",-50).attr("y",-8)
                        .style("text-anchor","middle")
                        .text("Primary");
                        g1.append("text")
                        .attr("x", 250)
                        .attr("y",-8).style("text-anchor","middle")
                        .text("Secondary");
                        g1.append("text")
                        .attr("x",100).attr("y",-25)
                        .style("text-anchor","middle")
                        .attr("class","header")
                        .text("Site");

 g1.selectAll(".mainBars")
                        .on("mouseover",mouseover)
                        .on("mouseout",mouseout);

 g1.selectAll(".mainBars").append("text").attr("class","label")
                        .attr("x",d=>(d.part=="primary"? -33.2:35.6))
                        .attr("y",d=>+6)
                        .text(d=>d.key)
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start"));

 g1.selectAll(".mainBars").append("text").attr("class","perc")
                        .attr("x",d=>(d.part=="primary"? -171:198))
                        .attr("y",d=>+6)
                        .text(function(d){ return d3.format("0.0%")(d.percent)})
                        .attr("text-anchor",d=>(d.part=="primary"? "end": "start")); 

function mouseover(d){
bp1.mouseover(d);
                            g1.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.0%")(d.percent)});
}

                     function mouseout(d){
bp1.mouseout(d);
                            g1.selectAll(".mainBars")
                            .select(".perc")
                            .text(function(d){ return d3.format("0.0%")(d.percent)});
}


