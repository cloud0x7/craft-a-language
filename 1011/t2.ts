function circleArea(r : number):number{
  // let area : number = 3.14*r*r;
  let area : number = 3*r*r;
  return area;
}
let r:number =4;
println("r=" + r +", area="+circleArea(r));
r = 5;
println("r=" + r +", area="+circleArea(r));
