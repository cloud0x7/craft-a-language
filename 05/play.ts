
function circleArea(a:number):number{
  function inner(b:number):number{
      return b*b;
  }
  return 3.14*inner(a);
}
let r:number =4;
println("r=" + r +", area="+circleArea(r));
r = 5;
println("r=" + r +", area="+circleArea(r));