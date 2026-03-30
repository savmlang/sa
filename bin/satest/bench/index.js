function calcCost() {
  let ctr = 0;

  while (ctr < 1000000) {
    ctr += 1;
  }
}

let i = 0;
while (i < 1_000_000) {
  i += 1;

  console.time("processData");
  calcCost();
  console.timeEnd("processData");
}
